use crate::{File, FileIdGenerator, Item, Parse, ParseError, Phase, PhaseParse, token::Token};
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleTree<P: Phase> {
    pub name: String,
    pub rel_path: String,
    pub file: File<P>,
    pub children: Vec<ModuleTree<P>>,
}

#[derive(Debug)]
pub enum ModuleTreeError {
    Io {
        rel_path: String,
        error: std::io::Error,
    },
    Parse {
        rel_path: String,
        error: ParseError,
    },
    ParseFailed {
        rel_path: String,
    },
    UnconsumedTokens {
        rel_path: String,
        parsed: usize,
        total: usize,
    },
    InvalidModuleName {
        rel_path: String,
    },
}

impl std::fmt::Display for ModuleTreeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ModuleTreeError::Io { rel_path, error } => {
                write!(f, "failed to read {rel_path}: {error}")
            }
            ModuleTreeError::Parse { rel_path, error } => {
                write!(f, "parse error in {rel_path}: {error}")
            }
            ModuleTreeError::ParseFailed { rel_path } => {
                write!(f, "failed to parse module: {rel_path}")
            }
            ModuleTreeError::UnconsumedTokens {
                rel_path,
                parsed,
                total,
            } => write!(
                f,
                "parser did not consume all tokens for {rel_path} ({parsed}/{total})"
            ),
            ModuleTreeError::InvalidModuleName { rel_path } => {
                write!(f, "invalid module name for path: {rel_path}")
            }
        }
    }
}

impl std::error::Error for ModuleTreeError {}

pub fn collect_module_tree_from_rel_path(
    root_rel_path: &str,
) -> Result<ModuleTree<PhaseParse>, ModuleTreeError> {
    let mut file_id_generator = FileIdGenerator::new();
    let root_name =
        module_name_from_path(root_rel_path).ok_or_else(|| ModuleTreeError::InvalidModuleName {
            rel_path: root_rel_path.to_string(),
        })?;
    collect_module_tree_inner(root_rel_path, root_name, &mut file_id_generator)
}

fn collect_module_tree_inner(
    rel_path: &str,
    name: String,
    file_id_generator: &mut FileIdGenerator,
) -> Result<ModuleTree<PhaseParse>, ModuleTreeError> {
    let source = std::fs::read_to_string(rel_path).map_err(|error| ModuleTreeError::Io {
        rel_path: rel_path.to_string(),
        error,
    })?;

    let file_id = file_id_generator.generate_file_id();
    let tokens = Token::lex(&source, file_id);
    let mut i = 0;
    let file = File::<PhaseParse>::parse(&tokens, &mut i)
        .map_err(|error| ModuleTreeError::Parse {
            rel_path: rel_path.to_string(),
            error,
        })?
        .ok_or_else(|| ModuleTreeError::ParseFailed {
            rel_path: rel_path.to_string(),
        })?;
    if i != tokens.len() {
        return Err(ModuleTreeError::UnconsumedTokens {
            rel_path: rel_path.to_string(),
            parsed: i,
            total: tokens.len(),
        });
    }

    let mut children = Vec::new();
    for item in file.items() {
        if let Item::Submodule(submodule) = item {
            let child_name = submodule.name.s().to_string();
            let child_rel_path = submodule_rel_path(rel_path, &child_name);
            let child = collect_module_tree_inner(&child_rel_path, child_name, file_id_generator)?;
            children.push(child);
        }
    }

    Ok(ModuleTree {
        name,
        rel_path: rel_path.to_string(),
        file,
        children,
    })
}

fn module_name_from_path(rel_path: &str) -> Option<String> {
    Path::new(rel_path)
        .file_stem()
        .map(|stem| stem.to_string_lossy().to_string())
}

fn submodule_rel_path(parent_rel_path: &str, submodule_name: &str) -> String {
    let parent_dir = Path::new(parent_rel_path)
        .parent()
        .unwrap_or_else(|| Path::new(""));
    let mut rel_path = PathBuf::from(parent_dir);
    rel_path.push(format!("{submodule_name}.fe"));
    rel_path.to_string_lossy().to_string()
}
