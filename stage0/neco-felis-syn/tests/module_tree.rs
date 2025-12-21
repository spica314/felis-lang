use neco_felis_syn::collect_module_tree_from_rel_path;
use std::path::Path;

#[test]
fn collect_submodule_tree() {
    let manifest_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    let base_dir = manifest_dir.join("..").join("..");
    let root_path = base_dir.join("testcases/felis/packages/submodule/root.fe");
    let tree = collect_module_tree_from_rel_path(&root_path.to_string_lossy())
        .expect("collect module tree");

    assert_eq!(tree.name, "root");
    assert_eq!(tree.children.len(), 1);
    assert_eq!(tree.children[0].name, "sub");
    assert!(tree.children[0].children.is_empty());
}
