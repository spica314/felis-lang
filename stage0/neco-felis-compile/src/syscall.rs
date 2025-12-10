use crate::error::CompileError;
use neco_felis_syn::*;
use std::collections::HashMap;

pub struct SyscallCompiler;

impl SyscallCompiler {
    pub fn compile_proc_syscall(
        args: &[ProcTerm<PhaseParse>],
        variables: &HashMap<String, i32>,
        output: &mut String,
    ) -> Result<(), CompileError> {
        if args.len() != 6 {
            return Err(CompileError::InvalidSyscall);
        }

        let registers = ["rax", "rdi", "rsi", "rdx", "r10", "r8"];

        for (i, arg) in args.iter().enumerate() {
            match arg {
                ProcTerm::Number(num) => {
                    let number_value = Self::parse_number(num.number.s());
                    output.push_str(&format!("    mov {}, {}\n", registers[i], number_value));
                }
                ProcTerm::Variable(var) => {
                    let var_name = var.variable.s();
                    if let Some(&offset) = variables.get(var_name) {
                        // Load value from stack into register
                        output.push_str(&format!(
                            "    mov {}, qword ptr [rbp - 8 - {}]\n",
                            registers[i],
                            offset - 8
                        ));
                    } else {
                        return Err(CompileError::UnsupportedConstruct(format!(
                            "Unknown variable: {var_name}"
                        )));
                    }
                }
                _ => {
                    return Err(CompileError::InvalidSyscall);
                }
            }
        }

        output.push_str("    syscall\n");
        Ok(())
    }

    pub fn parse_number(number_str: &str) -> String {
        if number_str.ends_with("u64") {
            number_str.trim_end_matches("u64").to_string()
        } else {
            number_str.to_string()
        }
    }

    pub fn load_proc_argument_into_register(
        arg: &ProcTerm<PhaseParse>,
        register: &str,
        variables: &HashMap<String, i32>,
        output: &mut String,
    ) -> Result<(), CompileError> {
        match arg {
            ProcTerm::Number(number) => {
                let value = Self::parse_number(number.number.s());
                output.push_str(&format!("    mov {register}, {value}\n"));
                Ok(())
            }
            ProcTerm::Variable(var) => {
                if let Some(offset) = variables.get(var.variable.s()) {
                    output.push_str(&format!(
                        "    mov {register}, qword ptr [rbp - 8 - {}]\n",
                        offset - 8
                    ));
                } else {
                    output.push_str(&format!("    mov {register}, 0\n"));
                }
                Ok(())
            }
            _ => Err(CompileError::UnsupportedConstruct(format!(
                "Unsupported argument type: {arg:?}"
            ))),
        }
    }
}
