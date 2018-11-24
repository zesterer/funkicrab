use crate::ir::Inst;
use super::Error;

pub fn compile(insts: Vec<Inst>, ptr: i32, cells: Vec<u8>, outputs: Vec<u8>) -> Result<String, Error> {
    fn write_body(insts: Vec<Inst>, depth: usize) -> String {
        let mut code = String::new();
        for inst in insts {
            for _ in 0..depth { code.push_str("    "); }
            match inst {
                Inst::Move(0) => {},
                Inst::Move(1) => code.push_str(&format!("++ptr;\n")),
                Inst::Move(-1) => code.push_str(&format!("--ptr;\n")),
                Inst::Move(n) if n > 0 => code.push_str(&format!("ptr += {};\n", n)),
                Inst::Move(n) if n < 0 => code.push_str(&format!("ptr -= {};\n", -n)),

                Inst::Add(r, 0) => {},
                Inst::Add(r, n) => {
                    if n == 1 {
                        code.push_str("++");
                    } else if n == -1 {
                        code.push_str("--");
                    }
                    if r == 0 {
                        code.push_str("*ptr");
                    } else {
                        code.push_str(&format!("ptr[{}]", r));
                    }
                    if n > 1 {
                        code.push_str(&format!(" += {}", n));
                    } else if n < -1 {
                        code.push_str(&format!(" -= {}", -n));
                    }
                    code.push_str(";\n");
                },

                Inst::CopyMul(r, i, f) => {
                    if i == 0 {
                        code.push_str("*ptr");
                    } else {
                        code.push_str(&format!("ptr[{}]", i));
                    }
                    if f > 0 {
                        code.push_str(" +=");
                    } else if f < 0 {
                        code.push_str(" -=");
                    }
                    if r == 0 {
                        code.push_str(" *ptr");
                    } else {
                        code.push_str(&format!(" ptr[{}]", r));
                    }
                    code.push_str(&format!(" * {};\n", f.abs()));
                },

                Inst::SetC(r, n) => {
                    if r == 0 {
                        code.push_str("*ptr");
                    } else {
                        code.push_str(&format!("ptr[{}]", r));
                    }
                    code.push_str(&format!(" = {};\n", n));
                },

                Inst::Output(r) => code.push_str(&format!("putchar(ptr[{}]);\n", r)),
                Inst::Input(r) => code.push_str(&format!("ptr[{}] = getchar();\n", r)),
                Inst::Loop(base, insts) => {
                    code.push_str(&format!("while (ptr[{}]) {{\n", base));
                    code.push_str(&write_body(insts, depth + 1));
                    for _ in 0..depth { code.push_str("    "); }
                    code.push_str("}\n");
                },
                Inst::Nop => {},
                i => panic!("Found unexpected instruction: {:?}", i),
            }
        }
        code
    }

    let cell_str = cells.into_iter().map(|c| format!("{}", c)).collect::<Vec<_>>().join(", ");
    let outputs_str = outputs.iter().map(|&c| format!("{}", c)).collect::<Vec<_>>().join(", ");

    Ok(format!(r#"
#include <stdio.h>

char mem[30000] = {{ {} }};

#define OUTPUTS {}
char outputs[OUTPUTS] = {{ {} }};

int main() {{
    char* ptr = mem + {};
    fwrite(outputs, sizeof(char), OUTPUTS, stdout);

{}

    return 0;
}}
"#, cell_str, outputs.len(), outputs_str, ptr, write_body(insts, 1)))
}
