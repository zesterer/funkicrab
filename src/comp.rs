use crate::ir::Inst;
use super::Error;

pub fn compile(insts: Vec<Inst>) -> Result<String, Error> {
    fn write_body(insts: Vec<Inst>) -> String {
        let mut code = String::new();
        for inst in insts {
            match inst {
                Inst::Move(n) => code.push_str(&format!("ptr += {};\n", n)),
                Inst::Move(0) => {},

                Inst::Add(r, n) => code.push_str(&format!("*(ptr + {}) += ({});\n", r, n)),
                Inst::Add(r, 0) => {},

                Inst::CopyMul(r, i, f) => { code.push_str(&format!("*(ptr + {}) += *(ptr + {}) * ({});\n", i, r, f)); },

                Inst::SetC(r, n) => code.push_str(&format!("*(ptr + {}) = {};\n", r, n)),

                Inst::Output(r) => code.push_str(&format!("putchar(*(ptr + {}));\n", r)),
                Inst::Input(r) => code.push_str(&format!("*(ptr + {}) = getchar();\n", r)),
                Inst::Loop(base, insts) => {
                    code.push_str(&format!("while (*(ptr + {})) {{\n", base));
                    code.push_str(&write_body(insts));
                    code.push_str("}\n");
                },
                Inst::Nop => {},
                i => panic!("Found unexpected instruction: {:?}", i),
            }
        }
        code
    }

    Ok(format!(r#"
    #include <stdio.h>

    char mem[30000];

    int main() {{
        char* ptr = mem;

        {}

        return 0;
    }}

    "#, write_body(insts)))
}
