use std::collections::HashMap;

use bimap::BiMap;

use super::ir;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Idx(i32);

#[derive(Clone, Debug)]
pub enum Expr {
    Const(i32),
    Sum(Box<Expr>, Box<Expr>),
    Product(Box<Expr>, Box<Expr>),
    Neg(Box<Expr>),
    CellVal(Idx),
    LocalVal(usize),
    Input,
}

#[derive(Clone, Debug)]
pub enum Op {
    SetLocal {
        id: usize,
        val: Expr,
    },
    MovePtr(Expr),
    Output(Expr),
}

#[derive(Clone, Debug)]
pub struct Loop {
    predicate: Expr,
    sections: Vec<Section>,
}

#[derive(Clone, Debug)]
pub struct Proc {
    locals_counter: usize,
    locals: BiMap<usize, Idx>,
    ops: Vec<Op>,
}

impl Proc {
    fn new() -> Self {
        Self {
            locals_counter: 0,
            locals: BiMap::new(),
            ops: vec![],
        }
    }

    fn get_or_create_local(&mut self, idx: Idx) -> usize {
        if let Some(id) = self.locals.get_by_right(&idx) {
            *id
        } else {
            self.locals_counter += 1;
            self.locals.insert(self.locals_counter - 1, idx);
            self.locals_counter - 1
        }
    }
}

#[derive(Clone, Debug)]
pub enum Section {
    Proc(Proc),
    Loop(Loop),
}

#[derive(Clone, Debug)]
pub struct Program {
    sections: Vec<Section>,
}

impl From<Vec<ir::Inst>> for Program {
    fn from(ir: Vec<ir::Inst>) -> Self {
        fn generate_sections(ir: Vec<ir::Inst>) -> Vec<Section> {
            let mut local_id_count = 0;

            let mut sections = vec![];
            let mut cproc = Proc::new();

            for inst in ir {
                match inst {
                    ir::Inst::Move(n) => {
                        cproc.ops.push(Op::MovePtr(Expr::Const(n)));
                    },
                    ir::Inst::Add(r, n) => {
                        let local_id = cproc.get_or_create_local(Idx(r));
                        cproc.ops.push(Op::SetLocal {
                            id: local_id,
                            val: Expr::Sum(
                                Box::new(Expr::LocalVal(local_id)),
                                Box::new(Expr::Const(n))
                            ),
                        });
                    },
                    ir::Inst::SetC(r, n) => {
                        let local_id = cproc.get_or_create_local(Idx(r));
                        cproc.ops.push(Op::SetLocal {
                            id: local_id,
                            val: Expr::Const(n),
                        });
                    },
                    ir::Inst::CopyMul(b, r, n) => {
                        let b_local_id = cproc.get_or_create_local(Idx(b));
                        let r_local_id = cproc.get_or_create_local(Idx(r));
                        cproc.ops.push(Op::SetLocal {
                            id: r_local_id,
                            val: Expr::Sum(
                                Box::new(Expr::LocalVal(r_local_id)),
                                Box::new(Expr::Product(
                                    Box::new(Expr::LocalVal(b_local_id)),
                                    Box::new(Expr::Const(n)),
                                )),
                            ),
                        });
                    },
                    ir::Inst::Output(r) => {
                        let local_id = cproc.get_or_create_local(Idx(r));
                        cproc.ops.push(Op::Output(Expr::LocalVal(local_id)));
                    },
                    ir::Inst::Input(r) => {
                        let local_id = cproc.get_or_create_local(Idx(r));
                        cproc.ops.push(Op::SetLocal {
                            id: local_id,
                            val: Expr::Input,
                        });
                    },
                    ir::Inst::Loop(r, ir) => {
                        let mut proc = Proc::new();
                        std::mem::swap(&mut proc, &mut cproc);
                        sections.push(Section::Proc(proc));

                        sections.push(Section::Loop(Loop {
                            predicate: Expr::CellVal(Idx(r)),
                            sections: generate_sections(ir),
                        }));
                    },
                    i => panic!("Unknown instruction: {:?}", i),
                }
            }

            if cproc.ops.len() > 0 {
                sections.push(Section::Proc(cproc));
            }

            sections
        }

        Self { sections: generate_sections(ir) }
    }
}

impl Program {
    pub fn generate_c(&self) -> String {
        impl Expr {
            fn stringify(&self, proc: Option<&Proc>) -> String {
                match self {
                    Expr::Const(n) => { format!("{}", n) },
                    Expr::LocalVal(id) => { format!("loc_{}", id) },
                    Expr::Sum(a, b) => { format!("({} + {})", a.stringify(proc), b.stringify(proc)) },
                    Expr::Product(a, b) => { format!("({} * {})", a.stringify(proc), b.stringify(proc)) },
                    Expr::CellVal(Idx(idx)) => { format!("ptr[{}]", idx) },
                    Expr::Input => { format!("getchar()") },
                    e => panic!("Unknown expression type: {:?}", e),
                }
            }
        }

        fn stringify_proc(proc: &Proc, depth: usize) -> String {
            let mut code = String::new();

            if proc.ops.len() == 0 {
                return "".to_string();
            }

            for _ in 0..depth { code += "    "; }
            code += "{\n";

            // Read locals
            for (id, Idx(rel)) in proc.locals.iter() {
                for _ in 0..depth { code += "    "; }
                code += &format!("char loc_{} = ptr[{}];\n", id, rel);
            }

            for op in &proc.ops {
                match op {
                    Op::MovePtr(val) => {
                        for _ in 0..depth { code += "    "; }
                        code += &format!("ptr += {};\n", val.stringify(Some(proc)));
                    },
                    Op::SetLocal { id, val } => {
                        for _ in 0..depth { code += "    "; }
                        code += &format!("loc_{} = {};\n", id, val.stringify(Some(proc)));
                    },
                    Op::Output(val) => {
                        for _ in 0..depth { code += "    "; }
                        code += &format!("putchar({});\n", val.stringify(Some(proc)));
                    },
                    o => panic!("Unknown operation: {:?}", o),
                }
            }

            // Write locals back
            for (id, Idx(rel)) in proc.locals.iter() {
                for _ in 0..depth { code += "    "; }
                code += &format!("ptr[{}] = loc_{};\n", rel, id);
            }

            for _ in 0..depth { code += "    "; }
            code += "}\n";

            code
        }

        fn stringify_sections(sections: &Vec<Section>, depth: usize) -> String {
            let mut code = String::new();

            for sect in sections {
                match sect {
                    Section::Proc(proc) => { code += &stringify_proc(&proc, depth); },
                    Section::Loop(lp) => {
                        for _ in 0..depth { code += "    "; }
                        code += &format!("while ({}) {{\n", lp.predicate.stringify(None));

                        code += &stringify_sections(&lp.sections, depth + 1);

                        for _ in 0..depth { code += "    "; }
                        code += "}\n";
                    },
                    _ => panic!("Unknown section type!"),
                }
            }

            code
        }

        let mut code = String::new();

        format!(r#"
#include <stdio.h>

char mem[30000];

int main() {{
    char* ptr = mem;

{}

    return 0;
}}
"#, stringify_sections(&self.sections, 1))
    }
}
