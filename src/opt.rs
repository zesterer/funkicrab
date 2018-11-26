use crate::llir::{
    Idx,
    Diff,
    ValInfo,
    CellAccessInfo,
    Expr,
    Change,
    BasicSection,
    IoOp,
    IoSection,
    Section,
    Program,
};

pub fn optimise_expr(expr: &mut Expr) {
    expr.simplify_local();
}

pub fn optimise_basic_section(mut basic: BasicSection) -> BasicSection {
    for (idx, change) in &mut basic.changes {
        match change {
            Change::Set(expr) => optimise_expr(expr),
            Change::Incr(expr) => optimise_expr(expr),
        }
    }

    basic
        .changes
        .retain(|idx, change| match change {
            Change::Set(expr) => true,
            Change::Incr(expr) => expr.eval_local().map(|val| val.0 != 0).unwrap_or(true),
        });

    basic
}

pub fn optimise_io_section(mut io: IoSection) -> IoSection {
    for op in &mut io.ops {
        match op {
            IoOp::InputCell(_) => {},
            IoOp::Output(expr) => optimise_expr(expr),
        }
    }
    io
}

pub fn optimise_sections_combine(sections: Vec<Section>) -> Vec<Section> {
    let mut new_sections: Vec<Section> = vec![];

    let mut current = None;
    for section in sections {
        if let Some(mut prev) = current.take() {
            match (prev, section) {
                // Combine consecutive basic sections by applying the latter to the former
                (Section::Basic(prev), Section::Basic(next)) => {
                    // Basic sections can only be combined if they have non-conflicting cell writes
                    if let Some(new) = prev.apply(&next) {
                        current = Some(Section::Basic(new));
                    } else {
                        new_sections.push(Section::Basic(prev));
                        current = Some(Section::Basic(next));
                    }
                },
                // Default
                (prev, next) => { new_sections.push(prev); current = Some(next); },
            }
        } else {
            current = Some(section);
        }
    }

    current.take().map(|c| new_sections.push(c));
    new_sections
}

pub fn optimise_sections(sections: &Vec<Section>) -> Vec<Section> {
    let mut new_sections = vec![];

    for mut section in sections {
        let new_section = match section.clone() {
            Section::Basic(basic) => Section::Basic(optimise_basic_section(basic)),
            Section::Io(io) => Section::Io(optimise_io_section(io)),
            Section::Loop(mut luup) => {
                luup.sections = optimise_sections(&luup.sections);
                Section::Loop(luup)
            },
            s => s,
        };

        if !new_section.has_no_effect() {
            match &new_section {
                // Attempt to linearise the loop
                Section::Loop(luup) => {
                    new_sections.append(&mut luup.linearise().unwrap_or(vec![new_section.clone()]))
                },
                _ => new_sections.push(new_section.clone()),
            };
        }
    }

    optimise_sections_combine(new_sections)
}

pub fn optimise_program(mut prog: Program) -> Program {
    prog.sections = optimise_sections(&prog.sections);
    prog
}

pub fn optimise(mut prog: Program) -> Program {
    loop {
        let new_prog = optimise_program(prog.clone());
        if prog == new_prog {
            return prog;
        } else {
            prog = new_prog;
        }
    }
    prog
}
