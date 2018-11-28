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
    Predicate,
    LoopSection,
    Section,
    CellInfo,
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
                // Combine consecutive I/O sections by applying the latter to the former
                (Section::Io(prev), Section::Io(next)) => {
                    // Basic sections can only be combined if they have non-conflicting cell writes
                    if let Some(new) = prev.apply(&next) {
                        current = Some(Section::Io(new));
                    } else {
                        new_sections.push(Section::Io(prev));
                        current = Some(Section::Io(next));
                    }
                },
                // TODO: Find a more efficient way of swapping than *bubble sort*
                (Section::Basic(prev), Section::Io(next)) => {
                    if
                        next.get_cell_writes().intersection(Idx(0), &prev.get_cell_reads().union(Idx(0), &prev.get_cell_writes())).is_empty() &&
                        next.get_cell_reads().intersection(Idx(0), &prev.get_cell_writes()).is_empty()
                    {
                        // Swap!
                        new_sections.push(Section::Io(next));
                        current = Some(Section::Basic(prev));
                    } else {
                        new_sections.push(Section::Basic(prev));
                        current = Some(Section::Io(next));
                    }
                },
                // TODO: Add swapping to move inputs to start if possible
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

    const COMBINE_REPEATS: usize = 5;
    for _ in 0..COMBINE_REPEATS {
        new_sections = optimise_sections_combine(new_sections);
    }
    new_sections
}

pub fn optimise_program_analysis(mut prog: Program) -> Program {
    fn optimise_sections(sections: &mut Vec<Section>, cell_info: &mut CellInfo, depth: usize) {
        for section in sections {
            section.simplify_with(&cell_info);

            match section {
                Section::Basic(basic) => {
                    for (idx, change) in &basic.changes {
                        match change {
                            Change::Set(expr) => cell_info.set_cell(
                                depth,
                                *idx,
                                expr.get_info(),
                            ),
                            Change::Incr(expr) => cell_info.incr_cell(
                                depth,
                                *idx,
                                expr.get_info(),
                            ),
                            _ => cell_info.set_cell(
                                depth,
                                *idx,
                                ValInfo::Unknown
                            ),
                            // TODO: Add Change::Incr updating
                        }
                    }
                },
                Section::Io(io) => {
                    for op in &io.ops {
                        match op {
                            IoOp::InputCell(idx) => cell_info.set_cell(depth, *idx, ValInfo::Unknown),
                            IoOp::Output(_) => {},
                        }
                    }
                },
                Section::Loop(luup) => {
                    if let ValInfo::Exactly(0) = luup.get_total_shift() {
                        optimise_sections(&mut luup.sections, cell_info, depth + 1);

                        // Attempt to substitute a while loop with an if statement
                        match luup.predicate {
                            Predicate::Cell(idx) => match cell_info.get_cell(idx) {
                                ValInfo::Exactly(0) => luup.iters = ValInfo::Exactly(1),
                                _ => cell_info.invalidate_depth(depth + 1),
                            },
                            _ => cell_info.invalidate_depth(depth + 1),
                        }
                        // TODO: This line shouldn't be needed
                        cell_info.invalidate_depth(depth + 1);
                    } else {
                        // TODO: Make this faster
                        optimise_sections(&mut luup.sections, &mut CellInfo::luup(), depth + 1);
                        cell_info.invalidate_all();
                        break; // TODO: this shouldn't be needed
                    }
                },
                // TODO: Perform
            }

            if let ValInfo::Exactly(n) = section.get_total_shift() {
                cell_info.incr_ptr(n);
            } else {
                break;
            }
        }
    }

    println!("Performing static analysis...");
    let mut cell_info = CellInfo::root();

    optimise_sections(&mut prog.sections, &mut cell_info, 0);

    prog
}

pub fn optimise_program(mut prog: Program) -> Program {
    prog.sections = optimise_sections(&prog.sections);
    let prog = optimise_program_analysis(prog);
    prog
}

pub fn optimise(mut prog: Program) -> Program {
    const MAX_TRIES: usize = 5;
    for _ in 0..MAX_TRIES {
        let new_prog = optimise_program(prog.clone());
        if prog == new_prog {
            return prog;
        } else {
            prog = new_prog;
        }
    }
    prog
}
