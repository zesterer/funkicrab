use crate::llir::{
    ValInfo,
    CellAccessInfo,
    Program,
    Section,
};

// This optimisation attempts to calculate as much information about the net shift of a section
// as it possibly can. It does this by recursively searching through the section and its children,
// propagating what information it discovers downwards. For non-loops, this is a relatively
// trivial operation since the shifts are known at compile-time and need only by summed and
// propagated appropriately. However, we can also compute useful information for loops:
// - If the shift of a single iteration is 0, we are sure that N iterations also have a shift of 0
// - If the shift of a single iteration is F, we are sure that N iterations have a shift of F * N
pub fn optimise_calc_shifts(section: &mut Section) {
    fn calc_and_apply_shift(section: &mut Section) -> ValInfo {
        match section {
            Section::Loop(luup) => {
                let mut total_shift = ValInfo::Exactly(0);
                for mut section in luup.sections.iter_mut() {
                    total_shift = match (total_shift, calc_and_apply_shift(&mut section)) {
                        (ValInfo::Exactly(total), ValInfo::Exactly(val)) => ValInfo::Exactly(total + val),
                        (ValInfo::MultipleOf { base, factor }, ValInfo::Exactly(val)) => ValInfo::MultipleOf { base: base + val, factor },
                        (ValInfo::Exactly(total), ValInfo::MultipleOf { base, factor }) => ValInfo::MultipleOf { base: total + base, factor },
                        // Fallback to an unknown shift
                        _ => ValInfo::Unknown,
                    };
                }
                luup.shift = match total_shift {
                    ValInfo::Exactly(0) => ValInfo::Exactly(0),
                    ValInfo::Exactly(n) => ValInfo::MultipleOf { base: 0, factor: n },
                    // Everything else has no form that can be expressed by the current ValInfo system
                    // TODO: add base0 + (base1 + N * factor1) + factor0?
                    _ => ValInfo::Unknown,
                };
                luup.shift
            },
            // All other sections always have a deterministic shift
            section => section.get_shift(),
        }
    }

    calc_and_apply_shift(section);
}

pub fn optimise_sections(mut sections: Vec<Section>) -> Vec<Section> {
    let mut new_sections = vec![];
    for mut section in sections {
        optimise_calc_shifts(&mut section);

        if !section.has_no_effect() {
            new_sections.push(section);
        }
    }

    new_sections
}

pub fn optimise_program(mut prog: Program) -> Program {
    prog.sections = optimise_sections(prog.sections);
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
