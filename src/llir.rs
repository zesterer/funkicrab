use std::collections::HashMap;

use super::Token;
use crate::Error;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Idx(i32);
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Diff(i32);

#[derive(Copy, Clone, Debug)]
pub enum ValInfo {
    // Value is (N)
    Exactly(i32),
    // Value is (base + N * factor)
    MultipleOf { base: i32, factor: i32 },
    // No knowledge of value
    None,
}

// A change that occurs to a cell's value during the execution of a basic section. There are
// 2 possible forms this can take - increments (including decrements) of the cell's value or
// resetting the cell to a constant value.
#[derive(Copy, Clone, Debug)]
pub enum Change {
    Incr(Diff),
    Set(u8),
}

// An output from the program. This can be either be outputting a cell's value (plus an
// increment) or a compile-time constant.
#[derive(Copy, Clone, Debug)]
pub enum Output {
    Cell(Idx, Diff),
    Const(u8),
}

// A section that describes a so-called 'basic' section. These sections no looping code, only
// performing 2 operations upon data: increments (including decrements) and outputs. They have a
// net shift that is known at compile-time. Outputs are always intended to be executed at the
// beginning of the section.
#[derive(Clone, Debug)]
pub struct BasicSection {
    outs: Vec<Output>,
    changes: HashMap<Idx, Change>,
    shift: Idx,
}

impl BasicSection {
    fn new() -> Self {
        Self {
            outs: Vec::new(),
            changes: HashMap::new(),
            shift: Idx(0),
        }
    }

    // Increment a cell relative to this section's pointer
    fn add_cell_incr(&mut self, idx: Idx, diff: Diff) {
        if let Some(change) = self.changes.get_mut(&idx) {
            match change {
                Change::Incr(cdiff) => cdiff.0 += diff.0,
                Change::Set(val) => *val = (*val as i32 + diff.0) as u8,
            }
        } else {
            self.changes.insert(idx, Change::Incr(diff));
        }
    }

    // Output a cell relative to this section's pointer
    fn add_cell_output(&mut self, idx: Idx) {
        match self.changes.get_mut(&idx).cloned() {
            Some(Change::Incr(cdiff)) => self.outs.push(Output::Cell(idx, cdiff)),
            Some(Change::Set(val)) => self.outs.push(Output::Const(val)),
            None => self.outs.push(Output::Cell(idx, Diff(0))),
        }
    }

    // Determine whether the BasicSection can be deleted with no adverse effects
    fn has_no_effect(&self) -> bool {
        self.changes.len() == 0 && self.outs.len() == 0 && self.shift.0 == 0
    }
}

// A section that describes a resetting of cells through user input
#[derive(Clone, Debug)]
pub struct InputSection {
    cells: Vec<(Idx, Diff)>,
}

impl InputSection {
    fn new() -> Self {
        Self { cells: Vec::new() }
    }

    fn add_cell_input(&mut self, idx: Idx) {
        self.cells.push((idx, Diff(0)));
    }
}

// A section that describes a loop containing zero or more sub-sections. Loops may have a net
// shift that is incomputable at compile-time. Loops that do have a net shift that is computable
// at compile-time can often be optimised out in future passes.
#[derive(Clone, Debug)]
pub struct LoopSection {
    predicate: Idx,
    sections: Vec<Section>,
    shift: ValInfo,
}

impl LoopSection {
    fn new(predicate: Idx) -> Self {
        Self {
            predicate,
            sections: Vec::new(),
            shift: ValInfo::None,
        }
    }

    fn add_section(&mut self, section: Section) {
        self.sections.push(section);
    }
}

// A section is a segment of brainfuck code after which all temporarily cached values must be
// written back into the cell array to maintain memory consistency. They are designed to be
// easy to perform optimisations upon through static analysis through their nature as a large
// atomic unit of Brainfuck code.
#[derive(Clone, Debug)]
pub enum Section {
    Basic(BasicSection),
    Input(InputSection),
    Loop(LoopSection),
}

impl From<BasicSection> for Section {
    fn from(basic: BasicSection) -> Self { Section::Basic(basic) }
}

impl From<InputSection> for Section {
    fn from(input: InputSection) -> Self { Section::Input(input) }
}

impl From<LoopSection> for Section {
    fn from(luup: LoopSection) -> Self { Section::Loop(luup) }
}

#[derive(Clone, Debug)]
pub struct Program {
    sections: Vec<Section>,
}

impl From<Vec<Token>> for Program {
    fn from(tokens: Vec<Token>) -> Self {
        fn tokens_to_sections(tokens: Vec<Token>) -> Vec<Section> {
            let mut sections = vec![];

            let mut current_bs = BasicSection::new();

            for tok in tokens {
                match tok {
                    Token::Right => current_bs.shift.0 += 1,
                    Token::Left => current_bs.shift.0 -= 1,
                    Token::Inc => current_bs.add_cell_incr(current_bs.shift, Diff(1)),
                    Token::Dec => current_bs.add_cell_incr(current_bs.shift, Diff(-1)),
                    Token::Output => current_bs.add_cell_output(current_bs.shift),
                    Token::Input => {
                        // Push the current basic section onto the section stack
                        let mut other_basic = BasicSection::new();
                        std::mem::swap(&mut current_bs, &mut other_basic);
                        sections.push(other_basic.into());

                        // Add an input section
                        let mut input = InputSection::new();
                        input.add_cell_input(Idx(0));
                        sections.push(input.into());
                    },
                    Token::Loop(toks) => {
                        // Push the current basic section onto the section stack
                        let mut other_basic = BasicSection::new();
                        std::mem::swap(&mut current_bs, &mut other_basic);
                        sections.push(other_basic.into());

                        // Add a loop section
                        // By default, the loop execution is predicated by the cell pointed to
                        // by the current pointer value
                        let mut luup = LoopSection::new(Idx(0));
                        for section in tokens_to_sections(toks) {
                            luup.add_section(section);
                        }
                        sections.push(luup.into());
                    },
                    t => panic!("Unknown token: {:?}", t),
                }
            }

            sections.push(current_bs.into());

            sections
        }

        Self { sections: tokens_to_sections(tokens) }
    }
}

impl Program {
    pub fn generate_c(&self) -> Result<String, Error> {
        const DEBUG: bool = false;

        fn stringify_basic_section(basic: &BasicSection, depth: usize) -> String {
            let mut code = String::new();

            if DEBUG {
                for _ in 0..depth { code += "    "; }
                code += "// --- BEGIN BASIC SECTION ---\n";
            }

            // Generate code for any outputs performed in this basic section
            for out in &basic.outs {
                for _ in 0..depth { code += "    "; }
                code += &match out {
                    Output::Cell(idx, diff) => format!("putchar(ptr[{}] + {});\n", idx.0, diff.0),
                    Output::Const(val) => format!("putchar({});\n", val),
                }
            }

            // Generate code for any cell changes made in this basic section
            for (idx, change) in &basic.changes {
                for _ in 0..depth { code += "    "; }
                code += &match change {
                    Change::Incr(diff) => format!("ptr[{}] += {};\n", idx.0, diff.0),
                    Change::Set(val) => format!("ptr[{}] = {};\n", idx.0, val),
                }
            }

            // Generate code for the net shift for this basic section
            match basic.shift.0 {
                0 => {},
                n => {
                    for _ in 0..depth { code += "    "; }
                    code += &format!("ptr += {};\n", n);
                },
            }

            if DEBUG {
                for _ in 0..depth { code += "    "; }
                code += "// --- END BASIC SECTION ---\n\n";
            }

            code
        }

        fn stringify_input_section(input: &InputSection, depth: usize) -> String {
            let mut code = String::new();

            if DEBUG {
                for _ in 0..depth { code += "    "; }
                code += "// --- BEGIN INPUT SECTION ---\n";
            }

            // Generate code for any outputs performed in this basic section
            for (idx, diff) in &input.cells {
                for _ in 0..depth { code += "    "; }
                code += &format!("ptr[{}] = getchar() + {};\n", idx.0, diff.0);
            }

            if DEBUG {
                for _ in 0..depth { code += "    "; }
                code += "// --- END INPUT SECTION ---\n\n";
            }

            code
        }

        fn stringify_loop_section(luup: &LoopSection, depth: usize) -> String {
            let mut code = String::new();

            if DEBUG {
                for _ in 0..depth { code += "    "; }
                code += "// --- BEGIN LOOP SECTION ---\n";
            }

            // Generate code for loop body and contained sections
            for _ in 0..depth { code += "    "; }
            code += &format!("while (ptr[{}]) {{\n", luup.predicate.0);
            code += &stringify_sections(&luup.sections, depth + 1);
            for _ in 0..depth { code += "    "; }
            code += "}\n";

            if DEBUG {
                for _ in 0..depth { code += "    "; }
                code += "// --- END LOOP SECTION ---\n\n";
            }

            code
        }

        fn stringify_sections(sections: &Vec<Section>, depth: usize) -> String {
            let mut code = String::new();
            for section in sections {
                code += &match section {
                    Section::Basic(basic) => stringify_basic_section(&basic, depth),
                    Section::Input(input) => stringify_input_section(&input, depth),
                    Section::Loop(luup) => stringify_loop_section(&luup, depth),
                    _ => panic!("Unknown section type!"),
                };
            }
            code
        }

        Ok(format!(
            "#include <stdio.h>\
            \n\nchar mem[30000];\
            \n\nint main() {{\
            \n    char* ptr = mem;\
            \n\n{}\
            \n    return 0;\
            \n}}\
            ",
            stringify_sections(&self.sections, 1),
        ))
    }
}
