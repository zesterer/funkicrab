use std::{
    collections::{HashMap, HashSet},
    iter::FromIterator,
};

use super::Token;
use crate::Error;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Idx(i32);
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Diff(i32);

// Represents compile-time knowledge of a particular value. This information is usually computed
// through static analysis and is used to inform future optimisations.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ValInfo {
    // Value is (N)
    Exactly(i32),
    // Value is (base + N * factor)
    MultipleOf { base: i32, factor: i32 },
    // No knowledge of value
    Unknown,
}

// Represents compile-time knowledge of the cells that a section might possibly access throughout
// its execution. This information is usually computed through static analysis and is used to
// inform future optimisations. Effectively, this is a set.
#[derive(Clone, Debug, PartialEq)]
pub enum CellAccessInfo {
    // Only these specific pointer-relative cells are accessed
    Exactly(HashSet<Idx>),
    // Only this pointer-relative cell or any above it in the cell array are accessed
    EqualOrAbove(Idx),
    // Only cells below this pointer-relative cell (and not this cell) in the cell array are accessed
    Below(Idx),
    // Only cells within this range are accessed (lower bound is inclusive, upper bound is exclusive)
    Inside(Idx, Idx),
    // Only cells outside this range are accessed (lower bound is exclusive, upper bound is inclusive)
    Outside(Idx, Idx),
    // Only these specific cells are guaranteed to not be accessed
    Not(HashSet<Idx>),
    // No information about cell access can be determined, any of them could be accessed
    Unknown,
}

impl CellAccessInfo {
    // Find the union of two cell access information sets. The relative shift is applied to the
    // second set.
    pub fn union(&self, rel_shift: Idx, other: &CellAccessInfo) -> Self {
        match (self, other) {
            (CellAccessInfo::Exactly(cells_a), CellAccessInfo::Exactly(cells_b)) => {
                let shifted_b = cells_b.iter().map(|idx| Idx(idx.0 + rel_shift.0)).collect();
                CellAccessInfo::Exactly(cells_a.union(&shifted_b).map(|idx| *idx).collect())
            },
            // TODO: Don't default to Unknown all the time
            _ => CellAccessInfo::Unknown,
        }
    }

    // Find the intersection of two cell access information sets. The relative shift is applied to the
    // second set.
    pub fn intersection(&self, rel_shift: Idx, other: &CellAccessInfo) -> Self {
        match (self, other) {
            (CellAccessInfo::Exactly(cells_a), CellAccessInfo::Exactly(cells_b)) => {
                let shifted_b = cells_b.iter().map(|idx| Idx(idx.0 + rel_shift.0)).collect();
                CellAccessInfo::Exactly(cells_a.intersection(&shifted_b).map(|idx| *idx).collect())
            },
            // TODO: Don't default to Unknown all the time
            _ => CellAccessInfo::Unknown,
        }
    }
}

// A change that occurs to a cell's value during the execution of a basic section. There are
// 2 possible forms this can take - increments (including decrements) of the cell's value or
// resetting the cell to a constant value.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Change {
    Incr(Diff),
    Set(u8),
}

// An output from the program. This can be either be outputting a cell's value (plus an
// increment) or a compile-time constant.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Output {
    Cell(Idx, Diff),
    Const(u8),
}

// A section that describes a so-called 'basic' section. These sections no looping code, only
// performing 2 operations upon data: increments (including decrements) and outputs. They have a
// net shift that is known at compile-time. Outputs are always intended to be executed at the
// beginning of the section.
#[derive(Clone, Debug, PartialEq)]
pub struct BasicSection {
    pub outs: Vec<Output>,
    pub changes: HashMap<Idx, Change>,
    pub shift: Idx,
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

    // Determine whether this BasicSection can be deleted with no adverse effects
    pub fn has_no_effect(&self) -> bool {
        self.changes.len() == 0 && self.outs.len() == 0 && self.shift.0 == 0
    }

    // Determine the pointer-relative cells that are read by this BasicSection
    pub fn get_cell_reads(&self) -> CellAccessInfo {
        // Collect all of the cells that get read throughout the execution of this section
        let out_reads = self.outs.iter().filter_map(|o| if let Output::Cell(idx, _) = o { Some(*idx) } else { None });
        let change_reads = self.changes.iter().filter(|(idx, c)| if let Change::Incr(_) = c { true } else { false }).map(|(idx, _)| *idx);
        CellAccessInfo::Exactly(out_reads.chain(change_reads).collect())
    }
}

// A section that describes a resetting of cells through user input
#[derive(Clone, Debug, PartialEq)]
pub struct InputSection {
    pub cells: Vec<(Idx, Diff)>,
}

impl InputSection {
    fn new() -> Self {
        Self { cells: Vec::new() }
    }

    fn add_cell_input(&mut self, idx: Idx) {
        self.cells.push((idx, Diff(0)));
    }

    // Determine whether this InputSection can be deleted with no adverse effects
    pub fn has_no_effect(&self) -> bool {
        self.cells.len() == 0
    }

    // Determine the pointer-relative cells that are read by this InputSection
    pub fn get_cell_reads(&self) -> CellAccessInfo {
        CellAccessInfo::Exactly(HashSet::new())
    }
}

// A loop predicate. This is the expression that sits within the 'while' section of a loop. It can
// be a constant value (in which case it'll likely be optimised away in a future transformation),
// or it can be the value of a local cell.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Predicate {
    Cell(Idx, Diff),
    Const(u8),
}

// A section that describes a loop containing zero or more sub-sections. Loops may have a net
// shift that is incomputable at compile-time. Loops that do have a net shift that is computable
// at compile-time can often be optimised out in future passes.
#[derive(Clone, Debug, PartialEq)]
pub struct LoopSection {
    pub predicate: Predicate,
    pub sections: Vec<Section>,
    pub shift: ValInfo,
}

impl LoopSection {
    fn new(predicate: Predicate) -> Self {
        Self {
            predicate,
            sections: Vec::new(),
            shift: ValInfo::Unknown,
        }
    }

    fn add_section(&mut self, section: Section) {
        self.sections.push(section);
    }

    // Determine whether this LoopSection can be deleted with no adverse effects
    pub fn has_no_effect(&self) -> bool {
        if let Predicate::Const(val) = self.predicate {
            // Loops only execute if their predicate is non-zero. Therefore, a predicate of zero
            // means that the loop will not execute.
            val == 0
        } else {
            // A loop has no effect if all of its sub-sections have no effect and it has a net
            // shift of 0
            //self.sections.iter().all(|s| s.has_no_effect()) && if let ValInfo::Exactly(0) = self.shift { true } else { false }
            // TODO: The above is an unsound assumption, since an infinite loop may be desired.
            false
        }
    }

    // Determine the pointer-relative cells that are read by this LoopSection
    pub fn get_cell_reads(&self) -> CellAccessInfo {
        let mut cell_reads = CellAccessInfo::Exactly(if let Predicate::Cell(idx, _) = self.predicate {
            let mut h = HashSet::new();
            h.insert(idx);
            h
        } else {
            HashSet::new()
        });

        let mut total_shift = Idx(0);
        for section in self.sections.iter() {
            if let ValInfo::Exactly(shift) = section.get_shift() {
                cell_reads = cell_reads.union(total_shift, &section.get_cell_reads());
                total_shift.0 += shift;
            } else {
                cell_reads = CellAccessInfo::Unknown;
            }
        }

        match self.shift {
            ValInfo::Exactly(0) => cell_reads,
            ValInfo::Exactly(n) => {
                // If the shift is non-zero, we only have known reads if the number of reads if zero
                if (if let CellAccessInfo::Exactly(cells) = &cell_reads {
                    cells.len() == 0
                } else {
                    false
                }) {
                    cell_reads
                } else {
                    CellAccessInfo::Unknown
                }
            },
            _ => CellAccessInfo::Unknown,
        }
    }
}

// A section is a segment of brainfuck code after which all temporarily cached values must be
// written back into the cell array to maintain memory consistency. They are designed to be
// easy to perform optimisations upon through static analysis through their nature as a large
// atomic unit of Brainfuck code.
#[derive(Clone, Debug, PartialEq)]
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

impl Section {
    // Get the net shift of this section (i.e: the amount by which the pointer is incremented
    // after this section is executed compared to before). Note that this method does not attempt
    // to perform any sort of intelligent, recursive calculation - such things are left to
    // optimisation procedures to compute.
    pub fn get_shift(&self) -> ValInfo {
        match self {
            Section::Basic(basic) => ValInfo::Exactly(basic.shift.0),
            Section::Input(input) => ValInfo::Exactly(0),
            Section::Loop(luup) => luup.shift,
        }
    }

    // Determine whether the section can be deleted with no adverse effects
    pub fn has_no_effect(&self) -> bool {
        match self {
            Section::Basic(basic) => basic.has_no_effect(),
            Section::Input(input) => input.has_no_effect(),
            Section::Loop(luup) => luup.has_no_effect(),
        }
    }

    // Determine the pointer-relative cells that are read by this section
    pub fn get_cell_reads(&self) -> CellAccessInfo {
        match self {
            Section::Basic(basic) => basic.get_cell_reads(),
            Section::Input(input) => input.get_cell_reads(),
            Section::Loop(luup) => luup.get_cell_reads(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Program {
    pub sections: Vec<Section>,
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
                        let mut luup = LoopSection::new(Predicate::Cell(Idx(0), Diff(0)));
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
        const DEBUG: bool = true;

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
                code += &format!("// --- END BASIC SECTION (SHIFT = {}) ---\n\n", basic.shift.0);
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
            code += &match luup.predicate {
                Predicate::Cell(idx, diff) => format!("while (ptr[{}] + {}) {{\n", idx.0, diff.0),
                Predicate::Const(val) => format!("while ({}) {{\n", val),
            };
            code += &stringify_sections(&luup.sections, depth + 1);
            for _ in 0..depth { code += "    "; }
            code += "}\n";

            if DEBUG {
                for _ in 0..depth { code += "    "; }
                code += &format!(
                    "// --- END LOOP SECTION (SHIFT = {}) ---\n\n",
                    match luup.shift {
                        ValInfo::Exactly(shift) => format!("{}", shift),
                        ValInfo::MultipleOf { base, factor } => format!("{} + N * {}", base, factor),
                        ValInfo::Unknown => "unknown".to_string(),
                    },
                );
            }

            code
        }

        fn stringify_sections(sections: &Vec<Section>, depth: usize) -> String {
            let mut code = String::new();
            for section in sections {
                if DEBUG {
                    for _ in 0..depth { code += "    "; }
                    code += &format!("// NEXT SECTION CELL READS: {:?}\n", section.get_cell_reads());
                }

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
