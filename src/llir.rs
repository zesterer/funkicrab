use std::{
    collections::{HashMap, HashSet},
    iter::FromIterator,
};

use super::Token;
use crate::Error;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Idx(pub i32);
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Diff(pub i32);

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
    // The empty set
    pub fn empty() -> Self {
        CellAccessInfo::Exactly(HashSet::new())
    }

    // The empty set
    pub fn one(idx: Idx) -> Self {
        let mut hs = HashSet::new();
        hs.insert(idx);
        CellAccessInfo::Exactly(hs)
    }

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

    // Determine whether the cell access set could contain the given pointer-relative cell
    pub fn contains(&self, idx: Idx) -> bool {
        match self {
            CellAccessInfo::Exactly(cells) => cells.contains(&idx),
            CellAccessInfo::Not(cells) => !cells.contains(&idx),
            // TODO: Don't default to true for all other variants
            _ => true,
        }
    }

    // Return true if the cell access set is definitively empty
    pub fn is_empty(&self) -> bool {
        match self {
            CellAccessInfo::Exactly(cells) => cells.len() == 0,
            // TODO: Don't default to false for all other variants
            _ => false,
        }
    }
}

// Describes the manner in which a cell may have its value altered over a particular execution
// period. This is used in future passes to guide optimisations such as loop linearisation.
pub enum CellEffect {
    // The cell's value is not affected throughout the execution period
    None,
    // The cell's value is set to a specific compile-time constant at the end of the execution
    // period. This effect is guaranteed to not depend on the previous value of the cell.
    SetTo(u8),
    // The cell's value is incremented by a specific compile-time value at the end of the
    // execution period.
    IncrByExactly(Diff),
    // The cell's value is incremented by an unknown multiple of a specific compile-time value at
    // the end of the execution period. Note that the value takes the form (N * diff) where N is
    // never negative (although it may be 0). This means that if the diff is positive, the total
    // increment must also be positive (and vice-versa).
    IncrByMultipleOf(Diff),
    // The cell's value is set to an unknown (possibly undeterminable at compile-time) value at
    // the end of the execution period. An example of this might be due to user input, or an
    // operation that is too complex to statically evaluate information about. This effect is
    // guaranteed to not depend on the previous value of the cell.
    SetUnknown,
    // The effect on the cell's value is indeterminate - it may be set to a value, incremented,
    // decremented, or otherwise manipulated in a wholly unpredicatable way. This makes it
    // impossible to tell if the future value of the cell is dependent on the previous value of
    // the cell, and so we have to make the assumption that it is.
    Unknown,
}

// An expression that may be used to alter a cell value
#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Const(i32),
    CellVal(Idx),
    Sum(Box<Expr>, Box<Expr>),
    Product(Box<Expr>, Box<Expr>),
}

impl Expr {
    // Determine which cells are read by this expression
    pub fn get_cell_reads(&self) -> CellAccessInfo {
        match self {
            Expr::Const(val) => CellAccessInfo::empty(),
            Expr::CellVal(idx) => CellAccessInfo::one(*idx),
            Expr::Sum(expr0, expr1) => expr0.get_cell_reads().union(Idx(0), &expr1.get_cell_reads()),
            Expr::Product(expr0, expr1) => expr0.get_cell_reads().union(Idx(0), &expr1.get_cell_reads()),
        }
    }

    // Attempt to determine whether an expression is equivalent to another
    pub fn equiv_to(&self, other: &Expr) -> bool {
        match (self, other) {
            (Expr::Const(a), Expr::Const(b)) if a == b => true,
            (Expr::CellVal(a), Expr::CellVal(b)) if a == b => true,
            (Expr::Sum(sum0a, sum0b), Expr::Sum(sum1a, sum1b)) if (
                // Summing is commutative, so compare all possibilities
                (sum0a == sum1a && sum0b == sum1b) ||
                (sum0a == sum1b && sum0b == sum1a)
            ) => true,
            (Expr::Product(prod0a, prod0b), Expr::Product(prod1a, prod1b)) if (
                // Multiplication is commutative, so compare all possibilities
                (prod0a == prod1a && prod0b == prod1b) ||
                (prod0a == prod1b && prod0b == prod1a)
            ) => true,
            // Fallback to false
            _ => false,
        }
    }

    // Attempt to evaluate the result of the expression statically using only local information
    pub fn eval_local(&self) -> Option<Diff> {
        match self {
            // Constants evaluate to themselves
            Expr::Const(c) => Some(Diff(*c)),
            // Local information alone doesn't provide us with a value for a cell
            Expr::CellVal(_) => None,
            // Sum expressions can be evaluated if their sub-expressions can be evaluated
            Expr::Sum(expr0, expr1) => expr0.eval_local().and_then(|expr0| expr1.eval_local().map(|expr1| Diff(expr0.0 + expr1.0))),
            // Product expressions can be evaluated if their sub-expressions can be evaluated
            Expr::Product(expr0, expr1) => expr0.eval_local().and_then(|expr0| expr1.eval_local().map(|expr1| Diff(expr0.0 * expr1.0))),
        }
    }

    // Attempt to simplify an expression using only local information (i.e: (N + N) will become
    // (2 * N) and (X + Y) will become Z when both X and Y are known constants
    pub fn simplify_local(&mut self) {
        match self {
            Expr::Sum(expr0, expr1) => {
                // Simplify each sub-expression
                expr0.simplify_local();
                expr1.simplify_local();

                if let (Some(a), Some(b)) = (expr0.eval_local(), expr1.eval_local()) {
                    // Reduce (C + K) where C and K are constants
                    *self = Expr::Const(a.0 + b.0);
                } else if expr0.equiv_to(expr1) {
                    // Reduce (N + N) to (2 * N)
                    *self = Expr::Product(
                        expr0.clone(),
                        Box::new(Expr::Const(2)),
                    );
                }
            },
            Expr::Product(expr0, expr1) => {
                // Simplify each sub-expression
                expr0.simplify_local();
                expr1.simplify_local();

                if let (Some(a), Some(b)) = (expr0.eval_local(), expr1.eval_local()) {
                    // Reduce (C * K) where C and K are constants
                    *self = Expr::Const(a.0 * b.0);
                }
            },
            // Default to no simplification
            _ => {},
        }
    }
}

// A change that occurs to a cell's value during the execution of a basic section. There are
// 2 possible forms this can take - increments (including decrements) of the cell's value or
// resetting the cell to a constant value.
#[derive(Clone, Debug, PartialEq)]
pub enum Change {
    Set(Expr),
    Incr(Expr),
}

// A section that describes a so-called 'basic' section. These sections no looping code, only
// performing 2 operations: changes (including increments, decrements and multiplications) and
// outputs. They have a net shift that is known at compile-time. Outputs are always intended to
// be executed at the beginning of the section.
#[derive(Clone, Debug, PartialEq)]
pub struct BasicSection {
    pub changes: HashMap<Idx, Change>,
}

impl BasicSection {
    fn new() -> Self {
        Self {
            changes: HashMap::new(),
        }
    }

    // Increment a cell relative to this section's pointer
    fn add_cell_incr(&mut self, idx: Idx, diff: Diff) {
        if let Some(change) = self.changes.get_mut(&idx) {
            match change {
                Change::Set(expr) => *expr = Expr::Sum(
                    Box::new(expr.clone()),
                    Box::new(Expr::Const(diff.0)),
                ),
                Change::Incr(expr) => *expr = Expr::Sum(
                    Box::new(expr.clone()),
                    Box::new(Expr::Const(diff.0)),
                ),
            }
        } else {
            self.changes.insert(idx, Change::Incr(Expr::Const(diff.0)));
        }
    }

    // Set a cell relative to this section's pointer's value
    fn add_cell_set(&mut self, idx: Idx, expr: Expr) {
        if let Some(change) = self.changes.get_mut(&idx) {
            *change = Change::Set(expr);
        } else {
            self.changes.insert(idx, Change::Set(expr));
        }
    }

    // Remove any changes to the given pointer-relative cell from this BasicSection
    fn remove_cell_change(&mut self, idx: Idx) {
        self.changes.remove(&idx);
    }

    // Determine whether this BasicSection can be deleted with no adverse effects
    pub fn has_no_effect(&self) -> bool {
        self.changes.len() == 0
    }

    // Determine the pointer-relative cells that are read by this BasicSection
    pub fn get_cell_reads(&self) -> CellAccessInfo {
        self.changes.iter().map(|(idx, change)| match change {
            Change::Set(expr) => expr.get_cell_reads(),
            Change::Incr(expr) => expr.get_cell_reads(),
        }).fold(CellAccessInfo::empty(), |cai_sum, cai| cai_sum.union(Idx(0), &cai))
    }

    // Determine the pointer-relative cells that are written to by this BasicSection
    pub fn get_cell_writes(&self) -> CellAccessInfo {
        self.changes.iter().map(|(idx, change)| CellAccessInfo::one(*idx))
            .fold(CellAccessInfo::empty(), |cai_sum, cai| cai_sum.union(Idx(0), &cai))
    }

    // Determine what effect execution of the section will have on the cell with the given index
    pub fn get_effect_for_cell(&self, idx: Idx) -> CellEffect {
        self.changes.get(&idx).map(|change| match change {
            Change::Set(expr) => expr.eval_local().map(|val| CellEffect::SetTo(val.0 as u8)).unwrap_or(CellEffect::Unknown),
            Change::Incr(expr) => expr.eval_local().map(|val| CellEffect::IncrByExactly(val)).unwrap_or(CellEffect::Unknown),
        }).unwrap_or(CellEffect::None)
    }

    // Produce a new BasicSection that takes that performs the action as this section, but
    // repeated multiple times
    pub fn repeat(&self, factor: Expr) -> Self {
        Self {
            changes: self.changes.iter().map(|(idx, change)| (*idx, match change {
                Change::Set(_) => change.clone(),
                Change::Incr(expr) => Change::Incr(Expr::Product(Box::new(expr.clone()), Box::new(factor.clone()))),
            })).collect(),
        }
    }

    // Attempt to create a new BasicSection by combining a later one with this one
    pub fn apply(&self, other: &Self) -> Option<Self> {
        let mut basic = self.clone();

        return None;
        if !other.get_cell_reads().union(Idx(0), &basic.get_cell_writes()).is_empty() {
            return None;
        }

        for (idx, change) in &other.changes {
            match change {
                Change::Set(expr) => basic.add_cell_set(*idx, expr.clone()),
                _ => return None,
            }
        }

        Some(basic)
    }
}

// An output from the program. This can be either be outputting a cell's value (plus an
// increment) or a compile-time constant.
#[derive(Clone, Debug, PartialEq)]
pub enum IoOp {
    InputCell(Idx),
    Output(Expr),
}

// A section that describes I/O operations upon cells
#[derive(Clone, Debug, PartialEq)]
pub struct IoSection {
    pub ops: Vec<IoOp>,
}

impl IoSection {
    fn new() -> Self {
        Self { ops: Vec::new() }
    }

    fn add_cell_input(&mut self, idx: Idx) {
        self.ops.push(IoOp::InputCell(idx));
    }

    fn add_cell_output(&mut self, idx: Idx) {
        self.ops.push(IoOp::Output(Expr::CellVal(idx)));
    }

    // Determine whether this InputSection can be deleted with no adverse effects
    pub fn has_no_effect(&self) -> bool {
        self.ops.len() == 0
    }

    // Determine the pointer-relative cells that are read by this InputSection
    pub fn get_cell_reads(&self) -> CellAccessInfo {
        self.ops.iter().filter_map(|op| match op {
            IoOp::InputCell(_) => None,
            IoOp::Output(expr) => Some(expr.get_cell_reads()),
        }).fold(CellAccessInfo::empty(), |total_cai, cai| total_cai.union(Idx(0), &cai))
    }
}

// A loop predicate. This is the expression that sits within the 'while' section of a loop. It can
// be a constant value (in which case it'll likely be optimised away in a future transformation),
// or it can be the value of a local cell.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Predicate {
    Cell(Idx),
    Const(u8),
}

// A section that describes a loop containing zero or more sub-sections. Loops may have a net
// shift that is incomputable at compile-time. Loops that do have a net shift that is computable
// at compile-time can often be optimised out in future passes.
#[derive(Clone, Debug, PartialEq)]
pub struct LoopSection {
    pub postshift: Idx,
    pub predicate: Predicate,
    pub sections: Vec<Section>,
}

impl LoopSection {
    fn new(postshift: Idx, predicate: Predicate) -> Self {
        Self {
            postshift,
            predicate,
            sections: Vec::new(),
        }
    }

    fn add_section(&mut self, section: Section) {
        self.sections.push(section);
    }

    // Get the net shift of this section (i.e: the amount by which the pointer is incremented
    // after this section is executed compared to before).
    pub fn get_total_shift(&self) -> ValInfo {
        match self
            .sections
            .iter()
            .map(|section| section.get_total_shift())
            .chain(Some(ValInfo::Exactly(self.postshift.0)))
            .fold(ValInfo::Exactly(0), |total, section_shift| match section_shift {
                ValInfo::Exactly(n) => if let ValInfo::Exactly(t) = total {
                    ValInfo::Exactly(t + n)
                } else {
                    ValInfo::Unknown
                },
                _ => ValInfo::Unknown
            })
        {
            ValInfo::Exactly(0) => ValInfo::Exactly(0),
            _ => ValInfo::Unknown,
        }
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

    // Determine the pointer-relative cells that are read by the body of this LoopSection
    pub fn get_body_cell_reads(&self) -> CellAccessInfo {
        let mut cell_reads = CellAccessInfo::Exactly(HashSet::new());

        let mut total_shift = Idx(0);
        for section in self.sections.iter() {
            if let ValInfo::Exactly(shift) = section.get_total_shift() {
                cell_reads = cell_reads.union(total_shift, &section.get_cell_reads());
                total_shift.0 += shift;
            } else {
                cell_reads = CellAccessInfo::Unknown;
            }
        }

        match self.get_total_shift() {
            ValInfo::Exactly(0) => cell_reads,
            ValInfo::Exactly(n) => {
                // If the shift is non-zero, we only have known reads if the number of reads is zero
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

    // Determine the pointer-relative cells that are read by this LoopSection
    pub fn get_cell_reads(&self) -> CellAccessInfo {
        let mut body_cell_reads = CellAccessInfo::Exactly(if let Predicate::Cell(idx) = self.predicate {
            let mut h = HashSet::new();
            h.insert(idx);
            h
        } else {
            HashSet::new()
        });

        body_cell_reads.union(Idx(0), &CellAccessInfo::Exactly(if let Predicate::Cell(idx) = self.predicate {
            let mut h = HashSet::new();
            h.insert(idx);
            h
        } else {
            HashSet::new()
        }))
    }

    // Determine whether the loop can be linearised. If it can, determine the expression that acts
    // as a multiplier.
    // For example, [->++<] can be linearised to { ptr[1] += ptr[0] * 2; ptr[0] = 0; }.
    pub fn can_linearise(&self) -> Option<Expr> {
        match self.predicate {
            Predicate::Cell(idx) => {
                // Only loops that have a total shift of zero can be statically linearised
                if let ValInfo::Exactly(0) = self.get_total_shift() {
                    // Now, we ensure that no internal sections write to the predicate cell
                    // Reads are okay since we can be sure the value won't change over the course
                    // of the loop
                    if self.sections.len() != 1 {
                        // Only loops containing a single basic section may be linearised
                        None
                    } else if self.sections.iter().all(|section| match section {
                        Section::Basic(basic) => {
                            if let CellEffect::IncrByExactly(Diff(-1)) = basic.get_effect_for_cell(idx) {
                                true
                            } else {
                                false
                            }
                        },
                        // Only basic sections may be linearised!
                        _ => false,
                    }) {
                        Some(Expr::CellVal(idx))
                    } else {
                        None
                    }
                } else {
                    None
                }
            },
            Predicate::Const(_) => None,
        }
    }

    pub fn linearise(&self) -> Option<Vec<Section>> {
        self.can_linearise().map(|factor_expr| {
            match &self.sections[0] {
                Section::Basic(basic) => {
                    let mut loop_basic = basic.repeat(factor_expr);

                    let mut setzero_basic = BasicSection::new();
                    if let Predicate::Cell(idx) = self.predicate {
                        loop_basic.remove_cell_change(idx);
                        setzero_basic.add_cell_set(idx, Expr::Const(0));
                    }

                    vec![
                        Section::Basic(loop_basic),
                        Section::Basic(setzero_basic),
                    ]
                },
                // TODO: Remove this panic
                _ => panic!("This shouldn't be possible"),
            }
        })
    }
}

// A section is a segment of brainfuck code after which all temporarily cached values must be
// written back into the cell array to maintain memory consistency. They are designed to be
// easy to perform optimisations upon through static analysis through their nature as a large
// atomic unit of Brainfuck code.
#[derive(Clone, Debug, PartialEq)]
pub enum Section {
    Basic(BasicSection),
    Io(IoSection),
    Loop(LoopSection),
}

impl From<BasicSection> for Section {
    fn from(basic: BasicSection) -> Self { Section::Basic(basic) }
}

impl From<IoSection> for Section {
    fn from(io: IoSection) -> Self { Section::Io(io) }
}

impl From<LoopSection> for Section {
    fn from(luup: LoopSection) -> Self { Section::Loop(luup) }
}

impl Section {
    // Get the net shift of this section (i.e: the amount by which the pointer is incremented
    // after this section is executed compared to before).
    pub fn get_total_shift(&self) -> ValInfo {
        match self {
            Section::Basic(basic) => ValInfo::Exactly(0),
            Section::Io(io) => ValInfo::Exactly(0),
            Section::Loop(luup) => luup.get_total_shift(),
        }
    }

    // Determine whether the section can be deleted with no adverse effects
    pub fn has_no_effect(&self) -> bool {
        match self {
            Section::Basic(basic) => basic.has_no_effect(),
            Section::Io(io) => io.has_no_effect(),
            Section::Loop(luup) => luup.has_no_effect(),
        }
    }

    /*
    // Determine the impact this section may have on a pointer-relative cell
    pub fn effect_on(&self) -> CellEffect {

    }
    */

    // Determine the pointer-relative cells that are read by this section
    pub fn get_cell_reads(&self) -> CellAccessInfo {
        match self {
            Section::Basic(basic) => basic.get_cell_reads(),
            Section::Io(io) => io.get_cell_reads(),
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
        fn tokens_to_sections(tokens: Vec<Token>, mut current_shift: i32) -> (Vec<Section>, Idx) {
            let mut sections = vec![];

            let mut current_bs = BasicSection::new();

            let start_shift = current_shift;

            for tok in tokens {
                match tok {
                    Token::Right => current_shift += 1,
                    Token::Left => current_shift -= 1,
                    Token::Inc => current_bs.add_cell_incr(Idx(current_shift), Diff(1)),
                    Token::Dec => current_bs.add_cell_incr(Idx(current_shift), Diff(-1)),
                    Token::Output => {
                        // Push the current basic section onto the section stack
                        let mut other_basic = BasicSection::new();
                        std::mem::swap(&mut current_bs, &mut other_basic);
                        sections.push(other_basic.into());

                        // Add an I/O section
                        let mut io = IoSection::new();
                        io.add_cell_output(Idx(current_shift));
                        sections.push(io.into());
                    },
                    Token::Input => {
                        // Push the current basic section onto the section stack
                        let mut other_basic = BasicSection::new();
                        std::mem::swap(&mut current_bs, &mut other_basic);
                        sections.push(other_basic.into());

                        // Add an I/O section
                        let mut io = IoSection::new();
                        io.add_cell_input(Idx(current_shift));
                        sections.push(io.into());
                    },
                    Token::Loop(toks) => {
                        // Push the current basic section onto the section stack
                        let mut other_basic = BasicSection::new();
                        std::mem::swap(&mut current_bs, &mut other_basic);
                        sections.push(other_basic.into());

                        // Add a loop section
                        // By default, the loop execution is predicated by the cell pointed to
                        // by the current pointer value
                        let (mut loop_sections, post_shift) = tokens_to_sections(toks, current_shift);
                        let mut luup = LoopSection::new(Idx(post_shift.0 - current_shift), Predicate::Cell(Idx(current_shift)));
                        for section in loop_sections {
                            luup.add_section(section);
                        }
                        sections.push(luup.into());
                    },
                    t => panic!("Unknown token: {:?}", t),
                }
            }

            sections.push(current_bs.into());

            (sections, Idx(current_shift))
        }

        Self { sections: tokens_to_sections(tokens, 0).0 }
    }
}

impl Program {
    pub fn generate_c(&self) -> Result<String, Error> {
        const DEBUG: bool = true;

        fn stringify_expr(expr: &Expr) -> String {
            match expr {
                Expr::Const(val) => format!("{}", val),
                Expr::CellVal(idx) => format!("ptr[{}]", idx.0),
                Expr::Sum(expr0, expr1) => format!("({} + {})", stringify_expr(expr0), stringify_expr(expr1)),
                Expr::Product(expr0, expr1) => format!("({} * {})", stringify_expr(expr0), stringify_expr(expr1)),
            }
        }

        fn stringify_basic_section(basic: &BasicSection, depth: usize) -> String {
            let mut code = String::new();

            if DEBUG {
                for _ in 0..depth { code += "    "; }
                code += "// --- BEGIN BASIC SECTION ---\n";
            }

            // Generate code for any cell changes made in this basic section
            for (idx, change) in &basic.changes {
                for _ in 0..depth { code += "    "; }
                code += &match change {
                    Change::Set(expr) => format!("ptr[{}] = {};\n", idx.0, stringify_expr(expr)),
                    Change::Incr(expr) => format!("ptr[{}] += {};\n", idx.0, stringify_expr(expr)),
                }
            }

            if DEBUG {
                for _ in 0..depth { code += "    "; }
                code += "// --- END BASIC SECTION ---\n\n";
            }

            code
        }

        fn stringify_io_section(io: &IoSection, depth: usize) -> String {
            let mut code = String::new();

            if DEBUG {
                for _ in 0..depth { code += "    "; }
                code += "// --- BEGIN IO SECTION ---\n";
            }

            // Generate code for any outputs performed in this basic section
            for op in &io.ops {
                for _ in 0..depth { code += "    "; }
                code += &match op {
                    IoOp::Output(expr) => format!("putchar({});\n", stringify_expr(expr)),
                    IoOp::InputCell(idx) => format!("ptr[{}] = getchar();\n", idx.0),
                }
            }

            if DEBUG {
                for _ in 0..depth { code += "    "; }
                code += "// --- END IO SECTION ---\n\n";
            }

            code
        }

        fn stringify_loop_section(luup: &LoopSection, depth: usize) -> String {
            let mut code = String::new();

            if DEBUG {
                for _ in 0..depth { code += "    "; }
                code += "// --- BEGIN LOOP SECTION ---\n";
                for _ in 0..depth { code += "    "; }
                code += &format!("// CAN_LINEARISE = {} \n", luup.can_linearise().is_some());
            }

            // Generate code for loop body and contained sections
            for _ in 0..depth { code += "    "; }
            code += &match luup.predicate {
                Predicate::Cell(idx) => format!("while (ptr[{}]) {{\n", idx.0),
                Predicate::Const(val) => format!("while ({}) {{\n", val),
            };
            code += &stringify_sections(&luup.sections, depth + 1);
            if luup.postshift.0 != 0 {
                for _ in 0..depth + 1 { code += "    "; }
                code += &format!("ptr += {};\n", luup.postshift.0);
            }
            for _ in 0..depth { code += "    "; }
            code += "}\n";

            if DEBUG {
                for _ in 0..depth { code += "    "; }
                code += &format!(
                    "// --- END LOOP SECTION (SHIFT = {}) ---\n\n",
                    match luup.get_total_shift() {
                        ValInfo::Exactly(total_shift) => format!("{}", total_shift),
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
                    Section::Io(io) => stringify_io_section(&io, depth),
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
