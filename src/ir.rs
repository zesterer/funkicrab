use std::collections::HashMap;

use super::Token;

#[derive(Clone, Debug, PartialEq)]
pub enum Inst {
    Move(i32),
    Add(i32, i32),
    SetC(i32, i32),
    CopyMul(i32, i32, i32),

    Output(i32),
    Input(i32),

    Loop(i32, Vec<Inst>),

    Nop,
}

pub fn from_tokens(tokens: Vec<Token>) -> Vec<Inst> {
    let mut insts = vec![];
    let mut current = Inst::Nop;

    for tok in tokens {
        match (current.clone(), tok.clone()) {
            (c, Token::Right) => { insts.push(c); current = Inst::Move(1); },
            (c, Token::Left) => { insts.push(c); current = Inst::Move(-1); },
            (c, Token::Inc) => { insts.push(c); current = Inst::Add(0, 1); },
            (c, Token::Dec) => { insts.push(c); current = Inst::Add(0, -1); },
            (c, Token::Output) => { insts.push(c); current = Inst::Output(0); },
            (c, Token::Input) => { insts.push(c); current = Inst::Input(0); },
            (c, Token::Loop(t)) => { insts.push(c); current = Inst::Loop(0, from_tokens(t)); },
        }
    }

    insts.push(current);

    insts
}

pub fn optimise_to_copymuls(base_shift: i32, ir: &Vec<Inst>) -> Option<Vec<(i32, i32)>> {
    let mut copymuls = HashMap::new();
    let mut net = 0;
    for (i, inst) in ir.iter().enumerate() {
        match inst {
            &Inst::Add(r, f) => {
                if !copymuls.contains_key(&r) {
                    copymuls.insert(r, (i, f));
                } else {
                    copymuls.get_mut(&r).map(|(_, cf)| *cf += f);
                }
                if r == base_shift {
                    net += f;
                }
            },
            // Failure
            inst => { return None; },
        }
    }

    if let Some(&(i, n)) = copymuls.get(&base_shift) {
        if net == -1 {
            copymuls.remove(&0);
            return Some(copymuls.into_iter().map(|(shift, (i, f))| (shift, f)).collect());
        }
    }

    None
}

pub fn optimise_loop(base_shift: i32, ir: Vec<Inst>) -> Vec<Inst> {
    let ir = optimise_stream(ir);

    if let Some(copymuls) = optimise_to_copymuls(base_shift, &ir) {
        //println!("Optimising: {:?}", ir);
        let mut ir: Vec<_> = copymuls.into_iter().map(|(r, f)| Inst::CopyMul(0, r, f)).collect();
        ir.push(Inst::SetC(0, 0));
        //println!("Result: {:?}", ir);
        return ir;
    }

    if ir.len() == 1 {
        if let Inst::SetC(r, 0) = ir[0] {
            if r == base_shift {
                return vec![Inst::SetC(r, 0)];
            }
        }
    }

    vec![Inst::Loop(base_shift, ir)]
}

pub fn optimise_binary_combination(ir: Vec<Inst>) -> Vec<Inst> {
    let mut insts = vec![];
    let mut current = Inst::Nop;

    for i in ir {
        match (current.clone(), i.clone()) {
            // Dead code elimination
            (Inst::SetC(r0, _), Inst::SetC(r1, n)) if r0 == r1 => { current = Inst::SetC(r0, n); },
            (Inst::Add(r0, _), Inst::SetC(r1, n)) if r0 == r1 => { current = Inst::SetC(r0, n); },
            (Inst::Add(r0, _), Inst::Input(r1)) if r0 == r1 => { current = Inst::Input(r0); },
            (Inst::SetC(r0, _), Inst::Input(r1)) if r0 == r1 => { current = Inst::Input(r0); },

            // Combining
            (Inst::Move(a), Inst::Move(b)) => { current = Inst::Move(a + b); },
            (Inst::Add(r0, a), Inst::Add(r1, b)) if r0 == r1 => { current = Inst::Add(r0, a + b); },
            (Inst::SetC(r0, a), Inst::Add(r1, b)) if r0 == r1 => { current = Inst::SetC(r0, a + b); },

            // Shifting
            (Inst::Move(n), Inst::Add(r, a)) => { insts.push(Inst::Add(r + n, a)); current = Inst::Move(n); },
            (Inst::Move(n), Inst::Output(r)) => { insts.push(Inst::Output(r + n)); current = Inst::Move(n); },

            // Nop elimination
            (Inst::Nop, i) => { current = i; },
            (c, Inst::Add(_, 0)) => {},
            (c, Inst::CopyMul(_, _, 0)) => {},
            (c, Inst::Nop) => {},
            (c, Inst::Move(0)) => {},

            // Fallback
            (c, i) => { insts.push(c); current = i; },
        }
    }

    insts.push(current);

    insts
}

pub fn optimise_subloops(ir: Vec<Inst>) -> Vec<Inst> {
    let mut insts = vec![];

    for i in ir {
        match i.clone() {
            // Loop optimisation
            Inst::Loop(b, ir) => { optimise_loop(b, ir).drain(..).for_each(|e| { insts.push(e); }); },

            // Fallback
            i => { insts.push(i); },
        }
    }

    insts
}

pub fn optimise_move(ir: Vec<Inst>, base_shift: i32) -> Vec<Inst> {
    let mut shift = 0;
    let mut insts = vec![];
    for i in ir {
        match i {
            Inst::Move(s) => shift += s,
            Inst::Add(r, n) => { insts.push(Inst::Add(base_shift + r + shift, n)); },
            Inst::SetC(r, n) => { insts.push(Inst::SetC(base_shift + r + shift, n)); },
            Inst::CopyMul(r, d, f) => { insts.push(Inst::CopyMul(base_shift + r + shift, base_shift + d + shift, f)); },
            Inst::Input(r) => { insts.push(Inst::Input(base_shift + r + shift)); },
            Inst::Output(r) => { insts.push(Inst::Output(base_shift + r + shift)); },
            Inst::Loop(b, ir) => {
                insts.push(Inst::Loop(base_shift + shift + b, optimise_move(ir, base_shift + shift)));
            },
            i => { insts.push(i); },
        }
    }
    if shift != 0 {
        insts.push(Inst::Move(shift));
    }
    insts
}

// This must *only* be called upon the entire program
pub fn optimise_analyse_cells(ir: Vec<Inst>) -> Vec<Inst> {
    type CellInfo = Option<u8>;

    fn analyse_loop(ir: &Vec<Inst>, ptr: &mut i32, mut cells: Vec<CellInfo>) -> Vec<Inst> {
        fn get_cell_val(cells: &Vec<CellInfo>, idx: usize) -> CellInfo {
            *cells.get(idx).unwrap_or(&Some(0))
        }

        fn set_cell_val(cells: &mut Vec<CellInfo>, idx: usize, val: u8) {
            if val != 0 {
                while cells.len() <= idx { cells.push(Some(0)); }
            }
            cells.get_mut(idx).map(|c| *c = Some(val));
        }

        fn add_cell_val(cells: &mut Vec<CellInfo>, idx: usize, incr: i32) {
            if incr != 0 {
                while cells.len() <= idx { cells.push(Some(0)); }
                let cell = cells.get_mut(idx).unwrap();
                *cell = cell.map(|v| (v as i32 + incr) as u8);
            }
        }

        fn invalidate_cell_val(cells: &mut Vec<CellInfo>, idx: usize) {
            while cells.len() <= idx { cells.push(Some(0)); }
            let cell = cells.get_mut(idx).unwrap();
            *cell = None;
        }

        // At the start of a loop, we have to eliminate knowledge of all cells
        // TODO: Keep cells that we can prove are not altered
        for cell in cells.iter_mut() {
            *cell = None;
        }

        let mut insts = vec![];
        let mut broken = false;
        for inst in ir {
            if broken {
                insts.push(inst.clone());
                continue;
            }
            match inst.clone() {
                Inst::Add(r, n) => {
                    let idx = (*ptr + r).max(0) as usize;
                    add_cell_val(&mut cells, idx, n);
                    insts.push(inst.clone());
                },
                Inst::CopyMul(b, r, f) => {
                    // TODO: Properly test this
                    let idx_b = (*ptr + b).max(0) as usize;
                    let idx_r = (*ptr + r).max(0) as usize;
                    let val_b = get_cell_val(&cells, idx_b);
                    let val_r = get_cell_val(&cells, idx_r);
                    if let Some(val_b) = val_b {
                        add_cell_val(&mut cells, idx_r, val_b as i32 * f);
                    } else {
                        invalidate_cell_val(&mut cells, idx_r);
                    }

                    if let Some(0) = val_b {
                        // Do nothing
                    } else if f != 0 {
                        insts.push(inst.clone());
                    }
                },
                Inst::SetC(r, n) => {
                    let idx = (*ptr + r).max(0) as usize;
                    let val = get_cell_val(&cells, idx);
                    if val != Some(n as u8) {
                        set_cell_val(&mut cells, idx, n as u8);
                        insts.push(inst.clone());
                    }
                },
                Inst::Move(n) => {
                    *ptr += n;
                    insts.push(inst.clone());
                },
                Inst::Loop(b, ir) => {
                    let idx = (*ptr + b).max(0) as usize;
                    let val = get_cell_val(&cells, idx);
                    if val != Some(0) {
                        insts.push(Inst::Loop(b, analyse_loop(&ir, ptr, cells.clone())));
                        broken = true;
                    }
                },
                Inst::Input(r) => {
                    let idx = (*ptr + r).max(0) as usize;
                    invalidate_cell_val(&mut cells, idx);
                    insts.push(inst.clone());
                },
                inst => { insts.push(inst.clone()); },
            }
        }

        insts
    }

    let mut cells = Vec::new();
    let mut ptr = 0;

    analyse_loop(&ir, &mut ptr, cells)
}

pub fn optimise_stream(ir: Vec<Inst>) -> Vec<Inst> {
    let ir = optimise_binary_combination(ir);
    let ir = optimise_subloops(ir);
    let ir = optimise_move(ir, 0);

    ir
}

pub fn optimise_remove_prog_tail(mut ir: Vec<Inst>) -> Vec<Inst> {
    while let Some(inst) = ir.last().cloned() {
        match inst {
            Inst::Input(_) => break,
            Inst::Output(_) => break,
            Inst::Loop(_, _) => break,
            i => { ir.pop(); },
        }
    }

    ir
}

pub fn optimise(ir: Vec<Inst>) -> Vec<Inst> {
    const MAX_ATTEMPTS: usize = 10;
    let mut ir = ir;
    for _ in 0..MAX_ATTEMPTS {
        let new_ir = optimise_stream(ir.clone());
        let new_ir = optimise_analyse_cells(new_ir);
        let new_ir = optimise_remove_prog_tail(new_ir);
        if new_ir == ir {
            return ir;
        } else {
            ir = new_ir;
        }
    }
    ir
}
