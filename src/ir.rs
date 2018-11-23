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
    for (i, inst) in ir.iter().enumerate() {
        match inst {
            &Inst::Add(r, f) => if !copymuls.contains_key(&r) {
                copymuls.insert(r, (i, f));
            } else {
                return None;
            },
            // Failure
            inst => { return None; },
        }
    }

    if let Some(&(i, n)) = copymuls.get(&base_shift) {
        if n == -1 && i == 0 || i == ir.len() - 1 {
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
            (Inst::Add(r, a), Inst::Move(n)) => { insts.push(Inst::Move(n)); current = Inst::Add(r - n, a); },

            // Nop elimination
            (Inst::Nop, i) => { current = i; },
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
                //insts.push(Inst::Move(shift));
                //shift = 0;
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

pub fn optimise_stream(ir: Vec<Inst>) -> Vec<Inst> {
    let ir = optimise_binary_combination(ir);
    let ir = optimise_subloops(ir);
    let ir = optimise_move(ir, 0);

    ir
}

pub fn optimise(ir: Vec<Inst>) -> Vec<Inst> {
    let mut ir = ir;
    loop {
        let new_ir = optimise_stream(ir.clone());
        if new_ir == ir {
            return ir;
        } else {
            ir = new_ir;
        }
    }
}
