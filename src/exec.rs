use crate::ir::Inst;
use super::Error;

pub fn preexecute(insts: Vec<Inst>) -> (Vec<Inst>, i32, Vec<u8>, Vec<u8>) {
    fn create_if_none(cells: &mut Vec<u8>, idx: usize) -> bool {
        if idx >= 30000 {
            return false;
        }
        while cells.len() < idx + 1 {
            cells.push(0);
        }
        true
    }

    enum Done {
        Full,
        Partial(usize),
        None,
    }

    fn exec(mut iter_left: &mut usize, insts: &Vec<Inst>, ptr: &mut i32, cells: &mut Vec<u8>, outputs: &mut Vec<u8>) -> Done {
        for (i, inst) in insts.iter().enumerate() {
            match inst.clone() {
                Inst::Move(n) => { *ptr += n },
                Inst::SetC(r, n) => {
                    if !create_if_none(cells, (*ptr + r) as usize) {
                        return Done::Partial(i);
                    }
                    cells[(*ptr + r) as usize] = n as u8;
                },
                Inst::Add(r, n) => {
                    let idx = (*ptr + r) as usize;
                    if !create_if_none(cells, idx) {
                        return Done::Partial(i);
                    }
                    cells[idx] = cells[idx].wrapping_add(n as u8);
                },
                Inst::CopyMul(b, r, f) => {
                    let idx_r = (*ptr + r) as usize;
                    let idx_b = (*ptr + b) as usize;
                    if !create_if_none(cells, idx_r) || create_if_none(cells, idx_b) {
                        return Done::Partial(i);
                    }
                    cells[idx_r] = cells[idx_r].wrapping_add((cells[idx_b] as i32 * f) as u8);
                },
                Inst::Loop(r, ir) => {
                    let mut tmp_ptr = *ptr;
                    let mut tmp_cells = cells.clone();
                    let mut tmp_outputs = outputs.clone();
                    while {
                        if !create_if_none(&mut tmp_cells, (tmp_ptr + r) as usize) {
                            return Done::Partial(i);
                        }
                        tmp_cells[(tmp_ptr + r) as usize] != 0
                    } {
                        match exec(&mut iter_left, &ir, &mut tmp_ptr, &mut tmp_cells, &mut tmp_outputs) {
                            Done::Full => continue,
                            _ => { return Done::Partial(i); },
                        }
                    }
                    *ptr = tmp_ptr;
                    *cells = tmp_cells;
                    *outputs = tmp_outputs;
                },
                Inst::Output(r) => {
                    let idx = (*ptr + r) as usize;
                    if !create_if_none(cells, idx) {
                        return Done::Partial(i);
                    }
                    outputs.push(cells[idx]);
                },
                Inst::Nop => {},
                _ => { return Done::Partial(i); },
            }
            if *iter_left == 0 || outputs.len() > 10000 || cells.len() >= 30000 {
                return Done::Partial(i);
            }
            *iter_left -= 1;
        }
        Done::Full
    }

    let mut iter_left = 1000000;
    let mut ptr = 0;
    let mut cells = vec![];
    let mut outputs = vec![];

    let done = exec(&mut iter_left, &insts, &mut ptr, &mut cells, &mut outputs);
    //let done = Done::None;

    match done {
        Done::Full => (vec![], ptr, vec![], outputs),
        Done::Partial(elided) => (insts.into_iter().skip(elided).collect(), ptr, cells, outputs),
        Done::None => (insts, 0, vec![], vec![]),
    }
}
