#![feature(label_break_value)]

mod ir;
mod comp;

use std::{
    env,
    fs::File,
    io::{self, prelude::*},
};

#[derive(Debug)]
pub enum Error {
    IoError(io::Error),
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Self {
        Error::IoError(err)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Right,
    Left,
    Inc,
    Dec,
    Output,
    Input,
    Loop(Vec<Token>),
}

fn parse(txt: String) -> Result<Vec<Token>, Error> {
    fn read_body(chars: &mut impl Iterator<Item=char>) -> Vec<Token> {
        let mut body = vec![];
        let mut bracket_count = 0;
        loop {
            match {
                if let Some(c) = chars.next() {
                    c
                } else {
                    return body;
                }
            } {
                '>' => body.push(Token::Right),
                '<' => body.push(Token::Left),
                '+' => body.push(Token::Inc),
                '-' => body.push(Token::Dec),
                '.' => body.push(Token::Output),
                ',' => body.push(Token::Input),
                '[' => body.push(Token::Loop(read_body(chars))),
                ']' => return body,
                _ => {},
            }
        }
    }

    Ok(read_body(&mut txt.chars()))
}

const USAGE: &'static str = "Usage: ./bfa <input file> <output file>";

fn main() -> Result<(), Error> {
    let in_file = env::args().nth(1).expect(USAGE);
    let out_file = env::args().nth(2).expect(USAGE);
    println!("(input = {}, output = {})", in_file, out_file);

    let mut in_txt = String::new();
    File::open(in_file)?.read_to_string(&mut in_txt)?;

    let tokens = parse(in_txt)?;
    //println!("Unoptimised ({} tokens):\n{:?}", tokens.len(), tokens);
    let ir = ir::from_tokens(tokens);
    let ir = ir::optimise(ir);
    //println!("Optimised ({} instructions):\n{:?}", ir.len(), ir);
    let out_txt = comp::compile(ir)?;

    write!(File::create(out_file)?, "{}", out_txt)?;

    Ok(())
}
