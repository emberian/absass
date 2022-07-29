extern crate pest;
#[macro_use]
extern crate pest_derive;
#[macro_use]
extern crate enum_repr;

pub mod grammar;
pub mod gen;

use std::io::Read;
use pest::Parser;
use grammar::{ProgramParser, Rule, Program};
use gen::{Cx, Gen, insn::ToAsm};

fn main() -> std::io::Result<()> {
    let mut source = String::new();
    std::io::stdin().read_to_string(&mut source)?;
    let parse = ProgramParser::parse(Rule::toplevel, &source).unwrap();
    let pgm = Program::from_parse(parse);
    println!("{:?}", pgm);
    let (mut cx, top) = Cx::new(pgm);
    let res = top.gen(&mut cx);
    println!("{:?}", res);
    let lines = res.block.unwrap().to_linear();
    for line in lines {
        println!("{}", line.to_asm());
    }
    Ok(())
}
