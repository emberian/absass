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

fn main() -> std::io::Result<()> {
    let mut source = String::new();
    std::io::stdin().read_to_string(&mut source)?;
    let parse = ProgramParser::parse(Rule::toplevel, &source).unwrap();
    let pgm = Program::from_parse(parse);
    println!("{:?}", pgm);
    Ok(())
}
