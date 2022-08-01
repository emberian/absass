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
use gen::{Cx, Gen, insn::ToAsm, rall::{RAlloc, RaCx}};

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
    for (time, line) in lines.iter().enumerate() {
        println!("{}: {}", time, line.to_asm());
    }
    println!("\t(the result is in {})", res.place.unwrap().to_asm());
    let mut rc = RaCx::new(Default::default(), lines.clone());
    rc.allocate();
    for temp_desc in rc.temp_descs.values() {
        println!("temp {}:", temp_desc.reg);
        if temp_desc.spans.is_empty() {
            println!("\tNo spans");
        } else {
            for span in &temp_desc.spans {
                println!("\t{} -> {} (in reg {:?})", span.0, span.1, span.2);
            }
        }
    }
    let mut descs = rc.temp_descs.values().cloned().collect::<Vec<_>>();
    descs.sort_by_key(|desc| desc.reg);
    for (time, line) in lines.iter().enumerate() {
        for d in &descs {
            print!("{}", if d.defined_at(time) {
                'V'
            } else if d.used_at(time) {
                '-'
            } else if d.live_at(time) {
                '|'
            } else {
                ' '
            });
        }
        println!(" . {}: {}", time, line.to_asm());
    }
    let newlines = rc.to_linear();
    for (time, line) in newlines.iter().enumerate() {
        println!("{}: {}", time, line.to_asm());
    }
    Ok(())
}
