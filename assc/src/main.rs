extern crate pest;
#[macro_use]
extern crate pest_derive;
#[macro_use]
extern crate enum_repr;

pub mod grammar;
pub mod gen;

use std::io::Read;
use pest::{Parser, error::{Error, ErrorVariant, LineColLocation}};
use grammar::{ProgramParser, Rule, Program};
use gen::{Cx, Gen, insn::ToAsm, rall::{RAlloc, RaCx}};

fn main() -> std::io::Result<()> {
    let mut source = String::new();
    std::io::stdin().read_to_string(&mut source)?;
    let parse = match ProgramParser::parse(Rule::toplevel, &source) {
        Ok(p) => p,
        Err(ref err @ Error { ref variant, ref line_col, .. }) => match variant {
            ErrorVariant::ParsingError { positives, negatives } => {
                println!("Parse error here:");
                println!("{}", err.line());
                for _ in 0 .. match line_col {
                    LineColLocation::Pos((_, col)) => *col,
                    LineColLocation::Span((_, col), (_, _)) => *col,
                } {
                    print!(" ");
                }
                println!("^");
                println!("Expected whitespace, comment, or any of these:");
                println!("- {}", positives.iter()
                         .map(|r| format!("{:?}", r))
                         .collect::<Vec<_>>()
                         .join(", ")
                );
                println!("While not having one of these:");
                println!("- {}", negatives.iter()
                         .map(|r| format!("{:?}", r))
                         .collect::<Vec<_>>()
                         .join(", ")
                );
                panic!("parsing error");
            },
            o => panic!("Unusual error: {:?}", o),
        },
    };
    let pgm = Program::from_parse(parse);
    println!("{:?}", pgm);
    let (mut cx, top) = Cx::new(pgm);
    let res = top.gen(&mut cx);
    println!("{:?}", res);
    let lines = res.block.unwrap().to_linear();
    for (time, line) in lines.iter().enumerate() {
        println!("{}: {}", time, line.to_asm());
    }
    // println!("\t(the result is in {})", res.place.unwrap().to_asm());
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
    for (_, line) in newlines.iter().enumerate() {
        println!("{}", line.to_asm());
    }
    Ok(())
}
