use assrs::*;
use std::io::prelude::*;

pub fn main() {
    match std::env::args().nth(1).as_deref() {
        Some("run") => {
            let mut m = Machine::default();
            if let Some(filename) = std::env::args().nth(2) {
                let mut rdr = std::fs::File::open(filename).expect("opening program file");
                rdr.read_to_end(&mut m.memory).unwrap();
            } else {
                std::io::stdin()
                    .read_to_end(&mut m.memory)
                    .expect("reading program file");
            }
            m.run();
        }
        Some("disasm") => {
            let mut insn_buf = [0u8; 2];
            let mut i = std::io::stdin().lock();
            while let Ok(2) = i.read(&mut insn_buf) {
                println!(
                    "{}",
                    Insn::decode((insn_buf[0] as u16) << 8 | insn_buf[1] as u16)
                );
            }
        }
        Some("dishex") => {
            for l in std::io::stdin().lock().lines() {
                let l = l.unwrap();
                let l = l.trim_start_matches("0x");

                let iv = Insn::decode(u16::from_str_radix(l, 16).unwrap());
                println!("{:?}", iv);
            }
        }

        Some("wtfxf") => {
            let mut m = Machine::default();
            m.regs[1] = 3;
            let i = Insn::Move {
                src: 1,
                dst: 1,
                s_mode: MoveMode::Incr,
                s_deref: false,
                d_mode: MoveMode::Incr,
                d_deref: false,
            };
            m.exec(i);
            println!("after {}\t\t R1 = {}", i.to_asm(), m.regs[1]);
            m.regs[1] = 3;

            let i = Insn::Move {
                src: 1,
                dst: 1,
                s_mode: MoveMode::Direct,
                s_deref: false,
                d_mode: MoveMode::Incr,
                d_deref: false,
            };

            m.exec(i);
            println!("after {}\t\t R1 = {}", i.to_asm(), m.regs[1]);
            m.regs[1] = 0;
            println!("{:?}", m);
            let i = Insn::Move {
                src: 1,
                dst: 1,
                s_mode: MoveMode::Incr,
                s_deref: false,
                d_mode: MoveMode::Incr,
                d_deref: true,
            };

            m.exec(i);
            println!("after {}\t\t R1 = {}", i.to_asm(), m.regs[1]);
            println!("{:4x}", i.encode());

        }

        Some("gen_opcode") => match std::env::args().nth(2).as_deref() {
            Some("html") => {
                println!("<html>");
                println!("<head><style type=\"text/css\">.valid {{ background-color: #0f03; }} table {{ border-collapse: collapse; }} table td {{ border: 1px solid #000; }}</style></head>");
                println!("<body><table><tr>");
                for insn in 0..=std::u16::MAX {
                    if insn != 0 && insn % 256 == 0 {
                        println!("</tr></tr>")
                    }
                    let ins = Insn::decode(insn);
                    match ins.brief() {
                        None => println!("<td></td><!--{:04x}-->", insn),
                        Some(b) => println!("<td class=\"valid\">{}</td><!--{:04x}-->", b, insn),
                    }
                }
                println!("</tr></table></body></html>");
            }
            Some("ppm") => {
                println!("P3");
                println!("256 256 255");
                for insn in 0..=std::u16::MAX {
                    let insn = Insn::decode(insn);
                    let col = insn.color();
                    println!("{} {} {}", col.0, col.1, col.2);
                }
            }
            Some(_) => {
                eprintln!("bad format; try html, ppm");
            }
            None => {
                eprintln!("no format argument");
            }
        },
        Some("unused") => {
            let all = (0..u16::MAX).map(|i| Insn::decode(i)).collect::<Vec<_>>();
            let mut all_used = all.iter().map(|i| i.encode()).collect::<Vec<_>>();
            for i in 0..u16::MAX {
                let f = Insn::decode(i);
                let fenc = f.encode();
                if let Insn::NotSure { .. } = f {
                    continue;
                }
                if i != fenc {
                    println!(
                        "enc(dec({:04x})) != {:04x}: {} did not examine bits {:016b} / {:016b}",
                        i,
                        fenc,
                        f,
                        i & !f.encode(),
                        !i & f.encode()
                    );
                }
            }
            all_used.sort();
            all_used.dedup();
            let mut first = all_used[0];
            let mut last = all_used[0];
            for used in all_used {
                if used != last + 1 {
                    println!(
                        "{:04x}-{:04x} {} {:04x} {}",
                        first,
                        last,
                        last - first,
                        (first ..=last).fold(0xffff, <u16 as std::ops::BitAnd>::bitand),
                        Insn::decode(last + 1)
                    );

                    first = last;
                }
                last = used;
            }
        }
        Some(_) => eprintln!("no such command!"),
        None => eprintln!("no command given!"),
    }
}
