use std::convert::Infallible;
use std::future::Future;
use std::net::SocketAddr;
use std::pin::Pin;
use std::sync::mpsc::{Receiver, Sender, TryRecvError};
use std::sync::Mutex;
use std::task::{Context, Poll};

use actix_web::{get, web, App, HttpRequest, HttpResponse, HttpServer, Responder};
use assrs::Machine;
use mlua::{Function, Lua, Table};
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Clone)]
enum Req {
    SetProgram(String),
    SetReg(u8, assrs::Word),
    GetRegs,
    RunProg,
    SetMem {
        addr: assrs::Word,
        data: Vec<u8>,
    },
    GetMem {
        addr: assrs::Word,
        size: assrs::Word,
    },
    SetBP(u8, assrs::Word),
    GetBPs,
    ClearBP(u8),
    Reset,
    SendText(String),
    Halt,
    Poll,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
enum Resp {
    Regs(Vec<assrs::Word>),
    Text(String),
    BPReached(u8),
    MemContents(Vec<u8>),
    AssemblerError(String),
    Halted,
    Ok,
}

const ASSEMBLER_SRC: &str = include_str!(concat!(env!("OUT_DIR"), "/assembler.lua"));

fn run_mach(rx: Receiver<(Req, oneshot::Sender<Resp>)>) {
    let mut mach = Machine::default();
    let lua = Lua::new();
    let (new, run): (Function, Function) = {
        let tbl: Table = lua.load(ASSEMBLER_SRC).eval().unwrap();
        let asm: Table = tbl.get("Assembler").unwrap();
        (tbl.get("Assembler").unwrap(), asm.get("run").unwrap())
    };
    let mut halted = true;
    loop {
        match rx.try_recv() {
            Ok((req, tx)) => match req {
                Req::SetProgram(asm) => {
                    let neasm: Table = new.call(64).unwrap();
                    let () = run.call((neasm.clone(), asm)).unwrap();
                    let buf: Table = neasm.get("buf").unwrap();
                    let prog_bytes = buf.len().unwrap() as usize;
                    if prog_bytes > mach.memory.len() {
                        mach.memory
                            .extend(std::iter::repeat(0).take(prog_bytes - mach.memory.len()));
                    }
                    for i in 1..=prog_bytes {
                        mach.memory[i - 1] = buf.get(i).unwrap();
                    }
                    mach.regs[0] = 0;
                    tx.send(Resp::Ok).unwrap();
                }
                Req::SetReg(reg, val) => {
                    mach.regs[reg as usize] = val;
                    tx.send(Resp::Ok).unwrap();
                }
                Req::GetRegs => {
                    tx.send(Resp::Regs(mach.regs.to_vec())).unwrap();
                }
                Req::RunProg => {
                    halted = false;
                    tx.send(Resp::Halted).unwrap();
                }
                Req::SetBP(reg, val) => {}
                Req::GetBPs => todo!(),
                Req::ClearBP(_) => todo!(),
                Req::Reset => todo!(),
                Req::SendText(_) => todo!(),
                Req::SetMem { addr, data } => todo!(),
                Req::GetMem { addr, size } => todo!(),
                Req::Halt => {
                    halted = true;
                    tx.send(Resp::Halted).unwrap();
                }
                Req::Poll => todo!(),
            },
            Err(TryRecvError::Empty) => {
                std::thread::sleep(std::time::Duration::from_millis(10));
            }
            Err(TryRecvError::Disconnected) => {
                eprintln!("machine pipe broke");
                break;
            }
        }
    }
}

async fn http_server(
    req: web::Json<Req>,
    tx: web::Data<Sender<(Req, oneshot::Sender<Resp>)>>,
) -> web::Json<Resp> {
    let (resptx, resprx) = oneshot::channel();
    tx.send((req.into_inner(), resptx)).unwrap();
    web::Json(resprx.await.unwrap())
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    let (tx, rx) = std::sync::mpsc::channel();
    run_mach(rx);

    HttpServer::new(move || {
        App::new()
            .app_data(tx.clone())
            .service(web::resource("/avm").to(http_server))
    })
    .bind(("127.0.0.1", 8080))?
    .run()
    .await
}
