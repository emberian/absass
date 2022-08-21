use std::convert::Infallible;
use std::future::Future;
use std::net::SocketAddr;
use std::pin::Pin;
use std::sync::mpsc::{Receiver, Sender, TryRecvError};
use std::sync::{Arc, Mutex};
use std::task::{Context, Poll};

use actix_web::middleware::Logger;
use actix_web::{get, web, App, HttpRequest, HttpResponse, HttpServer, Responder};
use assrs::Machine;
use futures::{SinkExt, StreamExt};
use mlua::{Function, Lua, Table, TableExt};
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
    Step,
    SendText(String),
    Halt,
    Poll,
    Run,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
enum Resp {
    Regs(Vec<assrs::Word>),
    Text(String),
    BPReached(u8),
    MemContents(Vec<u8>),
    AssemblerError(String),
    Breakpoints(Vec<Option<assrs::Word>>),
    Halted,
    Ok,
}

const ASSEMBLER_SRC: &str = include_str!(concat!(env!("OUT_DIR"), "/assembler.lua"));

fn run_mach(
    rx: Receiver<(Req, oneshot::Sender<Resp>)>,
    mut events: futures::channel::mpsc::UnboundedSender<Resp>,
) {
    let mut mach = Machine::default();
    let lua = Lua::new();
    let (new, run): (Table, Function) = {
        let tbl: Table = lua.load(ASSEMBLER_SRC).eval().unwrap();
        let asm: Table = tbl.get("Assembler").unwrap();
        (asm.clone(), asm.get("run").unwrap())
    };
    let mut halted = true;
    let mut ln_buf = vec![];
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
                    halted = true;
                }
                Req::Step => {
                    if let assrs::StepOut::Halt = mach.step() {
                        tx.send(Resp::Halted).unwrap();
                    } else {
                        tx.send(Resp::Ok).unwrap();
                    }
                    if let Some(ch) = mach.sr10.take() {
                        ln_buf.push(ch as u8);
                        if ch == 10 {
                            futures::executor::block_on(
                                events.send(Resp::Text(String::from_utf8_lossy(&ln_buf).into())),
                            )
                            .unwrap();
                            ln_buf.clear();
                        }
                    }
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
                Req::SetBP(reg, val) => {
                    mach.bps[reg as usize] = Some(val);
                    tx.send(Resp::Ok).unwrap();
                }
                Req::GetBPs => {
                    tx.send(Resp::Breakpoints(mach.bps.to_vec())).unwrap();
                }
                Req::ClearBP(reg) => {
                    mach.bps[reg as usize] = None;
                    tx.send(Resp::Ok).unwrap();
                }
                Req::Reset => {
                    halted = true;
                    mach.reset();
                    tx.send(Resp::Ok).unwrap();
                }
                Req::SendText(_) => todo!(),
                Req::SetMem { addr, data } => {
                    mach.memory[addr as usize..].copy_from_slice(&data);
                    tx.send(Resp::Ok).unwrap();
                }
                Req::GetMem { addr, size } => {
                    tx.send(Resp::MemContents(
                        mach.memory[addr as usize..addr as usize + size as usize].to_vec(),
                    ))
                    .unwrap();
                }
                Req::Halt => {
                    halted = true;
                    tx.send(Resp::Halted).unwrap();
                }
                Req::Run => {
                    halted = false;
                }
                Req::Poll => unreachable!(),
            },
            Err(TryRecvError::Empty) => {
                if !halted {
                    if let assrs::StepOut::Halt = mach.step() {
                        halted = true;
                        futures::executor::block_on(events.send(Resp::Halted)).unwrap();
                    }
                    if let Some(which) = mach.check_bps() {
                        halted = true;
                        futures::executor::block_on(events.send(Resp::BPReached(which))).unwrap();
                    }
                    if let Some(ch) = mach.sr10.take() {
                        ln_buf.push(ch as u8);
                        if ch == 10 {
                            futures::executor::block_on(
                                events.send(Resp::Text(String::from_utf8_lossy(&ln_buf).into())),
                            )
                            .unwrap();
                            ln_buf.clear();
                        }
                    }
                }
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
    events: web::Data<Arc<Mutex<futures::channel::mpsc::UnboundedReceiver<Resp>>>>,
) -> web::Json<Resp> {
    if let Req::Poll = &*req {
        web::Json(events.lock().unwrap().next().await.unwrap_or(Resp::Halted))
    } else {
        println!("Asked: {:?}", req);
        let (resptx, resprx) = oneshot::channel();
        tx.send((req.into_inner(), resptx)).unwrap();
        let resp = resprx.await.unwrap();
        println!("Answered: {:?}", resp);
        web::Json(resp)
    }
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    let (tx, rx) = std::sync::mpsc::channel();
    let (txe, rxe) = futures::channel::mpsc::unbounded();
    let rcv = web::Data::new(Arc::new(Mutex::new(rxe)));
    std::thread::spawn(|| run_mach(rx, txe));
    println!("{}", serde_json::to_string(&Req::GetRegs).unwrap());

    println!(
        "{}",
        serde_json::to_string(&Req::SetMem {
            addr: 0,
            data: vec![1, 2, 3]
        })
        .unwrap()
    );
    println!(
        "{}",
        serde_json::to_string(&Req::SetProgram("foo".into())).unwrap()
    );

    env_logger::init_from_env(env_logger::Env::new().default_filter_or("debug"));
    HttpServer::new(move || {
        App::new()
            .wrap(Logger::default())
            .app_data(web::Data::new(tx.clone()))
            .app_data(rcv.clone())
            .service(web::resource("/avm").to(http_server))
    })
    .bind(("127.0.0.1", 8080))?
    .run()
    .await
}
