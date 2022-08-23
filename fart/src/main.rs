// adapted from ftdi example

use rand::Fill;
use std::io::{Read, Write};

fn main() {
    let device = ftdi::find_by_vid_pid(0x0403, 0x6010)
        .interface(ftdi::Interface::B)
        .open();

    let mut shitty = false;
    let mut failed_ping = false;
    let mut fucked_up = vec![];
    let mut random_failed = false;

    if let Ok(mut device) = device {
        device.usb_reset().unwrap();
        device
            .configure(ftdi::Bits::Eight, ftdi::StopBits::One, ftdi::Parity::None)
            .unwrap();
        device.set_baud_rate(9600).unwrap();
        device.usb_purge_buffers().unwrap();
        device.set_latency_timer(5).unwrap();

        let mut reply = vec![];
        let mut junk = vec![];

        device.read_to_end(&mut junk).unwrap();
        if junk.len() > 0 {
            shitty = true;
        }

        device.write_all(&vec![0x56]).unwrap();

        device.read_to_end(&mut reply).unwrap();
        if reply != vec![0x56] {
            failed_ping = true;
        }
        reply.clear();

        for num in 0u16..256 {
            let num = num as u8;

            device.write_all(&vec![num]).unwrap();
            device.read_to_end(&mut reply).unwrap();
            if reply != vec![num] {
                fucked_up.push((num, reply.clone()));
            }
            reply.clear();
        }

        let mut rd = [0u8; 1920];
        rd.try_fill(&mut rand::thread_rng()).unwrap();
        device.write_all(&rd).unwrap();
        device.read_to_end(&mut reply).unwrap();
        if reply != rd {
            random_failed = true;
        }

        if shitty || failed_ping || fucked_up.len() != 0 || random_failed {
            eprintln!("wow!!! the ass sucks ass!! i just want it fart");
            eprintln!(
                "{:?}{:?}{:?}{:?}",
                shitty, failed_ping, fucked_up, random_failed
            );
        } else {
            eprintln!("want a whiff?");
            let mut fart = reply.into_iter();
            for _ in 0..24 {
                for _ in 0..80 {
                    eprint!(
                        "{}",
                        char::from_u32(0x2580 + ((fart.next().unwrap() as u32) % 32)).unwrap()
                    )
                }
                eprint!("\n")
            }
        }
    } else {
        eprintln!("whose ass do YOU fart out of?");
    }
}
