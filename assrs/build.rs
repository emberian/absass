fn main() {
    let out_dir = std::env::var("OUT_DIR").unwrap();

    std::process::Command::new("moonc")
        .arg("-o")
        .arg(out_dir + "/assembler.lua")
        .arg("../assembler/assembler.moon")
        .spawn()
        .unwrap();

    println!("cargo:rerun-if-changed=../assembler/assembler.moon");
}