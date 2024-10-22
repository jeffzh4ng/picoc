use din::{generator, lexer, parser};
use std::{env, fs, io::Write};

fn main() {
    println!(
        "
    ⠀⠀⠀⠀⠀⣼⣧⠀⠀⠀⠀⠀
    ⠀⠀⠀⠀⣼⣿⣿⣧⠀⠀⠀⠀
    ⠀⠀⠀⠾⠿⠿⠿⠿⠷⠀⠀⠀
    ⠀⠀⣼⣆⠀⠀⠀⠀⣰⣧⠀⠀
    ⠀⣼⣿⣿⣆⠀⠀⣰⣿⣿⣧⠀
    ⠾⠟⠿⠿⠿⠧⠼⠿⠿⠿⠻⠷
    picoc: C89/90 -> RV32I
    "
    );

    let src = env::args().nth(1).expect("error: no source file given");
    println!("Compiling source: {src}");

    let chars = fs::read(src)
        .expect("Should have been able to read the file")
        .iter()
        .map(|b| *b as char)
        .collect::<Vec<_>>();

    let tokens = lexer::lex(&chars);
    let tree = parser::parse(tokens).unwrap();
    let assembly = generator::gen(tree);

    let trgt = "./tmp.s";
    println!("Generating target: {trgt}");
    let mut f = fs::File::create(trgt).expect("Unable to create file");
    f.write_all(assembly.join("\n").as_bytes())
        .expect("Unable to write data");
}
