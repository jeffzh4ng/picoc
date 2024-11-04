use picoc089::{evaluator, lexer, parser};
use std::{env, fs};

fn main() {
    println!(
        "
    ⠀⠀⠀⠀⠀⣼⣧⠀⠀⠀⠀⠀
    ⠀⠀⠀⠀⣼⣿⣿⣧⠀⠀⠀⠀
    ⠀⠀⠀⠾⠿⠿⠿⠿⠷⠀⠀⠀
    ⠀⠀⣼⣆⠀⠀⠀⠀⣰⣧⠀⠀
    ⠀⣼⣿⣿⣆⠀⠀⣰⣿⣿⣧⠀
    ⠾⠟⠿⠿⠿⠧⠼⠿⠿⠿⠻⠷
    picoc089: C0 intepreter, C89 compiler
    "
    );

    let strat = env::args()
        .nth(1)
        .expect("picoc089-error: no strategy given");
    println!("picoc089-info: received strategy: {strat}");

    let src = env::args()
        .nth(2)
        .expect("picoc089-error: no source file given");
    println!("picoc089-info: received source: {src}");

    let chars = fs::read(src)
        .expect("picoc089-error: file dne`")
        .iter()
        .map(|b| *b as char)
        .collect::<Vec<_>>();
    let tokens = lexer::lex(&chars).unwrap();
    let tree = parser::parse_prg(&tokens).unwrap(); // C0 is a subset of C89 and share the same syntax

    match strat.as_str() {
        "interpretc0" => {
            let val = evaluator::eval_prg(tree);
            println!("picoc089-info: evaluated: {val}");
        }
        // "compilec89" => {
        //     let assembly = generator::gen(tree);

        //     let trgt = "./tmp.s";
        //     println!("picoc089-info: generating target: {trgt}");
        //     let mut f = fs::File::create(trgt).expect("picoc089-error: unable to create file");
        //     f.write_all(assembly.join("\n").as_bytes())
        //         .expect("picoc089-error: unable to write data");
        // }
        _ => {
            println!("picoc089-error: unknown strategy: {:?}", strat);
            std::process::exit(1);
        }
    }
}
