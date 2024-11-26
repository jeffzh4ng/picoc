use picoc089::{allocator, lexer, parser, selector, translator, typer, OptLevel};
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
    picoc: aot optimizing C89->RV32I compiler
    "
    );

    let strat = env::args().nth(1).expect("picoc-error: no strategy given");
    println!("picoc-info: received strategy: {strat}");

    let src = env::args()
        .nth(2)
        .expect("picoc-error: no source file given");
    println!("picoc-info: received source: {src}");

    let opt: OptLevel = env::args()
        .nth(3)
        .expect("picoc-error: no optimization level given")
        .chars()
        .nth(1)
        .expect("picoc-error: optimization level must be at least 1 character")
        .to_digit(10)
        .expect("picoc-error: invalid optimization level given (invalid number)")
        .try_into()
        .expect("picoc-error: invalid optimization level given (invalid level)");
    println!("picoc-info: received optimization level: {:?}", opt);

    let chars = fs::read(src)
        .expect("picoc-error: file dne`")
        .iter()
        .map(|b| *b as char)
        .collect::<Vec<_>>();
    let tokens = lexer::lex(&chars).unwrap();
    println!("picoc-info: lexed");
    let src_tree = parser::parse_prg(&tokens).unwrap(); // recursive descent -> pratt parsing
    println!("picoc-info: parsed");

    let typ = typer::type_prg(&src_tree).unwrap();
    println!("picoc-info: typed");

    match strat.as_str() {
        // "interpretc0" => {
        //     let val = evaluator::eval_prg(&src_tree).unwrap();
        //     println!("picoc-info: evaluated: {val}");
        // }
        "compilec89" => {
            let trgt_tree = translator::translate(&src_tree);
            println!("picoc-info: translated",);

            let abs_as = selector::select(&trgt_tree);
            println!("picoc-info: selected");
            let assembly = allocator::allocate(&abs_as, opt);
            println!("picoc-info: emitted");

            let mut f = fs::File::create("./tmp.s").expect("picoc-error: unable to create file");
            f.write_all(assembly.join("\n").as_bytes())
                .expect("picoc-error: unable to write data");
        }
        _ => {
            println!("picoc-error: unknown strategy: {:?}", strat);
            std::process::exit(1);
        }
    }
}
