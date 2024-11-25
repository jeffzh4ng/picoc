use picoc089::{lexer, parser, translator, typer};
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
    picoc: aot optimizing C89 compiler
    "
    );

    let strat = env::args().nth(1).expect("picoc-error: no strategy given");
    println!("picoc-info: received strategy: {strat}");

    let src = env::args()
        .nth(2)
        .expect("picoc-error: no source file given");
    println!("picoc-info: received source: {src}");

    let chars = fs::read(src)
        .expect("picoc-error: file dne`")
        .iter()
        .map(|b| *b as char)
        .collect::<Vec<_>>();
    let tokens = lexer::lex(&chars).unwrap();
    let src_tree = parser::parse_prg(&tokens).unwrap(); // recursive descent -> pratt parsing
    println!("picoc-info: tree: {:?}", src_tree);

    let typ = typer::type_prg(&src_tree).unwrap();
    println!("picoc-info: type: {:?}", typ);

    match strat.as_str() {
        // "interpretc0" => {
        //     let val = evaluator::eval_prg(&src_tree).unwrap();
        //     println!("picoc-info: evaluated: {val}");
        // }
        "compilec89" => {
            let trgt_tree = translator::translate(&src_tree); // tree -> ssa -> son
                                                              // let abs_as = selector::select(&trgt_tree); // maximal munch -> peephole
                                                              //   let assembly = allocator::allocate(&abstract_assembly).unwrap(); // graph coloring -> linear-scan

            // let mut f = fs::File::create("./tmp.s").expect("picoc-error: unable to create file");
            // f.write_all(assembly.join("\n").as_bytes())
            //     .expect("picoc-error: unable to write data");
        }
        _ => {
            println!("picoc-error: unknown strategy: {:?}", strat);
            std::process::exit(1);
        }
    }
}
