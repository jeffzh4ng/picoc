use crate::parser;

// c is statically, "weakly" typed
// loopholes
// - casting
// - void*

// pub fn type_program(p: &parser::Program) -> bool {
//     match &p.main_function.statement {
//         parser::Stmt::Return(e) => type_expr(e),
//         parser::Stmt::If { cond, then, els } => todo!(),
//     }
// }

// fn type_expr(e: &parser::Expr) -> bool {
//     match e {
//         parser::Expr::Num(_) => true,
//         parser::Expr::String(_) => todo!(),
//         parser::Expr::BinaryE { op: _, l, r } => type_expr(l) && type_expr(r),
//     }
// }

// #[cfg(test)]
// mod tests {
//     use std::fs;

//     use crate::{ir, lexer, parser};

//     use super::*;

//     #[test]
//     fn test_valid() {
//         #[rustfmt::skip]
//         let chars = fs::read("tests/valid/hello.c")
//             .expect("file dne")
//             .iter()
//             .map(|b| *b as char)
//             .collect::<Vec<_>>();

//         let tokens = lexer::scan(&chars);
//         let tree = parser::parse_program(tokens).unwrap();
//         let judgement = type_program(&tree);
//         insta::assert_yaml_snapshot!(judgement);
//     }

//     #[test]
//     fn test_valid_add() {
//         #[rustfmt::skip]
//         let chars = fs::read("tests/valid/arithmetic/add.c")
//             .expect("file dne")
//             .iter()
//             .map(|b| *b as char)
//             .collect::<Vec<_>>();

//         let tokens = lexer::scan(&chars);
//         let tree = parser::parse_program(tokens).unwrap();
//         let judgement = type_program(&tree);
//         insta::assert_yaml_snapshot!(judgement);
//     }

//     #[test]
//     fn test_valid_add_multi() {
//         #[rustfmt::skip]
//         let chars = fs::read("tests/valid/arithmetic/add_multi.c")
//             .expect("file dne")
//             .iter()
//             .map(|b| *b as char)
//             .collect::<Vec<_>>();

//         let tokens = lexer::scan(&chars);
//         let tree = parser::parse_program(tokens).unwrap();
//         let judgement = type_program(&tree);
//         insta::assert_yaml_snapshot!(judgement);
//     }

//     #[test]
//     fn test_valid_subtraction() {
//         #[rustfmt::skip]
//         let chars = fs::read("tests/valid/arithmetic/sub.c")
//             .expect("file dne")
//             .iter()
//             .map(|b| *b as char)
//             .collect::<Vec<_>>();

//         let tokens = lexer::scan(&chars);
//         let tree = parser::parse_program(tokens).unwrap();
//         let judgement = type_program(&tree);
//         insta::assert_yaml_snapshot!(judgement);
//     }

//     #[test]
//     fn test_valid_mult() {
//         #[rustfmt::skip]
//         let chars = fs::read("tests/valid/arithmetic/mult.c")
//             .expect("file dne")
//             .iter()
//             .map(|b| *b as char)
//             .collect::<Vec<_>>();

//         let tokens = lexer::scan(&chars);
//         let tree = parser::parse_program(tokens).unwrap();
//         let judgement = type_program(&tree);
//         insta::assert_yaml_snapshot!(judgement);
//     }

//     #[test]
//     fn test_valid_div() {
//         #[rustfmt::skip]
//         let chars = fs::read("tests/valid/arithmetic/div.c")
//             .expect("file dne")
//             .iter()
//             .map(|b| *b as char)
//             .collect::<Vec<_>>();

//         let tokens = lexer::scan(&chars);
//         let tree = parser::parse_program(tokens).unwrap();
//         let judgement = type_program(&tree);
//         insta::assert_yaml_snapshot!(judgement);
//     }

//     #[test]
//     fn test() {
//         let input = Expr::Binary {
//             op: ir::Op::Add,
//             l: Box::new(Expr::Num(9)),
//             r: Box::new(Expr::Num(10)),
//         };

//         let output = type_expr(&input);
//         assert!(output);
//     }
// }
