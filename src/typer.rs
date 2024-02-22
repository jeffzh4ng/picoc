use crate::rep::{Expr, Program, Statement};

// c is statically, "weakly" typed
// loopholes
// - casting
// - void*

pub fn type_program(p: &Program) -> bool {
    match &p.main_function.statement {
        Statement::Return(e) => type_expr(&e),
    }
}

fn type_expr(e: &Expr) -> bool {
    match e {
        Expr::Num(_) => true,
        Expr::String(_) => todo!(),
        Expr::Binary { op, l, r } => type_expr(l) && type_expr(r),
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use crate::{lexer, parser, rep::Op};

    use super::*;

    #[test]
    fn test_valid() {
        #[rustfmt::skip]
        let chars = fs::read("tests/valid/hello.c")
            .expect("Should have been able to read the file")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let tokens = lexer::scan(&chars);
        let tree = parser::parse_program(tokens).unwrap();
        let judgement = type_program(&tree);
        insta::assert_yaml_snapshot!(judgement);
    }

    #[test]
    fn test_valid_addition() {
        #[rustfmt::skip]
        let chars = fs::read("tests/valid/arithmetic/addition.c")
            .expect("Should have been able to read the file")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let tokens = lexer::scan(&chars);
        let tree = parser::parse_program(tokens).unwrap();
        let judgement = type_program(&tree);
        insta::assert_yaml_snapshot!(judgement);
    }

    #[test]
    fn test_valid_addition_multi() {
        #[rustfmt::skip]
        let chars = fs::read("tests/valid/arithmetic/addition_multi.c")
            .expect("Should have been able to read the file")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let tokens = lexer::scan(&chars);
        let tree = parser::parse_program(tokens).unwrap();
        let judgement = type_program(&tree);
        insta::assert_yaml_snapshot!(judgement);
    }

    #[test]
    fn test() {
        let input = Expr::Binary {
            op: Op::Add,
            l: Box::new(Expr::Num(9)),
            r: Box::new(Expr::Num(10)),
        };

        let output = type_expr(&input);
        assert!(output);
    }
}
