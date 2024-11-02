use crate::{
    lexer::{Token, TokenType},
    Lambda,
};
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, io};

// Program: HashMap<Id, Function>
pub fn parse_program(tokens: Vec<Token>) -> Result<HashMap<String, Lambda>, io::Error> {
    let mut env = HashMap::new();
    let mut r0 = tokens.as_slice();
    while let Ok((f, r1)) = parse_function(r0) {
        env.insert(f.name.clone(), f); // need the clone to avoid partial move
        r0 = r1;
    }

    Ok(env)
}

fn parse_function(tokens: &[Token]) -> Result<(Function, &[Token]), io::Error> {
    let (_, r) = mtch(&tokens, TokenType::KeywordInt)?;
    let (_, r) = mtch(r, TokenType::KeywordMain)?;
    let (_, r) = mtch(r, TokenType::PuncLeftParen)?;
    let (_, r) = mtch(r, TokenType::PuncRightParen)?;
    let (_, r) = mtch(r, TokenType::PuncLeftBrace)?;

    let mut stmts = vec![];
    let mut r0 = r;
    while let Ok((s, r1)) = parse_stmt(r0) {
        stmts.push(s);
        r0 = r1;
    }
    let (_, r) = mtch(r0, TokenType::PuncRightBrace)?;

    if !r.is_empty() {
        // panic?
    }

    Ok((
        Function {
            name: todo!(),
            identifier: todo!(),
            body: stmts,
        },
        r,
    ))
}

fn parse_asmt(tokens: &[Token]) -> Result<(Asnmt, &[Token]), io::Error> {
    match tokens {
        [] => todo!(),
        [f, r @ ..] => match f.typ {
            TokenType::KeywordInt => {
                let (idt, r) = mtch(r, TokenType::Identifier)?;
                let (_, r) = mtch(r, TokenType::Equals)?;
                let (expr, r) = parse_rel_expr(r)?;

                Ok((
                    Asnmt::CreateBind {
                        id: Id(idt.lexeme.to_owned()),
                        expr: Box::new(expr),
                    },
                    r,
                ))
            }
            TokenType::Identifier => match r {
                [] => todo!(),
                [s, t, r @ ..] => match (s.typ, t.typ) {
                    (TokenType::Plus, TokenType::Equals) => {
                        let (expr, r) = parse_rel_expr(r)?;
                        Ok((
                            Asnmt::UpdateBind {
                                id: Id(f.lexeme.parse().unwrap()),
                                op: BinOp::Add,
                                expr: Box::new(expr),
                            },
                            r,
                        ))
                    }
                    (TokenType::Plus, TokenType::Plus) => Ok((
                        Asnmt::UpdateBind {
                            id: Id(f.lexeme.parse().unwrap()),
                            op: BinOp::Add,
                            expr: Box::new(Expr::Int(1)),
                        },
                        r,
                    )),
                    (TokenType::Minus, TokenType::Equals) => {
                        let (expr, r) = parse_rel_expr(r)?;

                        Ok((
                            Asnmt::UpdateBind {
                                id: Id(f.lexeme.parse().unwrap()),
                                op: BinOp::Sub,
                                expr: Box::new(expr),
                            },
                            r,
                        ))
                    }
                    (TokenType::Minus, TokenType::Minus) => Ok((
                        Asnmt::UpdateBind {
                            id: Id(f.lexeme.parse().unwrap()),
                            op: BinOp::Sub,
                            expr: Box::new(Expr::Int(1)),
                        },
                        r,
                    )),
                    (TokenType::Star, TokenType::Equals) => {
                        let (expr, r) = parse_rel_expr(r)?;

                        Ok((
                            Asnmt::UpdateBind {
                                id: Id(f.lexeme.parse().unwrap()),
                                op: BinOp::Mult,
                                expr: Box::new(expr),
                            },
                            r,
                        ))
                    }
                    (TokenType::Slash, TokenType::Equals) => {
                        let (expr, r) = parse_rel_expr(r)?;

                        Ok((
                            Asnmt::UpdateBind {
                                id: Id(f.lexeme.parse().unwrap()),
                                op: BinOp::Div,
                                expr: Box::new(expr),
                            },
                            r,
                        ))
                    }
                    t => {
                        todo!()
                    }
                },
                t => todo!(),
            },
            t => Err(io::Error::new(
                io::ErrorKind::Other,
                format!("token not recognizable {:?}", t),
            )),
        },
    }
}

fn parse_stmt(tokens: &[Token]) -> Result<(Cmd, &[Token]), io::Error> {
    match tokens {
        [] => todo!(),
        [f, r @ ..] => match f.typ {
            TokenType::KeywordInt | TokenType::Identifier => {
                let (a, r) = parse_asmt(tokens)?;
                let (_, r) = mtch(r, TokenType::PuncSemiColon)?;

                Ok((Cmd::Asnmt(a), r))
            }
            TokenType::KeywordRet => {
                let (expr, r) = parse_rel_expr(r)?;
                let (_, r) = mtch(r, TokenType::PuncSemiColon)?;
                Ok((Cmd::Return(expr), r))
            }
            TokenType::KeywordIf => {
                let (_, r) = mtch(r, TokenType::PuncLeftParen)?;
                let (cond, r) = parse_rel_expr(r)?;
                let (_, r) = mtch(r, TokenType::PuncRightParen)?;
                let (_, r) = mtch(r, TokenType::PuncLeftBrace)?;
                let (then, r) = parse_stmt(r)?;
                let (_, r) = mtch(r, TokenType::PuncRightBrace)?;
                let (_, r) = mtch(r, TokenType::KeywordEls)?;
                let (_, r) = mtch(r, TokenType::PuncLeftBrace)?;
                let (els, r) = parse_stmt(r)?;
                let (_, r) = mtch(r, TokenType::PuncRightBrace)?;

                Ok((
                    Cmd::IfEls {
                        cond: Box::new(cond),
                        then: Box::new(then),
                        els: Box::new(els),
                    },
                    r,
                ))
            }
            TokenType::KeywordFor => {
                let (_, r) = mtch(r, TokenType::PuncLeftParen)?;
                let (asnmt, r) = parse_asmt(r)?;
                let (_, r) = mtch(r, TokenType::PuncSemiColon)?;
                let (cond, r) = parse_rel_expr(r)?;
                let (_, r) = mtch(r, TokenType::PuncSemiColon)?;
                let (update, r) = parse_asmt(r)?;
                let (_, r) = mtch(r, TokenType::PuncRightParen)?;
                let (_, r) = mtch(r, TokenType::PuncLeftBrace)?;

                let mut body = vec![];
                let mut r0 = r;
                while let Ok((s, r1)) = parse_stmt(r0) {
                    body.push(s);
                    r0 = r1;
                }
                let (_, r) = mtch(r0, TokenType::PuncRightBrace)?;

                Ok((
                    Cmd::For {
                        asnmt: Box::new(asnmt),
                        cond: Box::new(cond),
                        update: Box::new(update),
                        body,
                    },
                    r,
                ))
            }
            t => Err(io::Error::new(
                io::ErrorKind::Other,
                format!("token not recognizable {:?}", t),
            )),
        },
    }
}

fn parse_rel_expr(tokens: &[Token]) -> Result<(Expr, &[Token]), io::Error> {
    let (left, r) = parse_term(tokens)?;

    match r {
        [] => Ok((left, r)),
        r => {
            let mut cur_node = left;
            let mut r = r;

            while let Ok((op, r_temp)) = parse_rel_op(r) {
                let (right, r_temp) = parse_term(r_temp)?;

                cur_node = Expr::RelE {
                    op,
                    l: Box::new(cur_node),
                    r: Box::new(right),
                };

                r = r_temp;
            }

            Ok((cur_node, r))
        }
    }
}

fn parse_term(tokens: &[Token]) -> Result<(Expr, &[Token]), io::Error> {
    let (left, r) = parse_factor(tokens)?;

    match r {
        [] => Ok((left, r)),
        r => {
            let mut cur_node = left;
            let mut r = r;

            while let Ok((op, r_temp)) = parse_term_op(r) {
                let (right, r_temp) = parse_factor(r_temp)?;

                cur_node = Expr::BinE {
                    op,
                    l: Box::new(cur_node),
                    r: Box::new(right),
                };

                r = r_temp;
            }

            Ok((cur_node, r))
        }
    }
}

fn parse_factor(tokens: &[Token]) -> Result<(Expr, &[Token]), io::Error> {
    let (left, r) = parse_atom(tokens)?;

    match r {
        [] => Ok((left, r)),
        r => {
            let mut cur_node = left;
            let mut r = r;

            while let Ok((op, r_temp)) = parse_factor_op(r) {
                let (right, r_temp) = parse_atom(r_temp)?;

                cur_node = Expr::BinE {
                    op,
                    l: Box::new(cur_node),
                    r: Box::new(right),
                };

                r = r_temp;
            }

            Ok((cur_node, r))
        }
    }
}

fn parse_atom(tokens: &[Token]) -> Result<(Expr, &[Token]), io::Error> {
    match tokens {
        [] => todo!(),
        [f, r @ ..] => match f.typ {
            TokenType::Identifier => Ok((Expr::Var(Id(f.lexeme.to_owned())), r)),
            TokenType::LiteralInt => Ok((Expr::Int(f.lexeme.parse().unwrap()), r)),
            t => Err(io::Error::new(
                io::ErrorKind::Other,
                format!("token not recognizable {:?}", t),
            )),
        },
    }
}

fn parse_rel_op(tokens: &[Token]) -> Result<(RelOp, &[Token]), io::Error> {
    match tokens {
        [] => todo!(),
        [f, r @ ..] => match f.typ {
            TokenType::LeftAngleBracket => match r {
                [] => todo!(),
                [s, r @ ..] => match s.typ {
                    TokenType::Equals => Ok((RelOp::LtEq, r)),
                    _ => Ok((RelOp::Lt, &tokens[1..])), // include s
                },
            },
            TokenType::RightAngleBracket => match r {
                [] => todo!(),
                [s, r @ ..] => match s.typ {
                    TokenType::Equals => Ok((RelOp::GtEq, r)),
                    _ => Ok((RelOp::Gt, &tokens[1..])), // include s
                },
            },
            TokenType::Equals => match r {
                [] => todo!(),
                [s, r @ ..] => match s.typ {
                    TokenType::Equals => Ok((RelOp::Eq, r)),
                    t => Err(io::Error::new(
                        io::ErrorKind::Other,
                        format!("token not recognizable {:?}", t),
                    )),
                },
            },
            TokenType::Bang => match r {
                [] => todo!(),
                [s, r @ ..] => match s.typ {
                    TokenType::Equals => Ok((RelOp::Neq, r)),
                    t => Err(io::Error::new(
                        io::ErrorKind::Other,
                        format!("token not recognizable {:?}", t),
                    )),
                },
            },
            TokenType::Amp => match r {
                [] => todo!(),
                [s, r @ ..] => match s.typ {
                    TokenType::Amp => Ok((RelOp::And, r)),
                    t => Err(io::Error::new(
                        io::ErrorKind::Other,
                        format!("token not recognizable {:?}", t),
                    )),
                },
            },
            TokenType::Bar => match r {
                [] => todo!(),
                [s, r @ ..] => match s.typ {
                    TokenType::Bar => Ok((RelOp::Or, r)),
                    t => Err(io::Error::new(
                        io::ErrorKind::Other,
                        format!("token not recognizable {:?}", t),
                    )),
                },
            },
            t => Err(io::Error::new(
                io::ErrorKind::Other,
                format!("token not recognizable {:?}", t),
            )),
        },
    }
}

fn parse_term_op(tokens: &[Token]) -> Result<(BinOp, &[Token]), io::Error> {
    match tokens {
        [] => todo!(),
        [f, r @ ..] => match f.typ {
            TokenType::Plus => Ok((BinOp::Add, r)),
            TokenType::Minus => Ok((BinOp::Sub, r)),
            t => Err(io::Error::new(
                io::ErrorKind::Other,
                format!("token not recognizable {:?}", t),
            )),
        },
    }
}

fn parse_factor_op(tokens: &[Token]) -> Result<(BinOp, &[Token]), io::Error> {
    match tokens {
        [] => todo!(),
        [f, r @ ..] => match f.typ {
            TokenType::Star => Ok((BinOp::Mult, r)),
            TokenType::Slash => Ok((BinOp::Div, r)),
            t => Err(io::Error::new(
                io::ErrorKind::Other,
                format!("token not recognizable {:?}", t),
            )),
        },
    }
}

fn mtch(tokens: &[Token], tt: TokenType) -> Result<(&Token, &[Token]), io::Error> {
    match tokens {
        [] => todo!(),
        [f, r @ ..] => {
            if f.typ == tt {
                // Use an if-guard to compare values
                Ok((f, r))
            } else {
                Err(io::Error::new(
                    io::ErrorKind::Other,
                    format!("expected: {:?} got: {:?}", tt, f),
                ))
            }
        }
    }
}

#[cfg(test)]
mod test_legal_arithmetic {
    use crate::lexer;
    use std::fs;

    const TEST_DIR: &str = "tests/fixtures/legal/arithmetic";

    #[test]
    fn lit() {
        let chars = fs::read(format!("{TEST_DIR}/lit.c"))
            .expect("Should have been able to read the file")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let tokens = lexer::lex(&chars);
        let tree = super::parse_program(tokens).unwrap();
        insta::assert_yaml_snapshot!(tree, @r###"
        ---
        main_function:
          stmts:
            - Return:
                Int: 8
        "###);
    }

    #[test]
    fn add() {
        let chars = fs::read(format!("{TEST_DIR}/add.c"))
            .expect("Should have been able to read the file")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let tokens = lexer::lex(&chars);
        let tree = super::parse_program(tokens).unwrap();
        insta::assert_yaml_snapshot!(tree, @r###"
        ---
        main_function:
          stmts:
            - Return:
                BinE:
                  op: Add
                  l:
                    Int: 9
                  r:
                    Int: 10
        "###);
    }

    #[test]
    fn add_multi() {
        let chars = fs::read(format!("{TEST_DIR}/add_multi.c"))
            .expect("Should have been able to read the file")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let tokens = lexer::lex(&chars);
        let tree = super::parse_program(tokens).unwrap();
        insta::assert_yaml_snapshot!(tree, @r###"
        ---
        main_function:
          stmts:
            - Return:
                BinE:
                  op: Add
                  l:
                    BinE:
                      op: Add
                      l:
                        Int: 9
                      r:
                        Int: 10
                  r:
                    Int: 11
        "###);
    }

    #[test]
    fn sub() {
        #[rustfmt::skip]
        let chars = fs::read(format!("{TEST_DIR}/sub.c"))
            .expect("Should have been able to read the file")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let tokens = lexer::lex(&chars);
        let tree = super::parse_program(tokens).unwrap();
        insta::assert_yaml_snapshot!(tree, @r###"
        ---
        main_function:
          stmts:
            - Return:
                BinE:
                  op: Sub
                  l:
                    Int: 88
                  r:
                    Int: 32
        "###);
    }

    #[test]
    fn mult() {
        #[rustfmt::skip]
        let chars = fs::read(format!("{TEST_DIR}/mult.c"))
            .expect("Should have been able to read the file")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let tokens = lexer::lex(&chars);
        let tree = super::parse_program(tokens).unwrap();
        insta::assert_yaml_snapshot!(tree, @r###"
        ---
        main_function:
          stmts:
            - Return:
                BinE:
                  op: Mult
                  l:
                    Int: 9
                  r:
                    Int: 10
        "###);
    }

    #[test]
    fn div() {
        #[rustfmt::skip]
        let chars = fs::read(format!("{TEST_DIR}/div.c"))
            .expect("Should have been able to read the file")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let tokens = lexer::lex(&chars);
        let tree = super::parse_program(tokens).unwrap();
        insta::assert_yaml_snapshot!(tree, @r###"
        ---
        main_function:
          stmts:
            - Return:
                BinE:
                  op: Div
                  l:
                    Int: 100
                  r:
                    Int: 9
        "###);
    }
}

#[cfg(test)]
mod test_legal_arithmetic_precedence {
    use crate::lexer;
    use std::fs;

    const TEST_DIR: &str = "tests/fixtures/legal/arithmetic_precedence";

    #[test]
    fn add_associative() {
        let chars = fs::read(format!("{TEST_DIR}/add_associative.c"))
            .expect("Should have been able to read the file")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let tokens = lexer::lex(&chars);
        let tree = super::parse_program(tokens).unwrap();
        insta::assert_yaml_snapshot!(tree, @r###"
        ---
        main_function:
          stmts:
            - Return:
                BinE:
                  op: Add
                  l:
                    BinE:
                      op: Add
                      l:
                        Int: 9
                      r:
                        Int: 10
                  r:
                    Int: 11
        "###);
    }

    #[test]
    fn sub_associative() {
        let chars = fs::read(format!("{TEST_DIR}/sub_associative.c"))
            .expect("Should have been able to read the file")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let tokens = lexer::lex(&chars);
        let tree = super::parse_program(tokens).unwrap();
        insta::assert_yaml_snapshot!(tree, @r###"
        ---
        main_function:
          stmts:
            - Return:
                BinE:
                  op: Sub
                  l:
                    BinE:
                      op: Sub
                      l:
                        Int: 30
                      r:
                        Int: 9
                  r:
                    Int: 10
        "###);
    }

    #[test]
    fn mult_add_precedence() {
        let chars = fs::read(format!("{TEST_DIR}/mult_add_precedence.c"))
            .expect("Should have been able to read the file")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let tokens = lexer::lex(&chars);
        let tree = super::parse_program(tokens).unwrap();
        insta::assert_yaml_snapshot!(tree, @r###"
        ---
        main_function:
          stmts:
            - Return:
                BinE:
                  op: Add
                  l:
                    BinE:
                      op: Mult
                      l:
                        Int: 9
                      r:
                        Int: 10
                  r:
                    Int: 11
        "###);
    }

    #[test]
    fn mult_add_precedence_multi() {
        let chars = fs::read(format!("{TEST_DIR}/mult_add_precedence_multi.c"))
            .expect("Should have been able to read the file")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let tokens = lexer::lex(&chars);
        let tree = super::parse_program(tokens).unwrap();
        insta::assert_yaml_snapshot!(tree, @r###"
        ---
        main_function:
          stmts:
            - Return:
                BinE:
                  op: Add
                  l:
                    BinE:
                      op: Mult
                      l:
                        Int: 9
                      r:
                        Int: 10
                  r:
                    BinE:
                      op: Mult
                      l:
                        Int: 11
                      r:
                        Int: 12
        "###);
    }
}

#[cfg(test)]
mod test_legal_control_flow {
    use crate::lexer;
    use std::fs;

    const TEST_DIR: &str = "tests/fixtures/legal/control_flow";

    #[test]
    fn eq() {
        let chars = fs::read(format!("{TEST_DIR}/eq_true.c"))
            .expect("Should have been able to read the file")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let tokens = lexer::lex(&chars);
        let tree = super::parse_program(tokens).unwrap();
        insta::assert_yaml_snapshot!(tree, @r###"
        ---
        main_function:
          stmts:
            - Return:
                RelE:
                  op: Eq
                  l:
                    Int: 9
                  r:
                    Int: 9
        "###);
    }

    #[test]
    fn neq() {
        let chars = fs::read(format!("{TEST_DIR}/neq_true.c"))
            .expect("Should have been able to read the file")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let tokens = lexer::lex(&chars);
        let tree = super::parse_program(tokens).unwrap();
        insta::assert_yaml_snapshot!(tree, @r###"
        ---
        main_function:
          stmts:
            - Return:
                RelE:
                  op: Neq
                  l:
                    Int: 9
                  r:
                    Int: 10
        "###);
    }

    #[test]
    fn and() {
        let chars = fs::read(format!("{TEST_DIR}/and_true.c"))
            .expect("Should have been able to read the file")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let tokens = lexer::lex(&chars);
        let tree = super::parse_program(tokens).unwrap();
        insta::assert_yaml_snapshot!(tree, @r###"
        ---
        main_function:
          stmts:
            - Return:
                RelE:
                  op: And
                  l:
                    Int: 1
                  r:
                    Int: 1
        "###);
    }

    #[test]
    fn or() {
        let chars = fs::read(format!("{TEST_DIR}/or_true.c"))
            .expect("Should have been able to read the file")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let tokens = lexer::lex(&chars);
        let tree = super::parse_program(tokens).unwrap();
        insta::assert_yaml_snapshot!(tree, @r###"
        ---
        main_function:
          stmts:
            - Return:
                RelE:
                  op: Or
                  l:
                    Int: 1
                  r:
                    Int: 1
        "###);
    }

    #[test]
    fn lt() {
        let chars = fs::read(format!("{TEST_DIR}/lt_true.c"))
            .expect("Should have been able to read the file")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let tokens = lexer::lex(&chars);
        let tree = super::parse_program(tokens).unwrap();
        insta::assert_yaml_snapshot!(tree, @r###"
        ---
        main_function:
          stmts:
            - Return:
                RelE:
                  op: Lt
                  l:
                    Int: 9
                  r:
                    Int: 10
        "###);
    }

    #[test]
    fn gt() {
        let chars = fs::read(format!("{TEST_DIR}/gt_true.c"))
            .expect("Should have been able to read the file")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let tokens = lexer::lex(&chars);
        let tree = super::parse_program(tokens).unwrap();
        insta::assert_yaml_snapshot!(tree, @r###"
        ---
        main_function:
          stmts:
            - Return:
                RelE:
                  op: Gt
                  l:
                    Int: 10
                  r:
                    Int: 9
        "###);
    }

    #[test]
    fn ifels_then() {
        let chars = fs::read(format!("{TEST_DIR}/ifels_then.c"))
            .expect("Should have been able to read the file")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let tokens = lexer::lex(&chars);
        let tree = super::parse_program(tokens).unwrap();
        insta::assert_yaml_snapshot!(tree, @r###"
        ---
        main_function:
          stmts:
            - IfEls:
                cond:
                  RelE:
                    op: Lt
                    l:
                      Int: 9
                    r:
                      Int: 10
                then:
                  Return:
                    Int: 0
                els:
                  Return:
                    Int: 1
        "###);
    }

    #[test]
    fn for_loop() {
        let chars = fs::read(format!("{TEST_DIR}/for.c"))
            .expect("Should have been able to read the file")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let tokens = lexer::lex(&chars);
        let tree = super::parse_program(tokens).unwrap();
        insta::assert_yaml_snapshot!(tree, @r###"
        ---
        main_function:
          stmts:
            - Asnmt:
                CreateBind:
                  id: n
                  expr:
                    Int: 0
            - For:
                asnmt:
                  CreateBind:
                    id: i
                    expr:
                      Int: 0
                cond:
                  RelE:
                    op: Lt
                    l:
                      Var: i
                    r:
                      Int: 10
                update:
                  UpdateBind:
                    id: i
                    op: Add
                    expr:
                      Int: 1
                body:
                  - Asnmt:
                      UpdateBind:
                        id: n
                        op: Add
                        expr:
                          Int: 1
                  - Asnmt:
                      UpdateBind:
                        id: n
                        op: Add
                        expr:
                          Int: 1
            - Return:
                Var: n
        "###);
    }
}

#[cfg(test)]
mod test_legal_data_flow {
    use crate::lexer;
    use std::fs;

    const TEST_DIR: &str = "tests/fixtures/legal/data_flow";

    #[test]
    fn asnmt() {
        let chars = fs::read(format!("{TEST_DIR}/asnmt.c"))
            .expect("Should have been able to read the file")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let tokens = lexer::lex(&chars);
        let tree = super::parse_program(tokens).unwrap();
        insta::assert_yaml_snapshot!(tree, @r###"
        ---
        main_function:
          stmts:
            - Asnmt:
                CreateBind:
                  id: x
                  expr:
                    Int: 8
            - Return:
                Var: x
        "###);
    }

    #[test]
    fn asnmt_update() {
        let chars = fs::read(format!("{TEST_DIR}/asnmt_update.c"))
            .expect("Should have been able to read the file")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let tokens = lexer::lex(&chars);
        let tree = super::parse_program(tokens).unwrap();
        insta::assert_yaml_snapshot!(tree, @r###"
        ---
        main_function:
          stmts:
            - Asnmt:
                CreateBind:
                  id: n
                  expr:
                    Int: 0
            - Asnmt:
                UpdateBind:
                  id: n
                  op: Add
                  expr:
                    Int: 10
            - Return:
                Var: n
        "###);
    }
}

// proptest! {
//     #[test]
//     fn doesnt_crash(s in "\\PC*") {
//         let t = Token{ lexeme: s, typ: TokenType::Identifier };
//         let tokens = vec![t];

//         let _ = parse_program(tokens);
//     }
// }
