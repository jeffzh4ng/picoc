use crate::{
    lexer::{Token, TT},
    BinOp, Defs, Expr, FuncDef, Prg, RelOp, Stmt, VarDef,
};
use std::{io, num::ParseIntError};

fn eat(tokens: &[Token], tt: TT) -> Result<(&Token, &[Token]), io::Error> {
    match tokens {
        [] => Err(io::Error::new(
            io::ErrorKind::Other,
            format!("expected: {:?} got: {:?}", tt, tokens),
        )),
        [f, r @ ..] => {
            if f.typ == tt {
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

pub fn parse_prg(tokens: &[Token]) -> Result<Prg, io::Error> {
    let (mut fds, mut r) = (vec![], tokens);
    while let Ok((fd, _r)) = parse_funcdef(r) {
        fds.push(fd);
        r = _r;
    }

    Ok(fds.into_iter().map(Defs::FuncDef).collect())
}

fn parse_funcdef(tokens: &[Token]) -> Result<(FuncDef, &[Token]), io::Error> {
    let (_, r) = eat(&tokens, TT::KeywordInt)?; // todo: return type can only be int for now
    let (alias, r) = eat(r, TT::Alias)?;
    let (_, r) = eat(r, TT::PuncLeftParen)?;

    // todo: formal param needs to include type (int, bool, etc)
    let (mut formal_params, mut r) = (vec![], r);
    // for now, single formal param
    if let Ok((_, _r)) = eat(r, TT::KeywordInt) {
        let (alias, _r) = eat(_r, TT::Alias)?;
        formal_params.push(alias.lexeme.to_owned());
        r = _r;
    }

    let (_, r) = eat(r, TT::PuncRightParen)?;
    let (_, r) = eat(r, TT::PuncLeftBrace)?;

    let (mut stmts, mut r) = (vec![], r);
    while let Ok((s, _r)) = parse_stmt(r) {
        stmts.push(s);
        r = _r;
    }
    let (_, r) = eat(r, TT::PuncRightBrace)?;

    // if !matches!(stmts.last(), Some(Stmt::Return(_))) {
    //     Err(io::Error::new(io::ErrorKind::Other, "no return"))
    // } else { todo: is this semantic check possible when return is nested?
    Ok((
        FuncDef {
            alias: alias.lexeme.to_string(),
            formal_param: formal_params,
            body: stmts,
        },
        r,
    ))
}

fn parse_vardef(tokens: &[Token]) -> Result<(VarDef, &[Token]), io::Error> {
    match tokens {
        [] => todo!(),
        [f, r @ ..] => match f.typ {
            // for now int is parsed with vardef
            TT::KeywordInt => {
                let (alias, r) = eat(r, TT::Alias)?;
                let (_, r) = eat(r, TT::Equals)?;
                let (expr, r) = parse_expr(r)?;

                Ok((
                    VarDef {
                        alias: alias.lexeme.to_owned(),
                        expr: Box::new(expr),
                    },
                    r,
                ))
            }
            TT::Alias => match r {
                [] => todo!(),
                [s, t, r @ ..] => match (s.typ, t.typ) {
                    // (TT::Plus, TT::Equals) => {
                    //     let (expr, r) = parse_rel_expr(r)?;
                    //     Ok((
                    //         VarDef::UpdateBind {
                    //             alias: f.lexeme.parse().unwrap(),
                    //             op: BinOp::Add,
                    //             expr: Box::new(expr),
                    //         },
                    //         r,
                    //     ))
                    // }
                    // (TT::Plus, TT::Plus) => Ok((
                    //     VarDef::UpdateBind {
                    //         alias: f.lexeme.parse().unwrap(),
                    //         op: BinOp::Add,
                    //         expr: Box::new(Expr::Int(1)),
                    //     },
                    //     r,
                    // )),
                    // (TT::Minus, TT::Equals) => {
                    //     let (expr, r) = parse_rel_expr(r)?;

                    //     Ok((
                    //         VarDef::UpdateBind {
                    //             alias: f.lexeme.parse().unwrap(),
                    //             op: BinOp::Sub,
                    //             expr: Box::new(expr),
                    //         },
                    //         r,
                    //     ))
                    // }
                    // (TT::Minus, TT::Minus) => Ok((
                    //     VarDef::UpdateBind {
                    //         alias: f.lexeme.parse().unwrap(),
                    //         op: BinOp::Sub,
                    //         expr: Box::new(Expr::Int(1)),
                    //     },
                    //     r,
                    // )),
                    // (TT::Star, TT::Equals) => {
                    //     let (expr, r) = parse_rel_expr(r)?;

                    //     Ok((
                    //         VarDef::UpdateBind {
                    //             alias: f.lexeme.parse().unwrap(),
                    //             op: BinOp::Mult,
                    //             expr: Box::new(expr),
                    //         },
                    //         r,
                    //     ))
                    // }
                    // (TT::Slash, TT::Equals) => {
                    //     let (expr, r) = parse_rel_expr(r)?;

                    //     Ok((
                    //         VarDef::UpdateBind {
                    //             alias: f.lexeme.parse().unwrap(),
                    //             op: BinOp::Div,
                    //             expr: Box::new(expr),
                    //         },
                    //         r,
                    //     ))
                    // }
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

fn parse_stmt(tokens: &[Token]) -> Result<(Stmt, &[Token]), io::Error> {
    match tokens {
        [] => todo!(),
        [f, r @ ..] => match f.typ {
            TT::KeywordInt => {
                // todo: | TT:KeywordAlias{++, --, -=}, etc.
                let (a, r) = parse_vardef(tokens)?;
                let (_, r) = eat(r, TT::PuncSemiColon)?;

                Ok((Stmt::Asnmt(a), r))
            }
            TT::KeywordRet => {
                let (expr, r) = parse_rel(r)?;
                let (_, r) = eat(r, TT::PuncSemiColon)?;
                Ok((Stmt::Return(expr), r))
            }
            TT::KeywordIf => {
                let (_, r) = eat(r, TT::PuncLeftParen)?;
                let (cond, r) = parse_rel(r)?;
                let (_, r) = eat(r, TT::PuncRightParen)?;
                let (_, r) = eat(r, TT::PuncLeftBrace)?;
                let (then, r) = parse_stmt(r)?;
                let (_, r) = eat(r, TT::PuncRightBrace)?;
                let (_, r) = eat(r, TT::KeywordEls)?;
                let (_, r) = eat(r, TT::PuncLeftBrace)?;
                let (els, r) = parse_stmt(r)?;
                let (_, r) = eat(r, TT::PuncRightBrace)?;

                Ok((
                    Stmt::IfEls {
                        cond: Box::new(cond),
                        then: Box::new(then),
                        els: Box::new(els),
                    },
                    r,
                ))
            }
            // TT::KeywordFor => {
            //     let (_, r) = mtch(r, TT::PuncLeftParen)?;
            //     let (asnmt, r) = parse_asmt(r)?;
            //     let (_, r) = mtch(r, TT::PuncSemiColon)?;
            //     let (cond, r) = parse_rel_expr(r)?;
            //     let (_, r) = mtch(r, TT::PuncSemiColon)?;
            //     let (update, r) = parse_asmt(r)?;
            //     let (_, r) = mtch(r, TT::PuncRightParen)?;
            //     let (_, r) = mtch(r, TT::PuncLeftBrace)?;

            //     let mut body = vec![];
            //     let mut r0 = r;
            //     while let Ok((s, r1)) = parse_stmt(r0) {
            //         body.push(s);
            //         r0 = r1;
            //     }
            //     let (_, r) = mtch(r0, TT::PuncRightBrace)?;

            //     Ok((
            //         Stmt::For {
            //             asnmt: Box::new(asnmt),
            //             cond: Box::new(cond),
            //             update: Box::new(update),
            //             body,
            //         },
            //         r,
            //     ))
            // }
            t => Err(io::Error::new(
                io::ErrorKind::Other,
                format!("token not recognizable {:?}", t),
            )),
        },
    }
}

fn parse_expr(tokens: &[Token]) -> Result<(Expr, &[Token]), io::Error> {
    parse_rel(tokens)
}

fn parse_rel(tokens: &[Token]) -> Result<(Expr, &[Token]), io::Error> {
    let (left, r) = parse_term(tokens)?;

    match r {
        [] => Ok((left, r)),
        r => {
            let mut cur_node = left;
            let mut r = r;

            while let Ok((op, _r)) = parse_rel_op(r) {
                let (right, _r) = parse_term(_r)?;

                cur_node = Expr::RelE {
                    op,
                    l: Box::new(cur_node),
                    r: Box::new(right),
                };

                r = _r;
            }

            Ok((cur_node, r))
        }
    }
}
fn parse_rel_op(tokens: &[Token]) -> Result<(RelOp, &[Token]), io::Error> {
    match tokens {
        [] => todo!(),
        [f, r @ ..] => match f.typ {
            TT::LeftAngleBracket => match r {
                [] => todo!(),
                [s, r @ ..] => match s.typ {
                    TT::Equals => Ok((RelOp::LtEq, r)),
                    _ => Ok((RelOp::Lt, &tokens[1..])), // include s
                },
            },
            TT::RightAngleBracket => match r {
                [] => todo!(),
                [s, r @ ..] => match s.typ {
                    TT::Equals => Ok((RelOp::GtEq, r)),
                    _ => Ok((RelOp::Gt, &tokens[1..])), // include s
                },
            },
            TT::Equals => match r {
                [] => todo!(),
                [s, r @ ..] => match s.typ {
                    TT::Equals => Ok((RelOp::Eq, r)),
                    t => Err(io::Error::new(
                        io::ErrorKind::Other,
                        format!("token not recognizable {:?}", t),
                    )),
                },
            },
            TT::Bang => match r {
                [] => todo!(),
                [s, r @ ..] => match s.typ {
                    TT::Equals => Ok((RelOp::Neq, r)),
                    t => Err(io::Error::new(
                        io::ErrorKind::Other,
                        format!("token not recognizable {:?}", t),
                    )),
                },
            },
            TT::Amp => match r {
                [] => todo!(),
                [s, r @ ..] => match s.typ {
                    TT::Amp => Ok((RelOp::And, r)),
                    t => Err(io::Error::new(
                        io::ErrorKind::Other,
                        format!("token not recognizable {:?}", t),
                    )),
                },
            },
            TT::Bar => match r {
                [] => todo!(),
                [s, r @ ..] => match s.typ {
                    TT::Bar => Ok((RelOp::Or, r)),
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

fn parse_term(tokens: &[Token]) -> Result<(Expr, &[Token]), io::Error> {
    let (left, r) = parse_factor(tokens)?;

    match r {
        [] => Ok((left, r)),
        r => {
            let mut cur_node = left;
            let mut r = r;

            while let Ok((op, _r)) = parse_term_op(r) {
                let (right, _r) = parse_factor(_r)?;

                cur_node = Expr::BinE {
                    op,
                    l: Box::new(cur_node),
                    r: Box::new(right),
                };

                r = _r;
            }

            Ok((cur_node, r))
        }
    }
}
fn parse_term_op(tokens: &[Token]) -> Result<(BinOp, &[Token]), io::Error> {
    match tokens {
        [] => todo!(),
        [f, r @ ..] => match f.typ {
            TT::Plus => Ok((BinOp::Add, r)),
            TT::Minus => Ok((BinOp::Sub, r)),
            t => Err(io::Error::new(
                io::ErrorKind::Other,
                format!("token not recognizable {:?}", t),
            )),
        },
    }
}

fn parse_factor(tokens: &[Token]) -> Result<(Expr, &[Token]), io::Error> {
    let (left, r) = parse_funcapp(tokens)?;

    match r {
        [] => Ok((left, r)),
        r => {
            let mut cur_node = left;
            let mut r = r;
            while let Ok((op, _r)) = parse_factor_op(r) {
                let (right, _r) = parse_atom(_r)?;

                cur_node = Expr::BinE {
                    op,
                    l: Box::new(cur_node),
                    r: Box::new(right),
                };

                r = _r;
            }

            Ok((cur_node, r))
        }
    }
}
fn parse_factor_op(tokens: &[Token]) -> Result<(BinOp, &[Token]), io::Error> {
    match tokens {
        [] => todo!(),
        [f, r @ ..] => match f.typ {
            TT::Star => Ok((BinOp::Mult, r)),
            TT::Slash => Ok((BinOp::Div, r)),
            t => Err(io::Error::new(
                io::ErrorKind::Other,
                format!("token not recognizable {:?}", t),
            )),
        },
    }
}

fn parse_funcapp(tokens: &[Token]) -> Result<(Expr, &[Token]), io::Error> {
    let (left, r) = parse_atom(tokens)?;

    match r {
        [] => Ok((left, r)),
        [f, _r @ ..] => {
            // not while because C doesn't have function as values: f()()()
            if let TT::PuncLeftParen = f.typ {
                let (actual_param, r) = if !_r.is_empty() && _r[0].typ != TT::PuncRightParen {
                    let (actual_param, r) = parse_expr(_r)?; // single param for now
                    ((vec![actual_param]), r)
                } else {
                    (vec![], _r)
                };
                let (_, _r) = eat(r, TT::PuncRightParen)?;

                match left {
                    Expr::VarApp(alias) => Ok((
                        Expr::FuncApp {
                            alias,
                            ap: actual_param,
                        },
                        _r,
                    )),
                    _ => Err(io::Error::new(
                        io::ErrorKind::Other,
                        "expected alias".to_string(),
                    )),
                }
            } else {
                Ok((left, r))
            }
        }
    }
}

fn parse_atom(tokens: &[Token]) -> Result<(Expr, &[Token]), io::Error> {
    match tokens {
        [] => todo!(),
        [f, r @ ..] => match f.typ {
            TT::Alias => Ok((Expr::VarApp(f.lexeme.to_owned()), r)),
            TT::LiteralInt => Ok((
                Expr::Int(f.lexeme.parse().map_err(|e: ParseIntError| {
                    io::Error::new(io::ErrorKind::Other, e.to_string())
                })?),
                r,
            )),
            t => Err(io::Error::new(
                io::ErrorKind::Other,
                format!("token not recognizable {:?}", t),
            )),
        },
    }
}

#[cfg(test)]
mod test_arith {
    use crate::lexer;
    use std::fs;

    const TEST_DIR: &str = "tests/fixtures/arith";

    #[test]
    fn lit() {
        let chars = fs::read(format!("{TEST_DIR}/lit.c"))
            .expect("file dne")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let tokens = lexer::lex(&chars).unwrap();
        let tree = super::parse_prg(&tokens).unwrap();
        insta::assert_yaml_snapshot!(tree, @r###"
        ---
        - FuncDef:
            alias: main
            formal_param: []
            body:
              - Return:
                  Int: 8
        "###);
    }

    #[test]
    fn add() {
        let chars = fs::read(format!("{TEST_DIR}/add.c"))
            .expect("file dne")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let tokens = lexer::lex(&chars).unwrap();
        let tree = super::parse_prg(&tokens).unwrap();
        insta::assert_yaml_snapshot!(tree, @r###"
        ---
        - FuncDef:
            alias: main
            formal_param: []
            body:
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
            .expect("file dne")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let tokens = lexer::lex(&chars).unwrap();
        let tree = super::parse_prg(&tokens).unwrap();
        insta::assert_yaml_snapshot!(tree, @r###"
        ---
        - FuncDef:
            alias: main
            formal_param: []
            body:
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
        let chars = fs::read(format!("{TEST_DIR}/sub.c"))
            .expect("file dne")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let tokens = lexer::lex(&chars).unwrap();
        let tree = super::parse_prg(&tokens).unwrap();
        insta::assert_yaml_snapshot!(tree, @r###"
        ---
        - FuncDef:
            alias: main
            formal_param: []
            body:
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
        let chars = fs::read(format!("{TEST_DIR}/mult.c"))
            .expect("file dne")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let tokens = lexer::lex(&chars).unwrap();
        let tree = super::parse_prg(&tokens).unwrap();
        insta::assert_yaml_snapshot!(tree, @r###"
        ---
        - FuncDef:
            alias: main
            formal_param: []
            body:
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
        let chars = fs::read(format!("{TEST_DIR}/div.c"))
            .expect("file dne")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let tokens = lexer::lex(&chars).unwrap();
        let tree = super::parse_prg(&tokens).unwrap();
        insta::assert_yaml_snapshot!(tree, @r###"
        ---
        - FuncDef:
            alias: main
            formal_param: []
            body:
              - Return:
                  BinE:
                    op: Div
                    l:
                      Int: 100
                    r:
                      Int: 9
        "###);
    }

    #[test]
    fn add_associative() {
        let chars = fs::read(format!("{TEST_DIR}/add_associative.c"))
            .expect("file dne")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let tokens = lexer::lex(&chars).unwrap();
        let tree = super::parse_prg(&tokens).unwrap();
        insta::assert_yaml_snapshot!(tree, @r###"
        ---
        - FuncDef:
            alias: main
            formal_param: []
            body:
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
            .expect("file dne")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let tokens = lexer::lex(&chars).unwrap();
        let tree = super::parse_prg(&tokens).unwrap();
        insta::assert_yaml_snapshot!(tree, @r###"
        ---
        - FuncDef:
            alias: main
            formal_param: []
            body:
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
            .expect("file dne")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let tokens = lexer::lex(&chars).unwrap();
        let tree = super::parse_prg(&tokens).unwrap();
        insta::assert_yaml_snapshot!(tree, @r###"
        ---
        - FuncDef:
            alias: main
            formal_param: []
            body:
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
            .expect("file dne")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let tokens = lexer::lex(&chars).unwrap();
        let tree = super::parse_prg(&tokens).unwrap();
        insta::assert_yaml_snapshot!(tree, @r###"
        ---
        - FuncDef:
            alias: main
            formal_param: []
            body:
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
mod test_bindings {
    use crate::lexer;
    use std::fs;

    const TEST_DIR: &str = "tests/fixtures/bindings";

    #[test]
    fn composition() {
        let chars = fs::read(format!("{TEST_DIR}/composition.c"))
            .expect("file dne")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let tokens = lexer::lex(&chars).unwrap();
        let tree = super::parse_prg(&tokens).unwrap();
        insta::assert_yaml_snapshot!(tree, @r###"
        ---
        - FuncDef:
            alias: main
            formal_param: []
            body:
              - Return:
                  FuncApp:
                    alias: f
                    ap: []
        - FuncDef:
            alias: f
            formal_param: []
            body:
              - Return:
                  BinE:
                    op: Add
                    l:
                      Int: 9
                    r:
                      FuncApp:
                        alias: g
                        ap: []
        - FuncDef:
            alias: g
            formal_param: []
            body:
              - Return:
                  BinE:
                    op: Add
                    l:
                      Int: 10
                    r:
                      FuncApp:
                        alias: h
                        ap: []
        - FuncDef:
            alias: h
            formal_param: []
            body:
              - Return:
                  Int: 11
        "###);
    }

    #[test]
    fn formal_param() {
        let chars = fs::read(format!("{TEST_DIR}/formal_param.c"))
            .expect("file dne")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let tokens = lexer::lex(&chars).unwrap();
        let tree = super::parse_prg(&tokens).unwrap();
        insta::assert_yaml_snapshot!(tree, @r###"
        ---
        - FuncDef:
            alias: main
            formal_param: []
            body:
              - Return:
                  FuncApp:
                    alias: f
                    ap:
                      - Int: 9
        - FuncDef:
            alias: f
            formal_param:
              - x
            body:
              - Return:
                  BinE:
                    op: Add
                    l:
                      VarApp: x
                    r:
                      Int: 10
        "###);
    }

    #[test]
    fn asnmt() {
        let chars = fs::read(format!("{TEST_DIR}/assignment.c"))
            .expect("file dne")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let tokens = lexer::lex(&chars).unwrap();
        let tree = super::parse_prg(&tokens).unwrap();
        insta::assert_yaml_snapshot!(tree, @r###"
        ---
        - FuncDef:
            alias: main
            formal_param: []
            body:
              - Asnmt:
                  alias: x
                  expr:
                    Int: 9
              - Return:
                  VarApp: x
        "###);
    }

    // #[test]
    // fn asnmt_update() {
    //     let chars = fs::read(format!("{TEST_DIR}/asnmt_update.c"))
    //         .expect("file dne")
    //         .iter()
    //         .map(|b| *b as char)
    //         .collect::<Vec<_>>();

    //     let tokens = lexer::lex(&chars).unwrap();
    //     let tree = super::parse_prg(&tokens).unwrap();
    //     insta::assert_yaml_snapshot!(tree, @r###"
    //     ---
    //     main_function:
    //       stmts:
    //         - Asnmt:
    //             CreateBind:
    //               id: n
    //               expr:
    //                 Int: 0
    //         - Asnmt:
    //             UpdateBind:
    //               id: n
    //               op: Add
    //               expr:
    //                 Int: 10
    //         - Return:
    //             Var: n
    //     "###);
    // }
}

#[cfg(test)]
mod test_control {
    use crate::lexer;
    use std::fs;
    const TEST_DIR: &str = "tests/fixtures/control";

    #[test]
    fn eq() {
        let chars = fs::read(format!("{TEST_DIR}/eq_true.c"))
            .expect("file dne")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let tokens = lexer::lex(&chars).unwrap();
        let tree = super::parse_prg(&tokens).unwrap();
        insta::assert_yaml_snapshot!(tree, @r###"
        ---
        - FuncDef:
            alias: main
            formal_param: []
            body:
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
            .expect("file dne")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let tokens = lexer::lex(&chars).unwrap();
        let tree = super::parse_prg(&tokens).unwrap();
        insta::assert_yaml_snapshot!(tree, @r###"
        ---
        - FuncDef:
            alias: main
            formal_param: []
            body:
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
            .expect("file dne")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let tokens = lexer::lex(&chars).unwrap();
        let tree = super::parse_prg(&tokens).unwrap();
        insta::assert_yaml_snapshot!(tree, @r###"
        ---
        - FuncDef:
            alias: main
            formal_param: []
            body:
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
            .expect("file dne")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let tokens = lexer::lex(&chars).unwrap();
        let tree = super::parse_prg(&tokens).unwrap();
        insta::assert_yaml_snapshot!(tree, @r###"
        ---
        - FuncDef:
            alias: main
            formal_param: []
            body:
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
            .expect("file dne")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let tokens = lexer::lex(&chars).unwrap();
        let tree = super::parse_prg(&tokens).unwrap();
        insta::assert_yaml_snapshot!(tree, @r###"
        ---
        - FuncDef:
            alias: main
            formal_param: []
            body:
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
            .expect("file dne")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let tokens = lexer::lex(&chars).unwrap();
        let tree = super::parse_prg(&tokens).unwrap();
        insta::assert_yaml_snapshot!(tree, @r###"
        ---
        - FuncDef:
            alias: main
            formal_param: []
            body:
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
            .expect("file dne")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let tokens = lexer::lex(&chars).unwrap();
        let tree = super::parse_prg(&tokens).unwrap();
        insta::assert_yaml_snapshot!(tree, @r###"
        ---
        - FuncDef:
            alias: main
            formal_param: []
            body:
              - IfEls:
                  cond:
                    Int: 1
                  then:
                    Return:
                      Int: 9
                  els:
                    Return:
                      Int: 10
        "###);
    }

    // #[test]
    // fn for_loop() {
    //     let chars = fs::read(format!("{TEST_DIR}/for.c"))
    //         .expect("file dne")
    //         .iter()
    //         .map(|b| *b as char)
    //         .collect::<Vec<_>>();

    //     let tokens = lexer::lex(&chars).unwrap();
    //     let tree = super::parse_prg(&tokens).unwrap();
    //     insta::assert_yaml_snapshot!(tree, @r###"
    //     ---
    //     main_function:
    //       stmts:
    //         - Asnmt:
    //             CreateBind:
    //               id: n
    //               expr:
    //                 Int: 0
    //         - For:
    //             asnmt:
    //               CreateBind:
    //                 id: i
    //                 expr:
    //                   Int: 0
    //             cond:
    //               RelE:
    //                 op: Lt
    //                 l:
    //                   Var: i
    //                 r:
    //                   Int: 10
    //             update:
    //               UpdateBind:
    //                 id: i
    //                 op: Add
    //                 expr:
    //                   Int: 1
    //             body:
    //               - Asnmt:
    //                   UpdateBind:
    //                     id: n
    //                     op: Add
    //                     expr:
    //                       Int: 1
    //               - Asnmt:
    //                   UpdateBind:
    //                     id: n
    //                     op: Add
    //                     expr:
    //                       Int: 1
    //         - Return:
    //             Var: n
    //     "###);
    // }
}
