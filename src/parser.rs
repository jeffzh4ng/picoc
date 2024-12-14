use crate::{
    lexer::{Token, TT},
    ConstantNode, Node, ReturnNode, StartNode,
};
use std::io;
use std::rc::Rc;

fn mtch(tokens: &[Token], tt: TT) -> Result<(&Token, &[Token]), io::Error> {
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

pub fn parse_prg(tokens: &[Token]) -> Result<Rc<dyn Node>, io::Error> {
    let start = Rc::new(StartNode::new()); // todo: static? for now we thread.
    let r = tokens;
    let (_, r) = mtch(r, TT::KeywordInt)?;
    let (_, r) = mtch(r, TT::Alias)?;
    let (_, r) = mtch(r, TT::PuncLeftParen)?;
    let (_, r) = mtch(r, TT::PuncRightParen)?;

    let (_, r) = mtch(r, TT::PuncLeftBrace)?;
    let (stmt, r) = parse_stmt(start.clone(), r)?;
    let (_, r) = mtch(r, TT::PuncRightBrace)?;

    if r.len() == 0 {
        Ok(stmt.clone())
    } else {
        Err(io::Error::new(
            io::ErrorKind::Other,
            format!("expected empty token stream, got {:?}", r),
        ))
    }
}

fn parse_stmt(
    start: Rc<dyn Node>,
    tokens: &[Token],
) -> Result<(Rc<dyn Node>, &[Token]), io::Error> {
    match tokens {
        [] => Err(io::Error::new(
            io::ErrorKind::Other,
            "expected: {:?} got an empty token stream",
        )),
        [f, r @ ..] => match f.typ {
            TT::KeywordRet => {
                let (expr, r) = parse_expr(start.clone(), r)?;
                let (_, r) = mtch(r, TT::PuncSemiColon)?;
                Ok((Rc::new(ReturnNode::new(start, expr)), r))
            }
            t => Err(io::Error::new(
                io::ErrorKind::Other,
                format!("expected: {:?} got: {:?}", TT::KeywordRet, t),
            )),
        },
    }
}

fn parse_expr(
    start: Rc<dyn Node>,
    tokens: &[Token],
) -> Result<(Rc<dyn Node>, &[Token]), io::Error> {
    match tokens {
        [] => Err(io::Error::new(
            io::ErrorKind::Other,
            "expected: {:?} got an empty token stream",
        )),
        [f, r @ ..] => match f.typ {
            TT::LiteralInt => Ok((
                Rc::new(ConstantNode::new(start, f.lexeme.parse().unwrap())),
                r,
            )),
            t => Err(io::Error::new(
                io::ErrorKind::Other,
                format!("expected: {:?} got: {:?}", TT::LiteralInt, t),
            )),
        },
    }
}

#[cfg(test)]
mod test_arith {
    use crate::lexer;
    use std::fs;

    const TEST_DIR: &str = "tests/fixtures/snap/shared/arith";

    #[test]
    fn lit() {
        let chars = fs::read(format!("{TEST_DIR}/lit.c"))
            .expect("file dne")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let tokens = lexer::lex(&chars).unwrap();
        let tree = super::parse_prg(&tokens).unwrap();
        insta::assert_debug_snapshot!(tree, @r###"
        StartNode
        ConstantNode(8)
        ReturnNode
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
        insta::assert_debug_snapshot!(tree, @"");
    }
}
