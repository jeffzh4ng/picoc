use crate::{
    lexer::{Token, TT},
    ConstantNode, Node, ReturnNode, StartNode,
};
use std::{io, rc::Rc};

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

// todo: return type should be ReturnNode?
pub fn parse_prg(tokens: &[Token]) -> Result<Rc<dyn Node>, io::Error> {
    let start = Rc::new(StartNode::new()); // todo: static? for now we thread.
    let (stmt, r) = parse_stmt(start, tokens)?;
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
                let (_, r) = eat(r, TT::PuncSemiColon)?;
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
