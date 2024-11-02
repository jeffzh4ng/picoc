use crate::parser::{BinOp, Cmd, Expr, Function};
use std::collections::HashMap;

// TODO: in C, not everything is an expression
pub fn eval_main(nv: HashMap<String, Function>) -> i32 {
    // todo: handle error? if main dne
    match &nv.get("main").unwrap().body[0] {
        Cmd::Return(expr) => eval(expr, nv),
        _ => todo!(),
    }
}

// substitution is implemented with a cache (nv) and tree traversal,
// rather than tree rewriting
fn eval(e: &Expr, nv: HashMap<String, i32>) -> i32 {
    match e {
        Expr::BinE { op, l, r } => match op {
            BinOp::Add => eval(l, nv) + eval(r, nv),
            BinOp::Sub => eval(l, nv) - eval(r, nv),
            BinOp::Mult => eval(l, nv) * eval(r, nv),
            BinOp::Div => eval(l, nv) / eval(r, nv),
            BinOp::Mod => eval(l, nv) % eval(r, nv),
        },
        Expr::Int(n) => *n,
        _ => todo!(),
    }
}

// mod tests {
//     use super::*;

//     #[test]
//     fn simple() {
//         assert_eq!(9, super::eval(&Expr::Int(9)));
//     }
// }
