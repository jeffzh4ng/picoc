use crate::{BinOp, Defs, Expr, Lambda, Nv, Prg, Stmt};
use std::collections::HashMap;

pub fn eval_prg(prg: Prg) -> i32 {
    let fnv = prg
        .iter()
        .map(|defs| match defs {
            Defs::FuncDef(fd) => (
                fd.alias.clone(),
                Lambda {
                    formal_param: fd.formal_param.clone(),
                    body: fd.body.clone(),
                },
            ),
            _ => todo!(), // next: top-level vardefs
        })
        .collect::<HashMap<String, Lambda>>();

    // defining nv here so eval_fn can borrow both
    let nv = Nv {
        fnv,
        vnv: HashMap::new(),
    };
    eval_fn(&nv.fnv["main"], &nv)
}

// substitution is implemented with a cache (nv) and tree traversal,
// rather than tree rewriting
fn eval_fn(l: &Lambda, nv: &Nv) -> i32 {
    let foo = l
        .body
        .iter()
        .take_while(|&stmt| !matches!(stmt, Stmt::Return(_)))
        .map(|stmt| match stmt {
            Stmt::Asnmt(var_def) => todo!(),
            Stmt::While => todo!(),
            Stmt::If => todo!(),
            Stmt::IfEls { cond, then, els } => todo!(),
            _ => panic!(), // todo: fix
        })
        .collect::<Vec<_>>();

    if let Stmt::Return(e) = l.body.last().unwrap() {
        eval_expr(e, nv)
    } else {
        panic!() // todo: fix
    }
}

fn eval_expr(e: &Expr, nv: &Nv) -> i32 {
    match e {
        Expr::Int(n) => *n,
        Expr::UnaryE { op, l } => todo!(),
        Expr::BinE { op, l, r } => match op {
            BinOp::Add => eval_expr(l, nv) + eval_expr(r, nv),
            BinOp::Sub => eval_expr(l, nv) - eval_expr(r, nv),
            BinOp::Mult => eval_expr(l, nv) * eval_expr(r, nv),
            BinOp::Div => eval_expr(l, nv) / eval_expr(r, nv),
            BinOp::Mod => eval_expr(l, nv) % eval_expr(r, nv),
        },
        Expr::LogE { op, l, r } => todo!(),
        Expr::BitE { op, l, r } => todo!(),
        Expr::RelE { op, l, r } => todo!(),
        Expr::Alias(_) => todo!(),
        Expr::FuncApp {
            alias,
            actual_param,
        } => eval_fn(&nv.fnv[alias], &nv),
    }
}

// mod tests {
//     use super::*;

//     #[test]
//     fn simple() {
//         assert_eq!(9, super::eval(&Expr::Int(9)));
//     }
// }
