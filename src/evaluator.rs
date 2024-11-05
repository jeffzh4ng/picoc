use crate::{BinOp, Defs, Expr, Lambda, Nv, Prg, Stmt};
use std::{collections::HashMap, io};

pub fn eval_prg(prg: Prg) -> Result<i32, io::Error> {
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
    let mut nv = Nv {
        fnv,
        vnv: HashMap::new(),
    };
    eval_fn(nv.fnv["main"].clone(), &mut nv)
}

// substitution is implemented with a cache (nv) and tree traversal,
// rather than tree rewriting

// todo: avoid taking ownership of l, where l is a value of &mut nv?
fn eval_fn(l: Lambda, nv: &mut Nv) -> Result<i32, io::Error> {
    // anyhow, this error
    let foo = l
        .body
        .iter()
        .take_while(|&stmt| !matches!(stmt, Stmt::Return(_)))
        .map(|stmt| -> Result<(), io::Error> {
            match stmt {
                Stmt::Asnmt(var_def) => {
                    let val = eval_expr(&var_def.expr, nv)?; // eager
                    nv.vnv.insert(var_def.alias.clone(), val);
                    Ok(())
                }
                Stmt::While => todo!(),
                Stmt::If => todo!(),
                Stmt::IfEls { cond, then, els } => todo!(),
                _ => panic!(), // todo: fix
            }
        })
        .collect::<Result<Vec<_>, io::Error>>()?;

    if let Some(Stmt::Return(e)) = l.body.last() {
        Ok(eval_expr(e, nv)?)
    } else {
        todo!() // anyhow, thiserror
                // Err(Error::EvalError("no return stmt".to_string()))
    }
}

fn eval_expr(e: &Expr, nv: &mut Nv) -> Result<i32, io::Error> {
    match e {
        Expr::Int(n) => Ok(*n),
        Expr::UnaryE { op, l } => todo!(),
        Expr::BinE { op, l, r } => match op {
            BinOp::Add => Ok(eval_expr(l, nv)? + eval_expr(r, nv)?),
            BinOp::Sub => Ok(eval_expr(l, nv)? - eval_expr(r, nv)?),
            BinOp::Mult => Ok(eval_expr(l, nv)? * eval_expr(r, nv)?),
            BinOp::Div => Ok(eval_expr(l, nv)? / eval_expr(r, nv)?),
            BinOp::Mod => Ok(eval_expr(l, nv)? % eval_expr(r, nv)?),
        },
        Expr::LogE { op, l, r } => todo!(),
        Expr::BitE { op, l, r } => todo!(),
        Expr::RelE { op, l, r } => todo!(),
        Expr::VarApp(alias) => Ok(nv.vnv[alias].clone()),
        Expr::FuncApp {
            alias,
            actual_param,
        } => eval_fn(nv.fnv[alias].clone(), nv),
    }
}
