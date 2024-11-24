use crate::{Def, Expr, FuncDef, Prg, Stmt, TrgtPrg};
use std::io;

// peephole vs tiling
pub fn select(src_tree: &Prg) -> Result<TrgtPrg, io::Error> {
    // let _ = src_tree
    //     .iter()
    //     .map(|stmt| match stmt {
    //         Def::FuncDef(func_def) => todo!(),
    //         Def::VarDef(var_def) => todo!(),
    //     })
    //     .collect();

    todo!()
}

fn select_func_def(fd: &FuncDef) -> Result<(), io::Error> {
    let _ = fd.body.iter().map(|stmt| match stmt {
        Stmt::Asnmt(defn) => todo!(),
        Stmt::IfEls { cond, then, els } => todo!(),
        Stmt::While { cond, body } => todo!(),
        Stmt::Return(expr) => todo!(),
    });
    todo!()
}

fn select_expr(e: &Expr) -> Result<(), io::Error> {
    match e {
        Expr::Int(n) => todo!(),
        Expr::Bool(_) => todo!(),
        Expr::UnaryE { op, l } => todo!(),
        Expr::BinE { op, l, r } => todo!(),
        Expr::LogE { op, l, r } => todo!(),
        Expr::BitE { op, l, r } => todo!(),
        Expr::RelE { op, l, r } => todo!(),
        Expr::VarApp(_) => todo!(),
        Expr::FuncApp { alias, ap } => todo!(),
    }
    todo!()
}
