use crate::{SExpr, SFuncDef, SStmt, IPrg, TQuad};
use std::io;

// peephole vs tiling
pub fn select(intrm_tree: &IPrg) -> Vec<TQuad> {
    // let _ = src_tree
    //     .iter()
    //     .map(|stmt| match stmt {
    //         Def::FuncDef(func_def) => todo!(),
    //         Def::VarDef(var_def) => todo!(),
    //     })
    //     .collect();

    todo!()
}

fn select_func_def(fd: &SFuncDef) -> Result<(), io::Error> {
    let _ = fd.body.iter().map(|stmt| match stmt {
        SStmt::Asnmt(defn) => todo!(),
        SStmt::IfEls { cond, then, els } => todo!(),
        SStmt::While { cond, body } => todo!(),
        SStmt::Return(expr) => todo!(),
    });
    todo!()
}

fn select_expr(e: &SExpr) -> Result<(), io::Error> {
    match e {
        SExpr::Int(n) => todo!(),
        SExpr::Bool(_) => todo!(),
        SExpr::UnaryE { op, l } => todo!(),
        SExpr::BinE { op, l, r } => todo!(),
        SExpr::LogE { op, l, r } => todo!(),
        SExpr::BitE { op, l, r } => todo!(),
        SExpr::RelE { op, l, r } => todo!(),
        SExpr::VarApp(_) => todo!(),
        SExpr::FuncApp { alias, ap } => todo!(),
    }
    todo!()
}
