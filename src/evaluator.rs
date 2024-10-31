use crate::parser::{BinOp, Expr, Program, Stmt};

pub fn eval(p: Program) -> i32 {
    match &p.main_function.stmts[0] {
        Stmt::Return(expr) => _eval(expr),
        _ => todo!(),
    }
}

fn _eval(e: &Expr) -> i32 {
    match e {
        Expr::BinE { op, l, r } => match op {
            BinOp::Add => _eval(l) + _eval(r),
            BinOp::Sub => _eval(l) - _eval(r),
            BinOp::Mult => _eval(l) * _eval(r),
            BinOp::Div => _eval(l) / _eval(r),
            BinOp::Mod => _eval(l) % _eval(r),
        },
        Expr::Int(n) => *n,
        _ => todo!(),
    }
}
