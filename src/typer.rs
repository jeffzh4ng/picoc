use crate::Expr;
use std::io;

#[rustfmt::skip]
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int, Str,
}

pub fn tc(e: &Expr) -> Result<Type, io::Error> {
    match e {
        Expr::Int(_) => Ok(Type::Int), // ⊢ n : Int
        Expr::Str(_) => Ok(Type::Str), // ⊢ s : Str
        Expr::UnaryE { op, l } => tc(l),
        Expr::BinE { op, l, r } => match op {
            crate::BinOp::Add => {
                // ⊢ e1 : Int, ⊢ e2 : Int
                // ------------------------
                //     ⊢ e1 + e2 : Int
                if tc(l)? == Type::Int && tc(r)? == Type::Int {
                    Ok(Type::Int)
                } else {
                    Err(io::Error::new(io::ErrorKind::Other, "type error"))
                }
            }
            crate::BinOp::AddAdd => {
                // ⊢ e1 : Str, ⊢ e2 : Str
                // ------------------------
                //     ⊢ e1 + e2 : Str
                if tc(l)? == Type::Str && tc(r)? == Type::Str {
                    Ok(Type::Str)
                } else {
                    Err(io::Error::new(io::ErrorKind::Other, "type error"))
                }
            }
            crate::BinOp::Sub => todo!(),
            crate::BinOp::Mult => todo!(),
            crate::BinOp::Div => todo!(),
            crate::BinOp::Mod => todo!(),
        },
        _ => Err(io::Error::new(io::ErrorKind::Other, "type error")),
        // Expr::LogE { op, l, r } => tc(l) && tc(r),
        // Expr::BitE { op, l, r } => tc(l) && tc(r),
        // Expr::RelE { op, l, r } => tc(l) && tc(r),
        // Expr::VarApp(_) => todo!(),
        // Expr::FuncApp { alias, ap } => todo!(),
    }
}
