use crate::{BinOp, Defs, Expr, FuncDef, Prg, Stmt, Tnv, Type};
use std::{collections::HashMap, io};

pub fn type_prg(prg: Prg) -> Result<Type, io::Error> {
    let foo = prg.iter().map(|defs| match defs {
        // ---------------------intros-------------------------
        Defs::FuncDef(func_def) => todo!(),
        Defs::VarDef(var_def) => todo!(),
    });

    let tnv = Tnv {
        fnv: HashMap::new(),
        vnv: HashMap::new(),
    };
    todo!()
}

pub fn type_func(func_def: &FuncDef, tnv: &Tnv) -> Result<Type, io::Error> {
    todo!()
}

pub fn type_stmt(stmt: &Stmt, tnv: &Tnv) -> Result<Type, io::Error> {
    todo!()
}

pub fn type_expr(e: &Expr, tnv: &Tnv) -> Result<Type, io::Error> {
    match e {
        // ---------------------intros-------------------------
        // ⊢ n : Int
        Expr::Int(_) => Ok(Type::Int),
        // ⊢ s : Str
        Expr::Str(_) => Ok(Type::Str),
        // ---------------------elims--------------------------
        Expr::UnaryE { op, l } => type_expr(l, tnv),
        Expr::BinE { op, l, r } => match op {
            // type systems collapse ∞-space of points to a single point
            // ignoring distinctions within types (sum)
            BinOp::Add | BinOp::Sub | BinOp::Mult | BinOp::Div | BinOp::Mod => {
                // ⊢ e1 : Int, ⊢ e2 : Int
                // ------------------------
                //     ⊢ e1 + e2 : Int
                match (type_expr(l, tnv)?, type_expr(r, tnv)?) {
                    (Type::Int, Type::Int) => Ok(Type::Int),
                    _ => Err(io::Error::new(io::ErrorKind::Other, "type error")),
                }
            }
            // perserves distinctions between types (product)
            BinOp::AddAdd => {
                // ⊢ e1 : Str, ⊢ e2 : Str
                // ------------------------
                //     ⊢ e1 + e2 : Str
                match (type_expr(l, tnv)?, type_expr(r, tnv)?) {
                    (Type::Str, Type::Str) => Ok(Type::Str),
                    _ => Err(io::Error::new(io::ErrorKind::Other, "type error")),
                }
            }
        },
        Expr::VarApp(id) => tnv
            .vnv
            .get(id)
            .cloned()
            .ok_or(io::Error::new(io::ErrorKind::Other, "type error")),
        Expr::FuncApp { alias, ap } => {
            // 1. get the type of funcdef
            let f = tnv
                .fnv
                .get(alias)
                .cloned()
                .ok_or(io::Error::new(io::ErrorKind::Other, "type error"))?;

            //      Γ [V <- T] ⊢ e2 : T2
            // -------------------------------
            //    Γ ⊢ f(e1:T1, e2) : (T1-> T2)
            let parameters_match =
                f.fp.iter()
                    .zip(ap.iter())
                    .map(|(fpt, apt)| {
                        if let Ok(apt) = type_expr(apt, tnv) {
                            if apt == *fpt {
                                Ok(())
                            } else {
                                Err(io::Error::new(io::ErrorKind::Other, "type error"))
                            }
                        } else {
                            Err(io::Error::new(io::ErrorKind::Other, "type error"))
                        }
                    })
                    .collect::<Result<Vec<_>, _>>()
                    .is_ok();

            if parameters_match {
                Ok(f.body)
            } else {
                Err(io::Error::new(io::ErrorKind::Other, "type error"))
            }
        }
        _ => Err(io::Error::new(io::ErrorKind::Other, "type error")),
    }
}
