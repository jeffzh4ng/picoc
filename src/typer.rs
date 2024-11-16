use crate::{BinOp, Def, Expr, FuncDef, LambdaType, Prg, Stmt, Tnv, Type};
use std::{collections::HashMap, io};

// todo: type checking for conditionals and bindings (funcs, vars) on AST
// liveness analysis -- CFG?
// dataflow analysis -- CFG?

pub fn type_prg(prg: Prg) -> Result<Type, io::Error> {
    let mut tnv = Tnv {
        fnv: HashMap::new(),
        vnv: HashMap::new(),
    };

    let _ = prg
        .iter()
        .map(|def| match def {
            Def::FuncDef(fd) => {
                let ltnv = HashMap::new();
                let type_check = type_func(fd, &tnv, ltnv).and_then(|t| {
                    tnv.fnv.insert(
                        fd.alias.clone(),
                        LambdaType {
                            fp: fd.fp.iter().map(|(_, t)| t.clone()).collect(),
                            body: t,
                        },
                    );
                    Ok(())
                });
                type_check
            }
            Def::VarDef(vd) => todo!(),
        })
        .collect::<Result<Vec<_>, _>>()?;

    Ok(tnv
        .fnv
        .get("main")
        .ok_or(io::Error::new(
            io::ErrorKind::Other,
            "main function not found",
        ))?
        .body
        .clone())
}

pub fn type_func(
    fd: &FuncDef,
    gnv: &Tnv,
    mut ltnv: HashMap<String, Type>,
) -> Result<Type, io::Error> {
    //      Γ [e1 <- T1], ... [en <- Tn] ⊢ B : T2
    // -------------------------------------------------------
    //    Γ ⊢ (lambda e1:T1 ... en:Tn B) : (T1 * ... * Tn -> T2)

    let _ = fd.fp.iter().for_each(|(a, t)| {
        ltnv.insert(a.clone(), t.clone()); // Γ [e1 <- T1], ... [en <- Tn]
    });

    fd.body
        .iter()
        .map(|stmt| type_stmt(stmt, gnv, &mut ltnv))
        .collect::<Result<Vec<_>, _>>()?
        .iter()
        .try_fold(Type::Void, |prev_t, next_t| match (&prev_t, next_t) {
            (_, Type::Void) => Ok(prev_t),
            (Type::Void, _) => Ok(next_t.clone()),
            (prev_t, next_t) => {
                if prev_t == next_t {
                    Ok(prev_t.clone())
                } else {
                    Err(io::Error::new(io::ErrorKind::Other, "type error"))
                }
            }
        })
        .and_then(|bt| {
            // ⊢ B : T2
            if bt == fd.typ {
                Ok(bt) // Γ ⊢ (lambda e1:T1 ... en:Tn B) : (T1 * ... * Tn -> T2)
            } else {
                Err(io::Error::new(io::ErrorKind::Other, "type error"))
            }
        })
}

pub fn type_stmt(
    stmt: &Stmt,
    gnv: &Tnv,
    ltnv: &mut HashMap<String, Type>,
) -> Result<Type, io::Error> {
    match stmt {
        Stmt::IfEls { cond, then, els } => {
            let ct = type_expr(cond, gnv, &ltnv)?;
            let tt = type_stmt(then, gnv, ltnv)?;
            let et = type_stmt(els, gnv, ltnv)?; // todo: optional else

            // todo (for now):
            // 1. tt == et
            // 2. et is mandatory
            // 3. tt and et are Stmt, not Vec<Stmt>
            // 4. must return expression Type. not void/stmt/valid/cmd Type
            if ct == Type::Bool && tt == et {
                Ok(tt)
            } else {
                Err(io::Error::new(io::ErrorKind::Other, "type error"))
            }
        }
        Stmt::While { cond, body } => todo!(),
        Stmt::Asnmt(vd) => {
            let et = type_expr(&vd.expr, gnv, &ltnv)?;
            ltnv.insert(vd.alias.clone(), et.clone()); // Γ [x <- T]
            Ok(et)
        }
        Stmt::Return(expr) => type_expr(expr, gnv, &ltnv),
    }
}

pub fn type_expr(e: &Expr, gtnv: &Tnv, ltnv: &HashMap<String, Type>) -> Result<Type, io::Error> {
    match e {
        // ---------------------intros (axioms)-------------------------
        Expr::Int(_) => Ok(Type::Int),   // ⊢ n : Int
        Expr::Bool(_) => Ok(Type::Bool), // ⊢ b : Bool
        // ---------------------elims (rules)--------------------------
        Expr::UnaryE { op, l } => type_expr(l, gtnv, ltnv),
        Expr::BinE { op, l, r } => match op {
            // ignoring distinctions within types
            BinOp::Add | BinOp::Sub | BinOp::Mult | BinOp::Div | BinOp::Mod => {
                // ⊢ e1 : Int, ⊢ e2 : Int
                // ------------------------ BIN_OP
                //     ⊢ e1 + e2 : Int
                match (type_expr(l, gtnv, ltnv)?, type_expr(r, gtnv, ltnv)?) {
                    (Type::Int, Type::Int) => Ok(Type::Int),
                    _ => Err(io::Error::new(io::ErrorKind::Other, "type error")),
                }
            } // perserves distinctions between types
        },
        Expr::VarApp(alias) => gtnv // Γ ⊢ x: Γ(x)
            .vnv
            .get(alias)
            .cloned()
            .ok_or(io::Error::new(io::ErrorKind::Other, "type error")),
        Expr::FuncApp { alias, ap } => {
            //    Γ ⊢ f : (T1-> T2)      Γ ⊢ e : T1, ... Γ ⊢ e : Tn
            // ------------------------------------------------------- FUNC_APP
            //             Γ ⊢ f(e1, ... en) : T2

            // Γ ⊢ f : (T1-> T2)
            let f = gtnv
                .fnv
                .get(alias)
                .cloned()
                .ok_or(io::Error::new(io::ErrorKind::Other, "type error"))?;

            f.fp.iter()
                .zip(ap.iter())
                .map(|(fpt, ap)| {
                    type_expr(ap, gtnv, ltnv).and_then(|apt| {
                        //Γ ⊢ e : T1, ... Γ ⊢ e : Tn
                        if apt == *fpt {
                            Ok(())
                        } else {
                            Err(io::Error::new(io::ErrorKind::Other, "type error"))
                        }
                    })
                })
                .collect::<Result<Vec<_>, _>>()
                .and_then(|_| Ok(f.body)) // Γ ⊢ f(e) : T2
        }
        _ => Err(io::Error::new(io::ErrorKind::Other, "type error")),
    }
}

#[cfg(test)]
mod test_arith {
    use crate::lexer;
    use crate::parser;
    use std::fs;

    const TEST_DIR: &str = "tests/fixtures/snap/statics-c0/arith";

    #[test]
    fn lit() {
        let chars = fs::read(format!("{TEST_DIR}/lit.c0"))
            .expect("file dne")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let tokens = lexer::lex(&chars).unwrap();
        let tree = parser::parse_prg(&tokens).unwrap();
        let typ = super::type_prg(tree).unwrap();
        insta::assert_yaml_snapshot!(typ, @r###"
        ---
        Int
        "###);
    }
}

#[cfg(test)]
mod test_control {
    use crate::lexer;
    use crate::parser;
    use std::fs;

    const TEST_DIR: &str = "tests/fixtures/snap/statics-c0/control";

    #[test]
    fn ifels() {
        let chars = fs::read(format!("{TEST_DIR}/if.c0"))
            .expect("file dne")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let tokens = lexer::lex(&chars).unwrap();
        let tree = parser::parse_prg(&tokens).unwrap();
        let typ = super::type_prg(tree).unwrap();
        insta::assert_yaml_snapshot!(typ, @r###"
        ---
        Int
        "###);
    }
}
