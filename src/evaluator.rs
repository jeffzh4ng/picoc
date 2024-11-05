use crate::{BinOp, Defs, Expr, Lambda, Nv, Prg, Stmt};
use std::{collections::HashMap, io};

pub fn eval_prg(prg: Prg) -> Result<i32, io::Error> {
    let fnv = prg
        .iter()
        .map(|defs| match defs {
            Defs::FuncDef(fd) => (
                // funcdef simply creates the lambda
                fd.alias.clone(),
                Lambda {
                    fp: fd.formal_param.clone(),
                    body: fd.body.clone(),
                },
            ),
            _ => todo!(), // next: top-level vardefs
        })
        .collect::<HashMap<String, Lambda>>();

    let vnv = HashMap::new(); // todo: parse global vardefs
    let nv = Nv { fnv, vnv };

    // defining nv here so eval_fn can borrow both
    let lvnv = nv.vnv.clone(); // clone it first, before giving &mut
    eval_func(&nv.fnv["main"], &nv, lvnv)
}

fn eval_func(l: &Lambda, gnv: &Nv, mut lvnv: HashMap<String, i32>) -> Result<i32, io::Error> {
    let foo = l
        .body
        .iter()
        .take_while(|&stmt| !matches!(stmt, Stmt::Return(_)))
        .map(|stmt| -> Result<(), io::Error> {
            match stmt {
                Stmt::Asnmt(var_def) => {
                    let val = eval_expr(&var_def.expr, gnv, &lvnv)?; // eager
                    lvnv.insert(var_def.alias.clone(), val);
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
        Ok(eval_expr(e, gnv, &lvnv)?)
    } else {
        todo!() // anyhow, thiserror
                // Err(Error::EvalError("no return stmt".to_string()))
    }
}

fn eval_expr(e: &Expr, gnv: &Nv, lvnv: &HashMap<String, i32>) -> Result<i32, io::Error> {
    match e {
        Expr::Int(n) => Ok(*n),
        Expr::UnaryE { op, l } => todo!(),
        Expr::BinE { op, l, r } => match op {
            BinOp::Add => Ok(eval_expr(l, gnv, lvnv)? + eval_expr(r, gnv, lvnv)?),
            BinOp::Sub => Ok(eval_expr(l, gnv, lvnv)? - eval_expr(r, gnv, lvnv)?),
            BinOp::Mult => Ok(eval_expr(l, gnv, lvnv)? * eval_expr(r, gnv, lvnv)?),
            BinOp::Div => Ok(eval_expr(l, gnv, lvnv)? / eval_expr(r, gnv, lvnv)?),
            BinOp::Mod => Ok(eval_expr(l, gnv, lvnv)? % eval_expr(r, gnv, lvnv)?),
        },
        Expr::LogE { op, l, r } => todo!(),
        Expr::BitE { op, l, r } => todo!(),
        Expr::RelE { op, l, r } => todo!(),
        Expr::VarApp(alias) => {
            if lvnv.contains_key(alias) {
                Ok(lvnv[alias].clone())
            } else {
                Err(io::Error::new(io::ErrorKind::Other, "undefined variable"))
            }
        }
        Expr::FuncApp { alias, ap } => {
            let l = &gnv.fnv[alias];
            let mut new_lvnv = gnv.vnv.clone(); // this is what gnv is for. each func app needs it's own lvnv extended from gnv

            l.fp.iter().zip(ap.iter()).for_each(|(fp, ap)| {
                let ap = eval_expr(ap, gnv, &lvnv).unwrap();
                new_lvnv.insert(fp.clone(), ap);
            });
            eval_func(l, gnv, new_lvnv) // reusing lvnv would be dynamic scope!
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{lexer, parser};
    use std::fs;
    const TEST_DIR: &str = "tests/fixtures/bindings";

    #[test]
    fn dyn_scope() {
        let chars = fs::read(format!("{TEST_DIR}/dyn_scope.c"))
            .expect("file dne")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();
        let tokens = lexer::lex(&chars).unwrap();
        let tree = parser::parse_prg(&tokens).unwrap();
        let val = eval_prg(tree);
        assert!(matches!(
            val,
            Err(e) if e.kind() == io::ErrorKind::Other && e.to_string() == "undefined variable"
        ));
    }

    #[test]
    fn static_scope() {
        let chars = fs::read(format!("{TEST_DIR}/static_scope.c"))
            .expect("file dne")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();
        let tokens = lexer::lex(&chars).unwrap();
        let tree = parser::parse_prg(&tokens).unwrap();
        let val = eval_prg(tree).unwrap();
        assert_eq!(val, 19);
    }
}
