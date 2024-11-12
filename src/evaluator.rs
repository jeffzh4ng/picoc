use crate::{BinOp, Defs, Expr, LambdaVal, Prg, Stmt, Vnv};
use std::{collections::HashMap, io};

pub fn eval_prg(prg: Prg) -> Result<i32, io::Error> {
    let fnv = prg
        .iter()
        .map(|defs| match defs {
            Defs::FuncDef(fd) => (
                // funcdef simply creates the lambda
                fd.alias.clone(),
                LambdaVal {
                    fp: fd.formal_param.clone(),
                    body: fd.body.clone(),
                },
            ),
            _ => todo!(), // next: top-level vardefs
        })
        .collect::<HashMap<String, LambdaVal>>();

    let vnv = HashMap::new(); // todo: parse global vardefs
    let nv = Vnv { fnv, vnv };

    // defining nv here so eval_fn can borrow both
    let lvnv = nv.vnv.clone(); // clone it first, before giving &mut
    eval_func(&nv.fnv["main"], &nv, lvnv)
}

fn eval_func(l: &LambdaVal, gnv: &Vnv, mut lvnv: HashMap<String, i32>) -> Result<i32, io::Error> {
    l.body
        .iter()
        .try_fold(None, |acc, stmt| {
            if acc.is_none() {
                eval_stmt(stmt, gnv, &mut lvnv)
            } else {
                Ok(acc) // can't break from closures. switch to loop if perf is an issue
            }
        })?
        .ok_or(io::Error::new(io::ErrorKind::Other, "no return stmt"))
}

fn eval_stmt(
    stmt: &Stmt,
    gnv: &Vnv,
    lvnv: &mut HashMap<String, i32>,
) -> Result<Option<i32>, io::Error> {
    Ok(match stmt {
        Stmt::Asnmt(var_def) => {
            let val = eval_expr(&var_def.expr, gnv, &lvnv)?; // eager
            lvnv.insert(var_def.alias.clone(), val);
            None
        }
        Stmt::Return(e) => Some(eval_expr(e, gnv, &lvnv)?),
        Stmt::IfEls { cond, then, els } => {
            let mut new_lvnv = lvnv.clone();

            if eval_expr(cond, gnv, &new_lvnv)? == 1 {
                eval_stmt(then, gnv, &mut new_lvnv)?
            } else {
                eval_stmt(els, gnv, &mut new_lvnv)?
            }
        }
        Stmt::While { cond, body } => {
            while eval_expr(cond, gnv, &lvnv)? == 1 {
                eval_stmt(body, gnv, lvnv)?;
            }
            None
        }
    })
}

fn eval_expr(e: &Expr, gvnv: &Vnv, lvnv: &HashMap<String, i32>) -> Result<i32, io::Error> {
    match e {
        Expr::Int(n) => Ok(*n),
        Expr::Str(_) => todo!(), // check c0 spec
        Expr::UnaryE { op, l } => todo!(),
        Expr::BinE { op, l, r } => match op {
            BinOp::Add => Ok(eval_expr(l, gvnv, lvnv)? + eval_expr(r, gvnv, lvnv)?),
            BinOp::AddAdd => todo!(), // check c0 spec
            BinOp::Sub => Ok(eval_expr(l, gvnv, lvnv)? - eval_expr(r, gvnv, lvnv)?),
            BinOp::Mult => Ok(eval_expr(l, gvnv, lvnv)? * eval_expr(r, gvnv, lvnv)?),
            BinOp::Div => Ok(eval_expr(l, gvnv, lvnv)? / eval_expr(r, gvnv, lvnv)?),
            BinOp::Mod => Ok(eval_expr(l, gvnv, lvnv)? % eval_expr(r, gvnv, lvnv)?),
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
            let l = &gvnv.fnv[alias];
            let mut new_lvnv = gvnv.vnv.clone(); // this is what gnv is for. each func app needs it's own lvnv extended from gnv

            l.fp.iter().zip(ap.iter()).for_each(|(fp, ap)| {
                let ap = eval_expr(ap, gvnv, &lvnv).unwrap();
                new_lvnv.insert(fp.clone(), ap);
            });
            eval_func(l, gvnv, new_lvnv) // reusing lvnv would be dynamic scope!
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{lexer, parser};
    use std::fs;
    const TEST_DIR: &str = "tests/fixtures/snap/bindings";

    #[test]
    fn dyn_scope() {
        let chars = fs::read(format!("tests/fixtures/snap/illegal/dyn_scope.c"))
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
    fn if_scope() {
        let chars = fs::read(format!("tests/fixtures/snap/illegal/if_scope.c"))
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
