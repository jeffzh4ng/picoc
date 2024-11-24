use crate::{BinOp, Def, Expr, FuncDef, Prg, Stmt, TrgtBinOp, TrgtExpr, TrgtPrg, TrgtStmt};

pub fn translate(src_tree: &Prg) -> TrgtPrg {
    let trgt_prg = src_tree.iter().map(|def| match def {
        Def::FuncDef(func_def) => translate_func_def(func_def),
        Def::VarDef(var_def) => todo!(),
    }).collect::<Vec<_>>();

    trgt_prg[0].clone()
}

fn translate_func_def(fd: &FuncDef) -> Vec<TrgtStmt> {
    let label = TrgtStmt::LabelDef(fd.alias.clone());
    let translated_func_def = std::iter::once(label)
        .chain(fd.body.iter().map(|stmt| match stmt {
            Stmt::Asnmt(defn) => todo!(),
            Stmt::IfEls { cond, then, els } => todo!(),
            Stmt::While { cond, body } => todo!(),
            Stmt::Return(expr) => TrgtStmt::Return(translate_expr(expr)),
        }))
        .collect::<Vec<_>>();

    translated_func_def
}

fn translate_expr(e: &Expr) -> TrgtExpr {
    match e {
        Expr::Int(n) => TrgtExpr::Const(*n),
        Expr::Bool(b) => TrgtExpr::Const(*b as i32),
        Expr::UnaryE { op, l } => todo!(),
        Expr::BinE { op, l, r } => match op {
            // C language designed as portable assembly makes tree rewrites straightforward
            BinOp::Add => TrgtExpr::BinOp(
                TrgtBinOp::Add,
                Box::new(translate_expr(l)),
                Box::new(translate_expr(r)),
            ),
            BinOp::Sub => TrgtExpr::BinOp(
                TrgtBinOp::Sub,
                Box::new(translate_expr(l)),
                Box::new(translate_expr(r)),
            ),
            BinOp::Mult => TrgtExpr::BinOp(
                TrgtBinOp::Mult,
                Box::new(translate_expr(l)),
                Box::new(translate_expr(r)),
            ),
            BinOp::Div => TrgtExpr::BinOp(
                TrgtBinOp::Div,
                Box::new(translate_expr(l)),
                Box::new(translate_expr(r)),
            ),
            BinOp::Mod => TrgtExpr::BinOp(
                TrgtBinOp::Mod,
                Box::new(translate_expr(l)),
                Box::new(translate_expr(r)),
            ),
        },
        Expr::LogE { op, l, r } => todo!(),
        Expr::BitE { op, l, r } => todo!(),
        Expr::RelE { op, l, r } => todo!(),
        Expr::VarApp(alias) => TrgtExpr::TempUse(alias.clone()),
        Expr::FuncApp { alias, ap } => todo!(),
    }
}

#[cfg(test)]
mod test_arithmetic {
    use crate::lexer;
    use crate::parser;
    use crate::typer;
    use std::fs;

    const TEST_DIR: &str = "tests/fixtures/snap/shared/arith";

    #[test]
    fn add() {
        let chars = fs::read(format!("{TEST_DIR}/add.c"))
            .expect("file dne")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let tokens = lexer::lex(&chars).unwrap();
        let src_tree = parser::parse_prg(&tokens).unwrap();
        let _ = typer::type_prg(&src_tree).unwrap();
        let trgt_tree = super::translate(&src_tree);

        insta::assert_yaml_snapshot!(trgt_tree, @r###"
        ---
        - LabelDef: main
        - Return:
            BinOp:
              - Add
              - Const: 9
              - Const: 10
        "###);
    }
}
