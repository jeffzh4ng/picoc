use crate::{SBinOp, SDef, SExpr, SFuncDef, SPrg, SStmt, IBinOp, IExpr, IPrg, IStmt};

pub fn translate(src_tree: &SPrg) -> IPrg {
    let trgt_prg = src_tree.iter().map(|def| match def {
        SDef::FuncDef(func_def) => translate_func_def(func_def),
        SDef::VarDef(var_def) => todo!(),
    }).collect::<Vec<_>>();

    trgt_prg[0].clone()
}

fn translate_func_def(fd: &SFuncDef) -> Vec<IStmt> {
    let label = IStmt::LabelDef(fd.alias.clone());
    let translated_func_def = std::iter::once(label)
        .chain(fd.body.iter().map(|stmt| match stmt {
            SStmt::Asnmt(defn) => todo!(),
            SStmt::IfEls { cond, then, els } => todo!(),
            SStmt::While { cond, body } => todo!(),
            SStmt::Return(expr) => IStmt::Return(translate_expr(expr)),
        }))
        .collect::<Vec<_>>();

    translated_func_def
}

fn translate_expr(e: &SExpr) -> IExpr {
    match e {
        SExpr::Int(n) => IExpr::Const(*n),
        SExpr::Bool(b) => IExpr::Const(*b as i32),
        SExpr::UnaryE { op, l } => todo!(),
        SExpr::BinE { op, l, r } => match op {
            // C language designed as portable assembly makes tree rewrites straightforward
            SBinOp::Add => IExpr::BinOp(
                IBinOp::Add,
                Box::new(translate_expr(l)),
                Box::new(translate_expr(r)),
            ),
            SBinOp::Sub => IExpr::BinOp(
                IBinOp::Sub,
                Box::new(translate_expr(l)),
                Box::new(translate_expr(r)),
            ),
            SBinOp::Mult => IExpr::BinOp(
                IBinOp::Mult,
                Box::new(translate_expr(l)),
                Box::new(translate_expr(r)),
            ),
            SBinOp::Div => IExpr::BinOp(
                IBinOp::Div,
                Box::new(translate_expr(l)),
                Box::new(translate_expr(r)),
            ),
            SBinOp::Mod => IExpr::BinOp(
                IBinOp::Mod,
                Box::new(translate_expr(l)),
                Box::new(translate_expr(r)),
            ),
        },
        SExpr::LogE { op, l, r } => todo!(),
        SExpr::BitE { op, l, r } => todo!(),
        SExpr::RelE { op, l, r } => todo!(),
        SExpr::VarApp(alias) => IExpr::TempUse(alias.clone()),
        SExpr::FuncApp { alias, ap } => todo!(),
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
