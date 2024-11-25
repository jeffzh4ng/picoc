use crate::{IBinOp, IExpr, IPrg, IStmt, Label, SBinOp, SDef, SExpr, SFuncDef, SPrg, SStmt, Temp};

pub fn translate(src_tree: &SPrg) -> IPrg {
    let trgt_prg = src_tree
        .iter()
        .flat_map(|def| match def {
            SDef::FuncDef(func_def) => translate_func_def(func_def),
            SDef::VarDef(var_def) => todo!(),
        })
        .collect::<Vec<_>>();

    trgt_prg
}

fn translate_func_def(fd: &SFuncDef) -> Vec<IStmt> {
    let label = IStmt::LabelDef(Label::User(fd.alias.clone()));
    // todo: formal params
    let stmts = fd
        .body
        .iter()
        .map(|s_stmt| match s_stmt {
            SStmt::Asnmt(vd) => {
                let expr = translate_expr(&vd.expr);
                let temp = Temp::User(vd.alias.clone());
                IStmt::Compute(temp, expr)
            }
            SStmt::IfEls { cond, then, els } => todo!(),
            SStmt::While { cond, body } => todo!(),
            SStmt::Return(expr) => IStmt::Return(translate_expr(expr)),
        })
        .map(|i_stmt| Box::new(i_stmt))
        .collect::<Vec<_>>();

    let fd = IStmt::Seq(stmts);
    let labeled_fd = std::iter::once(label).chain(vec![fd]).collect::<Vec<_>>();

    labeled_fd
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
        SExpr::VarApp(alias) => IExpr::TempUse(Temp::User(alias.clone())),
        SExpr::FuncApp { alias, aps: ap } => {
            let aps = ap.iter().map(|e| translate_expr(e)).collect::<Vec<_>>();
            IExpr::Call(Label::User(alias.clone()), aps)
        }
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
        - LabelDef:
            User: main
        - Seq:
            - Return:
                BinOp:
                  - Add
                  - Const: 9
                  - Const: 10
        "###);
    }
}

#[cfg(test)]
mod test_bindings {
    use crate::lexer;
    use crate::parser;
    use crate::typer;
    use std::fs;

    const TEST_DIR: &str = "tests/fixtures/snap/shared/bindings";

    #[test]
    fn asnmt() {
        let chars = fs::read(format!("{TEST_DIR}/asnmt.c"))
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
        - LabelDef:
            User: main
        - Seq:
            - Compute:
                - User: x
                - Const: 8
            - Return:
                TempUse:
                  User: x
        "###);
    }
}

#[cfg(test)]
mod test_functions {
    use crate::lexer;
    use crate::parser;
    use crate::typer;
    use std::fs;

    const TEST_DIR: &str = "tests/fixtures/snap/shared/bindings";

    #[test]
    fn composition() {
        let chars = fs::read(format!("{TEST_DIR}/composition.c"))
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
        - LabelDef:
            User: h
        - Seq:
            - Return:
                Const: 11
        - LabelDef:
            User: g
        - Seq:
            - Return:
                BinOp:
                  - Add
                  - Const: 10
                  - Call:
                      - User: h
                      - []
        - LabelDef:
            User: f
        - Seq:
            - Return:
                BinOp:
                  - Add
                  - Const: 9
                  - Call:
                      - User: g
                      - []
        - LabelDef:
            User: main
        - Seq:
            - Return:
                Call:
                  - User: f
                  - []
        "###);
    }

    #[test]
    fn formal_param() {
        let chars = fs::read(format!("{TEST_DIR}/formal_param.c"))
            .expect("file dne")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let tokens = lexer::lex(&chars).unwrap();
        let src_tree = parser::parse_prg(&tokens).unwrap();
        let _ = typer::type_prg(&src_tree).unwrap();
        let trgt_tree = super::translate(&src_tree);

        insta::assert_yaml_snapshot!(trgt_tree, @r"");
    }
}
