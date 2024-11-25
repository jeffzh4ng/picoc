use crate::{IBinOp, IExpr, IPrg, IStmt, TImmOp, TQuad, TRegOp, Temp};

static mut TEMP_COUNTER: usize = 0;

pub fn fresh_temp() -> Temp {
    unsafe {
        let temp = TEMP_COUNTER;
        TEMP_COUNTER += 1;
        temp
    }
}

pub fn select(prg: &IPrg) -> Vec<TQuad> {
    todo!()
}

fn select_stmt(s: &IStmt) -> Vec<TQuad> {
    match s {
        IStmt::Jump(_) => todo!(),
        IStmt::CJump(sexpr, _, _) => todo!(),
        IStmt::LabelDef(_) => todo!(),
        IStmt::Store => todo!(),
        IStmt::Seq(vec) => todo!(),
        IStmt::Return(iexpr) => {
            let t = fresh_temp();
            let foo = select_expr(t, iexpr);
            todo!()
        }
    }
}

// when flatening the tree to linear 3AC quads
// we refer to recursive computations via temps
fn select_expr(d: Temp, e: &IExpr) -> Vec<TQuad> {
    match e {
        IExpr::Const(n) => vec![TQuad::ImmQuad(TImmOp::AddI, d, 0, *n)],
        IExpr::BinOp(op, l, r) => {
            let op = match op {
                IBinOp::Add => TRegOp::Add,
                IBinOp::Sub => TRegOp::Sub,
                IBinOp::Mult => todo!(), // RV32M
                IBinOp::Div => todo!(),  // RV32M
                IBinOp::Mod => todo!(),  // RV32M
            };

            let (ltemp, rtemp) = (fresh_temp(), fresh_temp());
            let (lq, rq) = (select_expr(ltemp, l), select_expr(rtemp, r));
            let instr = vec![TQuad::RegQuad(op, d, ltemp, rtemp)];

            lq.into_iter().chain(rq).chain(instr).collect()
        }
        IExpr::TempUse(_) => todo!(),
        IExpr::Load(_) => todo!(),
        IExpr::Call(_, vec) => todo!(),
    }
}

#[cfg(test)]
mod test_arithmetic {
    use crate::lexer;
    use crate::parser;
    use crate::translator;
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
        let trgt_tree = translator::translate(&src_tree);
        let abs_as = super::select(&trgt_tree);

        insta::assert_yaml_snapshot!(trgt_tree, @r"");
    }
}
