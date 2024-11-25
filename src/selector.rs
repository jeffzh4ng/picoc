use crate::{fresh_temp, IBinOp, IExpr, IPrg, IStmt, TImmOp, TQuad, TRegOp, Temp, RISCV_ABI};

pub fn select(prg: &IPrg) -> Vec<TQuad> {
    let trgt_prg = prg.iter().flat_map(|stmt| select_stmt(stmt)).collect();
    trgt_prg
}

fn select_stmt(s: &IStmt) -> Vec<TQuad> {
    match s {
        IStmt::Jump(_) => todo!(),
        IStmt::CJump(sexpr, _, _) => todo!(),
        IStmt::LabelDef(l) => {
            // tile funcdef vs label? todo
            vec![]
        }
        IStmt::Compute(temp, iexpr) => todo!(),
        IStmt::Load(_, _) => todo!(),
        IStmt::Store(_, _) => todo!(),
        IStmt::Seq(stmts) => stmts.iter().flat_map(|stmt| select_stmt(stmt)).collect(),
        IStmt::Return(iexpr) => {
            let t = fresh_temp();
            let expr_instrs = select_expr(t.clone(), iexpr);
            let ret_instr = vec![TQuad::Imm(
                TImmOp::AddI,
                Temp::Reg(RISCV_ABI.ra.to_string()),
                t,
                0,
            )];
            expr_instrs.into_iter().chain(ret_instr).collect()
        }
    }
}

// when flatening the tree to linear 3AC quads
// we refer to recursive computations via temps
fn select_expr(d: Temp, e: &IExpr) -> Vec<TQuad> {
    match e {
        IExpr::Const(n) => vec![TQuad::Imm(
            TImmOp::AddI,
            d,
            Temp::Reg(RISCV_ABI.zero.to_string()),
            *n,
        )],
        IExpr::BinOp(op, l, r) => {
            let op = match op {
                IBinOp::Add => TRegOp::Add,
                IBinOp::Sub => TRegOp::Sub,
                IBinOp::Mult => todo!(), // RV32M
                IBinOp::Div => todo!(),  // RV32M
                IBinOp::Mod => todo!(),  // RV32M
            };

            let (ltemp, rtemp) = (fresh_temp(), fresh_temp());
            let (lq, rq) = (select_expr(ltemp.clone(), l), select_expr(rtemp.clone(), r));
            let instr = vec![TQuad::Reg(op, d, ltemp, rtemp)];

            lq.into_iter().chain(rq).chain(instr).collect()
        }
        IExpr::TempUse(_) => todo!(),
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

        insta::assert_yaml_snapshot!(abs_as, @r###"
        ---
        - Imm:
            - AddI
            - Machine: 1
            - Reg: zero
            - 9
        - Imm:
            - AddI
            - Machine: 2
            - Reg: zero
            - 10
        - Reg:
            - Add
            - Machine: 0
            - Machine: 1
            - Machine: 2
        - Imm:
            - AddI
            - Reg: ra
            - Machine: 0
            - 0
        "###);
    }
}
