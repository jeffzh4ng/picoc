use crate::{
    fresh_temp, IBinOp, IExpr, IPrg, IStmt, PseudoOp, RiscvUtilReg, TImmOp, TMemOp, TQuad, TRegOp,
    Temp,
};

pub fn select(prg: &IPrg) -> Vec<TQuad> {
    let trgt_prg = prg.iter().flat_map(|stmt| select_stmt(stmt)).collect();
    trgt_prg
}

fn select_stmt(s: &IStmt) -> Vec<TQuad> {
    match s {
        IStmt::Jump(_) => todo!(),
        IStmt::CJump(sexpr, _, _) => todo!(),
        IStmt::LabelDef(l) => vec![
            // allocate 4 words
            TQuad::Imm(
                TImmOp::AddI,
                Temp::UtilReg(RiscvUtilReg::Sp),
                Temp::UtilReg(RiscvUtilReg::Sp),
                -16,
            ),
            // save ra
            TQuad::Mem(
                TMemOp::Store,
                Temp::UtilReg(RiscvUtilReg::Ra),
                12,
                RiscvUtilReg::Sp,
            ),
            // save fp (s0)
            TQuad::Mem(
                TMemOp::Store,
                Temp::UtilReg(RiscvUtilReg::Fp),
                8,
                RiscvUtilReg::Sp,
            ),
            // setup fp
            TQuad::Imm(
                TImmOp::AddI,
                Temp::UtilReg(RiscvUtilReg::Fp),
                Temp::UtilReg(RiscvUtilReg::Sp),
                16,
            ),
        ],
        IStmt::Compute(temp, iexpr) => todo!(),
        IStmt::Load(_, _) => todo!(),
        IStmt::Store(_, _) => todo!(),
        IStmt::Seq(stmts) => stmts.iter().flat_map(|stmt| select_stmt(stmt)).collect(),
        IStmt::Return(iexpr) => {
            let t = fresh_temp();
            let expr_instrs = select_expr(t.clone(), iexpr);
            let ret_instr = vec![TQuad::Imm(
                TImmOp::AddI,
                Temp::UtilReg(RiscvUtilReg::Ra),
                t,
                0,
            )];
            let epilogue = vec![
                // restore ra
                TQuad::Mem(
                    TMemOp::Load,
                    Temp::UtilReg(RiscvUtilReg::Ra),
                    12,
                    RiscvUtilReg::Sp,
                ),
                // restore fp
                TQuad::Mem(
                    TMemOp::Load,
                    Temp::UtilReg(RiscvUtilReg::Fp),
                    8,
                    RiscvUtilReg::Sp,
                ),
                // shrink stack
                TQuad::Imm(
                    TImmOp::AddI,
                    Temp::UtilReg(RiscvUtilReg::Sp),
                    Temp::UtilReg(RiscvUtilReg::Sp),
                    16,
                ),
                // ret
                TQuad::Pseudo(PseudoOp::Ret),
            ];
            expr_instrs
                .into_iter()
                .chain(ret_instr)
                .chain(epilogue)
                .collect()
        }
    }
}

fn select_expr(d: Temp, e: &IExpr) -> Vec<TQuad> {
    match e {
        IExpr::Const(n) => vec![TQuad::Imm(
            TImmOp::AddI,
            d,
            Temp::UtilReg(RiscvUtilReg::Z),
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
mod test_arith {
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
            - UtilReg: Sp
            - UtilReg: Sp
            - -16
        - Mem:
            - Store
            - UtilReg: Ra
            - 12
            - Sp
        - Mem:
            - Store
            - UtilReg: Fp
            - 8
            - Sp
        - Imm:
            - AddI
            - UtilReg: Fp
            - UtilReg: Sp
            - 16
        - Imm:
            - AddI
            - MachineTemp: 1
            - UtilReg: Z
            - 9
        - Imm:
            - AddI
            - MachineTemp: 2
            - UtilReg: Z
            - 10
        - Reg:
            - Add
            - MachineTemp: 0
            - MachineTemp: 1
            - MachineTemp: 2
        - Imm:
            - AddI
            - UtilReg: Ra
            - MachineTemp: 0
            - 0
        - Mem:
            - Load
            - UtilReg: Ra
            - 12
            - Sp
        - Mem:
            - Load
            - UtilReg: Fp
            - 8
            - Sp
        - Imm:
            - AddI
            - UtilReg: Sp
            - UtilReg: Sp
            - 16
        - Pseudo: Ret
        "###);
    }
}
