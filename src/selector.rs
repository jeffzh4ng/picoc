use crate::{
    fresh_temp, IBinOp, IExpr, IPrg, IStmt, PseudoOp, RiscvPointerReg, TImmOp, TMemOp, TQuad,
    TRegOp, Temp,
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
            TQuad::Label(l.clone()),
            // allocate 4 words
            TQuad::Imm(
                TImmOp::AddI,
                Temp::PointerReg(RiscvPointerReg::Sp),
                Temp::PointerReg(RiscvPointerReg::Sp),
                -16,
            ),
            // save ra
            TQuad::Mem(
                TMemOp::Store,
                Temp::PointerReg(RiscvPointerReg::Ra),
                12,
                RiscvPointerReg::Sp,
            ),
            // save fp (s0)
            TQuad::Mem(
                TMemOp::Store,
                Temp::PointerReg(RiscvPointerReg::Fp),
                8,
                RiscvPointerReg::Sp,
            ),
            // setup fp
            TQuad::Imm(
                TImmOp::AddI,
                Temp::PointerReg(RiscvPointerReg::Fp),
                Temp::PointerReg(RiscvPointerReg::Sp),
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
                Temp::PointerReg(RiscvPointerReg::A0),
                t,
                0,
            )];
            let epilogue = vec![
                // restore ra
                TQuad::Mem(
                    TMemOp::Load,
                    Temp::PointerReg(RiscvPointerReg::Ra),
                    12,
                    RiscvPointerReg::Sp,
                ),
                // restore fp
                TQuad::Mem(
                    TMemOp::Load,
                    Temp::PointerReg(RiscvPointerReg::Fp),
                    8,
                    RiscvPointerReg::Sp,
                ),
                // shrink stack
                TQuad::Imm(
                    TImmOp::AddI,
                    Temp::PointerReg(RiscvPointerReg::Sp),
                    Temp::PointerReg(RiscvPointerReg::Sp),
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
            Temp::PointerReg(RiscvPointerReg::Z),
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
            - PointerReg: Sp
            - PointerReg: Sp
            - -16
        - Mem:
            - Store
            - PointerReg: Ra
            - 12
            - Sp
        - Mem:
            - Store
            - PointerReg: Fp
            - 8
            - Sp
        - Imm:
            - AddI
            - PointerReg: Fp
            - PointerReg: Sp
            - 16
        - Imm:
            - AddI
            - MachineTemp: 1
            - PointerReg: Z
            - 9
        - Imm:
            - AddI
            - MachineTemp: 2
            - PointerReg: Z
            - 10
        - Reg:
            - Add
            - MachineTemp: 0
            - MachineTemp: 1
            - MachineTemp: 2
        - Imm:
            - AddI
            - PointerReg: Ra
            - MachineTemp: 0
            - 0
        - Mem:
            - Load
            - PointerReg: Ra
            - 12
            - Sp
        - Mem:
            - Load
            - PointerReg: Fp
            - 8
            - Sp
        - Imm:
            - AddI
            - PointerReg: Sp
            - PointerReg: Sp
            - 16
        - Pseudo: Ret
        "###);
    }
}
