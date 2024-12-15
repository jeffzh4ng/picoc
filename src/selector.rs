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
        IStmt::CJump(_sexpr, _, _) => todo!(),
        IStmt::Compute(_temp, _iexpr) => todo!(),
        IStmt::Load(_, _) => todo!(),
        IStmt::Store(_, _) => todo!(),
        IStmt::Seq(l, stmts) => {
            let prologue = vec![
                TQuad::Label(l.clone()),
                // allocate 4 words
                TQuad::Imm(
                    TImmOp::AddI,
                    Temp::PointerReg(RiscvPointerReg::Sp),
                    Temp::PointerReg(RiscvPointerReg::Sp),
                    -16,
                ),
                // save caller's ra
                TQuad::Mem(
                    TMemOp::Store,
                    Temp::PointerReg(RiscvPointerReg::Ra),
                    12,
                    RiscvPointerReg::Sp,
                ),
                // save caller's fp (s0)
                TQuad::Mem(
                    TMemOp::Store,
                    Temp::PointerReg(RiscvPointerReg::Fp),
                    8,
                    RiscvPointerReg::Sp,
                ),
                // setup callee's fp
                TQuad::Imm(
                    TImmOp::AddI,
                    Temp::PointerReg(RiscvPointerReg::Fp),
                    Temp::PointerReg(RiscvPointerReg::Sp),
                    16,
                ),
            ];

            let _fps: Vec<TQuad> = vec![];

            let body = stmts
                .iter()
                .flat_map(|stmt| select_stmt(stmt))
                .collect::<Vec<_>>();

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
                // deallocate 4 words
                TQuad::Imm(
                    TImmOp::AddI,
                    Temp::PointerReg(RiscvPointerReg::Sp),
                    Temp::PointerReg(RiscvPointerReg::Sp),
                    16,
                ),
                // ret
                TQuad::Pseudo(PseudoOp::Ret),
            ];

            prologue.into_iter().chain(body).chain(epilogue).collect()
        }
        IStmt::Return(iexpr) => {
            let t = fresh_temp();
            let expr_instrs = select_expr(t.clone(), iexpr);
            let ret_instr = vec![TQuad::Imm(
                TImmOp::AddI,
                Temp::PointerReg(RiscvPointerReg::A0),
                t,
                0,
            )];

            expr_instrs.into_iter().chain(ret_instr).collect()
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
        IExpr::Call(l, aps) => {
            if aps.len() > 8 {
                panic!("todo: more than 8 args not supported");
            }

            let aps = aps.iter().enumerate().map(|(i, a)| match i {
                0 => select_expr(Temp::PointerReg(RiscvPointerReg::A0), a),
                1 => select_expr(Temp::PointerReg(RiscvPointerReg::A1), a),
                2 => select_expr(Temp::PointerReg(RiscvPointerReg::A2), a),
                3 => select_expr(Temp::PointerReg(RiscvPointerReg::A3), a),
                4 => select_expr(Temp::PointerReg(RiscvPointerReg::A4), a),
                5 => select_expr(Temp::PointerReg(RiscvPointerReg::A5), a),
                6 => select_expr(Temp::PointerReg(RiscvPointerReg::A6), a),
                7 => select_expr(Temp::PointerReg(RiscvPointerReg::A7), a),
                _ => unreachable!(),
            });

            aps.into_iter()
                .flatten()
                .chain(vec![
                    TQuad::Pseudo(PseudoOp::Call(l.clone())),
                    TQuad::Imm(TImmOp::AddI, d, Temp::PointerReg(RiscvPointerReg::A0), 0),
                ])
                .collect()
        }
    }
}

#[cfg(test)]
mod test_arith {
    use crate::lexer;
    use crate::parser_ast;
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
        let src_tree = parser_ast::parse_prg(&tokens).unwrap();
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
