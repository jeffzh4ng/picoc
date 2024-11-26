use crate::{OptLevel, TQuad, Temp};

pub fn allocate(abs_as: &[TQuad], opt: OptLevel) -> Vec<String> {
    match opt {
        OptLevel::O0 => allocate_1ac(abs_as),
        OptLevel::O1 => todo!(),
        OptLevel::O2 => todo!(),
    }
}

const POP_IMM_T0: &str = POP_RIGHT_T0;
const POP_RIGHT_T0: &str = "lw t0, 0(sp) # t0 <- pop\naddi sp,sp,8 # shrink stack";
const POP_LEFT_T1: &str = "lw t1, 0(sp) # t1 <- pop\naddi sp,sp,8 # shrink stack";
const PUSH_T2: &str = "addi sp,sp,-8 # grow stack\nsw t2, 0(sp) # push t2 ->\n";

fn allocate_1ac(abs_as: &[TQuad]) -> Vec<String> {
    abs_as
        .iter()
        .flat_map(|quad| match quad {
            TQuad::Reg(treg_op, dt, lt, rt) => {
                vec![
                    POP_RIGHT_T0.to_owned(),
                    POP_LEFT_T1.to_owned(),
                    format!("{} t2, t1, t0 # operate", treg_op.to_string()),
                    PUSH_T2.to_owned(),
                ]
            }
            TQuad::Imm(timm_op, dt, lt, imm) => match (dt, lt) {
                (Temp::UserTemp(_), Temp::UserTemp(_)) => todo!(),
                (Temp::UserTemp(_), Temp::MachineTemp(_)) => todo!(),
                (Temp::UserTemp(_), Temp::PointerReg(pr)) => todo!(),
                (Temp::MachineTemp(_), Temp::UserTemp(_)) => todo!(),
                (Temp::MachineTemp(_), Temp::MachineTemp(_)) => todo!(),
                (Temp::MachineTemp(_), Temp::PointerReg(pr)) => {
                    vec![
                        format!("{} t2, {}", timm_op.to_string(), pr.to_string()),
                        PUSH_T2.to_owned(), // push b/c we're using t0
                    ]
                }
                (Temp::PointerReg(pr), Temp::UserTemp(_)) => todo!(),
                (Temp::PointerReg(pr), Temp::MachineTemp(_)) => {
                    vec![
                        POP_IMM_T0.to_owned(),
                        format!("{} {}, t0, {}", timm_op.to_string(), pr.to_string(), imm),
                    ] // no push b/c we're using pr
                }
                (Temp::PointerReg(dpr), Temp::PointerReg(lpr)) => vec![format!(
                    "{} {}, {}({})",
                    timm_op.to_string(),
                    dpr.to_string(),
                    imm,
                    lpr.to_string()
                )],
            },
            TQuad::Mem(tmem_op, temp, offset, base) => match temp {
                Temp::UserTemp(_) => todo!(),
                Temp::MachineTemp(_) => todo!(),
                Temp::PointerReg(riscv_pointer_reg) => {
                    vec![format!(
                        "{} {}, {}({})",
                        tmem_op.to_string(),
                        riscv_pointer_reg.to_string(),
                        offset,
                        base.to_string()
                    )]
                }
            },
            TQuad::Pseudo(pseudo_op) => vec![pseudo_op.to_string()],
        })
        .collect()
}

#[cfg(test)]
mod test_arith {
    use crate::lexer;
    use crate::parser;
    use crate::selector;
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
        let abs_as = selector::select(&trgt_tree);
        let assembly = super::allocate(&abs_as, super::OptLevel::O0);
        insta::assert_yaml_snapshot!(assembly, @r###"
        ---
        - "addi sp, -16(sp)"
        - "sw ra, 12(sp)"
        - "sw fp, 8(sp)"
        - "addi fp, 16(sp)"
        - "addi t2, zero"
        - "addi sp,sp,-8 # grow stack\nsw t2, 0(sp) # push t2 ->\n"
        - "addi t2, zero"
        - "addi sp,sp,-8 # grow stack\nsw t2, 0(sp) # push t2 ->\n"
        - "lw t0, 0(sp) # t0 <- pop\naddi sp,sp,8 # shrink stack"
        - "lw t1, 0(sp) # t1 <- pop\naddi sp,sp,8 # shrink stack"
        - "add t2, t1, t0 # operate"
        - "addi sp,sp,-8 # grow stack\nsw t2, 0(sp) # push t2 ->\n"
        - "lw t0, 0(sp) # t0 <- pop\naddi sp,sp,8 # shrink stack"
        - "addi a0, t0, 0"
        - "lw ra, 12(sp)"
        - "lw fp, 8(sp)"
        - "addi sp, 16(sp)"
        - ret
        "###);
    }
}
