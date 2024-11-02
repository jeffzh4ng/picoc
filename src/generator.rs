use crate::parser;

pub fn gen(tree: parser::Program) -> Vec<String> {
    let prologue = vec!["mv fp,sp".to_owned(), "addi sp,sp,208".to_owned()]; // 26 vars

    let program: Vec<String> = todo!();
    // let program = tree
    //     .main_function
    //     .stmts
    //     .into_iter()
    //     .map(|s| gen_stmt(s))
    //     .flatten()
    //     .collect::<Vec<_>>();

    let output: Vec<String> = vec![
        ".text".to_owned(),
        ".globl main".to_owned(),
        ".section .text".to_owned(),
        "main:".to_owned(),
        prologue
            .iter()
            .map(|line| format!("    {line}"))
            .collect::<Vec<_>>()
            .join("\n"),
        program.join("\n"),
        // mov rbp rsp
        // pop rbp
        "".to_owned(),
    ];

    output
}

fn calc_offset(id: &parser::Id) -> u8 {
    let binding = id.0.chars().next().unwrap();
    let offset = binding as u8 - 'a' as u8;
    offset * 8
}

fn gen_asnmt(a: parser::Asnmt) -> Vec<String> {
    match a {
        parser::Asnmt::CreateBind {
            var: id,
            body: expr,
        } => {
            let offset = calc_offset(&id);
            let expr = gen_expr(*expr);

            vec![
                expr.join("\n"),
                "# assigning...".to_owned(),
                "lw t0,0(sp)".to_owned(),
                format!("sw t0,{offset}(fp)"),
                "# done...".to_owned(),
            ]
        }
        parser::Asnmt::UpdateBind { id, op, expr } => {
            let expr = gen_expr(*expr);
            let offset = calc_offset(&id);
            let update = match op {
                parser::BinOp::Add => "add",
                parser::BinOp::Sub => "sub",
                parser::BinOp::Mult => "mul",
                parser::BinOp::Div => "div",
                parser::BinOp::Mod => todo!(),
            };

            vec![
                expr.join("\n"),
                format!("lw t0,0(sp)").to_owned(),
                format!("lw t1,{offset}(fp)").to_owned(),
                format!("{update} t2,t0,t1").to_owned(),
                format!("sw t2,{offset}(fp)").to_owned(),
            ]
        }
    }
}

fn gen_stmt(s: parser::Cmd) -> Vec<String> {
    match s {
        parser::Cmd::Asnmt(a) => gen_asnmt(a),
        parser::Cmd::For {
            asnmt,
            cond,
            update,
            body,
        } => todo!(),
        parser::Cmd::While => todo!(),
        parser::Cmd::Return(e) => {
            let output = vec![
                gen_expr(e)
                    .iter()
                    .map(|line| format!("    {line}"))
                    .collect::<Vec<_>>()
                    .join("\n"),
                "# return expr".to_owned(),
                "lw a0,0(sp)".to_owned(),
                "addi sp,sp,8".to_owned(),
                "ret".to_owned(),
            ];

            output
        }
        parser::Cmd::If => todo!(),
        parser::Cmd::IfEls { cond, then, els } => {
            let cond_mc = gen_expr(*cond);
            let then_mc = gen_stmt(*then);
            let els_mc = gen_stmt(*els);

            let output = vec![
                "  ########################### evaluating cond expr ###########################"
                    .to_owned(),
                cond_mc
                    .iter()
                    .map(|line| format!("  {line}"))
                    .collect::<Vec<_>>()
                    .join("\n"),
                "  ################################### branch ###################################"
                    .to_owned(),
                "  lw t1,0(sp)".to_owned(),
                "  addi sp,sp,8".to_owned(),
                "  li t2,1".to_owned(),
                "  beq t1,t2,then".to_owned(),
                "  bne t1,t2,els".to_owned(),
                "################################### .then ###################################"
                    .to_owned(),
                "then:".to_owned(),
                then_mc
                    .iter()
                    .map(|line| format!("  {line}"))
                    .chain(std::iter::once("  j end".to_owned()))
                    .collect::<Vec<_>>()
                    .join("\n"),
                "################################### .els ###################################"
                    .to_owned(),
                "els:".to_owned(),
                els_mc
                    .iter()
                    .map(|line| format!("  {line}"))
                    .chain(std::iter::once("  j end".to_owned()))
                    .collect::<Vec<_>>()
                    .join("\n"),
                "################################### .end ###################################"
                    .to_owned(),
                "end:".to_owned(),
                "  ret".to_owned(),
            ];

            output
        }
    }
}

fn gen_expr(e: parser::Expr) -> Vec<String> {
    match e {
        parser::Expr::Var(id) => {
            let offset = calc_offset(&id);
            vec![
                "# elimination of variable".to_owned(),
                format!("lw t0,{offset}(fp)"),
                "sw t0,0(sp)".to_owned(),
            ]
        }
        parser::Expr::Int(n) => {
            let mut output = Vec::new();
            output.push("# 1. load".to_owned());
            output.push(format!("li t1,{n}"));
            output.push("".to_owned());

            output.push("# 2. push".to_owned());
            output.push("addi sp,sp,-8".to_owned());
            output.push("sw t1,0(sp)".to_owned()); // i128?
            output.push(
                "#----------------------------------------------------------------------------"
                    .to_owned(),
            );

            output
        }
        parser::Expr::Str(_) => todo!(),
        parser::Expr::UnaryE { op, l } => todo!(),
        parser::Expr::BinE { op, l, r } => {
            let left_expr = gen_expr(*l);
            let right_expr = gen_expr(*r);

            let mut output = Vec::with_capacity(left_expr.len() + right_expr.len() + 8);
            output.extend(left_expr);
            output.extend(right_expr);

            // emulating stack machine's push/pop 1AC with register machine's load/store 3AC
            // 1. pop the operands
            output.push("# 1. pop the operands".to_owned());
            output.push("lw t1,0(sp)".to_owned());
            output.push("addi sp,sp,8".to_owned());
            output.push("lw t2,0(sp)".to_owned());
            output.push("addi sp,sp,8".to_owned());
            output.push("".to_owned());

            // 2. operate on the operands
            let instr = match op {
                parser::BinOp::Add => "add t3,t2,t1".to_owned(),
                parser::BinOp::Sub => "sub t3,t2,t1".to_owned(),
                parser::BinOp::Mult => "mul t3,t2,t1".to_owned(),
                parser::BinOp::Div => "div t3,t2,t1".to_owned(),
                parser::BinOp::Mod => todo!(),
            };
            output.push("# 2. operate on the operands".to_owned());
            output.push(instr);
            output.push("".to_owned());

            // 3. push the value
            output.push("# 3. push the value".to_owned());
            output.push("addi sp,sp,-8".to_owned());
            output.push("sw t3,0(sp)".to_owned());
            output.push(
                "#----------------------------------------------------------------------------"
                    .to_owned(),
            );

            output
        }
        parser::Expr::RelE { op, l, r } => {
            let left_expr = gen_expr(*l);
            let right_expr = gen_expr(*r);

            let mut output = Vec::with_capacity(left_expr.len() + right_expr.len() + 8);
            output.extend(left_expr);
            output.extend(right_expr);

            // emulating stack machine's push/pop 1AC with register machine's load/store 3AC
            output.push("# 1. (t2, t1) <- pop".to_owned());
            output.push("lw t1,0(sp)".to_owned());
            output.push("addi sp,sp,8".to_owned());
            output.push("lw t2,0(sp)".to_owned());
            output.push("addi sp,sp,8".to_owned());
            output.push("".to_owned());

            // 2. operate on the operands
            let instr = match op {
                parser::RelOp::Eq => {
                    vec!["sub t3,t2,t1".to_owned(), "seqz t3,t3".to_owned()].join("\n")
                }
                parser::RelOp::Neq => vec![
                    "sub t3,t2,t1".to_owned(),
                    "seqz t3,t3".to_owned(),
                    "xori t3,t3,1".to_owned(),
                ]
                .join("\n"),
                parser::RelOp::And => "and t3,t2,t1".to_owned(), // TODO: does riscv short circuit?
                parser::RelOp::Or => "or t3,t2,t1".to_owned(),   // TODO: does riscv short circuit?
                parser::RelOp::LtEq => vec![
                    // a <= b equivalent to !(b < a)
                    "slt t3,t1,t2".to_owned(),   // b < a
                    "  xori t3,t3,1".to_owned(), // !(b < a)
                ]
                .join("\n"),
                parser::RelOp::Lt => "slt t3,t2,t1".to_owned(),
                parser::RelOp::GtEq => vec![
                    // a >= b equivalent b <= a equivalent to !(a < b)
                    "slt t3,t2,t1".to_owned(),   // a < b
                    "  xori t3,t3,1".to_owned(), // !(a < b)
                ]
                .join("\n"),
                parser::RelOp::Gt => "slt t3,t1,t2".to_owned(),
            };
            output.push("# 2. op(t2, t1)".to_owned());
            output.push(instr);
            output.push("".to_owned());

            // 3. push value in t3 onto stack
            output.push("# 3. push t3 ->".to_owned());
            output.push("addi sp,sp,-8".to_owned());
            output.push("sw t3,0(sp)".to_owned());
            output.push(
                "#----------------------------------------------------------------------------"
                    .to_owned(),
            );

            output
        }
        parser::Expr::BitE { op, l, r } => todo!(),
        parser::Expr::LogE { op, l, r } => todo!(),
    }
}
