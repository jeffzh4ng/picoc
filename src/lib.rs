use serde::{Deserialize, Serialize};
use std::collections::HashMap;

// pub mod evaluator;
pub mod allocator;
pub mod lexer;
pub mod parser;
pub mod selector;
pub mod translator;
pub mod typer;

#[macro_use]
macro_rules! common_struct {

    ($(#[$meta:meta])* $vis:vis struct $name:ident $body:tt) => {
        $(#[$meta])*
        #[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
        $vis struct $name $body
    }
}

#[macro_use]
macro_rules! common_enum {
    ($(#[$meta:meta])* $vis:vis enum $name:ident { $($variants:tt)* }) => {
        $(#[$meta])*
        #[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
        $vis enum $name { $($variants)* }
    }
}

// *********************************************************************************************************************
// ************************************************ SOURCE ENVIRONMENTS ************************************************
// *********************************************************************************************************************

// ***** static tnv: Map<Alias, Type> *****
common_struct! { pub struct Tnv { fnv: HashMap<String, LambdaType>, vnv: HashMap<String, Type> }}
common_struct! { pub struct LambdaType { fp: Vec<Type>, body: Type } }
common_enum! { pub enum Type { Int, Bool, Void } } // Cond(Type::Bool, Box<Type>, Box<Type>),

// ***** dynamic vnv: Map<Alias, Val> *****
common_struct! { pub struct Vnv { fnv: HashMap<String, LambdaVal>, vnv: HashMap<String, i32> }} // todo, -> Val
common_struct! { pub struct LambdaVal { pub fp: Vec<String>, pub body: Vec<SStmt>} } // fp's only need types (tags) if implementing safety dynamically
common_enum! { pub enum Val { Int(i32), Bool(bool), Str(String) } }

// *********************************************************************************************************************
// *********************************************** SOURCE REPRESENTATION ***********************************************
// *********************************************************************************************************************

// TODO: for loops, etc.
type SugaredPrg = Vec<()>;

// ***** prg: Vec<Defs> *****
type SPrg = Vec<SDef>;
common_enum! { pub enum SDef { FuncDef(SFuncDef), VarDef(SVarDef) } }
common_struct! { pub struct SFuncDef {pub alias: String,  pub typ: Type, pub fps: Vec<(String, Type)>, pub body: Vec<SStmt> } } // fp needs Type for statics, and String for dynamics
common_struct! { pub struct SVarDef { pub alias: String, pub typ: Type, pub expr: Box<SExpr> }} // UpdateBind { alias: String, op: BinOp, expr: Box<Expr> }

common_enum! {
    pub enum SStmt {
        IfEls { cond: Box<SExpr>, then: Box<SStmt>, els: Option<Box<SStmt>> }, While { cond: Box<SExpr>, body: Box<SStmt> }, // control
        Asnmt(SVarDef), Return(SExpr), // bindings (intros in C)
    }
}

common_enum! {
    #[rustfmt::skip]
    pub enum SExpr {
        // intros
        Int(i32), Bool(bool),

        // elims
        UnaryE { op: SUnaryOp, l: Box<SExpr> }, BinE { op: SBinOp, l: Box<SExpr>, r: Box<SExpr> }, LogE { op: SLogOp, l: Box<SExpr>, r: Box<SExpr> },
        BitE { op: SBitOp, l: Box<SExpr>, r: Box<SExpr> }, RelE { op: SRelOp, l: Box<SExpr>, r: Box<SExpr> },
        VarApp(String), FuncApp{ alias: String, aps: Vec<SExpr> }
    }
}

common_enum! { pub enum SLogOp { And, Or } }
common_enum! { pub enum SBitOp { And, Or, Xor } }
common_enum! { pub enum SRelOp { Eq, Neq, And, Or, LtEq, Lt, GtEq, Gt } }
common_enum! { pub enum SBinOp { Add, Sub, Mult, Div, Mod } }
common_enum! { pub enum SUnaryOp { Add, Sub } }

//
//
//
// *********************************************************************************************************************
// ******************************************** INTERMEDIATE REPRESENTATION ********************************************
// *********************************************************************************************************************

// intermediate AST is not too different from source AST,
// since C was designed as portable assembly

// the semantics are closer to metal:
// - arithmetic: remains more or less the same
// - control: conditionals and loops -> jump w/ labels
// - bindings: vardef and varapp -> loads/stores w/ unlimited temps
//                               -> functions jumps w/ labels

type IPrg = Vec<IStmt>;
common_enum! {
    pub enum IStmt {
        Jump(Label), CJump(SExpr, Label, Label), LabelDef(Label), // control
        Compute(Temp, IExpr), Load(Temp, Mem), Store(Mem, Temp), // bindings
        Seq(Vec<Box<IStmt>>), Return(IExpr), // functions
    }
}

common_enum! {
    pub enum IExpr {
        Const(i32), BinOp(IBinOp, Box<IExpr>, Box<IExpr>), // arithmetic``
        TempUse(Temp), // bindings
        Call(Label, Vec<IExpr>), // functions
    }
}

common_enum! { pub enum IBinOp { Add, Sub, Mult, Div, Mod } }
common_enum! { pub enum IBitOp { And, Or, Xor } }
common_enum! { pub enum IRelOp { Eq, Neq, And, Or, LtEq, Lt, GtEq, Gt } }

//
//
//
// *********************************************************************************************************************
// ****************************************** INTERMEDIATE/TARGET REFERENCES *******************************************
// *********************************************************************************************************************

type Imm = i32;
common_enum! { pub enum Temp { User(String), Machine(usize), Reg(String) } }
common_enum! { pub enum Mem { String } }
common_enum! { pub enum Label { User(String), Machine(usize) } }

static mut TEMP_COUNTER: usize = 0;
static mut LABEL_COUNTER: usize = 0;

pub fn fresh_temp() -> Temp {
    unsafe {
        let temp = TEMP_COUNTER;
        TEMP_COUNTER += 1;
        Temp::Machine(temp)
    }
}

pub fn fresh_label() -> Label {
    unsafe {
        let label = LABEL_COUNTER;
        LABEL_COUNTER += 1;
        Label::Machine(label)
    }
}

// target register abi
pub struct RiscvAbi {
    pub zero: &'static str,
    pub ra: &'static str, // return addres
    pub sp: &'static str, // stack pointer
    pub gp: &'static str, // global pointer
    pub tp: &'static str, // frame pointer
    // temporaries
    pub t0: &'static str,
    pub t1: &'static str,
    pub t2: &'static str,
    // saved registers
    pub s0: &'static str,
    pub s1: &'static str,
    // argument/return registers
    pub a0: &'static str,
    pub a1: &'static str,
    pub a2: &'static str,
    pub a3: &'static str,
    pub a4: &'static str,
    pub a5: &'static str,
    pub a6: &'static str,
    pub a7: &'static str,
    // more saved registers
    pub s2: &'static str,
    pub s3: &'static str,
    pub s4: &'static str,
    pub s5: &'static str,
    pub s6: &'static str,
    pub s7: &'static str,
    pub s8: &'static str,
    pub s9: &'static str,
    pub s10: &'static str,
    pub s11: &'static str,
    pub t3: &'static str,
    pub t4: &'static str,
    pub t5: &'static str,
    pub t6: &'static str,
    pub pc: &'static str,
}

pub const RISCV_ABI: RiscvAbi = RiscvAbi {
    zero: "zero", // x0
    ra: "ra",     // x1
    sp: "sp",     // x2
    gp: "gp",     // x3
    tp: "tp",     // x4
    t0: "t0",     // x5
    t1: "t1",     // x6
    t2: "t2",     // x7
    s0: "s0",     // x8
    s1: "s1",     // x9
    a0: "a0",     // x10
    a1: "a1",     // x11
    a2: "a2",     // x12
    a3: "a3",     // x13
    a4: "a4",     // x14
    a5: "a5",     // x15
    a6: "a6",     // x16
    a7: "a7",     // x17
    s2: "s2",     // x18
    s3: "s3",     // x19
    s4: "s4",     // x20
    s5: "s5",     // x21
    s6: "s6",     // x22
    s7: "s7",     // x23
    s8: "s8",     // x24
    s9: "s9",     // x25
    s10: "s10",   // x26
    s11: "s11",   // x27
    t3: "t3",     // x28
    t4: "t4",     // x29
    t5: "t5",     // x30
    t6: "t6",     // x31
    pc: "pc",     // x32
};

//
//
//
// *********************************************************************************************************************
// *********************************************** TARGET REPRESENTATION ***********************************************
// *********************************************************************************************************************

// target 3AC quads
common_enum! { pub enum TQuad {
    Reg(TRegOp, Temp, Temp, Temp),
    Imm(TImmOp, Temp, Temp, Imm),
    Mem(TMemOp, Temp, Mem, Mem),
}}

common_enum! { pub enum TRegOp {
    Add, Sub, // arithmetic
    And, Or, Xor, // logicals
    Beq, Bneq, Bge, Blt, Jal, // control
}}

common_enum! { pub enum TImmOp {
    AddI, SubI, // arithmetic
    AndI, OrI, XorI, // logical
}}

common_enum! { pub enum TMemOp {
    Load, Store, // bindings
}}
