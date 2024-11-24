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

// TODO: for loops, etc..
type SugaredPrg = Vec<()>;

// ***** prg: Vec<Defs> *****
type SPrg = Vec<SDef>;
common_enum! { pub enum SDef { FuncDef(SFuncDef), VarDef(SVarDef) } }
common_struct! { pub struct SFuncDef {pub alias: String,  pub typ: Type, pub fp: Vec<(String, Type)>, pub body: Vec<SStmt> } } // fp needs Type for statics, and String for dynamics
common_struct! { pub struct SVarDef { pub alias: String, pub typ: Type, pub expr: Box<SExpr> }} // UpdateBind { alias: String, op: BinOp, expr: Box<Expr> }

// ***** static tnv: Map<Alias, Type> *****
common_struct! { pub struct Tnv { fnv: HashMap<String, LambdaType>, vnv: HashMap<String, Type> }}
common_struct! { pub struct LambdaType { fp: Vec<Type>, body: Type } }
common_enum! { pub enum Type { Int, Bool, Void } } // Cond(Type::Bool, Box<Type>, Box<Type>),

// ***** dynamic vnv: Map<Alias, Val> *****
common_struct! { pub struct Vnv { fnv: HashMap<String, LambdaVal>, vnv: HashMap<String, i32> }} // todo, -> Val
common_struct! { pub struct LambdaVal { pub fp: Vec<String>, pub body: Vec<SStmt>} } // fp's only need types (tags) if implementing safety dynamically
common_enum! { pub enum Val { Int(i32), Bool(bool), Str(String) } }

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
        VarApp(String), FuncApp{ alias: String, ap: Vec<SExpr> }
    }
}

common_enum! { pub enum SLogOp { And, Or } }
common_enum! { pub enum SBitOp { And, Or, Xor } }
common_enum! { pub enum SRelOp { Eq, Neq, And, Or, LtEq, Lt, GtEq, Gt } }
common_enum! { pub enum SBinOp { Add, Sub, Mult, Div, Mod } }
common_enum! { pub enum SUnaryOp { Add, Sub } }

// intermediate AST is not too different from source AST,
// since C was designed as portable assembly

// the semantics are closer to metal:
// - arithmetic: remains more or less the same
// - control: conditionals and loops -> jump w/ labels
// - bindings: vardef and varapp -> loads/stores w/ unlimited temps
// - function: ??

type IPrg = Vec<IStmt>;
common_enum! {
    pub enum IStmt {
        Jump(String), CJump(SExpr, String, String), LabelDef(String), // control
        Load, Store, // bindings
        Seq(Vec<Box<IStmt>>), Return(IExpr), // functions
    }
}

common_enum! {
    pub enum IExpr {
        Const(i32), BinOp(IBinOp, Box<IExpr>, Box<IExpr>), // arithmetic
        TempUse(String), MemUse(String), LabelUse(String), // mem use if target is riscv?
        Call, // functions
    }
}

common_enum! { pub enum IBinOp { Add, Sub, Mult, Div, Mod } }
common_enum! { pub enum IBitOp { And, Or, Xor } }
common_enum! { pub enum IRelOp { Eq, Neq, And, Or, LtEq, Lt, GtEq, Gt } }

// target quads (3AC)
common_enum! { pub enum TQuad {
    RegQuad(TRegOp, Temp, Temp, Temp),
    ImmQuad(TImmOp, Temp, Temp, Imm),
    MemQuad(TMemOp, Temp, Mem, Mem),
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

type Temp = String;
type Mem = String;
type Label = String;
type Imm = i32;

// target register abi
pub const RISCV_ABI: &[(&str, &str)] = &[
    // zero register
    ("x0", "zero"),
    // return address
    ("x1", "ra"),
    // stack/frame pointers
    ("x2", "sp"),
    ("x3", "gp"),
    ("x4", "tp"),
    // temporaries
    ("x5", "t0"),
    ("x6", "t1"),
    ("x7", "t2"),
    // saved registers
    ("x8", "s0"),
    ("x9", "s1"),
    // arguments/return values
    ("x10", "a0"),
    ("x11", "a1"),
    ("x12", "a2"),
    ("x13", "a3"),
    ("x14", "a4"),
    ("x15", "a5"),
    ("x16", "a6"),
    ("x17", "a7"),
    // more saved registers
    ("x18", "s2"),
    ("x19", "s3"),
    ("x20", "s4"),
    ("x21", "s5"),
    ("x22", "s6"),
    ("x23", "s7"),
    ("x24", "s8"),
    ("x25", "s9"),
    ("x26", "s10"),
    ("x27", "s11"),
    // more temporaries
    ("x28", "t3"),
    ("x29", "t4"),
    ("x30", "t5"),
    ("x31", "t6"),
    // program counter
    ("x32", "pc"),
];
