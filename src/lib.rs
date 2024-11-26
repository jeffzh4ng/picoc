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

// picoc's source representation is a forest of ASTS
// since variable and function are not values

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
// *********************************4************************************************************************************
// ******************************************** INTERMEDIATE REPRESENTATION ********************************************
// *********************************************************************************************************************

// IAST not too different from SAST since C was designed as
// portable assembly. the semantics do change however, with
// less structure that a high level language gives like C gives you

// - arithmetic: remains the same
// - control/functions: -> jump w/ labels
// - bindings: -> loads/stores w/ unlimited temps

type IPrg = Vec<IStmt>;
common_enum! {
    pub enum IStmt {
        Jump(Label), CJump(SExpr, Label, Label), LabelDef(Label), // control
        Compute(Temp, IExpr), Load(Temp, RiscvUtilReg), Store(RiscvUtilReg, Temp), // bindings
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

// picoc lowers the representation by flattening
// intermediate ASTs -> target 3AC quads.

// referencing data requires temps, and referencing
// code requires labels, since order is no longer
// explicitly encoded (trees vs linear). references
// are still symbolic, and are patched with virtual
// addresses with as and ld.

type Imm = i32;
common_enum! { pub enum Temp { UserTemp(String), MachineTemp(usize), UtilReg(RiscvUtilReg) } } // only util regs in abstract assembly
common_enum! { pub enum Label { UserLabel(String), MachineLabel(usize) } }

static mut TEMP_COUNTER: usize = 0;
static mut LABEL_COUNTER: usize = 0;

pub fn fresh_temp() -> Temp {
    unsafe {
        let temp = TEMP_COUNTER;
        TEMP_COUNTER += 1;
        Temp::MachineTemp(temp)
    }
}

pub fn fresh_label() -> Label {
    unsafe {
        let label = LABEL_COUNTER;
        LABEL_COUNTER += 1;
        Label::MachineLabel(label)
    }
}

common_enum! { pub enum RiscvUtilReg { Z, Ra, Sp, Gp, Tp, Fp, Pc } }
impl From<RiscvUtilReg> for RscvReg {
    fn from(ptr: RiscvUtilReg) -> Self {
        match ptr {
            RiscvUtilReg::Z => RscvReg::Z,
            RiscvUtilReg::Ra => RscvReg::Ra,
            RiscvUtilReg::Sp => RscvReg::Sp,
            RiscvUtilReg::Gp => RscvReg::Gp,
            RiscvUtilReg::Tp => RscvReg::Tp,
            RiscvUtilReg::Fp => RscvReg::S0,
            RiscvUtilReg::Pc => RscvReg::Pc,
        }
    }
}

//
//
//
// *********************************************************************************************************************
// *********************************************** TARGET REPRESENTATION ***********************************************
// *********************************************************************************************************************

// target 3AC quads
common_enum! {
    pub enum TQuad {
        Reg(TRegOp, Temp, Temp, Temp),
        Imm(TImmOp, Temp, Temp, Imm),
        Mem(TMemOp, Temp, usize, RiscvUtilReg),
    }
}

common_enum! { pub enum TRegOp { Add, Sub, And, Or, Xor, Beq, Bneq, Bge, Blt, Jal } }
common_enum! { pub enum TImmOp { AddI, SubI, AndI, OrI, XorI } }
common_enum! { pub enum TMemOp { Load, Store } }

common_enum! {
    pub enum RscvReg {
        Z, Ra, Sp, Gp, Tp, // pointers
        T0, T1, T2, // temporaries
        S0, S1, // saved registers
        A0, A1, A2, A3, A4, A5, A6, A7, // argument registers
        S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, // more saved registers
        T3, T4, T5, T6, // more temporaries
        Pc, // program counter
    }
}
