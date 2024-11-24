use serde::{Deserialize, Serialize};
use std::collections::HashMap;

// pub mod evaluator;
pub mod allocator;
pub mod lexer;
pub mod translator;
pub mod parser;
pub mod selector;
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
type Prg = Vec<Def>;
common_enum! { pub enum Def { FuncDef(FuncDef), VarDef(VarDef) } }
common_struct! { pub struct FuncDef {pub alias: String,  pub typ: Type, pub fp: Vec<(String, Type)>, pub body: Vec<Stmt> } } // fp needs Type for statics, and String for dynamics
common_struct! { pub struct VarDef { pub alias: String, pub typ: Type, pub expr: Box<Expr> }} // UpdateBind { alias: String, op: BinOp, expr: Box<Expr> }

// ***** static tnv: Map<Alias, Type> *****
common_struct! { pub struct Tnv { fnv: HashMap<String, LambdaType>, vnv: HashMap<String, Type> }}
common_struct! { pub struct LambdaType { fp: Vec<Type>, body: Type } }
common_enum! { pub enum Type { Int, Bool, Void } } // Cond(Type::Bool, Box<Type>, Box<Type>),

// ***** dynamic vnv: Map<Alias, Val> *****
common_struct! { pub struct Vnv { fnv: HashMap<String, LambdaVal>, vnv: HashMap<String, i32> }} // todo, -> Val
common_struct! { pub struct LambdaVal { pub fp: Vec<String>, pub body: Vec<Stmt>} } // fp's only need types (tags) if implementing safety dynamically
common_enum! { pub enum Val { Int(i32), Bool(bool), Str(String) } }

common_enum! {
    pub enum Stmt {
        IfEls { cond: Box<Expr>, then: Box<Stmt>, els: Option<Box<Stmt>> }, While { cond: Box<Expr>, body: Box<Stmt> }, // control
        Asnmt(VarDef), Return(Expr), // bindings (intros in C)
    }
}

common_enum! {
    #[rustfmt::skip]
    pub enum Expr {
        // intros
        Int(i32), Bool(bool),

        // elims
        UnaryE { op: UnaryOp, l: Box<Expr> }, BinE { op: BinOp, l: Box<Expr>, r: Box<Expr> }, LogE { op: LogOp, l: Box<Expr>, r: Box<Expr> },
        BitE { op: BitOp, l: Box<Expr>, r: Box<Expr> }, RelE { op: RelOp, l: Box<Expr>, r: Box<Expr> },
        VarApp(String), FuncApp{ alias: String, ap: Vec<Expr> }
    }
}

common_enum! { pub enum LogOp { And, Or } }
common_enum! { pub enum BitOp { And, Or, Xor } }
common_enum! { pub enum RelOp { Eq, Neq, And, Or, LtEq, Lt, GtEq, Gt } }
common_enum! { pub enum BinOp { Add, Sub, Mult, Div, Mod } }
common_enum! { pub enum UnaryOp { Add, Sub } }

// trgt AST is not too different src AST,
// since C was designed as portable assembly

// the semantics are closer to metal:
// - arithmetic: remains more or less the same
// - control: conditionals and loops -> jump w/ labels
// - bindings: vardef and varapp -> loads/stores w/ unlimited temps
// - function: ??

type TrgtPrg = Vec<TrgtStmt>;
common_enum! {
    pub enum TrgtStmt {
        Jump(String), CJump(Expr, String, String), LabelDef(String), // control
        Load, Store, // bindings
        Seq(Vec<Box<TrgtStmt>>), Return(TrgtExpr), // functions
    }
}

common_enum! {
    pub enum TrgtExpr {
        Const(i32), BinOp(TrgtBinOp, Box<TrgtExpr>, Box<TrgtExpr>), // arithmetic
        TempUse(String), MemUse(String), LabelUse(String), // mem use if target is riscv?
        Call, // functions
    }
}

common_enum! { pub enum TrgtBinOp { Add, Sub, Mult, Div, Mod } }
common_enum! { pub enum TrgtBitOp { And, Or, Xor } }
common_enum! { pub enum TrgtRelOp { Eq, Neq, And, Or, LtEq, Lt, GtEq, Gt } }
