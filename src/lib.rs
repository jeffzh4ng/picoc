use serde::{Deserialize, Serialize};
use std::collections::HashMap;

// pub mod generator;
pub mod evaluator;
pub mod lexer;
pub mod parser;
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

// ***** prg: Vec<Defs> *****
type Prg = Vec<Defs>;
common_enum! { pub enum Defs { FuncDef(FuncDef), VarDef(VarDef) } }
common_struct! { pub struct FuncDef {pub alias: String,  pub typ: Type, pub formal_param: Vec<String>, pub body: Vec<Stmt> } }
common_struct! { pub struct VarDef { pub alias: String, pub typ: Type, pub expr: Box<Expr> }} // UpdateBind { alias: String, op: BinOp, expr: Box<Expr> }

// ***** static tnv: Map<Alias, Type> *****
common_struct! { pub struct Tnv { fnv: HashMap<String, LambdaType>, vnv: HashMap<String, Type> }}
common_struct! { pub struct LambdaType { fp: Vec<Type>, body: Type } }
common_enum! { pub enum Type { Int, Bool, Str } } // Cond(Type::Bool, Box<Type>, Box<Type>),

// ***** dynamic vnv: Map<Alias, Val> *****
common_struct! { pub struct Vnv { fnv: HashMap<String, LambdaVal>, vnv: HashMap<String, i32> }} // todo, -> Val
common_struct! { pub struct LambdaVal { pub fp: Vec<String>, pub body: Vec<Stmt>} }
common_enum! { pub enum Val { Int(i32), Bool(bool), Str(String) } }

common_enum! {
    #[rustfmt::skip]
    pub enum Expr {
        // ***** introductions (values) h*****
        Int(i32), Str(String),

        // ***** eliminations (operators) *****
        UnaryE { op: UnaryOp, l: Box<Expr> }, BinE { op: BinOp, l: Box<Expr>, r: Box<Expr> }, LogE { op: LogOp, l: Box<Expr>, r: Box<Expr> },
        BitE { op: BitOp, l: Box<Expr>, r: Box<Expr> }, RelE { op: RelOp, l: Box<Expr>, r: Box<Expr> },
        VarApp(String), FuncApp{ alias: String, ap: Vec<Expr> }
    }
}

common_enum! {
    pub enum Stmt {
        Asnmt(VarDef), Return(Expr), // bindings
        IfEls { cond: Box<Expr>, then: Box<Stmt>, els: Box<Stmt> }, While { cond: Box<Expr>, body: Box<Stmt> }, // control
    }
}

common_enum! { pub enum LogOp { And, Or } }
common_enum! { pub enum BitOp { And, Or, Xor } }
common_enum! { pub enum RelOp { Eq, Neq, And, Or, LtEq, Lt, GtEq, Gt } }
common_enum! { pub enum BinOp { Add, AddAdd, Sub, Mult, Div, Mod } }
common_enum! { pub enum UnaryOp { Add, Sub } }
