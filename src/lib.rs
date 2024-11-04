use serde::{Deserialize, Serialize};
use std::collections::HashMap;

// pub mod evaluator;
// pub mod generator;
pub mod lexer;
pub mod parser;
// pub mod typer;

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

// ***** nv: Map<Alias, Val> *****
common_struct! { pub struct Nv { fnv: HashMap<String, Lambda>, vnv: HashMap<String, Val>, body: Lambda }}
common_struct! { pub struct Lambda { pub formal_param: String, pub body: Expr } } // todo: formal_param: Vec<String>, body: Vec<Stmt> + Expr. desugar?
common_enum! { pub enum Val { Int(i32), Bool(bool) } }

// ***** prg: Vec<Defs> *****
type Prg = Vec<Defs>;
common_enum! { pub enum Defs { FuncDef(FuncDef), VarDef(VarDef) } }
common_struct! { pub struct FuncDef { pub alias: String, pub formal_param: String, pub body: Vec<Stmt> } }
common_struct! { pub struct VarDef { alias: String, expr: Box<Expr> }} // UpdateBind { alias: String, op: BinOp, expr: Box<Expr> }

common_enum! {
    #[rustfmt::skip]
    pub enum Expr {
        // ***** introductions (values) h*****
        Int(i32),

        // ***** eliminations (operators) *****
        UnaryE { op: UnaryOp, l: Box<Expr> }, BinE { op: BinOp, l: Box<Expr>, r: Box<Expr> }, LogE { op: LogOp, l: Box<Expr>, r: Box<Expr> },
        BitE { op: BitOp, l: Box<Expr>, r: Box<Expr> }, RelE { op: RelOp, l: Box<Expr>, r: Box<Expr> },
        Alias(String), FuncApp{ alias: String, actual_param: Option<Box<Expr>> }
    }
}

common_enum! {
    pub enum Stmt {
        Asnmt(VarDef),
        Return(Expr), // hm...
        While, If, IfEls { cond: Box<Expr>, then: Box<Stmt>, els: Box<Stmt> }
    }
}

common_enum! { pub enum LogOp { And, Or } }
common_enum! { pub enum BitOp { And, Or, Xor } }
common_enum! { pub enum RelOp { Eq, Neq, And, Or, LtEq, Lt, GtEq, Gt } }
common_enum! { pub enum BinOp { Add, Sub, Mult, Div, Mod } }
common_enum! { pub enum UnaryOp { Add, Sub } }
