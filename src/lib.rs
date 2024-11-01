use serde::{Deserialize, Serialize};
use std::collections::HashMap;

pub mod evaluator;
pub mod generator;
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

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct Program(HashMap<String, Lambda>); // todo: we still need a separate map for the nv where the user can store bindings

// funcdef and lambdas are pulled out of Expr because in C, functions are not expressions.
// funcapply is still an Expr.
struct FuncDef {} // todo: can we desugar to Let? rust doesn't have let?
common_struct! { pub struct Lambda { pub formal_param: String, pub body: Expr } } // todo: 1. func is not an expr in c? check spec 2. body is a List<___> ?>);

common_enum! {
    pub enum Cmd {
        Let(Asnmt),
        Return(Expr), // hm...
        While, If, IfEls { cond: Box<Expr>, then: Box<Cmd>, els: Box<Cmd> }
    }
}

// right now, before we get into the month of october, values seem to correspond to types.
// C has int x = 0, bool y = true, but NOT func my_adder = adder
// once you extend values from i32 -> bool -> FuncVal, you need a value type for your interprer to type.

// the only way to introduce functions in c is at the top level.
// in c, functions are not expressions!!

common_enum! {
    #[rustfmt::skip]
    pub enum Expr {
        // ***** introductions (values) h*****
        Int(i32),

        // ***** eliminations (operators) *****
        UnaryE { op: UnaryOp, l: Box<Expr> }, BinE { op: BinOp, l: Box<Expr>, r: Box<Expr> }, LogE { op: LogOp, l: Box<Expr>, r: Box<Expr> },
        BitE { op: BitOp, l: Box<Expr>, r: Box<Expr> }, RelE { op: RelOp, l: Box<Expr>, r: Box<Expr> },
        Var(String), FuncApply{ alias: String, actual_param: Box<Expr> }
        // design decision: are functions values? alias: String, or value: Expr
        // in C, functions are not values, so we have to alias them.

        // eval:
        // match ....
        //    funcapp => (let [fv (interp f nv)] [av (interp a nv)])
        //           match fv.. if not funcV, error.


        // funcExp (lambdaExp) is just FunVal(...) see: L42
        // todo shriram!!!!: funcdef is (let1 x funExp)

        // if Func: Expr, does it make sense to have a conditional in the function selection position??????????
        // it actually does...functions as values, then it can be wrapped as conditionals...
    }
}

common_enum! { pub enum Asnmt { CreateBind { id: String, expr: Box<Expr> }, UpdateBind { id: String, op: BinOp, expr: Box<Expr> }} }
common_enum! { pub enum LogOp { And, Or } }
common_enum! { pub enum BitOp { And, Or, Xor } }
common_enum! { pub enum RelOp { Eq, Neq, And, Or, LtEq, Lt, GtEq, Gt } }
common_enum! { pub enum BinOp { Add, Sub, Mult, Div, Mod } }
common_enum! { pub enum UnaryOp { Add, Sub } }
