use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt::{self, Debug};
use std::rc::Rc;

// pub mod evaluator;
pub mod allocator;
pub mod lexer;
pub mod parser;
pub mod parser_son;
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

common_enum! { pub enum OptLevel { O0, O1, O2 } }
impl TryFrom<u32> for OptLevel {
    type Error = &'static str;

    fn try_from(opt: u32) -> Result<Self, Self::Error> {
        match opt {
            0 => Ok(OptLevel::O0),
            1 => Ok(OptLevel::O1),
            2 => Ok(OptLevel::O2),
            _ => Err("picoc-error: invalid optimization level given"),
        }
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

// design 1: sum (polymorphic type) of products (data) with impl (behavior)
// ------------------------------------------------------------------------
// enum Node { FooNode({ inputs: Vec<Rc<Node>>, outputs: Vec<Rc<Node>> })}
// impl Node { fn inputs(&self) -> &Vec<Rc<Self>> { match self { FooNode(foo_node) => &foo_node.inputs, } } }
// -> smells: requires lots of matching to reveal the same data and perform same behavior.
//            loc = shared behavior (600loc) * variants (45) = 27000loc (non-monomorphized)
//            types and data are decoupled

// design 2. sum of products with different coarse-graining
// ------------------------------------------------------------------------
// heterogeneity with Op product type (like typescript's kind pattern), data reuse with Node sum type

// struct Node { op: Op, inputs: Vec<Rc<Node>>, outputs: Vec<Rc<Node>> }
// enum Op { StartNode, ReturnNode { ctrl: Option<Rc<Node>>, expr: Option<Rc<Node>> }, ConstantNode { value: i32 } }
// -> you can't match on Op because you need data on Node
// impl Node { fn foo(&self) -> () {
//   match self.op { Op::StartNode => todo!(), Op::ReturnNode { ctrl, expr } => todo!(), Op::ConstantNode { value } => todo!(), }
// }}
// -> smells: match on op to coarse-grain data.
//            data-specific behavior implemented with either
//            1. data duplication or (return node needs to store ctrl and expr, just to wrap inp[0] and inp[1])
//            2. methods with sparse variant behavior on match (ctrl() and expr() method only applicable to return variant)

// design 3: generics (polymorphic type) with trait bounds (behavior)
// ------------------------------------------------------------------------
// trait Node { fn inputs(&self) -> Vec<Rc<dyn Node>> { todo!() }}
// struct StartNode<N: Node> { inputs: Vec<Rc<N>> }
// impl<N: Node> Node for StartNode<N> { fn inputs(&self) -> Vec<Rc<dyn Node>> { todo!()  }}
// fn foo<T: Node>(n: T) -> () { } -> smells: the level of resolution on n is too high. no way to match on n for data-specific behavior.
//                                            generics with trait bounds is ok for homogenous data, not heterogeneous.

// ==============================================
// GRAPH
// ==============================================
// design 4: trait objects: heterogeneous data
// trait objects actually couple data and behavior,
// but only the behavior is inherited in rust.
// not as useful as trad oop

// FIXME: no static mut
static mut ID: i128 = 0;

pub fn fresh_id() -> i128 {
    unsafe {
        ID += 1;
        ID
    }
}

pub trait Node {
    fn inputs(&self) -> &[Rc<dyn Node>];
    fn print(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result;
}

impl Debug for dyn Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.print(f)
    }
}

pub struct StartNode {
    id: i128,
    inputs: Vec<Rc<dyn Node>>,
    outputs: Vec<Rc<dyn Node>>,
}

impl Node for StartNode {
    fn inputs(&self) -> &[Rc<dyn Node>] {
        &self.inputs
    }

    fn print(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "StartNode")
    }
}

impl StartNode {
    pub fn new() -> Self {
        Self {
            id: fresh_id(),
            inputs: vec![],
            outputs: vec![],
        }
    }
}

pub struct ReturnNode {
    id: i128,
    inputs: Vec<Rc<dyn Node>>,
    outputs: Vec<Rc<dyn Node>>,
}

impl Node for ReturnNode {
    fn inputs(&self) -> &[Rc<dyn Node>] {
        &self.inputs
    }

    fn print(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.inputs.iter().for_each(|n| n.print(f).unwrap()); // FIXME; no unwrap
        writeln!(f, "ReturnNode")
    }
}

impl ReturnNode {
    // todo: type ctrlnode and datanode?
    pub fn new(ctrl: Rc<dyn Node>, expr: Rc<dyn Node>) -> Self {
        Self {
            id: fresh_id(),
            inputs: vec![ctrl, expr],
            outputs: vec![],
        }
    }

    fn ctrl(&self) -> Rc<dyn Node> {
        self.inputs.get(0).cloned().unwrap() // todo: change vec to array?
    }

    fn expr(&self) -> Rc<dyn Node> {
        self.inputs.get(1).cloned().unwrap() // todo: change vec to array?
    }
}

pub struct ConstantNode {
    id: i128,
    value: i32,
    inputs: Vec<Rc<dyn Node>>,
    outputs: Vec<Rc<dyn Node>>,
}

impl Node for ConstantNode {
    fn inputs(&self) -> &[Rc<dyn Node>] {
        &self.inputs
    }

    fn print(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "ConstantNode({})", self.value)
    }
}

impl ConstantNode {
    pub fn new(start: Rc<dyn Node>, value: i32) -> Self {
        Self {
            id: fresh_id(),
            value,
            inputs: vec![start], // edge is not semantic. needed to enable graph walk.
            outputs: vec![],
        }
    }
}

// TODO: for loops, etc.
type SugaredPrg = Vec<()>;

// ==============================================
// TREE
// ==============================================
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
// *********************************4***********************************************************************************
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
        Jump(Label), CJump(SExpr, Label, Label), // control
        Compute(Temp, IExpr), Load(Temp, RiscvPointerReg), Store(RiscvPointerReg, Temp), // bindings
        Seq(Label, Vec<Box<IStmt>>), Return(IExpr), // functions
        // todo: maybe rename seq to func if not used for conditionals
    }
}

// do i need
// - compute?
// - seq if i have return?

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
common_enum! { pub enum Temp { UserTemp(String), MachineTemp(usize), PointerReg(RiscvPointerReg) } } // only util regs in abstract assembly
common_enum! { pub enum Label { UserLabel(String), MachineLabel(usize) } }
impl ToString for Label {
    fn to_string(&self) -> String {
        match self {
            Label::UserLabel(l) => l.clone(),
            Label::MachineLabel(l) => todo!(),
        }
    }
}

// FIXME: no static mut
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

common_enum! { pub enum RiscvPointerReg { Z, Ra, Sp, Gp, Tp, Fp, A0, A1, A2, A3, A4, A5, A6, A7, Pc } }
impl From<RiscvPointerReg> for RscvReg {
    fn from(ptr: RiscvPointerReg) -> Self {
        match ptr {
            RiscvPointerReg::Z => RscvReg::Z,
            RiscvPointerReg::Ra => RscvReg::Ra,
            RiscvPointerReg::Sp => RscvReg::Sp,
            RiscvPointerReg::Gp => RscvReg::Gp,
            RiscvPointerReg::Tp => RscvReg::Tp,
            RiscvPointerReg::Fp => RscvReg::S0,
            RiscvPointerReg::A0 => RscvReg::A0,
            RiscvPointerReg::A1 => RscvReg::A1,
            RiscvPointerReg::A2 => RscvReg::A2,
            RiscvPointerReg::A3 => RscvReg::A3,
            RiscvPointerReg::A4 => RscvReg::A4,
            RiscvPointerReg::A5 => RscvReg::A5,
            RiscvPointerReg::A6 => RscvReg::A6,
            RiscvPointerReg::A7 => RscvReg::A7,
            RiscvPointerReg::Pc => RscvReg::Pc,
        }
    }
}

impl ToString for RiscvPointerReg {
    fn to_string(&self) -> String {
        match self {
            RiscvPointerReg::Z => "zero".to_string(),
            RiscvPointerReg::Ra => "ra".to_string(),
            RiscvPointerReg::Sp => "sp".to_string(),
            RiscvPointerReg::Gp => "gp".to_string(),
            RiscvPointerReg::Tp => "tp".to_string(),
            RiscvPointerReg::Fp => "fp".to_string(),
            RiscvPointerReg::A0 => "a0".to_string(),
            RiscvPointerReg::A1 => "a1".to_string(),
            RiscvPointerReg::A2 => "a2".to_string(),
            RiscvPointerReg::A3 => "a3".to_string(),
            RiscvPointerReg::A4 => "a4".to_string(),
            RiscvPointerReg::A5 => "a5".to_string(),
            RiscvPointerReg::A6 => "a6".to_string(),
            RiscvPointerReg::A7 => "a7".to_string(),
            RiscvPointerReg::Pc => "pc".to_string(),
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
        Mem(TMemOp, Temp, usize, RiscvPointerReg),
        Pseudo(PseudoOp),
        Label(Label),
    }
}

common_enum! { pub enum TRegOp { Add, Sub, And, Or, Xor, Beq, Bneq, Bge, Blt, Jal } }
common_enum! { pub enum TImmOp { AddI, SubI, AndI, OrI, XorI } }
common_enum! { pub enum TMemOp { Load, Store } }
common_enum! { pub enum PseudoOp { Call(Label), Ret } }

impl ToString for TRegOp {
    fn to_string(&self) -> String {
        match self {
            TRegOp::Add => "add".to_string(),
            TRegOp::Sub => "sub".to_string(),
            TRegOp::And => "and".to_string(),
            TRegOp::Or => "or".to_string(),
            TRegOp::Xor => "xor".to_string(),
            TRegOp::Beq => "beq".to_string(),
            TRegOp::Bneq => "bne".to_string(),
            TRegOp::Bge => "bge".to_string(),
            TRegOp::Blt => "blt".to_string(),
            TRegOp::Jal => "jal".to_string(),
        }
    }
}

impl ToString for TImmOp {
    fn to_string(&self) -> String {
        match self {
            TImmOp::AddI => "addi".to_string(),
            TImmOp::SubI => "subi".to_string(),
            TImmOp::AndI => "andi".to_string(),
            TImmOp::OrI => "ori".to_string(),
            TImmOp::XorI => "xori".to_string(),
        }
    }
}

impl ToString for TMemOp {
    fn to_string(&self) -> String {
        match self {
            TMemOp::Load => "lw".to_string(),
            TMemOp::Store => "sw".to_string(),
        }
    }
}

impl ToString for PseudoOp {
    fn to_string(&self) -> String {
        match self {
            PseudoOp::Call(l) => format!("call {}", l.to_string()),
            PseudoOp::Ret => "ret".to_string(),
        }
    }
}

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
