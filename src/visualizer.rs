use crate::Node;
use std::rc::Rc;

pub fn gen_dot(start: Rc<dyn Node>) -> String {
    let mut dot = String::new();
    dot.push_str("/*\n");
    // dot.push(parser.src());
    dot.push_str("\n*/\n");
    dot.push_str("\trankdir=BT;\n"); // force nodes before scopes
    dot.push_str("\tordering=\"in\";\n"); // preserve node input order
    dot.push_str("\tconcentrate=\"true\";\n"); // merge multiple edges
    gen_nodes(&mut dot);
    gen_edges(&mut dot);
    dot.push_str("}\n");
    dot
}

fn gen_nodes(_d: &mut String) {}
fn gen_edges(_d: &mut String) {}

fn _graph_vertices(_start: Rc<dyn Node>) -> Vec<Rc<dyn Node>> {
    todo!()
}
