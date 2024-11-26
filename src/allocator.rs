use std::io;

use crate::{IPrg, OptLevel, TQuad};

pub fn allocate(abs_as: &[TQuad], opt: OptLevel) -> Result<IPrg, io::Error> {
    match opt {
        OptLevel::O0 => allocate_1ac(abs_as),
        OptLevel::O1 => todo!(),
        OptLevel::O2 => todo!(),
    }
}

fn allocate_1ac(abs_as: &[TQuad]) -> Result<IPrg, io::Error> {
    todo!()
}
