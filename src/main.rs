mod ast;
mod compile;
mod parser;

use crate::compile::compile;

fn main() {
    compile("sr/prelude.sr").unwrap();
}
