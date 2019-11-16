use std::fs::read_to_string;
use std::io::{Error,ErrorKind};
use crate::parser::parse_program;

fn limit(x: &str, n: usize) -> &str {
    &x[..x.len().min(n)]
}

fn to_io_error(e: nom::Err<(&str,nom::error::ErrorKind)>) -> Error {
    let string = match e {
        nom::Err::Incomplete(needed) => format!("incomplete {:?}", needed),
        nom::Err::Error((s,k)) |
        nom::Err::Failure((s,k)) => format!("error {:?} at {}", k, limit(s,100))
    };
    Error::new(ErrorKind::InvalidData,string)
}

pub fn compile(path: &str) -> Result<(),Error> {
    let text = read_to_string(path)?;
    let program = parse_program(&text).map_err(to_io_error)?;
    println!("{:?}", program);
    Ok(())
}
