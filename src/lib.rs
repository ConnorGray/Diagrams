mod diagram;
mod render;


pub use crate::diagram::{Diagram, Arrow, Label, Text};


#[derive(Debug, Clone)]
pub struct Error(String);

impl From<std::io::Error> for Error {
    fn from(err: std::io::Error) -> Error {
        Error(format!("IO error: {}", err))
    }
}