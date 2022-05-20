pub mod diagram;
pub mod layout;

mod from_expr;
mod render;
mod themes;

pub use crate::diagram::Diagram;

#[derive(Debug, Clone)]
pub struct Error(String);

impl From<std::io::Error> for Error {
    fn from(err: std::io::Error) -> Error {
        Error(format!("IO error: {}", err))
    }
}
