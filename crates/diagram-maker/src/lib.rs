pub mod diagram;
pub mod graphics;
pub mod layout;

mod from_expr;
mod render;
mod themes;

mod wll_api;

pub use crate::diagram::Diagram;

#[derive(Debug, Clone)]
pub struct Error(#[allow(dead_code)] String);

impl From<std::io::Error> for Error {
    fn from(err: std::io::Error) -> Error {
        Error(format!("IO error: {}", err))
    }
}
