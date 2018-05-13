//! A fast JavaScript parser. Currently only a [lexical analyzer](lex/index.html) and a [skipper](skip/index.html).

#![cfg_attr(test, feature(test))]

#![warn(missing_docs)]
#![doc(html_root_url = "https://docs.rs/esparse/0.0.4")]

#[macro_use]
extern crate matches;
extern crate memchr;

#[macro_use]
pub mod lex;
pub mod skip;
pub mod ast;

pub use ast::{Loc, Span};

#[doc(hidden)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParseOptions<'a> {
    pub first_line: usize,
    pub file_name: &'a str,
}

impl<'a> Default for ParseOptions<'a> {
    fn default() -> Self {
        ParseOptions {
            first_line: 0,
            file_name: "<input>",
        }
    }
}

#[doc(hidden)]
pub fn parse_script(_input: &str, _options: ParseOptions) -> ! {
    unimplemented!()
}
#[doc(hidden)]
pub fn parse_module(_input: &str, _options: ParseOptions) -> ! {
    unimplemented!()
}
#[doc(hidden)]
pub fn parse_expr(_input: &str, _options: ParseOptions) -> ! {
    unimplemented!()
}

#[cfg(test)]
mod tests {
}
