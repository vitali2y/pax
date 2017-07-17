#![feature(test)]
#![allow(unused_features)]

pub mod lex;

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

pub fn parse_script(_input: &str, _options: ParseOptions) -> ! {
    unimplemented!()
}
pub fn parse_module(_input: &str, _options: ParseOptions) -> ! {
    unimplemented!()
}
pub fn parse_expr(_input: &str, _options: ParseOptions) -> ! {
    unimplemented!()
}

#[cfg(test)]
mod tests {
}
