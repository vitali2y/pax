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

pub fn parse_script(input: &str, options: ParseOptions) -> ! {
    unimplemented!()
}
pub fn parse_module(input: &str, options: ParseOptions) -> ! {
    unimplemented!()
}
pub fn parse_expr(input: &str, options: ParseOptions) -> ! {
    unimplemented!()
}

#[cfg(test)]
mod tests {
}
