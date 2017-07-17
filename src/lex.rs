
pub struct Lexer<'a, 'b> {
    file_name: &'a str,
    input: &'b str,
}

impl<'a, 'b> Lexer<'a, 'b> {
    pub fn new(file_name: &'a str, input: &'b str) -> Self {
        Lexer {
            file_name,
            input,
        }
    }
}
