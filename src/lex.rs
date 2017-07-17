#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Loc {
    pub pos: usize,
    pub row: usize,
    pub col: usize,
}

#[derive(Debug)]
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
