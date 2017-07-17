#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Loc {
    pub pos: usize,
    pub row: usize,
    pub col: usize,
}

impl Loc {
    pub fn new(pos: usize, row: usize, col: usize) -> Self {
        Loc {
            pos,
            row,
            col,
        }
    }

    pub fn zero() -> Self {
        Default::default()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span<'a> {
    pub file_name: &'a str,
    pub start: Loc,
    pub end: Loc,
}

impl<'a> Span<'a> {
    pub fn new(file_name: &'a str, start: Loc, end: Loc) -> Self {
        Span {
            file_name,
            start,
            end,
        }
    }

    pub fn zero(file_name: &'a str) -> Self {
        Span::new(file_name, Default::default(), Default::default())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Tok<'a, 'b> {
    pub tt: Tt<'a>,
    pub span: Span<'b>,
}

impl<'a, 'b> Tok<'a, 'b> {
    pub fn new(tt: Tt<'a>, span: Span<'b>) -> Self {
        Tok {
            tt,
            span,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Tt<'a> {
    StrLitSingle(&'a str),
    StrLitDouble(&'a str),
    NumLit(&'a str),
    // ...

    Eof,
}

#[derive(Debug)]
pub struct Lexer<'a, 'b> {
    file_name: &'a str,
    stream: Stream<'b>,
    here: Tok<'b, 'a>,
}

#[derive(Debug)]
pub struct Stream<'a> {
    input: &'a str,
    loc: Loc,
}

impl<'a, 'b> Lexer<'a, 'b> {
    pub fn new(file_name: &'a str, input: &'b str) -> Self {
        Lexer {
            file_name,
            stream: Stream::new(input),
            here: Tok::new(Tt::Eof, Span::zero(file_name)),
        }
    }
}

impl<'a> Stream<'a> {
    pub fn new(input: &'a str) -> Self {
        Stream {
            input,
            loc: Default::default(),
        }
    }
}
