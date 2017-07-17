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
pub struct Span<'f> {
    pub file_name: &'f str,
    pub start: Loc,
    pub end: Loc,
}

impl<'f> Span<'f> {
    pub fn new(file_name: &'f str, start: Loc, end: Loc) -> Self {
        Span {
            file_name,
            start,
            end,
        }
    }

    pub fn zero(file_name: &'f str) -> Self {
        Span::new(file_name, Default::default(), Default::default())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Tok<'f, 's> {
    pub tt: Tt<'s>,
    pub span: Span<'f>,
}

impl<'f, 's> Tok<'f, 's> {
    pub fn new(tt: Tt<'s>, span: Span<'f>) -> Self {
        Tok {
            tt,
            span,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Tt<'s> {
    StrLitSingle(&'s str),
    StrLitDouble(&'s str),
    NumLit(&'s str),
    // ...

    Eof,
}

#[derive(Debug)]
pub struct Lexer<'f, 's> {
    file_name: &'f str,
    stream: Stream<'s>,
    here: Tok<'f, 's>,
}

#[derive(Debug)]
pub struct Stream<'s> {
    input: &'s str,
    loc: Loc,
}

impl<'f, 's> Lexer<'f, 's> {
    pub fn new(file_name: &'f str, input: &'s str) -> Self {
        Lexer {
            file_name,
            stream: Stream::new(input),
            here: Tok::new(Tt::Eof, Span::zero(file_name)),
        }
    }
}

impl<'s> Stream<'s> {
    pub fn new(input: &'s str) -> Self {
        Stream {
            input,
            loc: Default::default(),
        }
    }
}
