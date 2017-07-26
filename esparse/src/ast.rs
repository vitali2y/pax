#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Loc {
    pub pos: usize,
    pub row: usize,
    pub col: usize,
}

impl Loc {
    #[inline]
    pub fn new(pos: usize, row: usize, col: usize) -> Self {
        Loc {
            pos,
            row,
            col,
        }
    }

    #[inline]
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
    #[inline]
    pub fn new(file_name: &'f str, start: Loc, end: Loc) -> Self {
        Span {
            file_name,
            start,
            end,
        }
    }

    #[inline]
    pub fn empty(file_name: &'f str, loc: Loc) -> Self {
        Span::new(file_name, loc, loc)
    }

    #[inline]
    pub fn zero(file_name: &'f str) -> Self {
        Span::new(file_name, Default::default(), Default::default())
    }
}
