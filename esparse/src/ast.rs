//! Syntactic constructs and related data structures.

/// A location in source code.
///
/// Stores both the bytewise [position](#structfield.pos) and the logical [line](#structfield.row) and [character](#structfield.col) numbers.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Loc {
    /// 0-based byte index.
    pub pos: usize,
    /// 0-based line number.
    pub row: usize,
    /// 0-based character number on the line.
    pub col: usize,
}

impl Loc {
    /// Creates a new `Loc` with the given positions.
    #[inline]
    pub fn new(pos: usize, row: usize, col: usize) -> Self {
        Loc {
            pos,
            row,
            col,
        }
    }

    /// Creates a new `Loc` pointing to the first byte of the source code (`pos`, `row`, and `col` all zero).
    #[inline]
    pub fn zero() -> Self {
        Default::default()
    }
}

/// A region of source code.
///
/// A pair of locations, representing a half-open range, and a file name, identifying the source code in which this region appears.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span<'f> {
    /// The name of the source code.
    ///
    /// Often a file name, but can be an arbitrary string like `<input>`.
    pub file_name: &'f str,
    /// The (inclusive) starting location.
    pub start: Loc,
    /// The (exclusive) ending location.
    pub end: Loc,
}

impl<'f> Span<'f> {
    /// Creates a new `Span` with the given file name and locations.
    #[inline]
    pub fn new(file_name: &'f str, start: Loc, end: Loc) -> Self {
        Span {
            file_name,
            start,
            end,
        }
    }

    /// Creates an empty `Span` at the given location, with the given file name.
    #[inline]
    pub fn empty(file_name: &'f str, loc: Loc) -> Self {
        Span::new(file_name, loc, loc)
    }

    /// Creates an empty `Span` with the given file name, pointing to the first position in the file.
    #[inline]
    pub fn zero(file_name: &'f str) -> Self {
        Span::new(file_name, Default::default(), Default::default())
    }
}
