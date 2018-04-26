//! Syntactic constructs and related data structures.
use std::fmt;
use std::rc::Rc;

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
pub struct SpanT<F, L> {
    /// The name of the source code.
    ///
    /// Often a file name, but can be an arbitrary string like `<input>` or even any other type.
    pub file_name: F,
    /// The (inclusive) starting location.
    pub start: L,
    /// The (exclusive) ending location.
    pub end: L,
}

// TODO link to parser
/// A `SpanT` with a borrowed string file name.
///
/// Used widely by the [lexer](../lex/index.html) and parser because it appears in every syntactic construct and is cheap to copy.
///
/// If the `SpanT` must own its filename, use [`SpanRc`](type.SpanRc.html) instead.
pub type Span<'f, L> = SpanT<&'f str, L>;

/// A `SpanT` with a reference-counted file name.
///
/// Useful for creating `SpanT`s which own their file name, but more expensive to clone than a regular [`Span`](type.Span.html).
pub type SpanRc<L> = SpanT<Rc<String>, L>;

impl<F, L> SpanT<F, L> {
    /// Creates a new `SpanT` with the given file name and locations.
    #[inline]
    pub fn new(file_name: F, start: L, end: L) -> Self {
        SpanT {
            file_name,
            start,
            end,
        }
    }
}
impl<F, L: Clone> SpanT<F, L> {
    /// Creates an empty `SpanT` at the given location, with the given file name.
    #[inline]
    pub fn empty(file_name: F, loc: L) -> Self {
        SpanT::new(file_name, loc.clone(), loc)
    }
}
impl<F, L: Default> SpanT<F, L> {
    /// Creates an empty `SpanT` with the given file name, pointing to the first position in the file.
    #[inline]
    pub fn zero(file_name: F) -> Self {
        SpanT::new(file_name, Default::default(), Default::default())
    }
}

impl<'f, L: Clone> Span<'f, L> {
    /// Converts a `Span` into a [`SpanRc`](type.SpanRc.html) by cloning the borrowed file name.
    pub fn with_rc(&self) -> SpanRc<L> {
        SpanT::new(Rc::new(self.file_name.to_owned()), self.start.clone(), self.end.clone())
    }

    /// Converts a `Span` into a [`SpanT`](struct.SpanT.html) which owns its data by cloning the borrowed file name.
    pub fn with_owned(&self) -> SpanT<String, L> {
        SpanT::new(self.file_name.to_owned(), self.start.clone(), self.end.clone())
    }
}

impl fmt::Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{},{}", self.row + 1, self.col + 1)
    }
}

impl<F: fmt::Display> fmt::Display for SpanT<F, Loc> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.start.row == self.end.row {
            if self.start.col == self.end.col {
                write!(
                    f,
                    "{}:{},{}",
                    self.file_name,
                    self.start.row + 1,
                    self.start.col + 1,
                )
            } else {
                write!(
                    f,
                    "{}:{},{}-{}",
                    self.file_name,
                    self.start.row + 1,
                    self.start.col + 1,
                    self.end.col + 1,
                )
            }
        } else {
            write!(
                f,
                "{}:{},{}-{},{}",
                self.file_name,
                self.start.row + 1,
                self.start.col + 1,
                self.end.row + 1,
                self.end.col + 1,
            )
        }
    }
}
