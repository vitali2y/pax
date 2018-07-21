//! Lexical analysis (tokenizing/lexing).
//!
//! The [`Lexer`](struct.Lexer.html) splits its input source code into a sequence of input elements called tokens, represented by the [`Tok`](struct.Tok.html) structure. It also removes whitespace and comments and attaches them to the next token.
//!
//! # Examples
//!
//! ```
//! use esparse::lex::Lexer;
//!
//! let mut lexer = Lexer::new_unnamed("1 + 2");
//!
//! for tok in lexer {
//!     println!("{} ", tok.tt);
//! }
//! ```

use std::{char, mem, fmt, error};
use std::borrow::Cow;
use memchr;
use unicode_xid::UnicodeXID;

use ast::{Span, SpanT, Loc};

/// A token (an atomic parsing unit).
///
/// Tokens have a [type](#structfield.tt) represented by the [`Tt`](enum.Tt.html) enumeration. They also have [location information](#structfield.span) and track the [whitespace and comments](#structfield.ws_before) that appeared before them in the source code.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Tok<'f, 's> {
    /// The token type.
    pub tt: Tt<'s>,
    /// The source region this token covers.
    pub span: Span<'f, usize>,
    /// Any whitespace and comments that appeared directly before this token.
    pub ws_before: &'s str,
    /// `true` if [`ws_before`](#structfield.ws_before) contains a line terminator.
    pub nl_before: bool,
}

impl<'f, 's> Tok<'f, 's> {
    /// Creates a new `Token` with no preceding whitespace.
    pub fn new(tt: Tt<'s>, span: Span<'f, usize>) -> Self {
        Tok {
            tt,
            span,
            ws_before: "",
            nl_before: false,
        }
    }
}

/// The content of a token.
///
/// Each token has a type. If the token's contents can vary, it also includes one or more string slices of the source code. The slices cover the full extent of the token and are direct slices of code, including delimiters, quotation marks, prefixes, escape sequences, etc.
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Tt<'s> {
    /// An identifier.
    Id(&'s str),

    /// A single-quoted string literal.
    ///
    /// The slice includes both quotation marks and escape sequences exactly as they appear in the source code. Use [str_lit_value](fn.str_lit_value.html) to extract the string value of the literal.
    StrLitSgl(&'s str),
    /// A double-quoted string literal.
    ///
    /// The slice includes both quotation marks and escape sequences exactly as they appear in the source code. Use [str_lit_value](fn.str_lit_value.html) to extract the string value of the literal.
    StrLitDbl(&'s str),

    /// A regular expression literal.
    ///
    /// The first slice contains the entire regular expression source (including both slashes and the flags), and the second contains just the flags.
    RegExpLit(&'s str, &'s str),

    /// A binary numeric literal.
    ///
    /// The slice includes the `0b` or `0B` prefix.
    NumLitBin(&'s str),
    /// An octal numeric literal.
    ///
    /// The slice includes the `0o` or `0O` prefix.
    NumLitOct(&'s str),
    /// A decimal numeric literal.
    NumLitDec(&'s str),
    /// A hexadecimal numeric literal.
    ///
    /// The slice includes the `0x` or `0X` prefix.
    NumLitHex(&'s str),

    /// A template without substitutions (e.g., <code>\`template\`</code>).
    ///
    /// The slice includes both backticks.
    TemplateNoSub(&'s str),
    /// The first literal part of a template with substitutions (e.g., <code>\`template${</code>).
    ///
    /// The slice includes the opening backtick and the `$[` sigil.
    TemplateStart(&'s str),
    /// A middle part of a template with substitutions (e.g., `}template${`).
    ///
    /// The slice includes the `}` and `$[` sigils.
    TemplateMiddle(&'s str),
    /// A middle part of a template with substitutions (e.g., <code>}template\`</code>).
    ///
    /// The slice includes the `}` sigil and the closing backtick.
    TemplateEnd(&'s str),

    // Punctuator ::
    Lbrace, Lparen, Rparen, Lbracket, Rbracket,
    Dot, DotDotDot, Semi, Comma,
    Lt, Gt, LtEq, GtEq,
    EqEq, BangEq, EqEqEq, BangEqEq,
    Plus, Minus, Star, Percent, StarStar,
    PlusPlus, MinusMinus,
    LtLt, GtGt, GtGtGt,
    And, Or, Circumflex,
    Bang, Tilde,
    AndAnd, OrOr,
    Question, Colon,
    Eq, PlusEq, MinusEq, StarEq, PercentEq, StarStarEq, LtLtEq, GtGtEq, GtGtGtEq, AndEq, OrEq, CircumflexEq,
    EqGt,

    // DivPunctuator ::
    Slash, SlashEq,

    // RightBracePunctuator ::
    Rbrace,

    // NullLiteral ::
    Null,

    // BooleanLiteral ::
    True,
    False,

    // Keyword ::
    Await,
    Break,
    Case, Catch, Class, Const, Continue,
    Debugger, Default, Delete, Do,
    Else, Export, Extends,
    Finally, For, Function,
    If, Import, In, Instanceof,
    New,
    Return,
    Super, Switch,
    This, Throw, Try, Typeof,
    Var, Void,
    While, With,
    Yield,

    // // FutureReservedWord ::
    // Enum,

    // TODO strict mode future reserved words

    /// The end-of-file token.
    ///
    /// This token is emitted endlessly after the lexical analyzer has reached the end of the source code.
    Eof,

    /// The error token.
    ///
    /// This token is emitted endlessly after the lexical analyzer reaches an error. You can retrieve specific error information with [`Lexer::error`](struct.Lexer.html#method.error) or [`Lexer::take_error`](struct.Lexer.html#method.take_error).
    Err,
}

impl<'s> Tt<'s> {
    /// The source code slice that this token covers.
    pub fn as_str(&self) -> &'s str {
        match *self {
            Tt::Id(s) |
            Tt::StrLitSgl(s) |
            Tt::StrLitDbl(s) |
            Tt::NumLitBin(s) |
            Tt::NumLitOct(s) |
            Tt::NumLitDec(s) |
            Tt::NumLitHex(s) |
            Tt::RegExpLit(s, _) |
            Tt::TemplateNoSub(s) |
            Tt::TemplateStart(s) |
            Tt::TemplateMiddle(s) |
            Tt::TemplateEnd(s) => s,

            Tt::Lbrace => "{",
            Tt::Lparen => "(",
            Tt::Rparen => ")",
            Tt::Lbracket => "[",
            Tt::Rbracket => "]",
            Tt::Dot => ".",
            Tt::DotDotDot => "...",
            Tt::Semi => ";",
            Tt::Comma => ",",
            Tt::Lt => "<",
            Tt::Gt => ">",
            Tt::LtEq => "<=",
            Tt::GtEq => ">=",
            Tt::EqEq => "==",
            Tt::BangEq => "!=",
            Tt::EqEqEq => "===",
            Tt::BangEqEq => "!==",
            Tt::Plus => "+",
            Tt::Minus => "-",
            Tt::Star => "*",
            Tt::Percent => "%",
            Tt::StarStar => "**",
            Tt::PlusPlus => "++",
            Tt::MinusMinus => "--",
            Tt::LtLt => "<<",
            Tt::GtGt => ">>",
            Tt::GtGtGt => ">>>",
            Tt::And => "&",
            Tt::Or => "|",
            Tt::Circumflex => "^",
            Tt::Bang => "!",
            Tt::Tilde => "~",
            Tt::AndAnd => "&&",
            Tt::OrOr => "||",
            Tt::Question => "?",
            Tt::Colon => ":",
            Tt::Eq => "=",
            Tt::PlusEq => "+=",
            Tt::MinusEq => "-=",
            Tt::StarEq => "*=",
            Tt::PercentEq => "%=",
            Tt::StarStarEq => "**=",
            Tt::LtLtEq => "<<=",
            Tt::GtGtEq => ">>=",
            Tt::GtGtGtEq => ">>>=",
            Tt::AndEq => "&=",
            Tt::OrEq => "|=",
            Tt::CircumflexEq => "^=",
            Tt::EqGt => "=>",
            Tt::Slash => "/",
            Tt::SlashEq => "/=",
            Tt::Rbrace => "}",

            Tt::Null => "null",
            Tt::True => "true",
            Tt::False => "false",
            Tt::Await => "await",
            Tt::Break => "break",
            Tt::Case => "case",
            Tt::Catch => "catch",
            Tt::Class => "class",
            Tt::Const => "const",
            Tt::Continue => "continue",
            Tt::Debugger => "debugger",
            Tt::Default => "default",
            Tt::Delete => "delete",
            Tt::Do => "do",
            Tt::Else => "else",
            Tt::Export => "export",
            Tt::Extends => "extends",
            Tt::Finally => "finally",
            Tt::For => "for",
            Tt::Function => "function",
            Tt::If => "if",
            Tt::Import => "import",
            Tt::In => "in",
            Tt::Instanceof => "instanceof",
            Tt::New => "new",
            Tt::Return => "return",
            Tt::Super => "super",
            Tt::Switch => "switch",
            Tt::This => "this",
            Tt::Throw => "throw",
            Tt::Try => "try",
            Tt::Typeof => "typeof",
            Tt::Var => "var",
            Tt::Void => "void",
            Tt::While => "while",
            Tt::With => "with",
            Tt::Yield => "yield",

            Tt::Eof => "",
            Tt::Err => "",
        }
    }
}

impl<'s> fmt::Display for Tt<'s> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

// TODO better errors
/// Parses a string literal and extracts its value, stripping the leading and trailing quotation marks and interpreting escape sequences.
///
/// If the slice given contains no escape sequences or line continuations, a subslice is return and nothing is allocated.
///
/// # Errors
///
/// Returns [`ParseStrLitError`](enum.ParseStrLitError.html) if the given source slice is syntactically invalid, i.e., contains an invalid escape sequence or line continuation.
///
/// # Examples
///
/// ```
/// use esparse::lex;
/// use std::borrow::Cow;
///
/// assert_eq!(lex::str_lit_value(r"'hello'"), Ok(Cow::Borrowed("hello")));
/// assert_eq!(lex::str_lit_value(r"'h\x65llo'"), Ok(Cow::Owned("hello".to_owned())));
/// ```
pub fn str_lit_value(source: &str) -> Result<Cow<str>, ParseStrLitError> {
    let mut result = String::new();
    let range = &source[1..source.len()-1]; // strip quotation marks
    let bytes = range.as_bytes();

    let len = range.len();
    let mut last_pos = 0;
    let mut got_bs = false;
    while let Some(bs_pos) = memchr::memchr(b'\\', &bytes[last_pos..]) {
        got_bs = true;
        result.push_str(&range[last_pos..bs_pos]);
        let esc_pos = bs_pos + 1;
        if esc_pos >= len {
            return Err(ParseStrLitError::ExpectedEscape)
        }
        last_pos = esc_pos + 1;
        match bytes[esc_pos] {
            b'0' => result.push_str("\0"),
            b'b' => result.push_str("\u{0008}"),
            b't' => result.push_str("\u{0009}"),
            b'n' => result.push_str("\u{000A}"),
            b'v' => result.push_str("\u{000B}"),
            b'f' => result.push_str("\u{000C}"),
            b'r' => result.push_str("\u{000D}"),

            b'x' => {
                let end_pos = last_pos + 2;
                if end_pos > len {
                    return Err(ParseStrLitError::ExpectedHex2)
                }

                let hex = &range[last_pos..end_pos];
                let code_point = u8::from_str_radix(hex, 16)
                .map_err(|_| ParseStrLitError::NotHex2)?;
                result.push(code_point as char);

                last_pos = end_pos;
            }
            b'u' => {
                match bytes.get(last_pos) {
                    Some(&b'{') => {
                        let l_pos = last_pos + 1;
                        let r_pos = memchr::memchr(b'}', &bytes[l_pos..])
                        .ok_or(ParseStrLitError::ExpectedRbrace)?;

                        let hex = &range[l_pos..r_pos];
                        let code_point = u32::from_str_radix(hex, 16)
                        .map_err(|_| ParseStrLitError::NotHex)?;

                        let ch = char::from_u32(code_point)
                        .ok_or(ParseStrLitError::NotChar)?;
                        result.push(ch);

                        last_pos = r_pos + 1;
                    }
                    _ => {
                        let end_pos = last_pos + 4;
                        if end_pos > len {
                            return Err(ParseStrLitError::ExpectedHex4)
                        }

                        let hex = &range[last_pos..end_pos];
                        let code_point = u32::from_str_radix(hex, 16)
                        .map_err(|_| ParseStrLitError::NotHex4)?;

                        let ch = char::from_u32(code_point)
                        .ok_or(ParseStrLitError::NotChar)?;
                        result.push(ch);

                        last_pos = end_pos;
                    }
                }
            }

            // ignore <cr> and <cr><lf>
            b'\r' => match bytes.get(last_pos) {
                Some(&b'\n') => last_pos += 1,
                _ => {}
            },
            // ignore line terminators
            b'\n' => {}

            // TODO ignore non-ASCII line terminators
            // '\u{2028}' |
            // '\u{2029}' => {}

            // TODO legacy octal
            // '1'...'9' => unimplemented!()

            // c @ '\'' |
            // c @ '\\' |
            // c @ '"' |
            c => result.push(c as char),
        }
    }
    Ok(if got_bs {
        result.push_str(&range[last_pos..]);
        Cow::from(result)
    } else {
        Cow::from(range)
    })
}

/// An error type for parsing string literals.
///
/// Returned by [str_lit_value](fn.str_lit_value.html) when the given string literal is syntactically invalid.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
///
/// Note that <code>\\<var>c</var></code>, where <var>c</var> is not a [SingleEscapeCharacter](https://tc39.github.io/ecma262/#prod-SingleEscapeCharacter), is *not* an invalid escape sequence. For example, `"\a"` is a valid string literal with the value `"a"`.
pub enum ParseStrLitError {
    /// The string literal contains a `\` at the end of its content.
    ExpectedEscape,
    /// The string literal contains an incomplete `\x__` escape sequence, i.e., there are not two characters after the `x`.
    ExpectedHex2,
    /// The string literal contains a `\x__` escape sequence but `__` is not a hexadecimal number.
    NotHex2,
    /// The string literal contains an incomplete `\u{…}` escape sequence with no closing `}`.
    ExpectedRbrace,
    /// The string literal contains a `\u{…}` escape sequence but the code between the braces is not a hexadecimal number.
    NotHex,
    /// The string literal contains a Unicode escape sequence but the code point is not a valid Unicode Scalar Value.
    NotChar,
    /// The string literal contains an incomplete `\u____` escape sequence, i.e., there are not four characters after the `u`.
    ExpectedHex4,
    /// The string literal contains a `\u____` escape sequence but `____` is not a hexadecimal number.
    NotHex4,
}

impl error::Error for ParseStrLitError {
    fn description(&self) -> &str {
        match *self {
            ParseStrLitError::ExpectedEscape => "expected escape sequence, but got end of string",
            ParseStrLitError::ExpectedHex2 |
            ParseStrLitError::NotHex2 => "expected two hexadecimal characters after '\\x'",
            ParseStrLitError::ExpectedRbrace => "expected `}` after `\\u{`",
            ParseStrLitError::NotHex => "expected hexadecimal number in `\\u{…}` escape",
            ParseStrLitError::NotChar => "hexadecimal number in `\\u{…}` escape is not a Unicode Scalar Value",
            ParseStrLitError::ExpectedHex4 |
            ParseStrLitError::NotHex4 => "expected four hexadecimal characters or '{…}' after '\\u'",
        }
    }
}

impl fmt::Display for ParseStrLitError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use std::error::Error;
        f.write_str(self.description())
    }
}

/// The specific kind of error that occurred.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ErrorKind {
    // Expected(&'static str),
    /// An exponent was expected but not found in a numeric literal.
    ExpectedExponent,
    /// The end of a template literal (<code>\`…\`</code>) was missing.
    UnterminatedTemplateLiteral,
    /// The end of a string literal (`'…'` or `"…"`) was missing.
    UnterminatedStringLiteral,
    /// The end of a regular expression literal (`/…/…`) was missing.
    UnterminatedRegExpLiteral,
    /// The end of a multiline comment (`/* … */`) was missing.
    UnterminatedMultilineComment,
    /// A right brace (`}`) unmatched by a left one was encountered.
    UnmatchedRbrace,
    /// An unexpected character was encountered.
    Unexpected(char),
}

/// The error type for lexical analysis.
#[derive(Debug)]
pub struct Error {
    /// The kind of error.
    pub kind: ErrorKind,
    /// The source code region in which the error appeared.
    pub span: SpanT<String, Loc>,
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            // ErrorKind::Expected(s) => {
            //     write!(f, "expected {}", s)
            // }
            ErrorKind::Unexpected(c) => {
                write!(f, "unexpected {}", c)
            }
            ErrorKind::ExpectedExponent => {
                write!(f, "expected exponent in numeric literal")
            }
            ErrorKind::UnterminatedTemplateLiteral => {
                write!(f, "unterminated template literal")
            }
            ErrorKind::UnterminatedStringLiteral => {
                write!(f, "unterminated string literal")
            }
            ErrorKind::UnterminatedRegExpLiteral => {
                write!(f, "unterminated regular expression literal")
            }
            ErrorKind::UnterminatedMultilineComment => {
                write!(f, "unterminated multiline comment")
            }
            ErrorKind::UnmatchedRbrace => {
                write!(f, "unmatched '}}'")
            }
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} at {}", self.kind, self.span)
    }
}

/// A lexical analyzer for JavaScript source code.
#[derive(Debug)]
pub struct Lexer<'f, 's> {
    file_name: &'f str,
    stream: PosStream<'s>,
    here: Tok<'f, 's>,
    frame: LexFrame,
    stack: Vec<LexFrame>,
    error: Option<Error>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum LexFrame {
    Outer,
    Template,
    Brace,
}

/// Consumes a token from the given lexer if it matches one of the patterns given.
///
/// # Syntax
///
/// ```rust,ignore
/// eat!(lexer => tok { all_cases },
///     pat1_1 | pat1_2 | ... if guard1 => expr1,
///     pat2_1 | pat2_2 | ... if guard2 => expr2,
///     _ => else_expr,
/// )
/// ```
///
/// Each pattern is matched against `lexer`'s current token type ([`Tt`](enum.Tt.html)), and, if it matches, `lexer` is advanced, `all_cases` is evaluated, and finally the corresponding expression is evaluated. `tok` is an identifier to which the consumed token is bound.
///
/// `{ all_cases }` can be omitted, as can the entire `=> tok { all_cases }` block.
///
/// # Examples
///
/// Conditionally accept a token:
///
/// ```
/// #[macro_use]
/// extern crate esparse;
/// use esparse::lex::{self, Tt};
///
/// # fn main() {
/// let mut lexer = lex::Lexer::new_unnamed("foo = 1");
/// eat!(lexer,
///     Tt::Id(name) => println!("the name is: {}", name),
///     _ => panic!("expected identifier"),
/// );
/// # }
/// ```
///
/// Parse a token stream while outputting source code to a string:
///
/// ```
/// #[macro_use]
/// extern crate esparse;
/// use esparse::lex::{self, Tt};
/// use std::fmt::Write;
///
/// # fn main() {
/// let mut lexer = lex::Lexer::new_unnamed("/* example */ foo = 1");
/// let mut output = String::new();
///
/// eat!(lexer => tok { write!(output, "{}{}", tok.ws_before, tok.tt).unwrap() },
///     Tt::Id(name) => println!("the name is: {}", name),
///     _ => panic!("expected identifier"),
/// );
///
/// assert_eq!(output, "/* example */ foo");
/// # }
/// ```
#[macro_export]
macro_rules! eat {
    (@collect $lexer:expr => $id:tt $all:tt, { $($($p:pat)|+ if $c:expr => $e:expr ,)* }, _ => $else:expr $(,)*) => {{
        let tok = $lexer.here();
        let $id = tok;
        match tok.tt {
            $($($p)|+ if $c => {
                $lexer.advance();
                $all
                $e
            })*
            _ => $else
        }
    }};
    (@collect $lexer:expr => $id:tt $all:tt, { $($($p:pat)|+ if $c:expr => $e:expr ,)* }, $($q:pat)|+ => $f:expr, $($t:tt)+) => {
        eat!(@collect $lexer => $id $all, { $($($p)|+ if $c => $e ,)* $($q)|+ if true => $f, }, $($t)+)
    };
    (@collect $lexer:expr => $id:tt $all:tt, { $($($p:pat)|+ if $c:expr => $e:expr ,)* }, $($q:pat)|+ if $d:expr => $f:expr, $($t:tt)+) => {
        eat!(@collect $lexer => $id $all, { $($($p)|+ if $c => $e ,)* $($q)|+ if $d => $f, }, $($t)+)
    };
    ($lexer:expr => $id:tt { $($all:tt)* }, $($t:tt)+) => {
        eat!(@collect $lexer => $id { $($all)* }, {}, $($t)+)
    };
    ($lexer:expr => $id:tt, $($t:tt)+) => {
        eat!($lexer => $id {}, $($t)+)
    };
    ($lexer:expr, $($t:tt)+) => {
        eat!($lexer => _, $($t)+)
    };
}

macro_rules! eat_s {
    (@collect $stream:expr, { $($($($p:tt)...+)|+ => $e:expr ,)* }, _ => $else:expr $(,)*) => {
        match $stream.here() {
            $($(Some($($p)...+))|+ => {
                $stream.advance();
                $e
            })*
            _ => $else
        }
    };
    (@collect $stream:expr, { $($($($p:tt)...+)|+ => $e:expr ,)* }, $($($q:tt)...+)|+ => $f:expr, $($t:tt)+) => {
        eat_s!(@collect $stream, { $($($($p)...+)|+ => $e ,)* $($($q)...+)|+ => $f, }, $($t)+)
    };
    ($stream:expr, $($t:tt)+) => {
        eat_s!(@collect $stream, {}, $($t)+)
    };
}

impl<'f, 's> Lexer<'f, 's> {
    /// Creates a new `Lexer` with the given source code and input file name.
    ///
    /// # Examples
    ///
    /// ```
    /// use esparse::lex::Lexer;
    ///
    /// let lexer = Lexer::new("<input>", "1 + 2");
    /// println!("The first token is: {:?}", lexer.here());
    /// ```
    #[inline]
    pub fn new(file_name: &'f str, input: &'s str) -> Self {
        let mut lexer = Lexer {
            file_name,
            stream: Stream::new(input),
            here: Tok::new(Tt::Eof, Span::zero(file_name)),
            frame: LexFrame::Outer,
            stack: Vec::new(),
            error: None,
        };
        lexer.advance();
        lexer
    }

    /// Creates a new `Lexer` with the given source code and `<input>` as the file name.
    ///
    /// # Examples
    ///
    /// ```
    /// use esparse::lex::Lexer;
    ///
    /// let lexer = Lexer::new_unnamed("1 + 2");
    /// println!("The first token is: {:?}", lexer.here());
    /// ```
    #[inline]
    pub fn new_unnamed(input: &'s str) -> Self {
        Self::new("<input>", input)
    }

    /// The input source code.
    ///
    /// # Examples
    ///
    /// ```
    /// use esparse::lex::Lexer;
    ///
    /// let lexer = Lexer::new_unnamed("1 + 2");
    /// assert_eq!(lexer.input(), "1 + 2");
    /// ```
    #[inline]
    pub fn input(&self) -> &'s str {
        self.stream.input()
    }

    /// The current token.
    ///
    /// # Examples
    ///
    /// ```
    /// use esparse::lex::Lexer;
    ///
    /// let mut lexer = Lexer::new_unnamed("1 + 2");
    /// println!("The first token is: {:?}", lexer.here());
    /// lexer.advance();
    /// println!("The second token is: {:?}", lexer.here());
    /// ```
    #[inline]
    pub fn here(&self) -> Tok<'f, 's> {
        self.here
    }

    /// Moves the lexer forward to the next token, returning the current one.
    ///
    /// # Examples
    ///
    /// ```
    /// use esparse::lex::Lexer;
    ///
    /// let mut lexer = Lexer::new_unnamed("1 + 2");
    /// println!("The first token is: {:?}", lexer.advance());
    /// println!("The second token is: {:?}", lexer.advance());
    /// ```
    pub fn advance(&mut self) -> Tok<'f, 's> {
        let tok = self.read_tok();
        mem::replace(&mut self.here, tok)
    }

    /// The most recent error.
    ///
    /// # Examples
    ///
    /// ```
    /// use esparse::lex::Lexer;
    ///
    /// let mut lexer = Lexer::new_unnamed("}");
    /// lexer.advance();
    /// println!("The error is: {:?}", lexer.error().unwrap());
    /// ```
    #[inline]
    pub fn error(&self) -> Option<&Error> {
        self.error.as_ref()
    }

    /// Consumes and returns the most recent error, if it exists, resetting the lexer to an error-free state.
    ///
    /// # Examples
    ///
    /// ```
    /// # #[macro_use]
    /// # extern crate matches;
    /// # extern crate esparse;
    /// use esparse::lex::Lexer;
    /// # fn main() {
    ///
    /// let mut lexer = Lexer::new_unnamed("}");
    /// lexer.advance();
    /// println!("The error is: {:?}", lexer.take_error().unwrap());
    /// assert_matches!(lexer.error(), None);
    /// # }
    /// ```
    #[inline]
    pub fn take_error(&mut self) -> Option<Error> {
        self.error.take()
    }

    // fn stream_loc(&self) -> Loc {
    //     self.recover_loc(self.stream.pos())
    // }

    /// Recovers a [`Loc`](../ast/type.Loc.html) from a byte offset by scanning the preceding input.
    pub fn recover_loc(&self, pos: usize) -> Loc {
        self.recover_locs(vec![pos])[0]
    }

    /// Recovers a [`Span`](../ast/type.Span.html) from a byte-offset `Span` by scanning the preceding input.
    pub fn recover_span(&self, span: Span<'f, usize>) -> Span<'f, Loc> {
        let locs = self.recover_locs(vec![span.start, span.end]);
        Span::new(span.file_name, locs[0], locs[1])
    }

    /// Recovers an empty [`Span`](../ast/type.Span.html) from a byte offset by scanning the preceding input.
    pub fn recover_empty_span(&self, pos: usize) -> Span<'f, Loc> {
        Span::empty(self.file_name, self.recover_loc(pos))
    }

    /// Recovers a sequence of [`Loc`](../ast/type.Loc.html)s from a sequence of byte offsets by scanning the preceding input.
    #[cold]
    fn recover_locs<I: IntoIterator<Item = usize>>(&self, iter: I) -> Vec<Loc> {
        let mut s = LocStream::new(self.stream.input());
        let mut locs = Vec::new();
        for pos in iter.into_iter() {
            while s.pos() < pos {
                s.advance();
            }
            locs.push(s.loc());
        }
        locs
    }

    #[inline(always)]
    fn read_tok(&mut self) -> Tok<'f, 's> {
        let (ws_before, nl_before) = match self.stream.skip_ws() {
            Some(x) => x,
            None => {
                let span = Span::empty(self.file_name, self.stream.pos);
                self.error = Some(Error {
                    kind: ErrorKind::UnterminatedMultilineComment,
                    span: self.recover_empty_span(span.start).with_owned(),
                });
                self.stream.exhaust();
                return Tok {
                    tt: Tt::Err,
                    span,
                    ws_before: "",
                    nl_before: false,
                }
            }
        };

        let start = self.stream.pos();
        let here = match self.stream.advance() {
            Some(c) => c,
            None => {
                return Tok {
                    tt: Tt::Eof,
                    span: Span::empty(self.file_name, start),
                    ws_before,
                    nl_before,
                }
            }
        };

        macro_rules! mark_error {
            ($kind:expr) => {{
                let span = Span::new(self.file_name, start, self.stream.pos());
                self.error = Some(Error {
                    kind: $kind,
                    span: self.recover_span(span).with_owned(),
                });
                self.stream.exhaust();
                return Tok {
                    tt: Tt::Err,
                    span,
                    ws_before,
                    nl_before,
                }
            }};
        }

        let tt = match here {
            '{' => {
                self.stack.push(mem::replace(&mut self.frame, LexFrame::Brace));
                Tt::Lbrace
            }
            '(' => Tt::Lparen,
            ')' => {
                Tt::Rparen
            },
            '[' => Tt::Lbracket,
            ']' => {
                Tt::Rbracket
            },
            ';' => Tt::Semi,
            ',' => Tt::Comma,

            '<' => eat_s!(self.stream,
                '<' => eat_s!(self.stream,
                    '=' => Tt::LtLtEq,
                    _ => Tt::LtLt,
                ),
                '=' => Tt::LtEq,
                _ => Tt::Lt,
            ),
            '>' => eat_s!(self.stream,
                '>' => eat_s!(self.stream,
                    '>' => eat_s!(self.stream,
                        '=' => Tt::GtGtGtEq,
                        _ => Tt::GtGtGt,
                    ),
                    '=' => Tt::GtGtEq,
                    _ => Tt::GtGt,
                ),
                '=' => Tt::GtEq,
                _ => Tt::Gt,
            ),
            '=' => eat_s!(self.stream,
                '>' => Tt::EqGt,
                '=' => eat_s!(self.stream,
                    '=' => Tt::EqEqEq,
                    _ => Tt::EqEq,
                ),
                _ => Tt::Eq,
            ),
            '!' => eat_s!(self.stream,
                '=' => eat_s!(self.stream,
                    '=' => Tt::BangEqEq,
                    _ => Tt::BangEq,
                ),
                _ => Tt::Bang,
            ),
            '+' => eat_s!(self.stream,
                '+' => Tt::PlusPlus,
                '=' => Tt::PlusEq,
                _ => Tt::Plus,
            ),
            '-' => eat_s!(self.stream,
                '-' => Tt::MinusMinus,
                '=' => Tt::MinusEq,
                _ => Tt::Minus,
            ),
            '*' => eat_s!(self.stream,
                '*' => eat_s!(self.stream,
                    '=' => Tt::StarStarEq,
                    _ => Tt::StarStar,
                ),
                '=' => Tt::StarEq,
                _ => Tt::Star,
            ),
            '%' => eat_s!(self.stream,
                '=' => Tt::PercentEq,
                _ => Tt::Percent,
            ),
            '&' => eat_s!(self.stream,
                '&' => Tt::AndAnd,
                '=' => Tt::AndEq,
                _ => Tt::And,
            ),
            '|' => eat_s!(self.stream,
                '|' => Tt::OrOr,
                '=' => Tt::OrEq,
                _ => Tt::Or,
            ),
            '^' => eat_s!(self.stream,
                '=' => Tt::CircumflexEq,
                _ => Tt::Circumflex,
            ),
            '~' => Tt::Tilde,
            '?' => Tt::Question,
            ':' => Tt::Colon,

            '}' => {
                match self.frame {
                    LexFrame::Template => {
                        let result;
                        loop {
                            match self.stream.advance() {
                                Some('\\') => {
                                    // skip char after \
                                    match self.stream.advance() {
                                        Some('\u{000D}') => {
                                            self.stream.eat('\u{000A}');
                                        }
                                        _ => {}
                                    }
                                }
                                Some('`') => {
                                    self.frame = match self.stack.pop() {
                                        Some(f) => f,
                                        None => unreachable!(),
                                    };
                                    result = Tt::TemplateEnd(self.stream.str_from(start));
                                    break
                                }
                                Some('$') => {
                                    // TODO subopt
                                    if self.stream.eat('{') {
                                        result = Tt::TemplateMiddle(self.stream.str_from(start));
                                        break
                                    }
                                }
                                Some(_) => {}
                                None => {
                                    mark_error!(ErrorKind::UnterminatedTemplateLiteral)
                                }
                            }
                        }
                        result
                    }
                    LexFrame::Brace => {
                        self.frame = match self.stack.pop() {
                            Some(f) => f,
                            None => unreachable!(),
                        };
                        Tt::Rbrace
                    }
                    LexFrame::Outer => {
                        mark_error!(ErrorKind::UnmatchedRbrace)
                    }
                }
            }
            '`' => {
                let result;
                loop {
                    match self.stream.advance() {
                        Some('\\') => {
                            // skip char after \
                            match self.stream.advance() {
                                Some('\u{000D}') => {
                                    self.stream.eat('\u{000A}');
                                }
                                _ => {}
                            }
                        }
                        Some('`') => {
                            result = Tt::TemplateNoSub(self.stream.str_from(start));
                            break
                        }
                        Some('$') => {
                            // TODO subopt
                            if self.stream.eat('{') {
                                self.stack.push(mem::replace(&mut self.frame, LexFrame::Template));
                                result = Tt::TemplateStart(self.stream.str_from(start));
                                break
                            }
                        }
                        Some(_) => {}
                        None => {
                            mark_error!(ErrorKind::UnterminatedTemplateLiteral)
                        }
                    }
                }
                result
            }

            '"' => {
                loop {
                    match self.stream.advance() {
                        Some('\\') => {
                            // skip char after \
                            match self.stream.advance() {
                                Some('\u{000D}') => {
                                    self.stream.eat('\u{000A}');
                                }
                                _ => {}
                            }
                        }
                        Some('"') => {
                            break
                        }
                          Some('\u{000A}') // LINE FEED (LF)          <LF>
                        | Some('\u{000D}') // CARRIAGE RETURN (CR)    <CR>
                        | Some('\u{2028}') // LINE SEPARATOR          <LS>
                        | Some('\u{2029}') // PARAGRAPH SEPARATOR     <PS>
                        | None
                        => {
                            mark_error!(ErrorKind::UnterminatedStringLiteral)
                        }
                        Some(_) => {}
                    }
                }
                Tt::StrLitDbl(self.stream.str_from(start))
            }
            '\'' => {
                loop {
                    match self.stream.advance() {
                        Some('\\') => {
                            // skip char after \
                            match self.stream.advance() {
                                Some('\u{000D}') => {
                                    self.stream.eat('\u{000A}');
                                }
                                _ => {}
                            }
                        }
                        Some('\'') => {
                            break
                        }
                          Some('\u{000A}') // LINE FEED (LF)          <LF>
                        | Some('\u{000D}') // CARRIAGE RETURN (CR)    <CR>
                        | Some('\u{2028}') // LINE SEPARATOR          <LS>
                        | Some('\u{2029}') // PARAGRAPH SEPARATOR     <PS>
                        | None
                        => {
                            mark_error!(ErrorKind::UnterminatedStringLiteral)
                        }
                        Some(_) => {}
                    }
                }
                Tt::StrLitSgl(self.stream.str_from(start))
            }

            '/' => {
                match self.here.tt {
                    Tt::Rparen |
                    Tt::Rbracket |
                    Tt::TemplateEnd(_) |
                    Tt::TemplateNoSub(_) |
                    Tt::StrLitSgl(_) |
                    Tt::StrLitDbl(_) |
                    Tt::RegExpLit(_, _) |
                    Tt::NumLitBin(_) |
                    Tt::NumLitOct(_) |
                    Tt::NumLitDec(_) |
                    Tt::NumLitHex(_) |
                    Tt::Id(_) |
                    Tt::This |
                    Tt::Super => eat_s!(self.stream,
                        '=' => Tt::SlashEq,
                        _ => Tt::Slash,
                    ),
                    _ => {
                        loop {
                            match self.stream.advance() {
                                Some('\\') => {
                                    // skip char after \
                                    match self.stream.advance() {
                                        Some('\u{000D}') => {
                                            self.stream.eat('\u{000A}');
                                        }
                                        _ => {}
                                    }
                                }
                                Some('/') => {
                                    break
                                }
                                Some('[') => {
                                    loop {
                                        match self.stream.advance() {
                                            Some('\\') => {
                                                // skip char after \
                                                match self.stream.advance() {
                                                    Some('\u{000D}') => {
                                                        self.stream.eat('\u{000A}');
                                                    }
                                                    _ => {}
                                                }
                                            }
                                            Some(']') => {
                                                break
                                            }
                                              Some('\u{000A}') // LINE FEED (LF)          <LF>
                                            | Some('\u{000D}') // CARRIAGE RETURN (CR)    <CR>
                                            | Some('\u{2028}') // LINE SEPARATOR          <LS>
                                            | Some('\u{2029}') // PARAGRAPH SEPARATOR     <PS>
                                            | None
                                            => {
                                                mark_error!(ErrorKind::UnterminatedRegExpLiteral)
                                            }
                                            Some(_) => {}
                                        }
                                    }
                                }
                                  Some('\u{000A}') // LINE FEED (LF)          <LF>
                                | Some('\u{000D}') // CARRIAGE RETURN (CR)    <CR>
                                | Some('\u{2028}') // LINE SEPARATOR          <LS>
                                | Some('\u{2029}') // PARAGRAPH SEPARATOR     <PS>
                                | None
                                => {
                                    mark_error!(ErrorKind::UnterminatedRegExpLiteral)
                                }
                                Some(_) => {}
                            }
                        }
                        let flags_start = self.stream.pos();
                        self.stream.skip_id_continue_chars();

                        let source = self.stream.str_from(start);
                        let flags = self.stream.str_from(flags_start);
                        Tt::RegExpLit(source, flags)
                    }
                }
            }

            // TODO error if following char is IdentStart or DecimalDigit
            '.' => {
                match self.stream.here() {
                    Some('.') => match self.stream.next() {
                        Some('.') => {
                            self.stream.advance();
                            self.stream.advance();
                            Tt::DotDotDot
                        }
                        Some(_) | None => {
                            Tt::Dot
                        }
                    },
                    Some('0'...'9') => {
                        self.stream.advance();
                        self.stream.skip_dec_digits();
                        eat_s!(self.stream,
                            'e' | 'E' => eat_s!(self.stream,
                                '-' | '+' | '0'...'9' => {
                                    self.stream.skip_dec_digits();
                                },
                                _ => {
                                    mark_error!(ErrorKind::ExpectedExponent)
                                },
                            ),
                            _ => {},
                        );
                        Tt::NumLitDec(self.stream.str_from(start))
                    }
                    Some(_) | None => {
                        Tt::Dot
                    }
                }
            }
            '0' => eat_s!(self.stream,
                'b' | 'B' => {
                    self.stream.skip_bin_digits();
                    Tt::NumLitBin(self.stream.str_from(start))
                },
                'o' | 'O' => {
                    self.stream.skip_oct_digits();
                    Tt::NumLitOct(self.stream.str_from(start))
                },
                'x' | 'X' => {
                    self.stream.skip_hex_digits();
                    Tt::NumLitHex(self.stream.str_from(start))
                },
                '.' => {
                    self.stream.skip_dec_digits();
                    eat_s!(self.stream,
                        'e' | 'E' => eat_s!(self.stream,
                            '-' | '+' | '0'...'9' => {
                                self.stream.skip_dec_digits();
                                Tt::NumLitDec(self.stream.str_from(start))
                            },
                            _ => {
                                mark_error!(ErrorKind::ExpectedExponent)
                            },
                        ),
                        _ => {
                            Tt::NumLitDec(self.stream.str_from(start))
                        },
                    )
                },
                'e' | 'E' => eat_s!(self.stream,
                    '-' | '+' | '0'...'9' => {
                        self.stream.skip_dec_digits();
                        Tt::NumLitDec(self.stream.str_from(start))
                    },
                    _ => {
                        mark_error!(ErrorKind::ExpectedExponent)
                    },
                ),
                _ => Tt::NumLitDec(self.stream.str_from(start)),
            ),
            '1'...'9' => {
                self.stream.skip_dec_digits();
                eat_s!(self.stream,
                    '.' => {
                        self.stream.skip_dec_digits();
                        eat_s!(self.stream,
                            'e' | 'E' => eat_s!(self.stream,
                                '-' | '+' | '0'...'9' => {
                                    self.stream.skip_dec_digits();
                                },
                                _ => {
                                    mark_error!(ErrorKind::ExpectedExponent)
                                },
                            ),
                            _ => {},
                        );
                    },
                    'e' | 'E' => eat_s!(self.stream,
                        '-' | '+' | '0'...'9' => {
                            self.stream.skip_dec_digits();
                        },
                        _ => {
                            mark_error!(ErrorKind::ExpectedExponent)
                        },
                    ),
                    _ => {},
                );
                Tt::NumLitDec(self.stream.str_from(start))
            }

            _ => {
                // TODO '\\'
                if here == '$' || here == '_' || UnicodeXID::is_xid_start(here) {
                    self.stream.skip_id_continue_chars();
                    let id = self.stream.str_from(start);
                    match id {
                        "null" => Tt::Null,
                        "true" => Tt::True,
                        "false" => Tt::False,
                        "await" => Tt::Await,
                        "break" => Tt::Break,
                        "case" => Tt::Case,
                        "catch" => Tt::Catch,
                        "class" => Tt::Class,
                        "const" => Tt::Const,
                        "continue" => Tt::Continue,
                        "debugger" => Tt::Debugger,
                        "default" => Tt::Default,
                        "delete" => Tt::Delete,
                        "do" => Tt::Do,
                        "else" => Tt::Else,
                        "export" => Tt::Export,
                        "extends" => Tt::Extends,
                        "finally" => Tt::Finally,
                        "for" => Tt::For,
                        "function" => Tt::Function,
                        "if" => Tt::If,
                        "import" => Tt::Import,
                        "in" => Tt::In,
                        "instanceof" => Tt::Instanceof,
                        "new" => Tt::New,
                        "return" => Tt::Return,
                        "super" => Tt::Super,
                        "switch" => Tt::Switch,
                        "this" => Tt::This,
                        "throw" => Tt::Throw,
                        "try" => Tt::Try,
                        "typeof" => Tt::Typeof,
                        "var" => Tt::Var,
                        "void" => Tt::Void,
                        "while" => Tt::While,
                        "with" => Tt::With,
                        "yield" => Tt::Yield,
                        _ => Tt::Id(id),
                    }
                } else {
                    mark_error!(ErrorKind::Unexpected(here))
                }
            },
        };
        Tok {
            tt,
            span: Span::new(self.file_name, start, self.stream.pos()),
            ws_before,
            nl_before,
        }
    }
}

impl<'f, 's> Iterator for Lexer<'f, 's> {
    type Item = Tok<'f, 's>;

    fn next(&mut self) -> Option<Tok<'f, 's>> {
        let tok = self.advance();
        match tok.tt {
            Tt::Eof => None,
            _ => Some(tok),
        }
    }
}

/// Trait for source code streams.
///
/// A `Stream` advances over its input one character at a time, providing two characters of lookahead. It has two built-in implementations:
///
/// * [`PosStream`](type.PosStream.html) provides only byte offset information and is slightly faster.
/// * [`LocStream`](type.LocStream.html) provides row and column information and is slightly slower.
pub trait Stream<'s> {
    /// Creates a new `Stream` on the given input.
    fn new(input: &'s str) -> Self;

    // TODO pub fn advance_by(&mut self, n: usize) -> &'s str {}

    /// Advances the stream by one character, returning the character advanced past or `None` if the stream was already at the end of its input.
    fn advance(&mut self) -> Option<char>;

    /// The current character, or `None` if the stream has reached the end of its input.
    fn here(&self) -> Option<char>;

    /// The next character, or `None` if the stream has reached the end of its input.
    fn next(&self) -> Option<char>;

    /// The bytewise position of the current character.
    fn pos(&self) -> usize;

    /// The entire input source code.
    fn input(&self) -> &'s str;

    /// `true` if and only if the current character is `c`.
    #[inline]
    fn is(&self, c: char) -> bool {
        self.here().map_or(false, |cc| c == cc)
    }

    /// A slice of the input source code from the byte at `start` up to, but not including the first byte of the current character.
    #[inline]
    fn str_from(&self, start: usize) -> &'s str {
        &self.input()[start..self.pos()]
    }

    /// A slice of the input source code from the byte at `start` up to, but not including the byte at `end`.
    #[inline]
    fn str_range(&self, start: usize, end: usize) -> &'s str {
        &self.input()[start..end]
    }

    /// Advances the stream to the end of the file.
    #[inline]
    fn exhaust(&mut self) {
        while let Some(_) = self.advance() {}
    }

    /// Advances to the next character if and only if the current character is `c`.
    #[inline]
    fn eat(&mut self, c: char) -> bool {
        match self.here() {
            Some(cc) if c == cc => {
                self.advance();
                true
            }
            _ => false,
        }
    }

    /// Advances by two characters if and only if the current character is `c` and the next character is `d`.
    #[inline]
    fn eat2(&mut self, c: char, d: char) -> bool {
        match self.here() {
            Some(cc) if c == cc => {
                match self.next() {
                    Some(dd) if d == dd => {
                        self.advance();
                        self.advance();
                        true
                    }
                    _ => false,
                }
            }
            _ => false,
        }
    }

    /// Advances to the next character until the stream ends or `f` returns `false` for the current character.
    #[inline]
    fn skip_while<F>(&mut self, mut f: F) where
    F: FnMut(char) -> bool {
        loop {
            match self.here() {
                Some(c) => {
                    if f(c) {
                        self.advance();
                    } else {
                        break
                    }
                }
                None => break,
            }
        }
    }

    // #[inline]
    // fn skip_str_dbl_chars(&mut self) {
    //     self.skip_while(|c| match c {
    //         '\\' | '"' => false,
    //         _ => true,
    //     });
    // }

    // #[inline]
    // fn skip_str_sgl_chars(&mut self) {
    //     self.skip_while(|c| match c {
    //         '\\' | '\'' => false,
    //         _ => true,
    //     });
    // }

    /// Advances past any binary digits (`0` or `1`).
    #[inline]
    fn skip_bin_digits(&mut self) {
        self.skip_while(|c| match c {
            '0'...'1' => true,
            _ => false,
        });
    }

    /// Advances past any octal digits (`0` through `7`).
    #[inline]
    fn skip_oct_digits(&mut self) {
        self.skip_while(|c| match c {
            '0'...'7' => true,
            _ => false,
        });
    }

    /// Advances past any decimal digits (`0` through `9`).
    #[inline]
    fn skip_dec_digits(&mut self) {
        self.skip_while(|c| match c {
            '0'...'9' => true,
            _ => false,
        });
    }

    /// Advances past any hexadecimal digits (`0` through `9` and `a` through `f`, case insensitive).
    #[inline]
    fn skip_hex_digits(&mut self) {
        self.skip_while(|c| match c {
            '0'...'9' | 'a'...'f' | 'A'...'F' => true,
            _ => false,
        });
    }

    /// Advances past any characters in the Unicode category [ID_Continue](http://unicode.org/reports/tr31/).
    #[inline]
    fn skip_id_continue_chars(&mut self) {
        self.skip_while(|c| match c {
              '$'
            | '_'
            | '\u{200C}' // ZERO WIDTH NON-JOINER
            | '\u{200D}' // ZERO WIDTH JOINER
            => true,
            _ => UnicodeXID::is_xid_continue(c),
        })
    }

    /// Advances past any whitespace or JavaScript comments.
    ///
    /// Returns <code>(<var>ws</var>, <var>nl</var>)</code> where <var>ws</var> is a slice covering the whitespace skipped and <var>nl</var> is true if and only if the whitespace contained a newline.
    #[inline]
    fn skip_ws(&mut self) -> Option<(&'s str, bool)> {
        let start_pos = self.pos();
        let mut nl_before = false;
        loop {
            match self.here() {
                Some(c) => match c {
                      '\u{000A}' // LINE FEED (LF)          <LF>
                    | '\u{000D}' // CARRIAGE RETURN (CR)    <CR>
                    | '\u{2028}' // LINE SEPARATOR          <LS>
                    | '\u{2029}' // PARAGRAPH SEPARATOR     <PS>
                    => {
                        nl_before = true;
                        self.advance();
                    }

                      '\u{0009}' // CHARACTER TABULATION
                    | '\u{000B}' // LINE TABULATION
                    | '\u{000C}' // FORM FEED
                    | '\u{0020}' // SPACE
                    | '\u{00A0}' // NO-BREAK SPACE
                    | '\u{FEFF}' // ZERO WIDTH NO-BREAK SPACE

                    // Zs (Space_Separator):
                        | '\u{1680}' // OGHAM SPACE MARK
                        | '\u{2000}' // EN QUAD
                        | '\u{2001}' // EM QUAD
                        | '\u{2002}' // EN SPACE
                        | '\u{2003}' // EM SPACE
                        | '\u{2004}' // THREE-PER-EM SPACE
                        | '\u{2005}' // FOUR-PER-EM SPACE
                        | '\u{2006}' // SIX-PER-EM SPACE
                        | '\u{2007}' // FIGURE SPACE
                        | '\u{2008}' // PUNCTUATION SPACE
                        | '\u{2009}' // THIN SPACE
                        | '\u{200A}' // HAIR SPACE
                        | '\u{202F}' // NARROW NO-BREAK SPACE
                        | '\u{205F}' // MEDIUM MATHEMATICAL SPACE
                        | '\u{3000}' // IDEOGRAPHIC SPACE
                    => {
                        self.advance();
                    }
                    '/' => match self.next() {
                        Some('*') => {
                            self.advance();
                            self.advance();
                            'outer: loop {
                                match self.here() {
                                    Some('*') => {
                                        loop {
                                            self.advance();
                                            match self.here() {
                                                Some('/') => {
                                                    self.advance();
                                                    break 'outer
                                                },
                                                Some('*') => {}
                                                Some(_) => {
                                                    self.advance();
                                                    break
                                                }
                                                None => {
                                                    return None
                                                }
                                            }
                                        }
                                    }
                                      Some('\u{000A}') // LINE FEED (LF)          <LF>
                                    | Some('\u{000D}') // CARRIAGE RETURN (CR)    <CR>
                                    | Some('\u{2028}') // LINE SEPARATOR          <LS>
                                    | Some('\u{2029}') // PARAGRAPH SEPARATOR     <PS>
                                    => {
                                        nl_before = true;
                                        self.advance();
                                    }
                                    Some(_) => {
                                        self.advance();
                                    }
                                    None => {
                                        return None
                                    }
                                }
                            }
                        },
                        Some('/') => {
                            self.advance();
                            self.advance();
                            self.skip_while(|c| !matches!(c,
                                | '\u{000A}' // LINE FEED (LF)          <LF>
                                | '\u{000D}' // CARRIAGE RETURN (CR)    <CR>
                                | '\u{2028}' // LINE SEPARATOR          <LS>
                                | '\u{2029}' // PARAGRAPH SEPARATOR     <PS>
                            ));
                        },
                        _ => break,
                    },
                    _ => break,
                },
                None => break,
            }
        }
        Some((self.str_from(start_pos), nl_before))
    }
}

/// Generic source code stream with byte offset information.
///
/// A `PosStream` advances over its input one character at a time, providing two characters of lookahead. If you need location information, use a [`LocStream`](type.LocStream.html) instead.
#[derive(Debug)]
pub struct PosStream<'s> {
    input: &'s str,

    pos: usize,
    here: Option<char>,

    next_pos: usize,
    next_width: usize,
    next: Option<char>,
}

impl<'s> Stream<'s> for PosStream<'s> {
    #[inline]
    fn new(input: &'s str) -> Self {
        let mut stream = PosStream {
            input,
            pos: 0,
            here: None,
            next_pos: 0,
            next_width: 0,
            next: None,
        };
        stream.advance();
        stream.advance();
        stream
    }

    #[inline]
    fn advance(&mut self) -> Option<char> {
        let next = {
            self.pos = self.next_pos;
            self.next_pos += self.next_width;

            if self.next_pos >= self.input.len() {
                self.next_width = 0;
                None
            } else {
                let (next, width) = unsafe {
                    char_at_unchecked(self.input, self.next_pos)
                };
                self.next_width = width;
                Some(next)
            }
        };
        mem::replace(&mut self.here, mem::replace(&mut self.next, next))
    }

    #[inline]
    fn here(&self) -> Option<char> { self.here }

    #[inline]
    fn next(&self) -> Option<char> { self.next }

    #[inline]
    fn pos(&self) -> usize { self.pos }

    #[inline]
    fn input(&self) -> &'s str { self.input }
}

/// Generic source code stream with row/column information.
///
/// A `LocStream` advances over its input one character at a time, tracking line and column information and providing two characters of lookahead. If you don't need location information, you can use the faster [`PosStream`](type.PosStream.html) instead.
#[derive(Debug)]
pub struct LocStream<'s> {
    input: &'s str,

    loc: Loc,
    here: Option<char>,

    next_pos: usize,
    next_width: usize,
    next: Option<char>,
}

impl<'s> LocStream<'s> {
    /// The location of the current character.
    #[inline]
    fn loc(&self) -> Loc { self.loc }
}

impl<'s> Stream<'s> for LocStream<'s> {
    fn new(input: &'s str) -> Self {
        let mut stream = LocStream {
            input,
            loc: Default::default(),
            here: None,
            next_pos: 0,
            next_width: 0,
            next: None,
        };
        stream.advance();
        stream.advance();
        stream
    }

    /// The bytewise position of the current character.
    #[inline]
    fn pos(&self) -> usize { self.loc.pos }

    /// The current character, or `None` if the stream has reached the end of its input.
    #[inline]
    fn here(&self) -> Option<char> { self.here }

    /// The next character, or `None` if the stream has reached the end of its input.
    #[inline]
    fn next(&self) -> Option<char> { self.next }

    /// The entire input source code.
    #[inline]
    fn input(&self) -> &'s str { self.input }

    /// Advances the stream by one character, returning the character advanced past or `None` if the stream was already at the end of its input.
    fn advance(&mut self) -> Option<char> {
        match self.here {
            // TODO count both \r and \r\n as one newline
              Some('\u{000A}') // LINE FEED (LF)            <LF>
            // | Some('\u{000D}') // CARRIAGE RETURN (CR)   <CR>
            | Some('\u{2028}') // LINE SEPARATOR            <LS>
            | Some('\u{2029}') // PARAGRAPH SEPARATOR       <PS>
            => {
                self.loc.col = 0;
                self.loc.row += 1;
            },
            Some(_) => self.loc.col += 1,
            None => {}
        }
        let next = {
            self.loc.pos = self.next_pos;
            self.next_pos += self.next_width;

            if self.next_pos >= self.input.len() {
                self.next_width = 0;
                None
            } else {
                let (next, width) = unsafe {
                    char_at_unchecked(self.input, self.next_pos)
                };
                self.next_width = width;
                Some(next)
            }
        };
        mem::replace(&mut self.here, mem::replace(&mut self.next, next))
    }
}

#[inline]
unsafe fn char_at_unchecked(s: &str, n: usize) -> (char, usize) {
    let b = s.as_bytes();
    let b0 = b[n];
    /*let width = utf8_char_width(b0);
    let code = match width {
        1 => b0 as u32,
        2 => ((b0 & 0x1f) as u32) << 6 | (b[n+1] & 0x3f) as u32,
        3 => ((b0 & 0x0f) as u32) << 12 | ((b[n+1] & 0x3f) as u32) << 6 | (b[n+2] & 0x3f) as u32,
        4 => ((b0 & 0x07) as u32) << 18 | ((b[n+1] & 0x3f) as u32) << 12 | ((b[n+2] & 0x3f) as u32) << 6 | (b[n+3] & 0x3f) as u32,
        _ => panic!("invalid utf-8 sequence"),
    };*/
    let (width, code) = if b0 & 0x80 == 0 {
        (1, b0 as u32)
    } else if b0 & 0xe0 == 0xc0 {
        (2, ((b0 & 0x1f) as u32) << 6 | (b[n+1] & 0x3f) as u32)
    } else if b0 & 0xf0 == 0xe0 {
        (3, ((b0 & 0x0f) as u32) << 12 | ((b[n+1] & 0x3f) as u32) << 6 | (b[n+2] & 0x3f) as u32)
    } else if b0 & 0xf1 == 0xf0 {
        (4, ((b0 & 0x07) as u32) << 18 | ((b[n+1] & 0x3f) as u32) << 12 | ((b[n+2] & 0x3f) as u32) << 6 | (b[n+3] & 0x3f) as u32)
    } else {
        panic!("invalid utf-8 sequence")
    };
    (char::from_u32_unchecked(code), width)
}

/*const UTF8_CHAR_WIDTH: [u8; 256] = [
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, // 0x1F
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, // 0x3F
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, // 0x5F
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, // 0x7F
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 0x9F
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 0xBF
0,0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, // 0xDF
3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3, // 0xEF
4,4,4,4,4,0,0,0,0,0,0,0,0,0,0,0, // 0xFF
];

#[inline]
fn utf8_char_width(b: u8) -> usize {
    UTF8_CHAR_WIDTH[b as usize] as usize
}*/

/*fn char_utf8_bytes(c: char) -> usize {
    match c as u32 {
        0x00000...0x0007f => 1,
        0x00080...0x007ff => 2,
        0x00800...0x0ffff => 3,
        _ => 4,
    }
}*/

#[cfg(test)]
#[allow(unused_imports)]
mod test {
    #[cfg(feature = "bench")]
    extern crate test;

    use super::*;
    use std::fs;
    use std::io::prelude::*;
    use std::borrow::Cow;

    fn lex_test(source: &str, expected: &[Tt]) {
        let mut lexer = Lexer::new_unnamed(source);
        for tt in expected {
            assert_eq!(tt, &lexer.advance().tt);
        }
        assert_eq!(Tt::Eof, lexer.advance().tt);
    }

    fn lex_test_invalid(source: &str) {
        let mut lexer = Lexer::new_unnamed(source);
        for _ in &mut lexer {}
        assert_matches!(lexer.error(), Some(_));
    }

    #[test]
    fn test_keywords() {
        lex_test(r#"
            null

            true
            false

            await
            break
            case catch class const continue
            debugger default delete do
            else export extends
            finally for function
            if import in instanceof
            new
            return
            super switch
            this throw try typeof
            var void
            while with
            yield
        "#, &[
            Tt::Null,

            Tt::True,
            Tt::False,

            Tt::Await,
            Tt::Break,
            Tt::Case, Tt::Catch, Tt::Class, Tt::Const, Tt::Continue,
            Tt::Debugger, Tt::Default, Tt::Delete, Tt::Do,
            Tt::Else, Tt::Export, Tt::Extends,
            Tt::Finally, Tt::For, Tt::Function,
            Tt::If, Tt::Import, Tt::In, Tt::Instanceof,
            Tt::New,
            Tt::Return,
            Tt::Super, Tt::Switch,
            Tt::This, Tt::Throw, Tt::Try, Tt::Typeof,
            Tt::Var, Tt::Void,
            Tt::While, Tt::With,
            Tt::Yield,
        ])
    }

    #[test]
    fn test_identifiers() {
        // http://unicode.org/reports/tr31/
        lex_test("
            _ $ _$ $_ _1 $1
            ne retur return1 return$ into r1
            éüßΩπ
            \u{0646}\u{0627}\u{0645}\u{0647}\u{200C}\u{0627}\u{06CC}
            \u{0D26}\u{0D43}\u{0D15}\u{0D4D}\u{200C}\u{0D38}\u{0D3E}\u{0D15}\u{0D4D}\u{0D37}\u{0D3F}
            \u{0DC1}\u{0DCA}\u{200D}\u{0DBB}\u{0DD3}\u{0DBD}\u{0D82}\u{0D9A}\u{0DCF}
        ", &[
            Tt::Id("_"), Tt::Id("$"), Tt::Id("_$"), Tt::Id("$_"), Tt::Id("_1"), Tt::Id("$1"),
            Tt::Id("ne"), Tt::Id("retur"), Tt::Id("return1"), Tt::Id("return$"), Tt::Id("into"), Tt::Id("r1"),
            Tt::Id("éüßΩπ"),
            Tt::Id("\u{0646}\u{0627}\u{0645}\u{0647}\u{200C}\u{0627}\u{06CC}"),
            Tt::Id("\u{0D26}\u{0D43}\u{0D15}\u{0D4D}\u{200C}\u{0D38}\u{0D3E}\u{0D15}\u{0D4D}\u{0D37}\u{0D3F}"),
            Tt::Id("\u{0DC1}\u{0DCA}\u{200D}\u{0DBB}\u{0DD3}\u{0DBD}\u{0D82}\u{0D9A}\u{0DCF}"),
        ])
    }

    #[test]
    fn test_punctuators() {
        lex_test(r#"
            {()[]
            ....;,
            <><=>=
            ==!= ===!==
            +-*%**
            ++--
            <<>>>>>
            &|^
            !~
            &&||
            ?:
            =+=-=*=%=**=<<=>>=>>>=&=|=^=
            =>
            }
            a/b
            c/=d
        "#, &[
            Tt::Lbrace, Tt::Lparen, Tt::Rparen, Tt::Lbracket, Tt::Rbracket,
            Tt::DotDotDot, Tt::Dot, Tt::Semi, Tt::Comma,
            Tt::Lt, Tt::Gt, Tt::LtEq, Tt::GtEq,
            Tt::EqEq, Tt::BangEq, Tt::EqEqEq, Tt::BangEqEq,
            Tt::Plus, Tt::Minus, Tt::Star, Tt::Percent, Tt::StarStar,
            Tt::PlusPlus, Tt::MinusMinus,
            Tt::LtLt, Tt::GtGtGt, Tt::GtGt,
            Tt::And, Tt::Or, Tt::Circumflex,
            Tt::Bang, Tt::Tilde,
            Tt::AndAnd, Tt::OrOr,
            Tt::Question, Tt::Colon,
            Tt::Eq, Tt::PlusEq, Tt::MinusEq, Tt::StarEq, Tt::PercentEq, Tt::StarStarEq, Tt::LtLtEq, Tt::GtGtEq, Tt::GtGtGtEq, Tt::AndEq, Tt::OrEq, Tt::CircumflexEq,
            Tt::EqGt,
            Tt::Rbrace,

            Tt::Id("a"),
            Tt::Slash,
            Tt::Id("b"),

            Tt::Id("c"),
            Tt::SlashEq,
            Tt::Id("d"),
        ]);
    }

    #[test]
    fn test_comments() {
        lex_test(r#"
            /* multiline
            comment */
            //// single line comment //\
            /**a*b***/
            test
        "#, &[
            Tt::Id("test")
        ]);
    }

    #[test]
    fn test_dec_lits() {
        lex_test(r#"
            // DecimalLiteral
            0
            123456789
            0.
            123.
            .012300
            0.012300
            123.045600
            .123e0
            0.123e0
            123.456e0
            .123e01
            0.123e01
            123.456e02
            .123e+123
            0.123e+123
            123.456e+234
            .123e-123
            0.123e-123
            123.456e-234
            0e0
            123e0
            0e01
            123e02
            0e+123
            123e+234
            0e-123
            123e-234
        "#, &[
            Tt::NumLitDec("0"),
            Tt::NumLitDec("123456789"),
            Tt::NumLitDec("0."),
            Tt::NumLitDec("123."),
            Tt::NumLitDec(".012300"),
            Tt::NumLitDec("0.012300"),
            Tt::NumLitDec("123.045600"),
            Tt::NumLitDec(".123e0"),
            Tt::NumLitDec("0.123e0"),
            Tt::NumLitDec("123.456e0"),
            Tt::NumLitDec(".123e01"),
            Tt::NumLitDec("0.123e01"),
            Tt::NumLitDec("123.456e02"),
            Tt::NumLitDec(".123e+123"),
            Tt::NumLitDec("0.123e+123"),
            Tt::NumLitDec("123.456e+234"),
            Tt::NumLitDec(".123e-123"),
            Tt::NumLitDec("0.123e-123"),
            Tt::NumLitDec("123.456e-234"),
            Tt::NumLitDec("0e0"),
            Tt::NumLitDec("123e0"),
            Tt::NumLitDec("0e01"),
            Tt::NumLitDec("123e02"),
            Tt::NumLitDec("0e+123"),
            Tt::NumLitDec("123e+234"),
            Tt::NumLitDec("0e-123"),
            Tt::NumLitDec("123e-234"),
        ]);
    }

    #[test]
    fn test_hex_lits() {
        lex_test(r#"
            // HexIntegerLiteral
            0x0
            0x1
            0xa
            0xF
            0xABCDEF0123abcdef456789
            0X0
            0X1
            0Xa
            0XF
            0XABCDEF0123abcdef456789
        "#, &[
            Tt::NumLitHex("0x0"),
            Tt::NumLitHex("0x1"),
            Tt::NumLitHex("0xa"),
            Tt::NumLitHex("0xF"),
            Tt::NumLitHex("0xABCDEF0123abcdef456789"),
            Tt::NumLitHex("0X0"),
            Tt::NumLitHex("0X1"),
            Tt::NumLitHex("0Xa"),
            Tt::NumLitHex("0XF"),
            Tt::NumLitHex("0XABCDEF0123abcdef456789"),
        ]);
    }

    #[test]
    fn test_oct_lits() {
        lex_test(r#"
            // OctIntegerLiteral
            0o0
            0o1
            0o7
            0o01234567
            0O0
            0O1
            0O7
            0O01234567
        "#, &[
            Tt::NumLitOct("0o0"),
            Tt::NumLitOct("0o1"),
            Tt::NumLitOct("0o7"),
            Tt::NumLitOct("0o01234567"),
            Tt::NumLitOct("0O0"),
            Tt::NumLitOct("0O1"),
            Tt::NumLitOct("0O7"),
            Tt::NumLitOct("0O01234567"),
        ]);
    }

    #[test]
    fn test_bin_lits() {
        lex_test(r#"
            // BinIntegerLiteral
            0b0
            0b1
            0b10101000001
            0B0
            0B1
            0B10101000001
        "#, &[
            Tt::NumLitBin("0b0"),
            Tt::NumLitBin("0b1"),
            Tt::NumLitBin("0b10101000001"),
            Tt::NumLitBin("0B0"),
            Tt::NumLitBin("0B1"),
            Tt::NumLitBin("0B10101000001"),
        ]);
    }

    #[test]
    fn test_str_lits() {
        lex_test(r#"
            ""
            "a"
            "'"
            "\"\\\"\\\\"
            ''
            'a'
            '"'
            '\'\\\'\\\\'
        "#, &[
            Tt::StrLitDbl(r#""""#),
            Tt::StrLitDbl(r#""a""#),
            Tt::StrLitDbl(r#""'""#),
            Tt::StrLitDbl(r#""\"\\\"\\\\""#),
            Tt::StrLitSgl(r#"''"#),
            Tt::StrLitSgl(r#"'a'"#),
            Tt::StrLitSgl(r#"'"'"#),
            Tt::StrLitSgl(r#"'\'\\\'\\\\'"#),
        ]);
    }

    #[test]
    fn test_continuations() {
        lex_test(" \"a\\\u{000A}b\" ", &[ Tt::StrLitDbl("\"a\\\u{000A}b\"") ]);
        lex_test(" \"a\\\u{000D}b\" ", &[ Tt::StrLitDbl("\"a\\\u{000D}b\"") ]);
        lex_test(" \"a\\\u{000D}\u{000A}b\" ", &[ Tt::StrLitDbl("\"a\\\u{000D}\u{000A}b\"") ]);
        lex_test(" \"a\\\u{2028}b\" ", &[ Tt::StrLitDbl("\"a\\\u{2028}b\"") ]);
        lex_test(" \"a\\\u{2029}b\" ", &[ Tt::StrLitDbl("\"a\\\u{2029}b\"") ]);

        lex_test(" 'a\\\u{000A}b' ", &[ Tt::StrLitSgl("'a\\\u{000A}b'") ]);
        lex_test(" 'a\\\u{000D}b' ", &[ Tt::StrLitSgl("'a\\\u{000D}b'") ]);
        lex_test(" 'a\\\u{000D}\u{000A}b' ", &[ Tt::StrLitSgl("'a\\\u{000D}\u{000A}b'") ]);
        lex_test(" 'a\\\u{2028}b' ", &[ Tt::StrLitSgl("'a\\\u{2028}b'") ]);
        lex_test(" 'a\\\u{2029}b' ", &[ Tt::StrLitSgl("'a\\\u{2029}b'") ]);

        lex_test(" `a\\\u{000A}b` ", &[ Tt::TemplateNoSub("`a\\\u{000A}b`") ]);
        lex_test(" `a\\\u{000D}b` ", &[ Tt::TemplateNoSub("`a\\\u{000D}b`") ]);
        lex_test(" `a\\\u{000D}\u{000A}b` ", &[ Tt::TemplateNoSub("`a\\\u{000D}\u{000A}b`") ]);
        lex_test(" `a\\\u{2028}b` ", &[ Tt::TemplateNoSub("`a\\\u{2028}b`") ]);
        lex_test(" `a\\\u{2029}b` ", &[ Tt::TemplateNoSub("`a\\\u{2029}b`") ]);

        lex_test(" `a\\\u{000A}b${}c\\\u{000A}d${}e\\\u{000A}f` ", &[
            Tt::TemplateStart("`a\\\u{000A}b${"),
            Tt::TemplateMiddle("}c\\\u{000A}d${"),
            Tt::TemplateEnd("}e\\\u{000A}f`"),
        ]);
        lex_test(" `a\\\u{000D}b${}c\\\u{000D}d${}e\\\u{000D}f` ", &[
            Tt::TemplateStart("`a\\\u{000D}b${"),
            Tt::TemplateMiddle("}c\\\u{000D}d${"),
            Tt::TemplateEnd("}e\\\u{000D}f`"),
        ]);
        lex_test(" `a\\\u{000D}\u{000A}b${}c\\\u{000D}\u{000A}d${}e\\\u{000D}\u{000A}f` ", &[
            Tt::TemplateStart("`a\\\u{000D}\u{000A}b${"),
            Tt::TemplateMiddle("}c\\\u{000D}\u{000A}d${"),
            Tt::TemplateEnd("}e\\\u{000D}\u{000A}f`"),
        ]);
        lex_test(" `a\\\u{2028}b${}c\\\u{2028}d${}e\\\u{2028}f` ", &[
            Tt::TemplateStart("`a\\\u{2028}b${"),
            Tt::TemplateMiddle("}c\\\u{2028}d${"),
            Tt::TemplateEnd("}e\\\u{2028}f`"),
        ]);
        lex_test(" `a\\\u{2029}b${}c\\\u{2029}d${}e\\\u{2029}f` ", &[
            Tt::TemplateStart("`a\\\u{2029}b${"),
            Tt::TemplateMiddle("}c\\\u{2029}d${"),
            Tt::TemplateEnd("}e\\\u{2029}f`"),
        ]);

        lex_test(" /a\\\u{000A}b/ ", &[ Tt::RegExpLit("/a\\\u{000A}b/", "") ]);
        lex_test(" /a\\\u{000D}b/ ", &[ Tt::RegExpLit("/a\\\u{000D}b/", "") ]);
        lex_test(" /a\\\u{000D}\u{000A}b/ ", &[ Tt::RegExpLit("/a\\\u{000D}\u{000A}b/", "") ]);
        lex_test(" /a\\\u{2028}b/ ", &[ Tt::RegExpLit("/a\\\u{2028}b/", "") ]);
        lex_test(" /a\\\u{2029}b/ ", &[ Tt::RegExpLit("/a\\\u{2029}b/", "") ]);
    }

    #[test]
    fn test_str_lits_invalid() {
        lex_test_invalid(" \"\u{000A}\" ");
        lex_test_invalid(" \"\u{000D}\" ");
        lex_test_invalid(" \"\u{2028}\" ");
        lex_test_invalid(" \"\u{2029}\" ");
        lex_test_invalid(" '\u{000A}' ");
        lex_test_invalid(" '\u{000D}' ");
        lex_test_invalid(" '\u{2028}' ");
        lex_test_invalid(" '\u{2029}' ");
    }

    #[test]
    fn test_templates_no_subs() {
        lex_test(r#"
            ``
            `test`
            `\``
            `\\\`\\\\`
            `\${}`
            `$\{}`
        "#, &[
            Tt::TemplateNoSub(r#"``"#),
            Tt::TemplateNoSub(r#"`test`"#),
            Tt::TemplateNoSub(r#"`\``"#),
            Tt::TemplateNoSub(r#"`\\\`\\\\`"#),
            Tt::TemplateNoSub(r#"`\${}`"#),
            Tt::TemplateNoSub(r#"`$\{}`"#),
        ]);
    }

    #[test]
    fn test_templates_with_subs() {
        lex_test(r#"
            `${}`
            `${{{{a}}}}`
            `text\\${}more${or}less`
            `nesting${`templates`}is${{`cool${ish}.${}`}}o`
        "#, &[
            Tt::TemplateStart(r#"`${"#),
            Tt::TemplateEnd(r#"}`"#),

            Tt::TemplateStart(r#"`${"#),
            Tt::Lbrace,
            Tt::Lbrace,
            Tt::Lbrace,
            Tt::Id("a"),
            Tt::Rbrace,
            Tt::Rbrace,
            Tt::Rbrace,
            Tt::TemplateEnd(r#"}`"#),

            Tt::TemplateStart(r#"`text\\${"#),
            Tt::TemplateMiddle(r#"}more${"#),
            Tt::Id("or"),
            Tt::TemplateEnd(r#"}less`"#),

            Tt::TemplateStart(r#"`nesting${"#),
            Tt::TemplateNoSub(r#"`templates`"#),
            Tt::TemplateMiddle(r#"}is${"#),
            Tt::Lbrace,
            Tt::TemplateStart(r#"`cool${"#),
            Tt::Id("ish"),
            Tt::TemplateMiddle(r#"}.${"#),
            Tt::TemplateEnd(r#"}`"#),
            Tt::Rbrace,
            Tt::TemplateEnd(r#"}o`"#),
        ]);
    }

    #[test]
    fn test_regexp() {
        lex_test(r#"
            ;/abc/def
            ;/[/]\//i
        "#, &[
            Tt::Semi, Tt::RegExpLit(r#"/abc/def"#, "def"),
            Tt::Semi, Tt::RegExpLit(r#"/[/]\//i"#, "i"),
        ]);
    }

    #[test]
    fn test_slash_vs_regexp() {
        lex_test(r#"
            ;/re/+ /re/
            ;/re// /re/
            ;//comment/
            ;/re///comment
            ;a/b
            ;a/b/c
            ;``/b
            ;`${}`/b
            ;0/b
            ;0b0/b
            ;0o0/b
            ;0x0/b
            ;(/re/)
            ;{/re/}
            ;[/re/]
            ;`${/re/}`
            ;`${}${/re/}`
            ;()/c
            ;[]/a
            ;{}/re/
            ;a?/re/
            ;a:/re/
            ;a-/re/
            ;a*/re/
            ;a%/re/
            ;a/ /re/
            ;a**/re/
            ;a</re/
            ;a<=/re/
            ;a>/re/
            ;a>=/re/
            ;a==/re/
            ;a!=/re/
            ;a===/re/
            ;a!==/re/
            ;a<</re/
            ;a>>/re/
            ;a>>>/re/
            ;a&/re/
            ;a&&/re/
            ;a|/re/
            ;a||/re/
            ;a^/re/
            ;!/re/
            ;~/re/
            ;+/re/
            ;.../re/
            ;a,/re/
            ;++/re/
            ;--/re/
            ;a++/re/
            ;a--/re/
            ;a=/re/
            ;a+=/re/
            ;a-=/re/
            ;a*=/re/
            ;a%=/re/
            ;a**=/re/
            ;a<<=/re/
            ;a>>=/re/
            ;a>>>=/re/
            ;a&=/re/
            ;a|=/re/
            ;a^=/re/
            ;a=>/re/
            ;await/re/
            ;case/re/
            ;delete/re/
            ;export default /re/
            ;extends/re/
            ;in/re/
            ;instanceof/re/
            ;new/re/
            ;return/re/
            ;super/b
            ;switch/re/
            ;this/b
            ;throw/re/
            ;typeof/re/
            ;void/re/
            ;yield/re/
        "#, &[
            Tt::Semi,
            Tt::RegExpLit("/re/", ""),
            Tt::Plus,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::RegExpLit("/re/", ""),
            Tt::Slash,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,

            Tt::Semi,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::Id("a"),
            Tt::Slash,
            Tt::Id("b"),

            Tt::Semi,
            Tt::Id("a"),
            Tt::Slash,
            Tt::Id("b"),
            Tt::Slash,
            Tt::Id("c"),

            Tt::Semi,
            Tt::TemplateNoSub("``"),
            Tt::Slash,
            Tt::Id("b"),

            Tt::Semi,
            Tt::TemplateStart("`${"),
            Tt::TemplateEnd("}`"),
            Tt::Slash,
            Tt::Id("b"),

            Tt::Semi,
            Tt::NumLitDec("0"),
            Tt::Slash,
            Tt::Id("b"),

            Tt::Semi,
            Tt::NumLitBin("0b0"),
            Tt::Slash,
            Tt::Id("b"),

            Tt::Semi,
            Tt::NumLitOct("0o0"),
            Tt::Slash,
            Tt::Id("b"),

            Tt::Semi,
            Tt::NumLitHex("0x0"),
            Tt::Slash,
            Tt::Id("b"),

            Tt::Semi,
            Tt::Lparen,
            Tt::RegExpLit("/re/", ""),
            Tt::Rparen,

            Tt::Semi,
            Tt::Lbrace,
            Tt::RegExpLit("/re/", ""),
            Tt::Rbrace,

            Tt::Semi,
            Tt::Lbracket,
            Tt::RegExpLit("/re/", ""),
            Tt::Rbracket,

            Tt::Semi,
            Tt::TemplateStart("`${"),
            Tt::RegExpLit("/re/", ""),
            Tt::TemplateEnd("}`"),

            Tt::Semi,
            Tt::TemplateStart("`${"),
            Tt::TemplateMiddle("}${"),
            Tt::RegExpLit("/re/", ""),
            Tt::TemplateEnd("}`"),

            Tt::Semi,
            Tt::Lparen,
            Tt::Rparen,
            Tt::Slash,
            Tt::Id("c"),

            Tt::Semi,
            Tt::Lbracket,
            Tt::Rbracket,
            Tt::Slash,
            Tt::Id("a"),

            Tt::Semi,
            Tt::Lbrace,
            Tt::Rbrace,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::Id("a"),
            Tt::Question,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::Id("a"),
            Tt::Colon,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::Id("a"),
            Tt::Minus,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::Id("a"),
            Tt::Star,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::Id("a"),
            Tt::Percent,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::Id("a"),
            Tt::Slash,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::Id("a"),
            Tt::StarStar,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::Id("a"),
            Tt::Lt,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::Id("a"),
            Tt::LtEq,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::Id("a"),
            Tt::Gt,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::Id("a"),
            Tt::GtEq,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::Id("a"),
            Tt::EqEq,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::Id("a"),
            Tt::BangEq,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::Id("a"),
            Tt::EqEqEq,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::Id("a"),
            Tt::BangEqEq,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::Id("a"),
            Tt::LtLt,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::Id("a"),
            Tt::GtGt,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::Id("a"),
            Tt::GtGtGt,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::Id("a"),
            Tt::And,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::Id("a"),
            Tt::AndAnd,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::Id("a"),
            Tt::Or,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::Id("a"),
            Tt::OrOr,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::Id("a"),
            Tt::Circumflex,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::Bang,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::Tilde,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::Plus,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::DotDotDot,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::Id("a"),
            Tt::Comma,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::PlusPlus,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::MinusMinus,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::Id("a"),
            Tt::PlusPlus,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::Id("a"),
            Tt::MinusMinus,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::Id("a"),
            Tt::Eq,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::Id("a"),
            Tt::PlusEq,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::Id("a"),
            Tt::MinusEq,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::Id("a"),
            Tt::StarEq,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::Id("a"),
            Tt::PercentEq,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::Id("a"),
            Tt::StarStarEq,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::Id("a"),
            Tt::LtLtEq,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::Id("a"),
            Tt::GtGtEq,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::Id("a"),
            Tt::GtGtGtEq,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::Id("a"),
            Tt::AndEq,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::Id("a"),
            Tt::OrEq,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::Id("a"),
            Tt::CircumflexEq,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::Id("a"),
            Tt::EqGt,
            Tt::RegExpLit("/re/", ""),


            Tt::Semi,
            Tt::Await,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::Case,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::Delete,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::Export,
            Tt::Default,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::Extends,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::In,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::Instanceof,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::New,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::Return,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::Super,
            Tt::Slash,
            Tt::Id("b"),

            Tt::Semi,
            Tt::Switch,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::This,
            Tt::Slash,
            Tt::Id("b"),

            Tt::Semi,
            Tt::Throw,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::Typeof,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::Void,
            Tt::RegExpLit("/re/", ""),

            Tt::Semi,
            Tt::Yield,
            Tt::RegExpLit("/re/", ""),
        ]);
    }

    #[test]
    fn test_str_lit_value_basic() {
        assert_eq!(str_lit_value(r"''"), Ok(Cow::Borrowed("")));
        assert_eq!(str_lit_value(r"'c'"), Ok(Cow::Borrowed("c")));
    }

    #[test]
    fn test_str_lit_value_continuations() {
        assert_eq!(str_lit_value("'a\\\nb'"), Ok(Cow::Owned("ab".to_owned())));
    }

    #[test]
    fn test_str_lit_value_escapes() {
        assert_eq!(str_lit_value(r"'\b'"), Ok(Cow::Owned("\u{0008}".to_owned())));
        assert_eq!(str_lit_value(r"'\t'"), Ok(Cow::Owned("\u{0009}".to_owned())));
        assert_eq!(str_lit_value(r"'\n'"), Ok(Cow::Owned("\u{000A}".to_owned())));
        assert_eq!(str_lit_value(r"'\v'"), Ok(Cow::Owned("\u{000B}".to_owned())));
        assert_eq!(str_lit_value(r"'\f'"), Ok(Cow::Owned("\u{000C}".to_owned())));
        assert_eq!(str_lit_value(r"'\r'"), Ok(Cow::Owned("\u{000D}".to_owned())));
        assert_eq!(str_lit_value(r#"'\"'"#), Ok(Cow::Owned("\"".to_owned())));
        assert_eq!(str_lit_value(r"'\''"), Ok(Cow::Owned("'".to_owned())));
        assert_eq!(str_lit_value(r"'\\'"), Ok(Cow::Owned("\\".to_owned())));
        assert_eq!(str_lit_value(r"'\x65'"), Ok(Cow::Owned("e".to_owned())));
    }

    cfg_if! {
        if #[cfg(feature = "bench")] {
            fn bench_lex_file(b: &mut test::Bencher, filename: &str) {
                let mut file = fs::File::open(filename).unwrap();
                let mut contents = String::new();
                file.read_to_string(&mut contents).unwrap();

                b.iter(|| {
                    let mut lexer = Lexer::new_unnamed(&contents);
                    loop {
                        if lexer.advance().tt == Tt::Eof {
                            break
                        }
                    }
                });
                b.bytes = contents.len() as u64;
            }

            #[bench]
            fn bench_angular_lex(b: &mut test::Bencher) {
                bench_lex_file(b, "data/angular-1.2.5.js");
            }

            #[bench]
            #[ignore]
            fn bench_big_lex(b: &mut test::Bencher) {
                // 5,013,820
                bench_lex_file(b, "private/big.js");
            }

            #[bench]
            #[ignore]
            fn bench_big_stream(b: &mut test::Bencher) {
                // 5013820
                let mut file = fs::File::open("private/big.js").unwrap();
                let mut contents = String::new();
                file.read_to_string(&mut contents).unwrap();

                b.iter(|| {
                    let mut stream = PosStream::new(&contents);
                    while let Some(_) = stream.here() {
                        stream.advance();
                    }
                });
                b.bytes = contents.len() as u64;
            }
        }
    }
}
