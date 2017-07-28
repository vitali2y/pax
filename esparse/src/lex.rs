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

use std::{char, mem, fmt};
use std::borrow::Cow;
use memchr;

use ast::{Span, Loc};

/// A token (an atomic parsing unit).
///
/// Tokens have a [type](#structfield.tt) represented by the [`Tt`](enum.Tt.html) enumeration. They also have [location information](#structfield.span) and track the [whitespace and comments](#structfield.ws_before) that appeared before them in the source code.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Tok<'f, 's> {
    /// The token type.
    pub tt: Tt<'s>,
    /// The source region this token covers.
    pub span: Span<'f>,
    /// Any whitespace and comments that appeared directly before this token.
    pub ws_before: &'s str,
    /// `true` if [`ws_before`](#structfield.ws_before) contains a line terminator.
    pub nl_before: bool,
}

impl<'f, 's> Tok<'f, 's> {
    /// Creates a new `Token` with no preceding whitespace.
    pub fn new(tt: Tt<'s>, span: Span<'f>) -> Self {
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

    /// A template without substitutions (e.g., `\`template\``).
    ///
    /// The slice includes both backticks.
    TemplateNoSub(&'s str),
    /// The first literal part of a template with substitutions (e.g., `\`template${`).
    ///
    /// The slice includes the opening backtick and the `$[` sigil.
    TemplateStart(&'s str),
    /// A middle part of a template with substitutions (e.g., `}template${`).
    ///
    /// The slice includes the `}` and `$[` sigils.
    TemplateMiddle(&'s str),
    /// A middle part of a template with substitutions (e.g., `}template\``).
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
}

impl<'s> Tt<'s> {
    /// The source code slice that this token covers.
    pub fn source(&self) -> &'s str {
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
        }
    }
}

impl<'s> fmt::Display for Tt<'s> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(self.source())
    }
}

// TODO better errors
/// Parses a string literal and extracts its value, stripping the leading and trailing quotation marks and interpreting escape sequences.
///
/// If the slice given contains no escape sequences or line continuations, a subslice is return and nothing is allocated.
///
/// # Errors
///
/// Returns [`ParseStrLitError::InvalidEscape`](enum.ParseStrLitError.html#variant.InvalidEscape) if the given source slice is syntactically invalid.
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
            return Err(ParseStrLitError::InvalidEscape)
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
                    return Err(ParseStrLitError::InvalidEscape)
                }

                let hex = &range[last_pos..end_pos];
                let code_point = u8::from_str_radix(hex, 16)
                .map_err(|_| ParseStrLitError::InvalidEscape)?;
                result.push(code_point as char);

                last_pos = end_pos;
            }
            b'u' => {
                match bytes.get(last_pos) {
                    Some(&b'{') => {
                        let l_pos = last_pos + 1;
                        let r_pos = memchr::memchr(b'}', &bytes[l_pos..])
                        .ok_or(ParseStrLitError::InvalidEscape)?;

                        let hex = &range[l_pos..r_pos];
                        let code_point = u32::from_str_radix(hex, 16)
                        .map_err(|_| ParseStrLitError::InvalidEscape)?;

                        let ch = char::from_u32(code_point)
                        .ok_or(ParseStrLitError::InvalidEscape)?;
                        result.push(ch);

                        last_pos = r_pos + 1;
                    }
                    _ => {
                        let end_pos = last_pos + 4;
                        if end_pos > len {
                            return Err(ParseStrLitError::InvalidEscape)
                        }

                        let hex = &range[last_pos..end_pos];
                        let code_point = u32::from_str_radix(hex, 16)
                        .map_err(|_| ParseStrLitError::InvalidEscape)?;

                        let ch = char::from_u32(code_point)
                        .ok_or(ParseStrLitError::InvalidEscape)?;
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
pub enum ParseStrLitError {
    /// The string literal contains an invalid escape sequence.
    ///
    /// Note that <code>\\<var>c</var></code>, where <var>c</var> is not a [SingleEscapeCharacter](https://tc39.github.io/ecma262/#prod-SingleEscapeCharacter), is *not* an invalid escape sequence. For example, `"\a"` is a valid string literal with the value `"a"`.
    InvalidEscape,
}

impl fmt::Display for ParseStrLitError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ParseStrLitError::InvalidEscape => f.write_str("invalid escape"),
        }
    }
}

/// A lexical analyzer for JavaScript source code.
#[derive(Debug)]
pub struct Lexer<'f, 's> {
    file_name: &'f str,
    stream: Stream<'s>,
    here: Tok<'f, 's>,
    frame: LexFrame,
    stack: Vec<LexFrame>,
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

    #[inline(always)]
    fn read_tok(&mut self) -> Tok<'f, 's> {
        let (ws_before, nl_before) = self.stream.skip_ws();

        let start = self.stream.loc();
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
                                    result = Tt::TemplateEnd(self.stream.str_from(start.pos));
                                    break
                                }
                                Some('$') => {
                                    // TODO subopt
                                    if self.stream.eat('{') {
                                        result = Tt::TemplateMiddle(self.stream.str_from(start.pos));
                                        break
                                    }
                                }
                                Some(_) => {}
                                None => {
                                    panic!("unterminated template literal")
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
                        panic!("unmatched }")
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
                            result = Tt::TemplateNoSub(self.stream.str_from(start.pos));
                            break
                        }
                        Some('$') => {
                            // TODO subopt
                            if self.stream.eat('{') {
                                self.stack.push(mem::replace(&mut self.frame, LexFrame::Template));
                                result = Tt::TemplateStart(self.stream.str_from(start.pos));
                                break
                            }
                        }
                        Some(_) => {}
                        None => {
                            panic!("unterminated template literal")
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
                        => {
                            panic!("unterminated string literal")
                        }
                        Some(_) => {}
                        None => {
                            panic!("unterminated string literal")
                        }
                    }
                }
                Tt::StrLitDbl(self.stream.str_from(start.pos))
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
                        => {
                            panic!("unterminated string literal")
                        }
                        Some(_) => {}
                        None => {
                            panic!("unterminated string literal")
                        }
                    }
                }
                Tt::StrLitSgl(self.stream.str_from(start.pos))
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
                                            => {
                                                panic!("unterminated regexp literal")
                                            }
                                            Some(_) => {}
                                            None => {
                                                panic!("unterminated regexp literal")
                                            }
                                        }
                                    }
                                }
                                  Some('\u{000A}') // LINE FEED (LF)          <LF>
                                | Some('\u{000D}') // CARRIAGE RETURN (CR)    <CR>
                                | Some('\u{2028}') // LINE SEPARATOR          <LS>
                                | Some('\u{2029}') // PARAGRAPH SEPARATOR     <PS>
                                => {
                                    panic!("unterminated regexp literal")
                                }
                                Some(_) => {}
                                None => {
                                    panic!("unterminated regexp literal")
                                }
                            }
                        }
                        let flags_start = self.stream.loc().pos;
                        self.stream.skip_id_continue_chars();

                        let source = self.stream.str_from(start.pos);
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
                                    panic!("expected exponent")
                                },
                            ),
                            _ => {},
                        );
                        Tt::NumLitDec(self.stream.str_from(start.pos))
                    }
                    Some(_) | None => {
                        Tt::Dot
                    }
                }
            }
            '0' => eat_s!(self.stream,
                'b' | 'B' => {
                    self.stream.skip_bin_digits();
                    Tt::NumLitBin(self.stream.str_from(start.pos))
                },
                'o' | 'O' => {
                    self.stream.skip_oct_digits();
                    Tt::NumLitOct(self.stream.str_from(start.pos))
                },
                'x' | 'X' => {
                    self.stream.skip_hex_digits();
                    Tt::NumLitHex(self.stream.str_from(start.pos))
                },
                '.' => {
                    self.stream.skip_dec_digits();
                    eat_s!(self.stream,
                        'e' | 'E' => eat_s!(self.stream,
                            '-' | '+' | '0'...'9' => {
                                self.stream.skip_dec_digits();
                                Tt::NumLitDec(self.stream.str_from(start.pos))
                            },
                            _ => {
                                panic!("expected exponent")
                            },
                        ),
                        _ => {
                            Tt::NumLitDec(self.stream.str_from(start.pos))
                        },
                    )
                },
                'e' | 'E' => eat_s!(self.stream,
                    '-' | '+' | '0'...'9' => {
                        self.stream.skip_dec_digits();
                        Tt::NumLitDec(self.stream.str_from(start.pos))
                    },
                    _ => {
                        panic!("expected exponent")
                    },
                ),
                _ => Tt::NumLitDec(self.stream.str_from(start.pos)),
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
                                    panic!("expected exponent")
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
                            panic!("expected exponent")
                        },
                    ),
                    _ => {},
                );
                Tt::NumLitDec(self.stream.str_from(start.pos))
            }

            // TODO '\\' |
              '$'
            | '_'
            // ID_Start:
                | '\u{0041}'...'\u{005A}'    // LATIN CAPITAL LETTER A...LATIN CAPITAL LETTER Z
                | '\u{0061}'...'\u{007A}'    // LATIN SMALL LETTER A...LATIN SMALL LETTER Z
                | '\u{00AA}'                 // FEMININE ORDINAL INDICATOR
                | '\u{00B5}'                 // MICRO SIGN
                | '\u{00BA}'                 // MASCULINE ORDINAL INDICATOR
                | '\u{00C0}'...'\u{00D6}'    // LATIN CAPITAL LETTER A WITH GRAVE...LATIN CAPITAL LETTER O WITH DIAERESIS
                | '\u{00D8}'...'\u{00F6}'    // LATIN CAPITAL LETTER O WITH STROKE...LATIN SMALL LETTER O WITH DIAERESIS
                | '\u{00F8}'...'\u{01BA}'    // LATIN SMALL LETTER O WITH STROKE...LATIN SMALL LETTER EZH WITH TAIL
                | '\u{01BB}'                 // LATIN LETTER TWO WITH STROKE
                | '\u{01BC}'...'\u{01BF}'    // LATIN CAPITAL LETTER TONE FIVE...LATIN LETTER WYNN
                | '\u{01C0}'...'\u{01C3}'    // LATIN LETTER DENTAL CLICK...LATIN LETTER RETROFLEX CLICK
                | '\u{01C4}'...'\u{0293}'    // LATIN CAPITAL LETTER DZ WITH CARON...LATIN SMALL LETTER EZH WITH CURL
                | '\u{0294}'                 // LATIN LETTER GLOTTAL STOP
                | '\u{0295}'...'\u{02AF}'    // LATIN LETTER PHARYNGEAL VOICED FRICATIVE...LATIN SMALL LETTER TURNED H WITH FISHHOOK AND TAIL
                | '\u{02B0}'...'\u{02C1}'    // MODIFIER LETTER SMALL H...MODIFIER LETTER REVERSED GLOTTAL STOP
                | '\u{02C6}'...'\u{02D1}'    // MODIFIER LETTER CIRCUMFLEX ACCENT...MODIFIER LETTER HALF TRIANGULAR COLON
                | '\u{02E0}'...'\u{02E4}'    // MODIFIER LETTER SMALL GAMMA...MODIFIER LETTER SMALL REVERSED GLOTTAL STOP
                | '\u{02EC}'                 // MODIFIER LETTER VOICING
                | '\u{02EE}'                 // MODIFIER LETTER DOUBLE APOSTROPHE
                | '\u{0370}'...'\u{0373}'    // GREEK CAPITAL LETTER HETA...GREEK SMALL LETTER ARCHAIC SAMPI
                | '\u{0374}'                 // GREEK NUMERAL SIGN
                | '\u{0376}'...'\u{0377}'    // GREEK CAPITAL LETTER PAMPHYLIAN DIGAMMA...GREEK SMALL LETTER PAMPHYLIAN DIGAMMA
                | '\u{037A}'                 // GREEK YPOGEGRAMMENI
                | '\u{037B}'...'\u{037D}'    // GREEK SMALL REVERSED LUNATE SIGMA SYMBOL...GREEK SMALL REVERSED DOTTED LUNATE SIGMA SYMBOL
                | '\u{037F}'                 // GREEK CAPITAL LETTER YOT
                | '\u{0386}'                 // GREEK CAPITAL LETTER ALPHA WITH TONOS
                | '\u{0388}'...'\u{038A}'    // GREEK CAPITAL LETTER EPSILON WITH TONOS...GREEK CAPITAL LETTER IOTA WITH TONOS
                | '\u{038C}'                 // GREEK CAPITAL LETTER OMICRON WITH TONOS
                | '\u{038E}'...'\u{03A1}'    // GREEK CAPITAL LETTER UPSILON WITH TONOS...GREEK CAPITAL LETTER RHO
                | '\u{03A3}'...'\u{03F5}'    // GREEK CAPITAL LETTER SIGMA...GREEK LUNATE EPSILON SYMBOL
                | '\u{03F7}'...'\u{0481}'    // GREEK CAPITAL LETTER SHO...CYRILLIC SMALL LETTER KOPPA
                | '\u{048A}'...'\u{052F}'    // CYRILLIC CAPITAL LETTER SHORT I WITH TAIL...CYRILLIC SMALL LETTER EL WITH DESCENDER
                | '\u{0531}'...'\u{0556}'    // ARMENIAN CAPITAL LETTER AYB...ARMENIAN CAPITAL LETTER FEH
                | '\u{0559}'                 // ARMENIAN MODIFIER LETTER LEFT HALF RING
                | '\u{0561}'...'\u{0587}'    // ARMENIAN SMALL LETTER AYB...ARMENIAN SMALL LIGATURE ECH YIWN
                | '\u{05D0}'...'\u{05EA}'    // HEBREW LETTER ALEF...HEBREW LETTER TAV
                | '\u{05F0}'...'\u{05F2}'    // HEBREW LIGATURE YIDDISH DOUBLE VAV...HEBREW LIGATURE YIDDISH DOUBLE YOD
                | '\u{0620}'...'\u{063F}'    // ARABIC LETTER KASHMIRI YEH...ARABIC LETTER FARSI YEH WITH THREE DOTS ABOVE
                | '\u{0640}'                 // ARABIC TATWEEL
                | '\u{0641}'...'\u{064A}'    // ARABIC LETTER FEH...ARABIC LETTER YEH
                | '\u{066E}'...'\u{066F}'    // ARABIC LETTER DOTLESS BEH...ARABIC LETTER DOTLESS QAF
                | '\u{0671}'...'\u{06D3}'    // ARABIC LETTER ALEF WASLA...ARABIC LETTER YEH BARREE WITH HAMZA ABOVE
                | '\u{06D5}'                 // ARABIC LETTER AE
                | '\u{06E5}'...'\u{06E6}'    // ARABIC SMALL WAW...ARABIC SMALL YEH
                | '\u{06EE}'...'\u{06EF}'    // ARABIC LETTER DAL WITH INVERTED V...ARABIC LETTER REH WITH INVERTED V
                | '\u{06FA}'...'\u{06FC}'    // ARABIC LETTER SHEEN WITH DOT BELOW...ARABIC LETTER GHAIN WITH DOT BELOW
                | '\u{06FF}'                 // ARABIC LETTER HEH WITH INVERTED V
                | '\u{0710}'                 // SYRIAC LETTER ALAPH
                | '\u{0712}'...'\u{072F}'    // SYRIAC LETTER BETH...SYRIAC LETTER PERSIAN DHALATH
                | '\u{074D}'...'\u{07A5}'    // SYRIAC LETTER SOGDIAN ZHAIN...THAANA LETTER WAAVU
                | '\u{07B1}'                 // THAANA LETTER NAA
                | '\u{07CA}'...'\u{07EA}'    // NKO LETTER A...NKO LETTER JONA RA
                | '\u{07F4}'...'\u{07F5}'    // NKO HIGH TONE APOSTROPHE...NKO LOW TONE APOSTROPHE
                | '\u{07FA}'                 // NKO LAJANYALAN
                | '\u{0800}'...'\u{0815}'    // SAMARITAN LETTER ALAF...SAMARITAN LETTER TAAF
                | '\u{081A}'                 // SAMARITAN MODIFIER LETTER EPENTHETIC YUT
                | '\u{0824}'                 // SAMARITAN MODIFIER LETTER SHORT A
                | '\u{0828}'                 // SAMARITAN MODIFIER LETTER I
                | '\u{0840}'...'\u{0858}'    // MANDAIC LETTER HALQA...MANDAIC LETTER AIN
                | '\u{0860}'...'\u{086A}'    // SYRIAC LETTER MALAYALAM NGA...SYRIAC LETTER MALAYALAM SSA
                | '\u{08A0}'...'\u{08B4}'    // ARABIC LETTER BEH WITH SMALL V BELOW...ARABIC LETTER KAF WITH DOT BELOW
                | '\u{08B6}'...'\u{08BD}'    // ARABIC LETTER BEH WITH SMALL MEEM ABOVE...ARABIC LETTER AFRICAN NOON
                | '\u{0904}'...'\u{0939}'    // DEVANAGARI LETTER SHORT A...DEVANAGARI LETTER HA
                | '\u{093D}'                 // DEVANAGARI SIGN AVAGRAHA
                | '\u{0950}'                 // DEVANAGARI OM
                | '\u{0958}'...'\u{0961}'    // DEVANAGARI LETTER QA...DEVANAGARI LETTER VOCALIC LL
                | '\u{0971}'                 // DEVANAGARI SIGN HIGH SPACING DOT
                | '\u{0972}'...'\u{0980}'    // DEVANAGARI LETTER CANDRA A...BENGALI ANJI
                | '\u{0985}'...'\u{098C}'    // BENGALI LETTER A...BENGALI LETTER VOCALIC L
                | '\u{098F}'...'\u{0990}'    // BENGALI LETTER E...BENGALI LETTER AI
                | '\u{0993}'...'\u{09A8}'    // BENGALI LETTER O...BENGALI LETTER NA
                | '\u{09AA}'...'\u{09B0}'    // BENGALI LETTER PA...BENGALI LETTER RA
                | '\u{09B2}'                 // BENGALI LETTER LA
                | '\u{09B6}'...'\u{09B9}'    // BENGALI LETTER SHA...BENGALI LETTER HA
                | '\u{09BD}'                 // BENGALI SIGN AVAGRAHA
                | '\u{09CE}'                 // BENGALI LETTER KHANDA TA
                | '\u{09DC}'...'\u{09DD}'    // BENGALI LETTER RRA...BENGALI LETTER RHA
                | '\u{09DF}'...'\u{09E1}'    // BENGALI LETTER YYA...BENGALI LETTER VOCALIC LL
                | '\u{09F0}'...'\u{09F1}'    // BENGALI LETTER RA WITH MIDDLE DIAGONAL...BENGALI LETTER RA WITH LOWER DIAGONAL
                | '\u{09FC}'                 // BENGALI LETTER VEDIC ANUSVARA
                | '\u{0A05}'...'\u{0A0A}'    // GURMUKHI LETTER A...GURMUKHI LETTER UU
                | '\u{0A0F}'...'\u{0A10}'    // GURMUKHI LETTER EE...GURMUKHI LETTER AI
                | '\u{0A13}'...'\u{0A28}'    // GURMUKHI LETTER OO...GURMUKHI LETTER NA
                | '\u{0A2A}'...'\u{0A30}'    // GURMUKHI LETTER PA...GURMUKHI LETTER RA
                | '\u{0A32}'...'\u{0A33}'    // GURMUKHI LETTER LA...GURMUKHI LETTER LLA
                | '\u{0A35}'...'\u{0A36}'    // GURMUKHI LETTER VA...GURMUKHI LETTER SHA
                | '\u{0A38}'...'\u{0A39}'    // GURMUKHI LETTER SA...GURMUKHI LETTER HA
                | '\u{0A59}'...'\u{0A5C}'    // GURMUKHI LETTER KHHA...GURMUKHI LETTER RRA
                | '\u{0A5E}'                 // GURMUKHI LETTER FA
                | '\u{0A72}'...'\u{0A74}'    // GURMUKHI IRI...GURMUKHI EK ONKAR
                | '\u{0A85}'...'\u{0A8D}'    // GUJARATI LETTER A...GUJARATI VOWEL CANDRA E
                | '\u{0A8F}'...'\u{0A91}'    // GUJARATI LETTER E...GUJARATI VOWEL CANDRA O
                | '\u{0A93}'...'\u{0AA8}'    // GUJARATI LETTER O...GUJARATI LETTER NA
                | '\u{0AAA}'...'\u{0AB0}'    // GUJARATI LETTER PA...GUJARATI LETTER RA
                | '\u{0AB2}'...'\u{0AB3}'    // GUJARATI LETTER LA...GUJARATI LETTER LLA
                | '\u{0AB5}'...'\u{0AB9}'    // GUJARATI LETTER VA...GUJARATI LETTER HA
                | '\u{0ABD}'                 // GUJARATI SIGN AVAGRAHA
                | '\u{0AD0}'                 // GUJARATI OM
                | '\u{0AE0}'...'\u{0AE1}'    // GUJARATI LETTER VOCALIC RR...GUJARATI LETTER VOCALIC LL
                | '\u{0AF9}'                 // GUJARATI LETTER ZHA
                | '\u{0B05}'...'\u{0B0C}'    // ORIYA LETTER A...ORIYA LETTER VOCALIC L
                | '\u{0B0F}'...'\u{0B10}'    // ORIYA LETTER E...ORIYA LETTER AI
                | '\u{0B13}'...'\u{0B28}'    // ORIYA LETTER O...ORIYA LETTER NA
                | '\u{0B2A}'...'\u{0B30}'    // ORIYA LETTER PA...ORIYA LETTER RA
                | '\u{0B32}'...'\u{0B33}'    // ORIYA LETTER LA...ORIYA LETTER LLA
                | '\u{0B35}'...'\u{0B39}'    // ORIYA LETTER VA...ORIYA LETTER HA
                | '\u{0B3D}'                 // ORIYA SIGN AVAGRAHA
                | '\u{0B5C}'...'\u{0B5D}'    // ORIYA LETTER RRA...ORIYA LETTER RHA
                | '\u{0B5F}'...'\u{0B61}'    // ORIYA LETTER YYA...ORIYA LETTER VOCALIC LL
                | '\u{0B71}'                 // ORIYA LETTER WA
                | '\u{0B83}'                 // TAMIL SIGN VISARGA
                | '\u{0B85}'...'\u{0B8A}'    // TAMIL LETTER A...TAMIL LETTER UU
                | '\u{0B8E}'...'\u{0B90}'    // TAMIL LETTER E...TAMIL LETTER AI
                | '\u{0B92}'...'\u{0B95}'    // TAMIL LETTER O...TAMIL LETTER KA
                | '\u{0B99}'...'\u{0B9A}'    // TAMIL LETTER NGA...TAMIL LETTER CA
                | '\u{0B9C}'                 // TAMIL LETTER JA
                | '\u{0B9E}'...'\u{0B9F}'    // TAMIL LETTER NYA...TAMIL LETTER TTA
                | '\u{0BA3}'...'\u{0BA4}'    // TAMIL LETTER NNA...TAMIL LETTER TA
                | '\u{0BA8}'...'\u{0BAA}'    // TAMIL LETTER NA...TAMIL LETTER PA
                | '\u{0BAE}'...'\u{0BB9}'    // TAMIL LETTER MA...TAMIL LETTER HA
                | '\u{0BD0}'                 // TAMIL OM
                | '\u{0C05}'...'\u{0C0C}'    // TELUGU LETTER A...TELUGU LETTER VOCALIC L
                | '\u{0C0E}'...'\u{0C10}'    // TELUGU LETTER E...TELUGU LETTER AI
                | '\u{0C12}'...'\u{0C28}'    // TELUGU LETTER O...TELUGU LETTER NA
                | '\u{0C2A}'...'\u{0C39}'    // TELUGU LETTER PA...TELUGU LETTER HA
                | '\u{0C3D}'                 // TELUGU SIGN AVAGRAHA
                | '\u{0C58}'...'\u{0C5A}'    // TELUGU LETTER TSA...TELUGU LETTER RRRA
                | '\u{0C60}'...'\u{0C61}'    // TELUGU LETTER VOCALIC RR...TELUGU LETTER VOCALIC LL
                | '\u{0C80}'                 // KANNADA SIGN SPACING CANDRABINDU
                | '\u{0C85}'...'\u{0C8C}'    // KANNADA LETTER A...KANNADA LETTER VOCALIC L
                | '\u{0C8E}'...'\u{0C90}'    // KANNADA LETTER E...KANNADA LETTER AI
                | '\u{0C92}'...'\u{0CA8}'    // KANNADA LETTER O...KANNADA LETTER NA
                | '\u{0CAA}'...'\u{0CB3}'    // KANNADA LETTER PA...KANNADA LETTER LLA
                | '\u{0CB5}'...'\u{0CB9}'    // KANNADA LETTER VA...KANNADA LETTER HA
                | '\u{0CBD}'                 // KANNADA SIGN AVAGRAHA
                | '\u{0CDE}'                 // KANNADA LETTER FA
                | '\u{0CE0}'...'\u{0CE1}'    // KANNADA LETTER VOCALIC RR...KANNADA LETTER VOCALIC LL
                | '\u{0CF1}'...'\u{0CF2}'    // KANNADA SIGN JIHVAMULIYA...KANNADA SIGN UPADHMANIYA
                | '\u{0D05}'...'\u{0D0C}'    // MALAYALAM LETTER A...MALAYALAM LETTER VOCALIC L
                | '\u{0D0E}'...'\u{0D10}'    // MALAYALAM LETTER E...MALAYALAM LETTER AI
                | '\u{0D12}'...'\u{0D3A}'    // MALAYALAM LETTER O...MALAYALAM LETTER TTTA
                | '\u{0D3D}'                 // MALAYALAM SIGN AVAGRAHA
                | '\u{0D4E}'                 // MALAYALAM LETTER DOT REPH
                | '\u{0D54}'...'\u{0D56}'    // MALAYALAM LETTER CHILLU M...MALAYALAM LETTER CHILLU LLL
                | '\u{0D5F}'...'\u{0D61}'    // MALAYALAM LETTER ARCHAIC II...MALAYALAM LETTER VOCALIC LL
                | '\u{0D7A}'...'\u{0D7F}'    // MALAYALAM LETTER CHILLU NN...MALAYALAM LETTER CHILLU K
                | '\u{0D85}'...'\u{0D96}'    // SINHALA LETTER AYANNA...SINHALA LETTER AUYANNA
                | '\u{0D9A}'...'\u{0DB1}'    // SINHALA LETTER ALPAPRAANA KAYANNA...SINHALA LETTER DANTAJA NAYANNA
                | '\u{0DB3}'...'\u{0DBB}'    // SINHALA LETTER SANYAKA DAYANNA...SINHALA LETTER RAYANNA
                | '\u{0DBD}'                 // SINHALA LETTER DANTAJA LAYANNA
                | '\u{0DC0}'...'\u{0DC6}'    // SINHALA LETTER VAYANNA...SINHALA LETTER FAYANNA
                | '\u{0E01}'...'\u{0E30}'    // THAI CHARACTER KO KAI...THAI CHARACTER SARA A
                | '\u{0E32}'...'\u{0E33}'    // THAI CHARACTER SARA AA...THAI CHARACTER SARA AM
                | '\u{0E40}'...'\u{0E45}'    // THAI CHARACTER SARA E...THAI CHARACTER LAKKHANGYAO
                | '\u{0E46}'                 // THAI CHARACTER MAIYAMOK
                | '\u{0E81}'...'\u{0E82}'    // LAO LETTER KO...LAO LETTER KHO SUNG
                | '\u{0E84}'                 // LAO LETTER KHO TAM
                | '\u{0E87}'...'\u{0E88}'    // LAO LETTER NGO...LAO LETTER CO
                | '\u{0E8A}'                 // LAO LETTER SO TAM
                | '\u{0E8D}'                 // LAO LETTER NYO
                | '\u{0E94}'...'\u{0E97}'    // LAO LETTER DO...LAO LETTER THO TAM
                | '\u{0E99}'...'\u{0E9F}'    // LAO LETTER NO...LAO LETTER FO SUNG
                | '\u{0EA1}'...'\u{0EA3}'    // LAO LETTER MO...LAO LETTER LO LING
                | '\u{0EA5}'                 // LAO LETTER LO LOOT
                | '\u{0EA7}'                 // LAO LETTER WO
                | '\u{0EAA}'...'\u{0EAB}'    // LAO LETTER SO SUNG...LAO LETTER HO SUNG
                | '\u{0EAD}'...'\u{0EB0}'    // LAO LETTER O...LAO VOWEL SIGN A
                | '\u{0EB2}'...'\u{0EB3}'    // LAO VOWEL SIGN AA...LAO VOWEL SIGN AM
                | '\u{0EBD}'                 // LAO SEMIVOWEL SIGN NYO
                | '\u{0EC0}'...'\u{0EC4}'    // LAO VOWEL SIGN E...LAO VOWEL SIGN AI
                | '\u{0EC6}'                 // LAO KO LA
                | '\u{0EDC}'...'\u{0EDF}'    // LAO HO NO...LAO LETTER KHMU NYO
                | '\u{0F00}'                 // TIBETAN SYLLABLE OM
                | '\u{0F40}'...'\u{0F47}'    // TIBETAN LETTER KA...TIBETAN LETTER JA
                | '\u{0F49}'...'\u{0F6C}'    // TIBETAN LETTER NYA...TIBETAN LETTER RRA
                | '\u{0F88}'...'\u{0F8C}'    // TIBETAN SIGN LCE TSA CAN...TIBETAN SIGN INVERTED MCHU CAN
                | '\u{1000}'...'\u{102A}'    // MYANMAR LETTER KA...MYANMAR LETTER AU
                | '\u{103F}'                 // MYANMAR LETTER GREAT SA
                | '\u{1050}'...'\u{1055}'    // MYANMAR LETTER SHA...MYANMAR LETTER VOCALIC LL
                | '\u{105A}'...'\u{105D}'    // MYANMAR LETTER MON NGA...MYANMAR LETTER MON BBE
                | '\u{1061}'                 // MYANMAR LETTER SGAW KAREN SHA
                | '\u{1065}'...'\u{1066}'    // MYANMAR LETTER WESTERN PWO KAREN THA...MYANMAR LETTER WESTERN PWO KAREN PWA
                | '\u{106E}'...'\u{1070}'    // MYANMAR LETTER EASTERN PWO KAREN NNA...MYANMAR LETTER EASTERN PWO KAREN GHWA
                | '\u{1075}'...'\u{1081}'    // MYANMAR LETTER SHAN KA...MYANMAR LETTER SHAN HA
                | '\u{108E}'                 // MYANMAR LETTER RUMAI PALAUNG FA
                | '\u{10A0}'...'\u{10C5}'    // GEORGIAN CAPITAL LETTER AN...GEORGIAN CAPITAL LETTER HOE
                | '\u{10C7}'                 // GEORGIAN CAPITAL LETTER YN
                | '\u{10CD}'                 // GEORGIAN CAPITAL LETTER AEN
                | '\u{10D0}'...'\u{10FA}'    // GEORGIAN LETTER AN...GEORGIAN LETTER AIN
                | '\u{10FC}'                 // MODIFIER LETTER GEORGIAN NAR
                | '\u{10FD}'...'\u{1248}'    // GEORGIAN LETTER AEN...ETHIOPIC SYLLABLE QWA
                | '\u{124A}'...'\u{124D}'    // ETHIOPIC SYLLABLE QWI...ETHIOPIC SYLLABLE QWE
                | '\u{1250}'...'\u{1256}'    // ETHIOPIC SYLLABLE QHA...ETHIOPIC SYLLABLE QHO
                | '\u{1258}'                 // ETHIOPIC SYLLABLE QHWA
                | '\u{125A}'...'\u{125D}'    // ETHIOPIC SYLLABLE QHWI...ETHIOPIC SYLLABLE QHWE
                | '\u{1260}'...'\u{1288}'    // ETHIOPIC SYLLABLE BA...ETHIOPIC SYLLABLE XWA
                | '\u{128A}'...'\u{128D}'    // ETHIOPIC SYLLABLE XWI...ETHIOPIC SYLLABLE XWE
                | '\u{1290}'...'\u{12B0}'    // ETHIOPIC SYLLABLE NA...ETHIOPIC SYLLABLE KWA
                | '\u{12B2}'...'\u{12B5}'    // ETHIOPIC SYLLABLE KWI...ETHIOPIC SYLLABLE KWE
                | '\u{12B8}'...'\u{12BE}'    // ETHIOPIC SYLLABLE KXA...ETHIOPIC SYLLABLE KXO
                | '\u{12C0}'                 // ETHIOPIC SYLLABLE KXWA
                | '\u{12C2}'...'\u{12C5}'    // ETHIOPIC SYLLABLE KXWI...ETHIOPIC SYLLABLE KXWE
                | '\u{12C8}'...'\u{12D6}'    // ETHIOPIC SYLLABLE WA...ETHIOPIC SYLLABLE PHARYNGEAL O
                | '\u{12D8}'...'\u{1310}'    // ETHIOPIC SYLLABLE ZA...ETHIOPIC SYLLABLE GWA
                | '\u{1312}'...'\u{1315}'    // ETHIOPIC SYLLABLE GWI...ETHIOPIC SYLLABLE GWE
                | '\u{1318}'...'\u{135A}'    // ETHIOPIC SYLLABLE GGA...ETHIOPIC SYLLABLE FYA
                | '\u{1380}'...'\u{138F}'    // ETHIOPIC SYLLABLE SEBATBEIT MWA...ETHIOPIC SYLLABLE PWE
                | '\u{13A0}'...'\u{13F5}'    // CHEROKEE LETTER A...CHEROKEE LETTER MV
                | '\u{13F8}'...'\u{13FD}'    // CHEROKEE SMALL LETTER YE...CHEROKEE SMALL LETTER MV
                | '\u{1401}'...'\u{166C}'    // CANADIAN SYLLABICS E...CANADIAN SYLLABICS CARRIER TTSA
                | '\u{166F}'...'\u{167F}'    // CANADIAN SYLLABICS QAI...CANADIAN SYLLABICS BLACKFOOT W
                | '\u{1681}'...'\u{169A}'    // OGHAM LETTER BEITH...OGHAM LETTER PEITH
                | '\u{16A0}'...'\u{16EA}'    // RUNIC LETTER FEHU FEOH FE F...RUNIC LETTER X
                | '\u{16EE}'...'\u{16F0}'    // RUNIC ARLAUG SYMBOL...RUNIC BELGTHOR SYMBOL
                | '\u{16F1}'...'\u{16F8}'    // RUNIC LETTER K...RUNIC LETTER FRANKS CASKET AESC
                | '\u{1700}'...'\u{170C}'    // TAGALOG LETTER A...TAGALOG LETTER YA
                | '\u{170E}'...'\u{1711}'    // TAGALOG LETTER LA...TAGALOG LETTER HA
                | '\u{1720}'...'\u{1731}'    // HANUNOO LETTER A...HANUNOO LETTER HA
                | '\u{1740}'...'\u{1751}'    // BUHID LETTER A...BUHID LETTER HA
                | '\u{1760}'...'\u{176C}'    // TAGBANWA LETTER A...TAGBANWA LETTER YA
                | '\u{176E}'...'\u{1770}'    // TAGBANWA LETTER LA...TAGBANWA LETTER SA
                | '\u{1780}'...'\u{17B3}'    // KHMER LETTER KA...KHMER INDEPENDENT VOWEL QAU
                | '\u{17D7}'                 // KHMER SIGN LEK TOO
                | '\u{17DC}'                 // KHMER SIGN AVAKRAHASANYA
                | '\u{1820}'...'\u{1842}'    // MONGOLIAN LETTER A...MONGOLIAN LETTER CHI
                | '\u{1843}'                 // MONGOLIAN LETTER TODO LONG VOWEL SIGN
                | '\u{1844}'...'\u{1877}'    // MONGOLIAN LETTER TODO E...MONGOLIAN LETTER MANCHU ZHA
                | '\u{1880}'...'\u{1884}'    // MONGOLIAN LETTER ALI GALI ANUSVARA ONE...MONGOLIAN LETTER ALI GALI INVERTED UBADAMA
                | '\u{1885}'...'\u{1886}'    // MONGOLIAN LETTER ALI GALI BALUDA...MONGOLIAN LETTER ALI GALI THREE BALUDA
                | '\u{1887}'...'\u{18A8}'    // MONGOLIAN LETTER ALI GALI A...MONGOLIAN LETTER MANCHU ALI GALI BHA
                | '\u{18AA}'                 // MONGOLIAN LETTER MANCHU ALI GALI LHA
                | '\u{18B0}'...'\u{18F5}'    // CANADIAN SYLLABICS OY...CANADIAN SYLLABICS CARRIER DENTAL S
                | '\u{1900}'...'\u{191E}'    // LIMBU VOWEL-CARRIER LETTER...LIMBU LETTER TRA
                | '\u{1950}'...'\u{196D}'    // TAI LE LETTER KA...TAI LE LETTER AI
                | '\u{1970}'...'\u{1974}'    // TAI LE LETTER TONE-2...TAI LE LETTER TONE-6
                | '\u{1980}'...'\u{19AB}'    // NEW TAI LUE LETTER HIGH QA...NEW TAI LUE LETTER LOW SUA
                | '\u{19B0}'...'\u{19C9}'    // NEW TAI LUE VOWEL SIGN VOWEL SHORTENER...NEW TAI LUE TONE MARK-2
                | '\u{1A00}'...'\u{1A16}'    // BUGINESE LETTER KA...BUGINESE LETTER HA
                | '\u{1A20}'...'\u{1A54}'    // TAI THAM LETTER HIGH KA...TAI THAM LETTER GREAT SA
                | '\u{1AA7}'                 // TAI THAM SIGN MAI YAMOK
                | '\u{1B05}'...'\u{1B33}'    // BALINESE LETTER AKARA...BALINESE LETTER HA
                | '\u{1B45}'...'\u{1B4B}'    // BALINESE LETTER KAF SASAK...BALINESE LETTER ASYURA SASAK
                | '\u{1B83}'...'\u{1BA0}'    // SUNDANESE LETTER A...SUNDANESE LETTER HA
                | '\u{1BAE}'...'\u{1BAF}'    // SUNDANESE LETTER KHA...SUNDANESE LETTER SYA
                | '\u{1BBA}'...'\u{1BE5}'    // SUNDANESE AVAGRAHA...BATAK LETTER U
                | '\u{1C00}'...'\u{1C23}'    // LEPCHA LETTER KA...LEPCHA LETTER A
                | '\u{1C4D}'...'\u{1C4F}'    // LEPCHA LETTER TTA...LEPCHA LETTER DDA
                | '\u{1C5A}'...'\u{1C77}'    // OL CHIKI LETTER LA...OL CHIKI LETTER OH
                | '\u{1C78}'...'\u{1C7D}'    // OL CHIKI MU TTUDDAG...OL CHIKI AHAD
                | '\u{1C80}'...'\u{1C88}'    // CYRILLIC SMALL LETTER ROUNDED VE...CYRILLIC SMALL LETTER UNBLENDED UK
                | '\u{1CE9}'...'\u{1CEC}'    // VEDIC SIGN ANUSVARA ANTARGOMUKHA...VEDIC SIGN ANUSVARA VAMAGOMUKHA WITH TAIL
                | '\u{1CEE}'...'\u{1CF1}'    // VEDIC SIGN HEXIFORM LONG ANUSVARA...VEDIC SIGN ANUSVARA UBHAYATO MUKHA
                | '\u{1CF5}'...'\u{1CF6}'    // VEDIC SIGN JIHVAMULIYA...VEDIC SIGN UPADHMANIYA
                | '\u{1D00}'...'\u{1D2B}'    // LATIN LETTER SMALL CAPITAL A...CYRILLIC LETTER SMALL CAPITAL EL
                | '\u{1D2C}'...'\u{1D6A}'    // MODIFIER LETTER CAPITAL A...GREEK SUBSCRIPT SMALL LETTER CHI
                | '\u{1D6B}'...'\u{1D77}'    // LATIN SMALL LETTER UE...LATIN SMALL LETTER TURNED G
                | '\u{1D78}'                 // MODIFIER LETTER CYRILLIC EN
                | '\u{1D79}'...'\u{1D9A}'    // LATIN SMALL LETTER INSULAR G...LATIN SMALL LETTER EZH WITH RETROFLEX HOOK
                | '\u{1D9B}'...'\u{1DBF}'    // MODIFIER LETTER SMALL TURNED ALPHA...MODIFIER LETTER SMALL THETA
                | '\u{1E00}'...'\u{1F15}'    // LATIN CAPITAL LETTER A WITH RING BELOW...GREEK SMALL LETTER EPSILON WITH DASIA AND OXIA
                | '\u{1F18}'...'\u{1F1D}'    // GREEK CAPITAL LETTER EPSILON WITH PSILI...GREEK CAPITAL LETTER EPSILON WITH DASIA AND OXIA
                | '\u{1F20}'...'\u{1F45}'    // GREEK SMALL LETTER ETA WITH PSILI...GREEK SMALL LETTER OMICRON WITH DASIA AND OXIA
                | '\u{1F48}'...'\u{1F4D}'    // GREEK CAPITAL LETTER OMICRON WITH PSILI...GREEK CAPITAL LETTER OMICRON WITH DASIA AND OXIA
                | '\u{1F50}'...'\u{1F57}'    // GREEK SMALL LETTER UPSILON WITH PSILI...GREEK SMALL LETTER UPSILON WITH DASIA AND PERISPOMENI
                | '\u{1F59}'                 // GREEK CAPITAL LETTER UPSILON WITH DASIA
                | '\u{1F5B}'                 // GREEK CAPITAL LETTER UPSILON WITH DASIA AND VARIA
                | '\u{1F5D}'                 // GREEK CAPITAL LETTER UPSILON WITH DASIA AND OXIA
                | '\u{1F5F}'...'\u{1F7D}'    // GREEK CAPITAL LETTER UPSILON WITH DASIA AND PERISPOMENI...GREEK SMALL LETTER OMEGA WITH OXIA
                | '\u{1F80}'...'\u{1FB4}'    // GREEK SMALL LETTER ALPHA WITH PSILI AND YPOGEGRAMMENI...GREEK SMALL LETTER ALPHA WITH OXIA AND YPOGEGRAMMENI
                | '\u{1FB6}'...'\u{1FBC}'    // GREEK SMALL LETTER ALPHA WITH PERISPOMENI...GREEK CAPITAL LETTER ALPHA WITH PROSGEGRAMMENI
                | '\u{1FBE}'                 // GREEK PROSGEGRAMMENI
                | '\u{1FC2}'...'\u{1FC4}'    // GREEK SMALL LETTER ETA WITH VARIA AND YPOGEGRAMMENI...GREEK SMALL LETTER ETA WITH OXIA AND YPOGEGRAMMENI
                | '\u{1FC6}'...'\u{1FCC}'    // GREEK SMALL LETTER ETA WITH PERISPOMENI...GREEK CAPITAL LETTER ETA WITH PROSGEGRAMMENI
                | '\u{1FD0}'...'\u{1FD3}'    // GREEK SMALL LETTER IOTA WITH VRACHY...GREEK SMALL LETTER IOTA WITH DIALYTIKA AND OXIA
                | '\u{1FD6}'...'\u{1FDB}'    // GREEK SMALL LETTER IOTA WITH PERISPOMENI...GREEK CAPITAL LETTER IOTA WITH OXIA
                | '\u{1FE0}'...'\u{1FEC}'    // GREEK SMALL LETTER UPSILON WITH VRACHY...GREEK CAPITAL LETTER RHO WITH DASIA
                | '\u{1FF2}'...'\u{1FF4}'    // GREEK SMALL LETTER OMEGA WITH VARIA AND YPOGEGRAMMENI...GREEK SMALL LETTER OMEGA WITH OXIA AND YPOGEGRAMMENI
                | '\u{1FF6}'...'\u{1FFC}'    // GREEK SMALL LETTER OMEGA WITH PERISPOMENI...GREEK CAPITAL LETTER OMEGA WITH PROSGEGRAMMENI
                | '\u{2071}'                 // SUPERSCRIPT LATIN SMALL LETTER I
                | '\u{207F}'                 // SUPERSCRIPT LATIN SMALL LETTER N
                | '\u{2090}'...'\u{209C}'    // LATIN SUBSCRIPT SMALL LETTER A...LATIN SUBSCRIPT SMALL LETTER T
                | '\u{2102}'                 // DOUBLE-STRUCK CAPITAL C
                | '\u{2107}'                 // EULER CONSTANT
                | '\u{210A}'...'\u{2113}'    // SCRIPT SMALL G...SCRIPT SMALL L
                | '\u{2115}'                 // DOUBLE-STRUCK CAPITAL N
                | '\u{2118}'                 // SCRIPT CAPITAL P
                | '\u{2119}'...'\u{211D}'    // DOUBLE-STRUCK CAPITAL P...DOUBLE-STRUCK CAPITAL R
                | '\u{2124}'                 // DOUBLE-STRUCK CAPITAL Z
                | '\u{2126}'                 // OHM SIGN
                | '\u{2128}'                 // BLACK-LETTER CAPITAL Z
                | '\u{212A}'...'\u{212D}'    // KELVIN SIGN...BLACK-LETTER CAPITAL C
                | '\u{212E}'                 // ESTIMATED SYMBOL
                | '\u{212F}'...'\u{2134}'    // SCRIPT SMALL E...SCRIPT SMALL O
                | '\u{2135}'...'\u{2138}'    // ALEF SYMBOL...DALET SYMBOL
                | '\u{2139}'                 // INFORMATION SOURCE
                | '\u{213C}'...'\u{213F}'    // DOUBLE-STRUCK SMALL PI...DOUBLE-STRUCK CAPITAL PI
                | '\u{2145}'...'\u{2149}'    // DOUBLE-STRUCK ITALIC CAPITAL D...DOUBLE-STRUCK ITALIC SMALL J
                | '\u{214E}'                 // TURNED SMALL F
                | '\u{2160}'...'\u{2182}'    // ROMAN NUMERAL ONE...ROMAN NUMERAL TEN THOUSAND
                | '\u{2183}'...'\u{2184}'    // ROMAN NUMERAL REVERSED ONE HUNDRED...LATIN SMALL LETTER REVERSED C
                | '\u{2185}'...'\u{2188}'    // ROMAN NUMERAL SIX LATE FORM...ROMAN NUMERAL ONE HUNDRED THOUSAND
                | '\u{2C00}'...'\u{2C2E}'    // GLAGOLITIC CAPITAL LETTER AZU...GLAGOLITIC CAPITAL LETTER LATINATE MYSLITE
                | '\u{2C30}'...'\u{2C5E}'    // GLAGOLITIC SMALL LETTER AZU...GLAGOLITIC SMALL LETTER LATINATE MYSLITE
                | '\u{2C60}'...'\u{2C7B}'    // LATIN CAPITAL LETTER L WITH DOUBLE BAR...LATIN LETTER SMALL CAPITAL TURNED E
                | '\u{2C7C}'...'\u{2C7D}'    // LATIN SUBSCRIPT SMALL LETTER J...MODIFIER LETTER CAPITAL V
                | '\u{2C7E}'...'\u{2CE4}'    // LATIN CAPITAL LETTER S WITH SWASH TAIL...COPTIC SYMBOL KAI
                | '\u{2CEB}'...'\u{2CEE}'    // COPTIC CAPITAL LETTER CRYPTOGRAMMIC SHEI...COPTIC SMALL LETTER CRYPTOGRAMMIC GANGIA
                | '\u{2CF2}'...'\u{2CF3}'    // COPTIC CAPITAL LETTER BOHAIRIC KHEI...COPTIC SMALL LETTER BOHAIRIC KHEI
                | '\u{2D00}'...'\u{2D25}'    // GEORGIAN SMALL LETTER AN...GEORGIAN SMALL LETTER HOE
                | '\u{2D27}'                 // GEORGIAN SMALL LETTER YN
                | '\u{2D2D}'                 // GEORGIAN SMALL LETTER AEN
                | '\u{2D30}'...'\u{2D67}'    // TIFINAGH LETTER YA...TIFINAGH LETTER YO
                | '\u{2D6F}'                 // TIFINAGH MODIFIER LETTER LABIALIZATION MARK
                | '\u{2D80}'...'\u{2D96}'    // ETHIOPIC SYLLABLE LOA...ETHIOPIC SYLLABLE GGWE
                | '\u{2DA0}'...'\u{2DA6}'    // ETHIOPIC SYLLABLE SSA...ETHIOPIC SYLLABLE SSO
                | '\u{2DA8}'...'\u{2DAE}'    // ETHIOPIC SYLLABLE CCA...ETHIOPIC SYLLABLE CCO
                | '\u{2DB0}'...'\u{2DB6}'    // ETHIOPIC SYLLABLE ZZA...ETHIOPIC SYLLABLE ZZO
                | '\u{2DB8}'...'\u{2DBE}'    // ETHIOPIC SYLLABLE CCHA...ETHIOPIC SYLLABLE CCHO
                | '\u{2DC0}'...'\u{2DC6}'    // ETHIOPIC SYLLABLE QYA...ETHIOPIC SYLLABLE QYO
                | '\u{2DC8}'...'\u{2DCE}'    // ETHIOPIC SYLLABLE KYA...ETHIOPIC SYLLABLE KYO
                | '\u{2DD0}'...'\u{2DD6}'    // ETHIOPIC SYLLABLE XYA...ETHIOPIC SYLLABLE XYO
                | '\u{2DD8}'...'\u{2DDE}'    // ETHIOPIC SYLLABLE GYA...ETHIOPIC SYLLABLE GYO
                | '\u{3005}'                 // IDEOGRAPHIC ITERATION MARK
                | '\u{3006}'                 // IDEOGRAPHIC CLOSING MARK
                | '\u{3007}'                 // IDEOGRAPHIC NUMBER ZERO
                | '\u{3021}'...'\u{3029}'    // HANGZHOU NUMERAL ONE...HANGZHOU NUMERAL NINE
                | '\u{3031}'...'\u{3035}'    // VERTICAL KANA REPEAT MARK...VERTICAL KANA REPEAT MARK LOWER HALF
                | '\u{3038}'...'\u{303A}'    // HANGZHOU NUMERAL TEN...HANGZHOU NUMERAL THIRTY
                | '\u{303B}'                 // VERTICAL IDEOGRAPHIC ITERATION MARK
                | '\u{303C}'                 // MASU MARK
                | '\u{3041}'...'\u{3096}'    // HIRAGANA LETTER SMALL A...HIRAGANA LETTER SMALL KE
                | '\u{309B}'...'\u{309C}'    // KATAKANA-HIRAGANA VOICED SOUND MARK...KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK
                | '\u{309D}'...'\u{309E}'    // HIRAGANA ITERATION MARK...HIRAGANA VOICED ITERATION MARK
                | '\u{309F}'                 // HIRAGANA DIGRAPH YORI
                | '\u{30A1}'...'\u{30FA}'    // KATAKANA LETTER SMALL A...KATAKANA LETTER VO
                | '\u{30FC}'...'\u{30FE}'    // KATAKANA-HIRAGANA PROLONGED SOUND MARK...KATAKANA VOICED ITERATION MARK
                | '\u{30FF}'                 // KATAKANA DIGRAPH KOTO
                | '\u{3105}'...'\u{312E}'    // BOPOMOFO LETTER B...BOPOMOFO LETTER O WITH DOT ABOVE
                | '\u{3131}'...'\u{318E}'    // HANGUL LETTER KIYEOK...HANGUL LETTER ARAEAE
                | '\u{31A0}'...'\u{31BA}'    // BOPOMOFO LETTER BU...BOPOMOFO LETTER ZY
                | '\u{31F0}'...'\u{31FF}'    // KATAKANA LETTER SMALL KU...KATAKANA LETTER SMALL RO
                | '\u{3400}'...'\u{4DB5}'    // CJK UNIFIED IDEOGRAPH-3400...CJK UNIFIED IDEOGRAPH-4DB5
                | '\u{4E00}'...'\u{9FEA}'    // CJK UNIFIED IDEOGRAPH-4E00...CJK UNIFIED IDEOGRAPH-9FEA
                | '\u{A000}'...'\u{A014}'    // YI SYLLABLE IT...YI SYLLABLE E
                | '\u{A015}'                 // YI SYLLABLE WU
                | '\u{A016}'...'\u{A48C}'    // YI SYLLABLE BIT...YI SYLLABLE YYR
                | '\u{A4D0}'...'\u{A4F7}'    // LISU LETTER BA...LISU LETTER OE
                | '\u{A4F8}'...'\u{A4FD}'    // LISU LETTER TONE MYA TI...LISU LETTER TONE MYA JEU
                | '\u{A500}'...'\u{A60B}'    // VAI SYLLABLE EE...VAI SYLLABLE NG
                | '\u{A60C}'                 // VAI SYLLABLE LENGTHENER
                | '\u{A610}'...'\u{A61F}'    // VAI SYLLABLE NDOLE FA...VAI SYMBOL JONG
                | '\u{A62A}'...'\u{A62B}'    // VAI SYLLABLE NDOLE MA...VAI SYLLABLE NDOLE DO
                | '\u{A640}'...'\u{A66D}'    // CYRILLIC CAPITAL LETTER ZEMLYA...CYRILLIC SMALL LETTER DOUBLE MONOCULAR O
                | '\u{A66E}'                 // CYRILLIC LETTER MULTIOCULAR O
                | '\u{A67F}'                 // CYRILLIC PAYEROK
                | '\u{A680}'...'\u{A69B}'    // CYRILLIC CAPITAL LETTER DWE...CYRILLIC SMALL LETTER CROSSED O
                | '\u{A69C}'...'\u{A69D}'    // MODIFIER LETTER CYRILLIC HARD SIGN...MODIFIER LETTER CYRILLIC SOFT SIGN
                | '\u{A6A0}'...'\u{A6E5}'    // BAMUM LETTER A...BAMUM LETTER KI
                | '\u{A6E6}'...'\u{A6EF}'    // BAMUM LETTER MO...BAMUM LETTER KOGHOM
                | '\u{A717}'...'\u{A71F}'    // MODIFIER LETTER DOT VERTICAL BAR...MODIFIER LETTER LOW INVERTED EXCLAMATION MARK
                | '\u{A722}'...'\u{A76F}'    // LATIN CAPITAL LETTER EGYPTOLOGICAL ALEF...LATIN SMALL LETTER CON
                | '\u{A770}'                 // MODIFIER LETTER US
                | '\u{A771}'...'\u{A787}'    // LATIN SMALL LETTER DUM...LATIN SMALL LETTER INSULAR T
                | '\u{A788}'                 // MODIFIER LETTER LOW CIRCUMFLEX ACCENT
                | '\u{A78B}'...'\u{A78E}'    // LATIN CAPITAL LETTER SALTILLO...LATIN SMALL LETTER L WITH RETROFLEX HOOK AND BELT
                | '\u{A78F}'                 // LATIN LETTER SINOLOGICAL DOT
                | '\u{A790}'...'\u{A7AE}'    // LATIN CAPITAL LETTER N WITH DESCENDER...LATIN CAPITAL LETTER SMALL CAPITAL I
                | '\u{A7B0}'...'\u{A7B7}'    // LATIN CAPITAL LETTER TURNED K...LATIN SMALL LETTER OMEGA
                | '\u{A7F7}'                 // LATIN EPIGRAPHIC LETTER SIDEWAYS I
                | '\u{A7F8}'...'\u{A7F9}'    // MODIFIER LETTER CAPITAL H WITH STROKE...MODIFIER LETTER SMALL LIGATURE OE
                | '\u{A7FA}'                 // LATIN LETTER SMALL CAPITAL TURNED M
                | '\u{A7FB}'...'\u{A801}'    // LATIN EPIGRAPHIC LETTER REVERSED F...SYLOTI NAGRI LETTER I
                | '\u{A803}'...'\u{A805}'    // SYLOTI NAGRI LETTER U...SYLOTI NAGRI LETTER O
                | '\u{A807}'...'\u{A80A}'    // SYLOTI NAGRI LETTER KO...SYLOTI NAGRI LETTER GHO
                | '\u{A80C}'...'\u{A822}'    // SYLOTI NAGRI LETTER CO...SYLOTI NAGRI LETTER HO
                | '\u{A840}'...'\u{A873}'    // PHAGS-PA LETTER KA...PHAGS-PA LETTER CANDRABINDU
                | '\u{A882}'...'\u{A8B3}'    // SAURASHTRA LETTER A...SAURASHTRA LETTER LLA
                | '\u{A8F2}'...'\u{A8F7}'    // DEVANAGARI SIGN SPACING CANDRABINDU...DEVANAGARI SIGN CANDRABINDU AVAGRAHA
                | '\u{A8FB}'                 // DEVANAGARI HEADSTROKE
                | '\u{A8FD}'                 // DEVANAGARI JAIN OM
                | '\u{A90A}'...'\u{A925}'    // KAYAH LI LETTER KA...KAYAH LI LETTER OO
                | '\u{A930}'...'\u{A946}'    // REJANG LETTER KA...REJANG LETTER A
                | '\u{A960}'...'\u{A97C}'    // HANGUL CHOSEONG TIKEUT-MIEUM...HANGUL CHOSEONG SSANGYEORINHIEUH
                | '\u{A984}'...'\u{A9B2}'    // JAVANESE LETTER A...JAVANESE LETTER HA
                | '\u{A9CF}'                 // JAVANESE PANGRANGKEP
                | '\u{A9E0}'...'\u{A9E4}'    // MYANMAR LETTER SHAN GHA...MYANMAR LETTER SHAN BHA
                | '\u{A9E6}'                 // MYANMAR MODIFIER LETTER SHAN REDUPLICATION
                | '\u{A9E7}'...'\u{A9EF}'    // MYANMAR LETTER TAI LAING NYA...MYANMAR LETTER TAI LAING NNA
                | '\u{A9FA}'...'\u{A9FE}'    // MYANMAR LETTER TAI LAING LLA...MYANMAR LETTER TAI LAING BHA
                | '\u{AA00}'...'\u{AA28}'    // CHAM LETTER A...CHAM LETTER HA
                | '\u{AA40}'...'\u{AA42}'    // CHAM LETTER FINAL K...CHAM LETTER FINAL NG
                | '\u{AA44}'...'\u{AA4B}'    // CHAM LETTER FINAL CH...CHAM LETTER FINAL SS
                | '\u{AA60}'...'\u{AA6F}'    // MYANMAR LETTER KHAMTI GA...MYANMAR LETTER KHAMTI FA
                | '\u{AA70}'                 // MYANMAR MODIFIER LETTER KHAMTI REDUPLICATION
                | '\u{AA71}'...'\u{AA76}'    // MYANMAR LETTER KHAMTI XA...MYANMAR LOGOGRAM KHAMTI HM
                | '\u{AA7A}'                 // MYANMAR LETTER AITON RA
                | '\u{AA7E}'...'\u{AAAF}'    // MYANMAR LETTER SHWE PALAUNG CHA...TAI VIET LETTER HIGH O
                | '\u{AAB1}'                 // TAI VIET VOWEL AA
                | '\u{AAB5}'...'\u{AAB6}'    // TAI VIET VOWEL E...TAI VIET VOWEL O
                | '\u{AAB9}'...'\u{AABD}'    // TAI VIET VOWEL UEA...TAI VIET VOWEL AN
                | '\u{AAC0}'                 // TAI VIET TONE MAI NUENG
                | '\u{AAC2}'                 // TAI VIET TONE MAI SONG
                | '\u{AADB}'...'\u{AADC}'    // TAI VIET SYMBOL KON...TAI VIET SYMBOL NUENG
                | '\u{AADD}'                 // TAI VIET SYMBOL SAM
                | '\u{AAE0}'...'\u{AAEA}'    // MEETEI MAYEK LETTER E...MEETEI MAYEK LETTER SSA
                | '\u{AAF2}'                 // MEETEI MAYEK ANJI
                | '\u{AAF3}'...'\u{AAF4}'    // MEETEI MAYEK SYLLABLE REPETITION MARK...MEETEI MAYEK WORD REPETITION MARK
                | '\u{AB01}'...'\u{AB06}'    // ETHIOPIC SYLLABLE TTHU...ETHIOPIC SYLLABLE TTHO
                | '\u{AB09}'...'\u{AB0E}'    // ETHIOPIC SYLLABLE DDHU...ETHIOPIC SYLLABLE DDHO
                | '\u{AB11}'...'\u{AB16}'    // ETHIOPIC SYLLABLE DZU...ETHIOPIC SYLLABLE DZO
                | '\u{AB20}'...'\u{AB26}'    // ETHIOPIC SYLLABLE CCHHA...ETHIOPIC SYLLABLE CCHHO
                | '\u{AB28}'...'\u{AB2E}'    // ETHIOPIC SYLLABLE BBA...ETHIOPIC SYLLABLE BBO
                | '\u{AB30}'...'\u{AB5A}'    // LATIN SMALL LETTER BARRED ALPHA...LATIN SMALL LETTER Y WITH SHORT RIGHT LEG
                | '\u{AB5C}'...'\u{AB5F}'    // MODIFIER LETTER SMALL HENG...MODIFIER LETTER SMALL U WITH LEFT HOOK
                | '\u{AB60}'...'\u{AB65}'    // LATIN SMALL LETTER SAKHA YAT...GREEK LETTER SMALL CAPITAL OMEGA
                | '\u{AB70}'...'\u{ABBF}'    // CHEROKEE SMALL LETTER A...CHEROKEE SMALL LETTER YA
                | '\u{ABC0}'...'\u{ABE2}'    // MEETEI MAYEK LETTER KOK...MEETEI MAYEK LETTER I LONSUM
                | '\u{AC00}'...'\u{D7A3}'    // HANGUL SYLLABLE GA...HANGUL SYLLABLE HIH
                | '\u{D7B0}'...'\u{D7C6}'    // HANGUL JUNGSEONG O-YEO...HANGUL JUNGSEONG ARAEA-E
                | '\u{D7CB}'...'\u{D7FB}'    // HANGUL JONGSEONG NIEUN-RIEUL...HANGUL JONGSEONG PHIEUPH-THIEUTH
                | '\u{F900}'...'\u{FA6D}'    // CJK COMPATIBILITY IDEOGRAPH-F900...CJK COMPATIBILITY IDEOGRAPH-FA6D
                | '\u{FA70}'...'\u{FAD9}'    // CJK COMPATIBILITY IDEOGRAPH-FA70...CJK COMPATIBILITY IDEOGRAPH-FAD9
                | '\u{FB00}'...'\u{FB06}'    // LATIN SMALL LIGATURE FF...LATIN SMALL LIGATURE ST
                | '\u{FB13}'...'\u{FB17}'    // ARMENIAN SMALL LIGATURE MEN NOW...ARMENIAN SMALL LIGATURE MEN XEH
                | '\u{FB1D}'                 // HEBREW LETTER YOD WITH HIRIQ
                | '\u{FB1F}'...'\u{FB28}'    // HEBREW LIGATURE YIDDISH YOD YOD PATAH...HEBREW LETTER WIDE TAV
                | '\u{FB2A}'...'\u{FB36}'    // HEBREW LETTER SHIN WITH SHIN DOT...HEBREW LETTER ZAYIN WITH DAGESH
                | '\u{FB38}'...'\u{FB3C}'    // HEBREW LETTER TET WITH DAGESH...HEBREW LETTER LAMED WITH DAGESH
                | '\u{FB3E}'                 // HEBREW LETTER MEM WITH DAGESH
                | '\u{FB40}'...'\u{FB41}'    // HEBREW LETTER NUN WITH DAGESH...HEBREW LETTER SAMEKH WITH DAGESH
                | '\u{FB43}'...'\u{FB44}'    // HEBREW LETTER FINAL PE WITH DAGESH...HEBREW LETTER PE WITH DAGESH
                | '\u{FB46}'...'\u{FBB1}'    // HEBREW LETTER TSADI WITH DAGESH...ARABIC LETTER YEH BARREE WITH HAMZA ABOVE FINAL FORM
                | '\u{FBD3}'...'\u{FD3D}'    // ARABIC LETTER NG ISOLATED FORM...ARABIC LIGATURE ALEF WITH FATHATAN ISOLATED FORM
                | '\u{FD50}'...'\u{FD8F}'    // ARABIC LIGATURE TEH WITH JEEM WITH MEEM INITIAL FORM...ARABIC LIGATURE MEEM WITH KHAH WITH MEEM INITIAL FORM
                | '\u{FD92}'...'\u{FDC7}'    // ARABIC LIGATURE MEEM WITH JEEM WITH KHAH INITIAL FORM...ARABIC LIGATURE NOON WITH JEEM WITH YEH FINAL FORM
                | '\u{FDF0}'...'\u{FDFB}'    // ARABIC LIGATURE SALLA USED AS KORANIC STOP SIGN ISOLATED FORM...ARABIC LIGATURE JALLAJALALOUHOU
                | '\u{FE70}'...'\u{FE74}'    // ARABIC FATHATAN ISOLATED FORM...ARABIC KASRATAN ISOLATED FORM
                | '\u{FE76}'...'\u{FEFC}'    // ARABIC FATHA ISOLATED FORM...ARABIC LIGATURE LAM WITH ALEF FINAL FORM
                | '\u{FF21}'...'\u{FF3A}'    // FULLWIDTH LATIN CAPITAL LETTER A...FULLWIDTH LATIN CAPITAL LETTER Z
                | '\u{FF41}'...'\u{FF5A}'    // FULLWIDTH LATIN SMALL LETTER A...FULLWIDTH LATIN SMALL LETTER Z
                | '\u{FF66}'...'\u{FF6F}'    // HALFWIDTH KATAKANA LETTER WO...HALFWIDTH KATAKANA LETTER SMALL TU
                | '\u{FF70}'                 // HALFWIDTH KATAKANA-HIRAGANA PROLONGED SOUND MARK
                | '\u{FF71}'...'\u{FF9D}'    // HALFWIDTH KATAKANA LETTER A...HALFWIDTH KATAKANA LETTER N
                | '\u{FF9E}'...'\u{FF9F}'    // HALFWIDTH KATAKANA VOICED SOUND MARK...HALFWIDTH KATAKANA SEMI-VOICED SOUND MARK
                | '\u{FFA0}'...'\u{FFBE}'    // HALFWIDTH HANGUL FILLER...HALFWIDTH HANGUL LETTER HIEUH
                | '\u{FFC2}'...'\u{FFC7}'    // HALFWIDTH HANGUL LETTER A...HALFWIDTH HANGUL LETTER E
                | '\u{FFCA}'...'\u{FFCF}'    // HALFWIDTH HANGUL LETTER YEO...HALFWIDTH HANGUL LETTER OE
                | '\u{FFD2}'...'\u{FFD7}'    // HALFWIDTH HANGUL LETTER YO...HALFWIDTH HANGUL LETTER YU
                | '\u{FFDA}'...'\u{FFDC}'    // HALFWIDTH HANGUL LETTER EU...HALFWIDTH HANGUL LETTER I
                | '\u{10000}'...'\u{1000B}'  // LINEAR B SYLLABLE B008 A...LINEAR B SYLLABLE B046 JE
                | '\u{1000D}'...'\u{10026}'  // LINEAR B SYLLABLE B036 JO...LINEAR B SYLLABLE B032 QO
                | '\u{10028}'...'\u{1003A}'  // LINEAR B SYLLABLE B060 RA...LINEAR B SYLLABLE B042 WO
                | '\u{1003C}'...'\u{1003D}'  // LINEAR B SYLLABLE B017 ZA...LINEAR B SYLLABLE B074 ZE
                | '\u{1003F}'...'\u{1004D}'  // LINEAR B SYLLABLE B020 ZO...LINEAR B SYLLABLE B091 TWO
                | '\u{10050}'...'\u{1005D}'  // LINEAR B SYMBOL B018...LINEAR B SYMBOL B089
                | '\u{10080}'...'\u{100FA}'  // LINEAR B IDEOGRAM B100 MAN...LINEAR B IDEOGRAM VESSEL B305
                | '\u{10140}'...'\u{10174}'  // GREEK ACROPHONIC ATTIC ONE QUARTER...GREEK ACROPHONIC STRATIAN FIFTY MNAS
                | '\u{10280}'...'\u{1029C}'  // LYCIAN LETTER A...LYCIAN LETTER X
                | '\u{102A0}'...'\u{102D0}'  // CARIAN LETTER A...CARIAN LETTER UUU3
                | '\u{10300}'...'\u{1031F}'  // OLD ITALIC LETTER A...OLD ITALIC LETTER ESS
                | '\u{1032D}'...'\u{10340}'  // OLD ITALIC LETTER YE...GOTHIC LETTER PAIRTHRA
                | '\u{10341}'                // GOTHIC LETTER NINETY
                | '\u{10342}'...'\u{10349}'  // GOTHIC LETTER RAIDA...GOTHIC LETTER OTHAL
                | '\u{1034A}'                // GOTHIC LETTER NINE HUNDRED
                | '\u{10350}'...'\u{10375}'  // OLD PERMIC LETTER AN...OLD PERMIC LETTER IA
                | '\u{10380}'...'\u{1039D}'  // UGARITIC LETTER ALPA...UGARITIC LETTER SSU
                | '\u{103A0}'...'\u{103C3}'  // OLD PERSIAN SIGN A...OLD PERSIAN SIGN HA
                | '\u{103C8}'...'\u{103CF}'  // OLD PERSIAN SIGN AURAMAZDAA...OLD PERSIAN SIGN BUUMISH
                | '\u{103D1}'...'\u{103D5}'  // OLD PERSIAN NUMBER ONE...OLD PERSIAN NUMBER HUNDRED
                | '\u{10400}'...'\u{1044F}'  // DESERET CAPITAL LETTER LONG I...DESERET SMALL LETTER EW
                | '\u{10450}'...'\u{1049D}'  // SHAVIAN LETTER PEEP...OSMANYA LETTER OO
                | '\u{104B0}'...'\u{104D3}'  // OSAGE CAPITAL LETTER A...OSAGE CAPITAL LETTER ZHA
                | '\u{104D8}'...'\u{104FB}'  // OSAGE SMALL LETTER A...OSAGE SMALL LETTER ZHA
                | '\u{10500}'...'\u{10527}'  // ELBASAN LETTER A...ELBASAN LETTER KHE
                | '\u{10530}'...'\u{10563}'  // CAUCASIAN ALBANIAN LETTER ALT...CAUCASIAN ALBANIAN LETTER KIW
                | '\u{10600}'...'\u{10736}'  // LINEAR A SIGN AB001...LINEAR A SIGN A664
                | '\u{10740}'...'\u{10755}'  // LINEAR A SIGN A701 A...LINEAR A SIGN A732 JE
                | '\u{10760}'...'\u{10767}'  // LINEAR A SIGN A800...LINEAR A SIGN A807
                | '\u{10800}'...'\u{10805}'  // CYPRIOT SYLLABLE A...CYPRIOT SYLLABLE JA
                | '\u{10808}'                // CYPRIOT SYLLABLE JO
                | '\u{1080A}'...'\u{10835}'  // CYPRIOT SYLLABLE KA...CYPRIOT SYLLABLE WO
                | '\u{10837}'...'\u{10838}'  // CYPRIOT SYLLABLE XA...CYPRIOT SYLLABLE XE
                | '\u{1083C}'                // CYPRIOT SYLLABLE ZA
                | '\u{1083F}'...'\u{10855}'  // CYPRIOT SYLLABLE ZO...IMPERIAL ARAMAIC LETTER TAW
                | '\u{10860}'...'\u{10876}'  // PALMYRENE LETTER ALEPH...PALMYRENE LETTER TAW
                | '\u{10880}'...'\u{1089E}'  // NABATAEAN LETTER FINAL ALEPH...NABATAEAN LETTER TAW
                | '\u{108E0}'...'\u{108F2}'  // HATRAN LETTER ALEPH...HATRAN LETTER QOPH
                | '\u{108F4}'...'\u{108F5}'  // HATRAN LETTER SHIN...HATRAN LETTER TAW
                | '\u{10900}'...'\u{10915}'  // PHOENICIAN LETTER ALF...PHOENICIAN LETTER TAU
                | '\u{10920}'...'\u{10939}'  // LYDIAN LETTER A...LYDIAN LETTER C
                | '\u{10980}'...'\u{109B7}'  // MEROITIC HIEROGLYPHIC LETTER A...MEROITIC CURSIVE LETTER DA
                | '\u{109BE}'...'\u{109BF}'  // MEROITIC CURSIVE LOGOGRAM RMT...MEROITIC CURSIVE LOGOGRAM IMN
                | '\u{10A00}'                // KHAROSHTHI LETTER A
                | '\u{10A10}'...'\u{10A13}'  // KHAROSHTHI LETTER KA...KHAROSHTHI LETTER GHA
                | '\u{10A15}'...'\u{10A17}'  // KHAROSHTHI LETTER CA...KHAROSHTHI LETTER JA
                | '\u{10A19}'...'\u{10A33}'  // KHAROSHTHI LETTER NYA...KHAROSHTHI LETTER TTTHA
                | '\u{10A60}'...'\u{10A7C}'  // OLD SOUTH ARABIAN LETTER HE...OLD SOUTH ARABIAN LETTER THETH
                | '\u{10A80}'...'\u{10A9C}'  // OLD NORTH ARABIAN LETTER HEH...OLD NORTH ARABIAN LETTER ZAH
                | '\u{10AC0}'...'\u{10AC7}'  // MANICHAEAN LETTER ALEPH...MANICHAEAN LETTER WAW
                | '\u{10AC9}'...'\u{10AE4}'  // MANICHAEAN LETTER ZAYIN...MANICHAEAN LETTER TAW
                | '\u{10B00}'...'\u{10B35}'  // AVESTAN LETTER A...AVESTAN LETTER HE
                | '\u{10B40}'...'\u{10B55}'  // INSCRIPTIONAL PARTHIAN LETTER ALEPH...INSCRIPTIONAL PARTHIAN LETTER TAW
                | '\u{10B60}'...'\u{10B72}'  // INSCRIPTIONAL PAHLAVI LETTER ALEPH...INSCRIPTIONAL PAHLAVI LETTER TAW
                | '\u{10B80}'...'\u{10B91}'  // PSALTER PAHLAVI LETTER ALEPH...PSALTER PAHLAVI LETTER TAW
                | '\u{10C00}'...'\u{10C48}'  // OLD TURKIC LETTER ORKHON A...OLD TURKIC LETTER ORKHON BASH
                | '\u{10C80}'...'\u{10CB2}'  // OLD HUNGARIAN CAPITAL LETTER A...OLD HUNGARIAN CAPITAL LETTER US
                | '\u{10CC0}'...'\u{10CF2}'  // OLD HUNGARIAN SMALL LETTER A...OLD HUNGARIAN SMALL LETTER US
                | '\u{11003}'...'\u{11037}'  // BRAHMI SIGN JIHVAMULIYA...BRAHMI LETTER OLD TAMIL NNNA
                | '\u{11083}'...'\u{110AF}'  // KAITHI LETTER A...KAITHI LETTER HA
                | '\u{110D0}'...'\u{110E8}'  // SORA SOMPENG LETTER SAH...SORA SOMPENG LETTER MAE
                | '\u{11103}'...'\u{11126}'  // CHAKMA LETTER AA...CHAKMA LETTER HAA
                | '\u{11150}'...'\u{11172}'  // MAHAJANI LETTER A...MAHAJANI LETTER RRA
                | '\u{11176}'                // MAHAJANI LIGATURE SHRI
                | '\u{11183}'...'\u{111B2}'  // SHARADA LETTER A...SHARADA LETTER HA
                | '\u{111C1}'...'\u{111C4}'  // SHARADA SIGN AVAGRAHA...SHARADA OM
                | '\u{111DA}'                // SHARADA EKAM
                | '\u{111DC}'                // SHARADA HEADSTROKE
                | '\u{11200}'...'\u{11211}'  // KHOJKI LETTER A...KHOJKI LETTER JJA
                | '\u{11213}'...'\u{1122B}'  // KHOJKI LETTER NYA...KHOJKI LETTER LLA
                | '\u{11280}'...'\u{11286}'  // MULTANI LETTER A...MULTANI LETTER GA
                | '\u{11288}'                // MULTANI LETTER GHA
                | '\u{1128A}'...'\u{1128D}'  // MULTANI LETTER CA...MULTANI LETTER JJA
                | '\u{1128F}'...'\u{1129D}'  // MULTANI LETTER NYA...MULTANI LETTER BA
                | '\u{1129F}'...'\u{112A8}'  // MULTANI LETTER BHA...MULTANI LETTER RHA
                | '\u{112B0}'...'\u{112DE}'  // KHUDAWADI LETTER A...KHUDAWADI LETTER HA
                | '\u{11305}'...'\u{1130C}'  // GRANTHA LETTER A...GRANTHA LETTER VOCALIC L
                | '\u{1130F}'...'\u{11310}'  // GRANTHA LETTER EE...GRANTHA LETTER AI
                | '\u{11313}'...'\u{11328}'  // GRANTHA LETTER OO...GRANTHA LETTER NA
                | '\u{1132A}'...'\u{11330}'  // GRANTHA LETTER PA...GRANTHA LETTER RA
                | '\u{11332}'...'\u{11333}'  // GRANTHA LETTER LA...GRANTHA LETTER LLA
                | '\u{11335}'...'\u{11339}'  // GRANTHA LETTER VA...GRANTHA LETTER HA
                | '\u{1133D}'                // GRANTHA SIGN AVAGRAHA
                | '\u{11350}'                // GRANTHA OM
                | '\u{1135D}'...'\u{11361}'  // GRANTHA SIGN PLUTA...GRANTHA LETTER VOCALIC LL
                | '\u{11400}'...'\u{11434}'  // NEWA LETTER A...NEWA LETTER HA
                | '\u{11447}'...'\u{1144A}'  // NEWA SIGN AVAGRAHA...NEWA SIDDHI
                | '\u{11480}'...'\u{114AF}'  // TIRHUTA ANJI...TIRHUTA LETTER HA
                | '\u{114C4}'...'\u{114C5}'  // TIRHUTA SIGN AVAGRAHA...TIRHUTA GVANG
                | '\u{114C7}'                // TIRHUTA OM
                | '\u{11580}'...'\u{115AE}'  // SIDDHAM LETTER A...SIDDHAM LETTER HA
                | '\u{115D8}'...'\u{115DB}'  // SIDDHAM LETTER THREE-CIRCLE ALTERNATE I...SIDDHAM LETTER ALTERNATE U
                | '\u{11600}'...'\u{1162F}'  // MODI LETTER A...MODI LETTER LLA
                | '\u{11644}'                // MODI SIGN HUVA
                | '\u{11680}'...'\u{116AA}'  // TAKRI LETTER A...TAKRI LETTER RRA
                | '\u{11700}'...'\u{11719}'  // AHOM LETTER KA...AHOM LETTER JHA
                | '\u{118A0}'...'\u{118DF}'  // WARANG CITI CAPITAL LETTER NGAA...WARANG CITI SMALL LETTER VIYO
                | '\u{118FF}'                // WARANG CITI OM
                | '\u{11A00}'                // ZANABAZAR SQUARE LETTER A
                | '\u{11A0B}'...'\u{11A32}'  // ZANABAZAR SQUARE LETTER KA...ZANABAZAR SQUARE LETTER KSSA
                | '\u{11A3A}'                // ZANABAZAR SQUARE CLUSTER-INITIAL LETTER RA
                | '\u{11A50}'                // SOYOMBO LETTER A
                | '\u{11A5C}'...'\u{11A83}'  // SOYOMBO LETTER KA...SOYOMBO LETTER KSSA
                | '\u{11A86}'...'\u{11A89}'  // SOYOMBO CLUSTER-INITIAL LETTER RA...SOYOMBO CLUSTER-INITIAL LETTER SA
                | '\u{11AC0}'...'\u{11AF8}'  // PAU CIN HAU LETTER PA...PAU CIN HAU GLOTTAL STOP FINAL
                | '\u{11C00}'...'\u{11C08}'  // BHAIKSUKI LETTER A...BHAIKSUKI LETTER VOCALIC L
                | '\u{11C0A}'...'\u{11C2E}'  // BHAIKSUKI LETTER E...BHAIKSUKI LETTER HA
                | '\u{11C40}'                // BHAIKSUKI SIGN AVAGRAHA
                | '\u{11C72}'...'\u{11C8F}'  // MARCHEN LETTER KA...MARCHEN LETTER A
                | '\u{11D00}'...'\u{11D06}'  // MASARAM GONDI LETTER A...MASARAM GONDI LETTER E
                | '\u{11D08}'...'\u{11D09}'  // MASARAM GONDI LETTER AI...MASARAM GONDI LETTER O
                | '\u{11D0B}'...'\u{11D30}'  // MASARAM GONDI LETTER AU...MASARAM GONDI LETTER TRA
                | '\u{11D46}'                // MASARAM GONDI REPHA
                | '\u{12000}'...'\u{12399}'  // CUNEIFORM SIGN A...CUNEIFORM SIGN U U
                | '\u{12400}'...'\u{1246E}'  // CUNEIFORM NUMERIC SIGN TWO ASH...CUNEIFORM NUMERIC SIGN NINE U VARIANT FORM
                | '\u{12480}'...'\u{12543}'  // CUNEIFORM SIGN AB TIMES NUN TENU...CUNEIFORM SIGN ZU5 TIMES THREE DISH TENU
                | '\u{13000}'...'\u{1342E}'  // EGYPTIAN HIEROGLYPH A001...EGYPTIAN HIEROGLYPH AA032
                | '\u{14400}'...'\u{14646}'  // ANATOLIAN HIEROGLYPH A001...ANATOLIAN HIEROGLYPH A530
                | '\u{16800}'...'\u{16A38}'  // BAMUM LETTER PHASE-A NGKUE MFON...BAMUM LETTER PHASE-F VUEQ
                | '\u{16A40}'...'\u{16A5E}'  // MRO LETTER TA...MRO LETTER TEK
                | '\u{16AD0}'...'\u{16AED}'  // BASSA VAH LETTER ENNI...BASSA VAH LETTER I
                | '\u{16B00}'...'\u{16B2F}'  // PAHAWH HMONG VOWEL KEEB...PAHAWH HMONG CONSONANT CAU
                | '\u{16B40}'...'\u{16B43}'  // PAHAWH HMONG SIGN VOS SEEV...PAHAWH HMONG SIGN IB YAM
                | '\u{16B63}'...'\u{16B77}'  // PAHAWH HMONG SIGN VOS LUB...PAHAWH HMONG SIGN CIM NRES TOS
                | '\u{16B7D}'...'\u{16B8F}'  // PAHAWH HMONG CLAN SIGN TSHEEJ...PAHAWH HMONG CLAN SIGN VWJ
                | '\u{16F00}'...'\u{16F44}'  // MIAO LETTER PA...MIAO LETTER HHA
                | '\u{16F50}'                // MIAO LETTER NASALIZATION
                | '\u{16F93}'...'\u{16F9F}'  // MIAO LETTER TONE-2...MIAO LETTER REFORMED TONE-8
                | '\u{16FE0}'...'\u{16FE1}'  // TANGUT ITERATION MARK...NUSHU ITERATION MARK
                | '\u{17000}'...'\u{187EC}'  // TANGUT IDEOGRAPH-17000...TANGUT IDEOGRAPH-187EC
                | '\u{18800}'...'\u{18AF2}'  // TANGUT COMPONENT-001...TANGUT COMPONENT-755
                | '\u{1B000}'...'\u{1B11E}'  // KATAKANA LETTER ARCHAIC E...HENTAIGANA LETTER N-MU-MO-2
                | '\u{1B170}'...'\u{1B2FB}'  // NUSHU CHARACTER-1B170...NUSHU CHARACTER-1B2FB
                | '\u{1BC00}'...'\u{1BC6A}'  // DUPLOYAN LETTER H...DUPLOYAN LETTER VOCALIC M
                | '\u{1BC70}'...'\u{1BC7C}'  // DUPLOYAN AFFIX LEFT HORIZONTAL SECANT...DUPLOYAN AFFIX ATTACHED TANGENT HOOK
                | '\u{1BC80}'...'\u{1BC88}'  // DUPLOYAN AFFIX HIGH ACUTE...DUPLOYAN AFFIX HIGH VERTICAL
                | '\u{1BC90}'...'\u{1BC99}'  // DUPLOYAN AFFIX LOW ACUTE...DUPLOYAN AFFIX LOW ARROW
                | '\u{1D400}'...'\u{1D454}'  // MATHEMATICAL BOLD CAPITAL A...MATHEMATICAL ITALIC SMALL G
                | '\u{1D456}'...'\u{1D49C}'  // MATHEMATICAL ITALIC SMALL I...MATHEMATICAL SCRIPT CAPITAL A
                | '\u{1D49E}'...'\u{1D49F}'  // MATHEMATICAL SCRIPT CAPITAL C...MATHEMATICAL SCRIPT CAPITAL D
                | '\u{1D4A2}'                // MATHEMATICAL SCRIPT CAPITAL G
                | '\u{1D4A5}'...'\u{1D4A6}'  // MATHEMATICAL SCRIPT CAPITAL J...MATHEMATICAL SCRIPT CAPITAL K
                | '\u{1D4A9}'...'\u{1D4AC}'  // MATHEMATICAL SCRIPT CAPITAL N...MATHEMATICAL SCRIPT CAPITAL Q
                | '\u{1D4AE}'...'\u{1D4B9}'  // MATHEMATICAL SCRIPT CAPITAL S...MATHEMATICAL SCRIPT SMALL D
                | '\u{1D4BB}'                // MATHEMATICAL SCRIPT SMALL F
                | '\u{1D4BD}'...'\u{1D4C3}'  // MATHEMATICAL SCRIPT SMALL H...MATHEMATICAL SCRIPT SMALL N
                | '\u{1D4C5}'...'\u{1D505}'  // MATHEMATICAL SCRIPT SMALL P...MATHEMATICAL FRAKTUR CAPITAL B
                | '\u{1D507}'...'\u{1D50A}'  // MATHEMATICAL FRAKTUR CAPITAL D...MATHEMATICAL FRAKTUR CAPITAL G
                | '\u{1D50D}'...'\u{1D514}'  // MATHEMATICAL FRAKTUR CAPITAL J...MATHEMATICAL FRAKTUR CAPITAL Q
                | '\u{1D516}'...'\u{1D51C}'  // MATHEMATICAL FRAKTUR CAPITAL S...MATHEMATICAL FRAKTUR CAPITAL Y
                | '\u{1D51E}'...'\u{1D539}'  // MATHEMATICAL FRAKTUR SMALL A...MATHEMATICAL DOUBLE-STRUCK CAPITAL B
                | '\u{1D53B}'...'\u{1D53E}'  // MATHEMATICAL DOUBLE-STRUCK CAPITAL D...MATHEMATICAL DOUBLE-STRUCK CAPITAL G
                | '\u{1D540}'...'\u{1D544}'  // MATHEMATICAL DOUBLE-STRUCK CAPITAL I...MATHEMATICAL DOUBLE-STRUCK CAPITAL M
                | '\u{1D546}'                // MATHEMATICAL DOUBLE-STRUCK CAPITAL O
                | '\u{1D54A}'...'\u{1D550}'  // MATHEMATICAL DOUBLE-STRUCK CAPITAL S...MATHEMATICAL DOUBLE-STRUCK CAPITAL Y
                | '\u{1D552}'...'\u{1D6A5}'  // MATHEMATICAL DOUBLE-STRUCK SMALL A...MATHEMATICAL ITALIC SMALL DOTLESS J
                | '\u{1D6A8}'...'\u{1D6C0}'  // MATHEMATICAL BOLD CAPITAL ALPHA...MATHEMATICAL BOLD CAPITAL OMEGA
                | '\u{1D6C2}'...'\u{1D6DA}'  // MATHEMATICAL BOLD SMALL ALPHA...MATHEMATICAL BOLD SMALL OMEGA
                | '\u{1D6DC}'...'\u{1D6FA}'  // MATHEMATICAL BOLD EPSILON SYMBOL...MATHEMATICAL ITALIC CAPITAL OMEGA
                | '\u{1D6FC}'...'\u{1D714}'  // MATHEMATICAL ITALIC SMALL ALPHA...MATHEMATICAL ITALIC SMALL OMEGA
                | '\u{1D716}'...'\u{1D734}'  // MATHEMATICAL ITALIC EPSILON SYMBOL...MATHEMATICAL BOLD ITALIC CAPITAL OMEGA
                | '\u{1D736}'...'\u{1D74E}'  // MATHEMATICAL BOLD ITALIC SMALL ALPHA...MATHEMATICAL BOLD ITALIC SMALL OMEGA
                | '\u{1D750}'...'\u{1D76E}'  // MATHEMATICAL BOLD ITALIC EPSILON SYMBOL...MATHEMATICAL SANS-SERIF BOLD CAPITAL OMEGA
                | '\u{1D770}'...'\u{1D788}'  // MATHEMATICAL SANS-SERIF BOLD SMALL ALPHA...MATHEMATICAL SANS-SERIF BOLD SMALL OMEGA
                | '\u{1D78A}'...'\u{1D7A8}'  // MATHEMATICAL SANS-SERIF BOLD EPSILON SYMBOL...MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL OMEGA
                | '\u{1D7AA}'...'\u{1D7C2}'  // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL ALPHA...MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL OMEGA
                | '\u{1D7C4}'...'\u{1D7CB}'  // MATHEMATICAL SANS-SERIF BOLD ITALIC EPSILON SYMBOL...MATHEMATICAL BOLD SMALL DIGAMMA
                | '\u{1E800}'...'\u{1E8C4}'  // MENDE KIKAKUI SYLLABLE M001 KI...MENDE KIKAKUI SYLLABLE M060 NYON
                | '\u{1E900}'...'\u{1E943}'  // ADLAM CAPITAL LETTER ALIF...ADLAM SMALL LETTER SHA
                | '\u{1EE00}'...'\u{1EE03}'  // ARABIC MATHEMATICAL ALEF...ARABIC MATHEMATICAL DAL
                | '\u{1EE05}'...'\u{1EE1F}'  // ARABIC MATHEMATICAL WAW...ARABIC MATHEMATICAL DOTLESS QAF
                | '\u{1EE21}'...'\u{1EE22}'  // ARABIC MATHEMATICAL INITIAL BEH...ARABIC MATHEMATICAL INITIAL JEEM
                | '\u{1EE24}'                // ARABIC MATHEMATICAL INITIAL HEH
                | '\u{1EE27}'                // ARABIC MATHEMATICAL INITIAL HAH
                | '\u{1EE29}'...'\u{1EE32}'  // ARABIC MATHEMATICAL INITIAL YEH...ARABIC MATHEMATICAL INITIAL QAF
                | '\u{1EE34}'...'\u{1EE37}'  // ARABIC MATHEMATICAL INITIAL SHEEN...ARABIC MATHEMATICAL INITIAL KHAH
                | '\u{1EE39}'                // ARABIC MATHEMATICAL INITIAL DAD
                | '\u{1EE3B}'                // ARABIC MATHEMATICAL INITIAL GHAIN
                | '\u{1EE42}'                // ARABIC MATHEMATICAL TAILED JEEM
                | '\u{1EE47}'                // ARABIC MATHEMATICAL TAILED HAH
                | '\u{1EE49}'                // ARABIC MATHEMATICAL TAILED YEH
                | '\u{1EE4B}'                // ARABIC MATHEMATICAL TAILED LAM
                | '\u{1EE4D}'...'\u{1EE4F}'  // ARABIC MATHEMATICAL TAILED NOON...ARABIC MATHEMATICAL TAILED AIN
                | '\u{1EE51}'...'\u{1EE52}'  // ARABIC MATHEMATICAL TAILED SAD...ARABIC MATHEMATICAL TAILED QAF
                | '\u{1EE54}'                // ARABIC MATHEMATICAL TAILED SHEEN
                | '\u{1EE57}'                // ARABIC MATHEMATICAL TAILED KHAH
                | '\u{1EE59}'                // ARABIC MATHEMATICAL TAILED DAD
                | '\u{1EE5B}'                // ARABIC MATHEMATICAL TAILED GHAIN
                | '\u{1EE5D}'                // ARABIC MATHEMATICAL TAILED DOTLESS NOON
                | '\u{1EE5F}'                // ARABIC MATHEMATICAL TAILED DOTLESS QAF
                | '\u{1EE61}'...'\u{1EE62}'  // ARABIC MATHEMATICAL STRETCHED BEH...ARABIC MATHEMATICAL STRETCHED JEEM
                | '\u{1EE64}'                // ARABIC MATHEMATICAL STRETCHED HEH
                | '\u{1EE67}'...'\u{1EE6A}'  // ARABIC MATHEMATICAL STRETCHED HAH...ARABIC MATHEMATICAL STRETCHED KAF
                | '\u{1EE6C}'...'\u{1EE72}'  // ARABIC MATHEMATICAL STRETCHED MEEM...ARABIC MATHEMATICAL STRETCHED QAF
                | '\u{1EE74}'...'\u{1EE77}'  // ARABIC MATHEMATICAL STRETCHED SHEEN...ARABIC MATHEMATICAL STRETCHED KHAH
                | '\u{1EE79}'...'\u{1EE7C}'  // ARABIC MATHEMATICAL STRETCHED DAD...ARABIC MATHEMATICAL STRETCHED DOTLESS BEH
                | '\u{1EE7E}'                // ARABIC MATHEMATICAL STRETCHED DOTLESS FEH
                | '\u{1EE80}'...'\u{1EE89}'  // ARABIC MATHEMATICAL LOOPED ALEF...ARABIC MATHEMATICAL LOOPED YEH
                | '\u{1EE8B}'...'\u{1EE9B}'  // ARABIC MATHEMATICAL LOOPED LAM...ARABIC MATHEMATICAL LOOPED GHAIN
                | '\u{1EEA1}'...'\u{1EEA3}'  // ARABIC MATHEMATICAL DOUBLE-STRUCK BEH...ARABIC MATHEMATICAL DOUBLE-STRUCK DAL
                | '\u{1EEA5}'...'\u{1EEA9}'  // ARABIC MATHEMATICAL DOUBLE-STRUCK WAW...ARABIC MATHEMATICAL DOUBLE-STRUCK YEH
                | '\u{1EEAB}'...'\u{1EEBB}'  // ARABIC MATHEMATICAL DOUBLE-STRUCK LAM...ARABIC MATHEMATICAL DOUBLE-STRUCK GHAIN
                | '\u{20000}'...'\u{2A6D6}'  // CJK UNIFIED IDEOGRAPH-20000...CJK UNIFIED IDEOGRAPH-2A6D6
                | '\u{2A700}'...'\u{2B734}'  // CJK UNIFIED IDEOGRAPH-2A700...CJK UNIFIED IDEOGRAPH-2B734
                | '\u{2B740}'...'\u{2B81D}'  // CJK UNIFIED IDEOGRAPH-2B740...CJK UNIFIED IDEOGRAPH-2B81D
                | '\u{2B820}'...'\u{2CEA1}'  // CJK UNIFIED IDEOGRAPH-2B820...CJK UNIFIED IDEOGRAPH-2CEA1
                | '\u{2CEB0}'...'\u{2EBE0}'  // CJK UNIFIED IDEOGRAPH-2CEB0...CJK UNIFIED IDEOGRAPH-2EBE0
                | '\u{2F800}'...'\u{2FA1D}'  // CJK COMPATIBILITY IDEOGRAPH-2F800...CJK COMPATIBILITY IDEOGRAPH-2FA1D
            => {
                loop {
                    match self.stream.here() {
                        None => break,
                        Some(c) => match c {
                              '$'
                            | '_'
                            | '\u{200C}' // ZERO WIDTH NON-JOINER
                            | '\u{200D}' // ZERO WIDTH JOINER
                            // ID_Continue:
                                | '\u{0030}'...'\u{0039}'    // DIGIT ZERO...DIGIT NINE
                                | '\u{0041}'...'\u{005A}'    // LATIN CAPITAL LETTER A...LATIN CAPITAL LETTER Z
                                // | '\u{005F}'                 // LOW LINE
                                | '\u{0061}'...'\u{007A}'    // LATIN SMALL LETTER A...LATIN SMALL LETTER Z
                                | '\u{00AA}'                 // FEMININE ORDINAL INDICATOR
                                | '\u{00B5}'                 // MICRO SIGN
                                | '\u{00B7}'                 // MIDDLE DOT
                                | '\u{00BA}'                 // MASCULINE ORDINAL INDICATOR
                                | '\u{00C0}'...'\u{00D6}'    // LATIN CAPITAL LETTER A WITH GRAVE...LATIN CAPITAL LETTER O WITH DIAERESIS
                                | '\u{00D8}'...'\u{00F6}'    // LATIN CAPITAL LETTER O WITH STROKE...LATIN SMALL LETTER O WITH DIAERESIS
                                | '\u{00F8}'...'\u{01BA}'    // LATIN SMALL LETTER O WITH STROKE...LATIN SMALL LETTER EZH WITH TAIL
                                | '\u{01BB}'                 // LATIN LETTER TWO WITH STROKE
                                | '\u{01BC}'...'\u{01BF}'    // LATIN CAPITAL LETTER TONE FIVE...LATIN LETTER WYNN
                                | '\u{01C0}'...'\u{01C3}'    // LATIN LETTER DENTAL CLICK...LATIN LETTER RETROFLEX CLICK
                                | '\u{01C4}'...'\u{0293}'    // LATIN CAPITAL LETTER DZ WITH CARON...LATIN SMALL LETTER EZH WITH CURL
                                | '\u{0294}'                 // LATIN LETTER GLOTTAL STOP
                                | '\u{0295}'...'\u{02AF}'    // LATIN LETTER PHARYNGEAL VOICED FRICATIVE...LATIN SMALL LETTER TURNED H WITH FISHHOOK AND TAIL
                                | '\u{02B0}'...'\u{02C1}'    // MODIFIER LETTER SMALL H...MODIFIER LETTER REVERSED GLOTTAL STOP
                                | '\u{02C6}'...'\u{02D1}'    // MODIFIER LETTER CIRCUMFLEX ACCENT...MODIFIER LETTER HALF TRIANGULAR COLON
                                | '\u{02E0}'...'\u{02E4}'    // MODIFIER LETTER SMALL GAMMA...MODIFIER LETTER SMALL REVERSED GLOTTAL STOP
                                | '\u{02EC}'                 // MODIFIER LETTER VOICING
                                | '\u{02EE}'                 // MODIFIER LETTER DOUBLE APOSTROPHE
                                | '\u{0300}'...'\u{036F}'    // COMBINING GRAVE ACCENT...COMBINING LATIN SMALL LETTER X
                                | '\u{0370}'...'\u{0373}'    // GREEK CAPITAL LETTER HETA...GREEK SMALL LETTER ARCHAIC SAMPI
                                | '\u{0374}'                 // GREEK NUMERAL SIGN
                                | '\u{0376}'...'\u{0377}'    // GREEK CAPITAL LETTER PAMPHYLIAN DIGAMMA...GREEK SMALL LETTER PAMPHYLIAN DIGAMMA
                                | '\u{037A}'                 // GREEK YPOGEGRAMMENI
                                | '\u{037B}'...'\u{037D}'    // GREEK SMALL REVERSED LUNATE SIGMA SYMBOL...GREEK SMALL REVERSED DOTTED LUNATE SIGMA SYMBOL
                                | '\u{037F}'                 // GREEK CAPITAL LETTER YOT
                                | '\u{0386}'                 // GREEK CAPITAL LETTER ALPHA WITH TONOS
                                | '\u{0387}'                 // GREEK ANO TELEIA
                                | '\u{0388}'...'\u{038A}'    // GREEK CAPITAL LETTER EPSILON WITH TONOS...GREEK CAPITAL LETTER IOTA WITH TONOS
                                | '\u{038C}'                 // GREEK CAPITAL LETTER OMICRON WITH TONOS
                                | '\u{038E}'...'\u{03A1}'    // GREEK CAPITAL LETTER UPSILON WITH TONOS...GREEK CAPITAL LETTER RHO
                                | '\u{03A3}'...'\u{03F5}'    // GREEK CAPITAL LETTER SIGMA...GREEK LUNATE EPSILON SYMBOL
                                | '\u{03F7}'...'\u{0481}'    // GREEK CAPITAL LETTER SHO...CYRILLIC SMALL LETTER KOPPA
                                | '\u{0483}'...'\u{0487}'    // COMBINING CYRILLIC TITLO...COMBINING CYRILLIC POKRYTIE
                                | '\u{048A}'...'\u{052F}'    // CYRILLIC CAPITAL LETTER SHORT I WITH TAIL...CYRILLIC SMALL LETTER EL WITH DESCENDER
                                | '\u{0531}'...'\u{0556}'    // ARMENIAN CAPITAL LETTER AYB...ARMENIAN CAPITAL LETTER FEH
                                | '\u{0559}'                 // ARMENIAN MODIFIER LETTER LEFT HALF RING
                                | '\u{0561}'...'\u{0587}'    // ARMENIAN SMALL LETTER AYB...ARMENIAN SMALL LIGATURE ECH YIWN
                                | '\u{0591}'...'\u{05BD}'    // HEBREW ACCENT ETNAHTA...HEBREW POINT METEG
                                | '\u{05BF}'                 // HEBREW POINT RAFE
                                | '\u{05C1}'...'\u{05C2}'    // HEBREW POINT SHIN DOT...HEBREW POINT SIN DOT
                                | '\u{05C4}'...'\u{05C5}'    // HEBREW MARK UPPER DOT...HEBREW MARK LOWER DOT
                                | '\u{05C7}'                 // HEBREW POINT QAMATS QATAN
                                | '\u{05D0}'...'\u{05EA}'    // HEBREW LETTER ALEF...HEBREW LETTER TAV
                                | '\u{05F0}'...'\u{05F2}'    // HEBREW LIGATURE YIDDISH DOUBLE VAV...HEBREW LIGATURE YIDDISH DOUBLE YOD
                                | '\u{0610}'...'\u{061A}'    // ARABIC SIGN SALLALLAHOU ALAYHE WASSALLAM...ARABIC SMALL KASRA
                                | '\u{0620}'...'\u{063F}'    // ARABIC LETTER KASHMIRI YEH...ARABIC LETTER FARSI YEH WITH THREE DOTS ABOVE
                                | '\u{0640}'                 // ARABIC TATWEEL
                                | '\u{0641}'...'\u{064A}'    // ARABIC LETTER FEH...ARABIC LETTER YEH
                                | '\u{064B}'...'\u{065F}'    // ARABIC FATHATAN...ARABIC WAVY HAMZA BELOW
                                | '\u{0660}'...'\u{0669}'    // ARABIC-INDIC DIGIT ZERO...ARABIC-INDIC DIGIT NINE
                                | '\u{066E}'...'\u{066F}'    // ARABIC LETTER DOTLESS BEH...ARABIC LETTER DOTLESS QAF
                                | '\u{0670}'                 // ARABIC LETTER SUPERSCRIPT ALEF
                                | '\u{0671}'...'\u{06D3}'    // ARABIC LETTER ALEF WASLA...ARABIC LETTER YEH BARREE WITH HAMZA ABOVE
                                | '\u{06D5}'                 // ARABIC LETTER AE
                                | '\u{06D6}'...'\u{06DC}'    // ARABIC SMALL HIGH LIGATURE SAD WITH LAM WITH ALEF MAKSURA...ARABIC SMALL HIGH SEEN
                                | '\u{06DF}'...'\u{06E4}'    // ARABIC SMALL HIGH ROUNDED ZERO...ARABIC SMALL HIGH MADDA
                                | '\u{06E5}'...'\u{06E6}'    // ARABIC SMALL WAW...ARABIC SMALL YEH
                                | '\u{06E7}'...'\u{06E8}'    // ARABIC SMALL HIGH YEH...ARABIC SMALL HIGH NOON
                                | '\u{06EA}'...'\u{06ED}'    // ARABIC EMPTY CENTRE LOW STOP...ARABIC SMALL LOW MEEM
                                | '\u{06EE}'...'\u{06EF}'    // ARABIC LETTER DAL WITH INVERTED V...ARABIC LETTER REH WITH INVERTED V
                                | '\u{06F0}'...'\u{06F9}'    // EXTENDED ARABIC-INDIC DIGIT ZERO...EXTENDED ARABIC-INDIC DIGIT NINE
                                | '\u{06FA}'...'\u{06FC}'    // ARABIC LETTER SHEEN WITH DOT BELOW...ARABIC LETTER GHAIN WITH DOT BELOW
                                | '\u{06FF}'                 // ARABIC LETTER HEH WITH INVERTED V
                                | '\u{0710}'                 // SYRIAC LETTER ALAPH
                                | '\u{0711}'                 // SYRIAC LETTER SUPERSCRIPT ALAPH
                                | '\u{0712}'...'\u{072F}'    // SYRIAC LETTER BETH...SYRIAC LETTER PERSIAN DHALATH
                                | '\u{0730}'...'\u{074A}'    // SYRIAC PTHAHA ABOVE...SYRIAC BARREKH
                                | '\u{074D}'...'\u{07A5}'    // SYRIAC LETTER SOGDIAN ZHAIN...THAANA LETTER WAAVU
                                | '\u{07A6}'...'\u{07B0}'    // THAANA ABAFILI...THAANA SUKUN
                                | '\u{07B1}'                 // THAANA LETTER NAA
                                | '\u{07C0}'...'\u{07C9}'    // NKO DIGIT ZERO...NKO DIGIT NINE
                                | '\u{07CA}'...'\u{07EA}'    // NKO LETTER A...NKO LETTER JONA RA
                                | '\u{07EB}'...'\u{07F3}'    // NKO COMBINING SHORT HIGH TONE...NKO COMBINING DOUBLE DOT ABOVE
                                | '\u{07F4}'...'\u{07F5}'    // NKO HIGH TONE APOSTROPHE...NKO LOW TONE APOSTROPHE
                                | '\u{07FA}'                 // NKO LAJANYALAN
                                | '\u{0800}'...'\u{0815}'    // SAMARITAN LETTER ALAF...SAMARITAN LETTER TAAF
                                | '\u{0816}'...'\u{0819}'    // SAMARITAN MARK IN...SAMARITAN MARK DAGESH
                                | '\u{081A}'                 // SAMARITAN MODIFIER LETTER EPENTHETIC YUT
                                | '\u{081B}'...'\u{0823}'    // SAMARITAN MARK EPENTHETIC YUT...SAMARITAN VOWEL SIGN A
                                | '\u{0824}'                 // SAMARITAN MODIFIER LETTER SHORT A
                                | '\u{0825}'...'\u{0827}'    // SAMARITAN VOWEL SIGN SHORT A...SAMARITAN VOWEL SIGN U
                                | '\u{0828}'                 // SAMARITAN MODIFIER LETTER I
                                | '\u{0829}'...'\u{082D}'    // SAMARITAN VOWEL SIGN LONG I...SAMARITAN MARK NEQUDAA
                                | '\u{0840}'...'\u{0858}'    // MANDAIC LETTER HALQA...MANDAIC LETTER AIN
                                | '\u{0859}'...'\u{085B}'    // MANDAIC AFFRICATION MARK...MANDAIC GEMINATION MARK
                                | '\u{0860}'...'\u{086A}'    // SYRIAC LETTER MALAYALAM NGA...SYRIAC LETTER MALAYALAM SSA
                                | '\u{08A0}'...'\u{08B4}'    // ARABIC LETTER BEH WITH SMALL V BELOW...ARABIC LETTER KAF WITH DOT BELOW
                                | '\u{08B6}'...'\u{08BD}'    // ARABIC LETTER BEH WITH SMALL MEEM ABOVE...ARABIC LETTER AFRICAN NOON
                                | '\u{08D4}'...'\u{08E1}'    // ARABIC SMALL HIGH WORD AR-RUB...ARABIC SMALL HIGH SIGN SAFHA
                                | '\u{08E3}'...'\u{0902}'    // ARABIC TURNED DAMMA BELOW...DEVANAGARI SIGN ANUSVARA
                                | '\u{0903}'                 // DEVANAGARI SIGN VISARGA
                                | '\u{0904}'...'\u{0939}'    // DEVANAGARI LETTER SHORT A...DEVANAGARI LETTER HA
                                | '\u{093A}'                 // DEVANAGARI VOWEL SIGN OE
                                | '\u{093B}'                 // DEVANAGARI VOWEL SIGN OOE
                                | '\u{093C}'                 // DEVANAGARI SIGN NUKTA
                                | '\u{093D}'                 // DEVANAGARI SIGN AVAGRAHA
                                | '\u{093E}'...'\u{0940}'    // DEVANAGARI VOWEL SIGN AA...DEVANAGARI VOWEL SIGN II
                                | '\u{0941}'...'\u{0948}'    // DEVANAGARI VOWEL SIGN U...DEVANAGARI VOWEL SIGN AI
                                | '\u{0949}'...'\u{094C}'    // DEVANAGARI VOWEL SIGN CANDRA O...DEVANAGARI VOWEL SIGN AU
                                | '\u{094D}'                 // DEVANAGARI SIGN VIRAMA
                                | '\u{094E}'...'\u{094F}'    // DEVANAGARI VOWEL SIGN PRISHTHAMATRA E...DEVANAGARI VOWEL SIGN AW
                                | '\u{0950}'                 // DEVANAGARI OM
                                | '\u{0951}'...'\u{0957}'    // DEVANAGARI STRESS SIGN UDATTA...DEVANAGARI VOWEL SIGN UUE
                                | '\u{0958}'...'\u{0961}'    // DEVANAGARI LETTER QA...DEVANAGARI LETTER VOCALIC LL
                                | '\u{0962}'...'\u{0963}'    // DEVANAGARI VOWEL SIGN VOCALIC L...DEVANAGARI VOWEL SIGN VOCALIC LL
                                | '\u{0966}'...'\u{096F}'    // DEVANAGARI DIGIT ZERO...DEVANAGARI DIGIT NINE
                                | '\u{0971}'                 // DEVANAGARI SIGN HIGH SPACING DOT
                                | '\u{0972}'...'\u{0980}'    // DEVANAGARI LETTER CANDRA A...BENGALI ANJI
                                | '\u{0981}'                 // BENGALI SIGN CANDRABINDU
                                | '\u{0982}'...'\u{0983}'    // BENGALI SIGN ANUSVARA...BENGALI SIGN VISARGA
                                | '\u{0985}'...'\u{098C}'    // BENGALI LETTER A...BENGALI LETTER VOCALIC L
                                | '\u{098F}'...'\u{0990}'    // BENGALI LETTER E...BENGALI LETTER AI
                                | '\u{0993}'...'\u{09A8}'    // BENGALI LETTER O...BENGALI LETTER NA
                                | '\u{09AA}'...'\u{09B0}'    // BENGALI LETTER PA...BENGALI LETTER RA
                                | '\u{09B2}'                 // BENGALI LETTER LA
                                | '\u{09B6}'...'\u{09B9}'    // BENGALI LETTER SHA...BENGALI LETTER HA
                                | '\u{09BC}'                 // BENGALI SIGN NUKTA
                                | '\u{09BD}'                 // BENGALI SIGN AVAGRAHA
                                | '\u{09BE}'...'\u{09C0}'    // BENGALI VOWEL SIGN AA...BENGALI VOWEL SIGN II
                                | '\u{09C1}'...'\u{09C4}'    // BENGALI VOWEL SIGN U...BENGALI VOWEL SIGN VOCALIC RR
                                | '\u{09C7}'...'\u{09C8}'    // BENGALI VOWEL SIGN E...BENGALI VOWEL SIGN AI
                                | '\u{09CB}'...'\u{09CC}'    // BENGALI VOWEL SIGN O...BENGALI VOWEL SIGN AU
                                | '\u{09CD}'                 // BENGALI SIGN VIRAMA
                                | '\u{09CE}'                 // BENGALI LETTER KHANDA TA
                                | '\u{09D7}'                 // BENGALI AU LENGTH MARK
                                | '\u{09DC}'...'\u{09DD}'    // BENGALI LETTER RRA...BENGALI LETTER RHA
                                | '\u{09DF}'...'\u{09E1}'    // BENGALI LETTER YYA...BENGALI LETTER VOCALIC LL
                                | '\u{09E2}'...'\u{09E3}'    // BENGALI VOWEL SIGN VOCALIC L...BENGALI VOWEL SIGN VOCALIC LL
                                | '\u{09E6}'...'\u{09EF}'    // BENGALI DIGIT ZERO...BENGALI DIGIT NINE
                                | '\u{09F0}'...'\u{09F1}'    // BENGALI LETTER RA WITH MIDDLE DIAGONAL...BENGALI LETTER RA WITH LOWER DIAGONAL
                                | '\u{09FC}'                 // BENGALI LETTER VEDIC ANUSVARA
                                | '\u{0A01}'...'\u{0A02}'    // GURMUKHI SIGN ADAK BINDI...GURMUKHI SIGN BINDI
                                | '\u{0A03}'                 // GURMUKHI SIGN VISARGA
                                | '\u{0A05}'...'\u{0A0A}'    // GURMUKHI LETTER A...GURMUKHI LETTER UU
                                | '\u{0A0F}'...'\u{0A10}'    // GURMUKHI LETTER EE...GURMUKHI LETTER AI
                                | '\u{0A13}'...'\u{0A28}'    // GURMUKHI LETTER OO...GURMUKHI LETTER NA
                                | '\u{0A2A}'...'\u{0A30}'    // GURMUKHI LETTER PA...GURMUKHI LETTER RA
                                | '\u{0A32}'...'\u{0A33}'    // GURMUKHI LETTER LA...GURMUKHI LETTER LLA
                                | '\u{0A35}'...'\u{0A36}'    // GURMUKHI LETTER VA...GURMUKHI LETTER SHA
                                | '\u{0A38}'...'\u{0A39}'    // GURMUKHI LETTER SA...GURMUKHI LETTER HA
                                | '\u{0A3C}'                 // GURMUKHI SIGN NUKTA
                                | '\u{0A3E}'...'\u{0A40}'    // GURMUKHI VOWEL SIGN AA...GURMUKHI VOWEL SIGN II
                                | '\u{0A41}'...'\u{0A42}'    // GURMUKHI VOWEL SIGN U...GURMUKHI VOWEL SIGN UU
                                | '\u{0A47}'...'\u{0A48}'    // GURMUKHI VOWEL SIGN EE...GURMUKHI VOWEL SIGN AI
                                | '\u{0A4B}'...'\u{0A4D}'    // GURMUKHI VOWEL SIGN OO...GURMUKHI SIGN VIRAMA
                                | '\u{0A51}'                 // GURMUKHI SIGN UDAAT
                                | '\u{0A59}'...'\u{0A5C}'    // GURMUKHI LETTER KHHA...GURMUKHI LETTER RRA
                                | '\u{0A5E}'                 // GURMUKHI LETTER FA
                                | '\u{0A66}'...'\u{0A6F}'    // GURMUKHI DIGIT ZERO...GURMUKHI DIGIT NINE
                                | '\u{0A70}'...'\u{0A71}'    // GURMUKHI TIPPI...GURMUKHI ADDAK
                                | '\u{0A72}'...'\u{0A74}'    // GURMUKHI IRI...GURMUKHI EK ONKAR
                                | '\u{0A75}'                 // GURMUKHI SIGN YAKASH
                                | '\u{0A81}'...'\u{0A82}'    // GUJARATI SIGN CANDRABINDU...GUJARATI SIGN ANUSVARA
                                | '\u{0A83}'                 // GUJARATI SIGN VISARGA
                                | '\u{0A85}'...'\u{0A8D}'    // GUJARATI LETTER A...GUJARATI VOWEL CANDRA E
                                | '\u{0A8F}'...'\u{0A91}'    // GUJARATI LETTER E...GUJARATI VOWEL CANDRA O
                                | '\u{0A93}'...'\u{0AA8}'    // GUJARATI LETTER O...GUJARATI LETTER NA
                                | '\u{0AAA}'...'\u{0AB0}'    // GUJARATI LETTER PA...GUJARATI LETTER RA
                                | '\u{0AB2}'...'\u{0AB3}'    // GUJARATI LETTER LA...GUJARATI LETTER LLA
                                | '\u{0AB5}'...'\u{0AB9}'    // GUJARATI LETTER VA...GUJARATI LETTER HA
                                | '\u{0ABC}'                 // GUJARATI SIGN NUKTA
                                | '\u{0ABD}'                 // GUJARATI SIGN AVAGRAHA
                                | '\u{0ABE}'...'\u{0AC0}'    // GUJARATI VOWEL SIGN AA...GUJARATI VOWEL SIGN II
                                | '\u{0AC1}'...'\u{0AC5}'    // GUJARATI VOWEL SIGN U...GUJARATI VOWEL SIGN CANDRA E
                                | '\u{0AC7}'...'\u{0AC8}'    // GUJARATI VOWEL SIGN E...GUJARATI VOWEL SIGN AI
                                | '\u{0AC9}'                 // GUJARATI VOWEL SIGN CANDRA O
                                | '\u{0ACB}'...'\u{0ACC}'    // GUJARATI VOWEL SIGN O...GUJARATI VOWEL SIGN AU
                                | '\u{0ACD}'                 // GUJARATI SIGN VIRAMA
                                | '\u{0AD0}'                 // GUJARATI OM
                                | '\u{0AE0}'...'\u{0AE1}'    // GUJARATI LETTER VOCALIC RR...GUJARATI LETTER VOCALIC LL
                                | '\u{0AE2}'...'\u{0AE3}'    // GUJARATI VOWEL SIGN VOCALIC L...GUJARATI VOWEL SIGN VOCALIC LL
                                | '\u{0AE6}'...'\u{0AEF}'    // GUJARATI DIGIT ZERO...GUJARATI DIGIT NINE
                                | '\u{0AF9}'                 // GUJARATI LETTER ZHA
                                | '\u{0AFA}'...'\u{0AFF}'    // GUJARATI SIGN SUKUN...GUJARATI SIGN TWO-CIRCLE NUKTA ABOVE
                                | '\u{0B01}'                 // ORIYA SIGN CANDRABINDU
                                | '\u{0B02}'...'\u{0B03}'    // ORIYA SIGN ANUSVARA...ORIYA SIGN VISARGA
                                | '\u{0B05}'...'\u{0B0C}'    // ORIYA LETTER A...ORIYA LETTER VOCALIC L
                                | '\u{0B0F}'...'\u{0B10}'    // ORIYA LETTER E...ORIYA LETTER AI
                                | '\u{0B13}'...'\u{0B28}'    // ORIYA LETTER O...ORIYA LETTER NA
                                | '\u{0B2A}'...'\u{0B30}'    // ORIYA LETTER PA...ORIYA LETTER RA
                                | '\u{0B32}'...'\u{0B33}'    // ORIYA LETTER LA...ORIYA LETTER LLA
                                | '\u{0B35}'...'\u{0B39}'    // ORIYA LETTER VA...ORIYA LETTER HA
                                | '\u{0B3C}'                 // ORIYA SIGN NUKTA
                                | '\u{0B3D}'                 // ORIYA SIGN AVAGRAHA
                                | '\u{0B3E}'                 // ORIYA VOWEL SIGN AA
                                | '\u{0B3F}'                 // ORIYA VOWEL SIGN I
                                | '\u{0B40}'                 // ORIYA VOWEL SIGN II
                                | '\u{0B41}'...'\u{0B44}'    // ORIYA VOWEL SIGN U...ORIYA VOWEL SIGN VOCALIC RR
                                | '\u{0B47}'...'\u{0B48}'    // ORIYA VOWEL SIGN E...ORIYA VOWEL SIGN AI
                                | '\u{0B4B}'...'\u{0B4C}'    // ORIYA VOWEL SIGN O...ORIYA VOWEL SIGN AU
                                | '\u{0B4D}'                 // ORIYA SIGN VIRAMA
                                | '\u{0B56}'                 // ORIYA AI LENGTH MARK
                                | '\u{0B57}'                 // ORIYA AU LENGTH MARK
                                | '\u{0B5C}'...'\u{0B5D}'    // ORIYA LETTER RRA...ORIYA LETTER RHA
                                | '\u{0B5F}'...'\u{0B61}'    // ORIYA LETTER YYA...ORIYA LETTER VOCALIC LL
                                | '\u{0B62}'...'\u{0B63}'    // ORIYA VOWEL SIGN VOCALIC L...ORIYA VOWEL SIGN VOCALIC LL
                                | '\u{0B66}'...'\u{0B6F}'    // ORIYA DIGIT ZERO...ORIYA DIGIT NINE
                                | '\u{0B71}'                 // ORIYA LETTER WA
                                | '\u{0B82}'                 // TAMIL SIGN ANUSVARA
                                | '\u{0B83}'                 // TAMIL SIGN VISARGA
                                | '\u{0B85}'...'\u{0B8A}'    // TAMIL LETTER A...TAMIL LETTER UU
                                | '\u{0B8E}'...'\u{0B90}'    // TAMIL LETTER E...TAMIL LETTER AI
                                | '\u{0B92}'...'\u{0B95}'    // TAMIL LETTER O...TAMIL LETTER KA
                                | '\u{0B99}'...'\u{0B9A}'    // TAMIL LETTER NGA...TAMIL LETTER CA
                                | '\u{0B9C}'                 // TAMIL LETTER JA
                                | '\u{0B9E}'...'\u{0B9F}'    // TAMIL LETTER NYA...TAMIL LETTER TTA
                                | '\u{0BA3}'...'\u{0BA4}'    // TAMIL LETTER NNA...TAMIL LETTER TA
                                | '\u{0BA8}'...'\u{0BAA}'    // TAMIL LETTER NA...TAMIL LETTER PA
                                | '\u{0BAE}'...'\u{0BB9}'    // TAMIL LETTER MA...TAMIL LETTER HA
                                | '\u{0BBE}'...'\u{0BBF}'    // TAMIL VOWEL SIGN AA...TAMIL VOWEL SIGN I
                                | '\u{0BC0}'                 // TAMIL VOWEL SIGN II
                                | '\u{0BC1}'...'\u{0BC2}'    // TAMIL VOWEL SIGN U...TAMIL VOWEL SIGN UU
                                | '\u{0BC6}'...'\u{0BC8}'    // TAMIL VOWEL SIGN E...TAMIL VOWEL SIGN AI
                                | '\u{0BCA}'...'\u{0BCC}'    // TAMIL VOWEL SIGN O...TAMIL VOWEL SIGN AU
                                | '\u{0BCD}'                 // TAMIL SIGN VIRAMA
                                | '\u{0BD0}'                 // TAMIL OM
                                | '\u{0BD7}'                 // TAMIL AU LENGTH MARK
                                | '\u{0BE6}'...'\u{0BEF}'    // TAMIL DIGIT ZERO...TAMIL DIGIT NINE
                                | '\u{0C00}'                 // TELUGU SIGN COMBINING CANDRABINDU ABOVE
                                | '\u{0C01}'...'\u{0C03}'    // TELUGU SIGN CANDRABINDU...TELUGU SIGN VISARGA
                                | '\u{0C05}'...'\u{0C0C}'    // TELUGU LETTER A...TELUGU LETTER VOCALIC L
                                | '\u{0C0E}'...'\u{0C10}'    // TELUGU LETTER E...TELUGU LETTER AI
                                | '\u{0C12}'...'\u{0C28}'    // TELUGU LETTER O...TELUGU LETTER NA
                                | '\u{0C2A}'...'\u{0C39}'    // TELUGU LETTER PA...TELUGU LETTER HA
                                | '\u{0C3D}'                 // TELUGU SIGN AVAGRAHA
                                | '\u{0C3E}'...'\u{0C40}'    // TELUGU VOWEL SIGN AA...TELUGU VOWEL SIGN II
                                | '\u{0C41}'...'\u{0C44}'    // TELUGU VOWEL SIGN U...TELUGU VOWEL SIGN VOCALIC RR
                                | '\u{0C46}'...'\u{0C48}'    // TELUGU VOWEL SIGN E...TELUGU VOWEL SIGN AI
                                | '\u{0C4A}'...'\u{0C4D}'    // TELUGU VOWEL SIGN O...TELUGU SIGN VIRAMA
                                | '\u{0C55}'...'\u{0C56}'    // TELUGU LENGTH MARK...TELUGU AI LENGTH MARK
                                | '\u{0C58}'...'\u{0C5A}'    // TELUGU LETTER TSA...TELUGU LETTER RRRA
                                | '\u{0C60}'...'\u{0C61}'    // TELUGU LETTER VOCALIC RR...TELUGU LETTER VOCALIC LL
                                | '\u{0C62}'...'\u{0C63}'    // TELUGU VOWEL SIGN VOCALIC L...TELUGU VOWEL SIGN VOCALIC LL
                                | '\u{0C66}'...'\u{0C6F}'    // TELUGU DIGIT ZERO...TELUGU DIGIT NINE
                                | '\u{0C80}'                 // KANNADA SIGN SPACING CANDRABINDU
                                | '\u{0C81}'                 // KANNADA SIGN CANDRABINDU
                                | '\u{0C82}'...'\u{0C83}'    // KANNADA SIGN ANUSVARA...KANNADA SIGN VISARGA
                                | '\u{0C85}'...'\u{0C8C}'    // KANNADA LETTER A...KANNADA LETTER VOCALIC L
                                | '\u{0C8E}'...'\u{0C90}'    // KANNADA LETTER E...KANNADA LETTER AI
                                | '\u{0C92}'...'\u{0CA8}'    // KANNADA LETTER O...KANNADA LETTER NA
                                | '\u{0CAA}'...'\u{0CB3}'    // KANNADA LETTER PA...KANNADA LETTER LLA
                                | '\u{0CB5}'...'\u{0CB9}'    // KANNADA LETTER VA...KANNADA LETTER HA
                                | '\u{0CBC}'                 // KANNADA SIGN NUKTA
                                | '\u{0CBD}'                 // KANNADA SIGN AVAGRAHA
                                | '\u{0CBE}'                 // KANNADA VOWEL SIGN AA
                                | '\u{0CBF}'                 // KANNADA VOWEL SIGN I
                                | '\u{0CC0}'...'\u{0CC4}'    // KANNADA VOWEL SIGN II...KANNADA VOWEL SIGN VOCALIC RR
                                | '\u{0CC6}'                 // KANNADA VOWEL SIGN E
                                | '\u{0CC7}'...'\u{0CC8}'    // KANNADA VOWEL SIGN EE...KANNADA VOWEL SIGN AI
                                | '\u{0CCA}'...'\u{0CCB}'    // KANNADA VOWEL SIGN O...KANNADA VOWEL SIGN OO
                                | '\u{0CCC}'...'\u{0CCD}'    // KANNADA VOWEL SIGN AU...KANNADA SIGN VIRAMA
                                | '\u{0CD5}'...'\u{0CD6}'    // KANNADA LENGTH MARK...KANNADA AI LENGTH MARK
                                | '\u{0CDE}'                 // KANNADA LETTER FA
                                | '\u{0CE0}'...'\u{0CE1}'    // KANNADA LETTER VOCALIC RR...KANNADA LETTER VOCALIC LL
                                | '\u{0CE2}'...'\u{0CE3}'    // KANNADA VOWEL SIGN VOCALIC L...KANNADA VOWEL SIGN VOCALIC LL
                                | '\u{0CE6}'...'\u{0CEF}'    // KANNADA DIGIT ZERO...KANNADA DIGIT NINE
                                | '\u{0CF1}'...'\u{0CF2}'    // KANNADA SIGN JIHVAMULIYA...KANNADA SIGN UPADHMANIYA
                                | '\u{0D00}'...'\u{0D01}'    // MALAYALAM SIGN COMBINING ANUSVARA ABOVE...MALAYALAM SIGN CANDRABINDU
                                | '\u{0D02}'...'\u{0D03}'    // MALAYALAM SIGN ANUSVARA...MALAYALAM SIGN VISARGA
                                | '\u{0D05}'...'\u{0D0C}'    // MALAYALAM LETTER A...MALAYALAM LETTER VOCALIC L
                                | '\u{0D0E}'...'\u{0D10}'    // MALAYALAM LETTER E...MALAYALAM LETTER AI
                                | '\u{0D12}'...'\u{0D3A}'    // MALAYALAM LETTER O...MALAYALAM LETTER TTTA
                                | '\u{0D3B}'...'\u{0D3C}'    // MALAYALAM SIGN VERTICAL BAR VIRAMA...MALAYALAM SIGN CIRCULAR VIRAMA
                                | '\u{0D3D}'                 // MALAYALAM SIGN AVAGRAHA
                                | '\u{0D3E}'...'\u{0D40}'    // MALAYALAM VOWEL SIGN AA...MALAYALAM VOWEL SIGN II
                                | '\u{0D41}'...'\u{0D44}'    // MALAYALAM VOWEL SIGN U...MALAYALAM VOWEL SIGN VOCALIC RR
                                | '\u{0D46}'...'\u{0D48}'    // MALAYALAM VOWEL SIGN E...MALAYALAM VOWEL SIGN AI
                                | '\u{0D4A}'...'\u{0D4C}'    // MALAYALAM VOWEL SIGN O...MALAYALAM VOWEL SIGN AU
                                | '\u{0D4D}'                 // MALAYALAM SIGN VIRAMA
                                | '\u{0D4E}'                 // MALAYALAM LETTER DOT REPH
                                | '\u{0D54}'...'\u{0D56}'    // MALAYALAM LETTER CHILLU M...MALAYALAM LETTER CHILLU LLL
                                | '\u{0D57}'                 // MALAYALAM AU LENGTH MARK
                                | '\u{0D5F}'...'\u{0D61}'    // MALAYALAM LETTER ARCHAIC II...MALAYALAM LETTER VOCALIC LL
                                | '\u{0D62}'...'\u{0D63}'    // MALAYALAM VOWEL SIGN VOCALIC L...MALAYALAM VOWEL SIGN VOCALIC LL
                                | '\u{0D66}'...'\u{0D6F}'    // MALAYALAM DIGIT ZERO...MALAYALAM DIGIT NINE
                                | '\u{0D7A}'...'\u{0D7F}'    // MALAYALAM LETTER CHILLU NN...MALAYALAM LETTER CHILLU K
                                | '\u{0D82}'...'\u{0D83}'    // SINHALA SIGN ANUSVARAYA...SINHALA SIGN VISARGAYA
                                | '\u{0D85}'...'\u{0D96}'    // SINHALA LETTER AYANNA...SINHALA LETTER AUYANNA
                                | '\u{0D9A}'...'\u{0DB1}'    // SINHALA LETTER ALPAPRAANA KAYANNA...SINHALA LETTER DANTAJA NAYANNA
                                | '\u{0DB3}'...'\u{0DBB}'    // SINHALA LETTER SANYAKA DAYANNA...SINHALA LETTER RAYANNA
                                | '\u{0DBD}'                 // SINHALA LETTER DANTAJA LAYANNA
                                | '\u{0DC0}'...'\u{0DC6}'    // SINHALA LETTER VAYANNA...SINHALA LETTER FAYANNA
                                | '\u{0DCA}'                 // SINHALA SIGN AL-LAKUNA
                                | '\u{0DCF}'...'\u{0DD1}'    // SINHALA VOWEL SIGN AELA-PILLA...SINHALA VOWEL SIGN DIGA AEDA-PILLA
                                | '\u{0DD2}'...'\u{0DD4}'    // SINHALA VOWEL SIGN KETTI IS-PILLA...SINHALA VOWEL SIGN KETTI PAA-PILLA
                                | '\u{0DD6}'                 // SINHALA VOWEL SIGN DIGA PAA-PILLA
                                | '\u{0DD8}'...'\u{0DDF}'    // SINHALA VOWEL SIGN GAETTA-PILLA...SINHALA VOWEL SIGN GAYANUKITTA
                                | '\u{0DE6}'...'\u{0DEF}'    // SINHALA LITH DIGIT ZERO...SINHALA LITH DIGIT NINE
                                | '\u{0DF2}'...'\u{0DF3}'    // SINHALA VOWEL SIGN DIGA GAETTA-PILLA...SINHALA VOWEL SIGN DIGA GAYANUKITTA
                                | '\u{0E01}'...'\u{0E30}'    // THAI CHARACTER KO KAI...THAI CHARACTER SARA A
                                | '\u{0E31}'                 // THAI CHARACTER MAI HAN-AKAT
                                | '\u{0E32}'...'\u{0E33}'    // THAI CHARACTER SARA AA...THAI CHARACTER SARA AM
                                | '\u{0E34}'...'\u{0E3A}'    // THAI CHARACTER SARA I...THAI CHARACTER PHINTHU
                                | '\u{0E40}'...'\u{0E45}'    // THAI CHARACTER SARA E...THAI CHARACTER LAKKHANGYAO
                                | '\u{0E46}'                 // THAI CHARACTER MAIYAMOK
                                | '\u{0E47}'...'\u{0E4E}'    // THAI CHARACTER MAITAIKHU...THAI CHARACTER YAMAKKAN
                                | '\u{0E50}'...'\u{0E59}'    // THAI DIGIT ZERO...THAI DIGIT NINE
                                | '\u{0E81}'...'\u{0E82}'    // LAO LETTER KO...LAO LETTER KHO SUNG
                                | '\u{0E84}'                 // LAO LETTER KHO TAM
                                | '\u{0E87}'...'\u{0E88}'    // LAO LETTER NGO...LAO LETTER CO
                                | '\u{0E8A}'                 // LAO LETTER SO TAM
                                | '\u{0E8D}'                 // LAO LETTER NYO
                                | '\u{0E94}'...'\u{0E97}'    // LAO LETTER DO...LAO LETTER THO TAM
                                | '\u{0E99}'...'\u{0E9F}'    // LAO LETTER NO...LAO LETTER FO SUNG
                                | '\u{0EA1}'...'\u{0EA3}'    // LAO LETTER MO...LAO LETTER LO LING
                                | '\u{0EA5}'                 // LAO LETTER LO LOOT
                                | '\u{0EA7}'                 // LAO LETTER WO
                                | '\u{0EAA}'...'\u{0EAB}'    // LAO LETTER SO SUNG...LAO LETTER HO SUNG
                                | '\u{0EAD}'...'\u{0EB0}'    // LAO LETTER O...LAO VOWEL SIGN A
                                | '\u{0EB1}'                 // LAO VOWEL SIGN MAI KAN
                                | '\u{0EB2}'...'\u{0EB3}'    // LAO VOWEL SIGN AA...LAO VOWEL SIGN AM
                                | '\u{0EB4}'...'\u{0EB9}'    // LAO VOWEL SIGN I...LAO VOWEL SIGN UU
                                | '\u{0EBB}'...'\u{0EBC}'    // LAO VOWEL SIGN MAI KON...LAO SEMIVOWEL SIGN LO
                                | '\u{0EBD}'                 // LAO SEMIVOWEL SIGN NYO
                                | '\u{0EC0}'...'\u{0EC4}'    // LAO VOWEL SIGN E...LAO VOWEL SIGN AI
                                | '\u{0EC6}'                 // LAO KO LA
                                | '\u{0EC8}'...'\u{0ECD}'    // LAO TONE MAI EK...LAO NIGGAHITA
                                | '\u{0ED0}'...'\u{0ED9}'    // LAO DIGIT ZERO...LAO DIGIT NINE
                                | '\u{0EDC}'...'\u{0EDF}'    // LAO HO NO...LAO LETTER KHMU NYO
                                | '\u{0F00}'                 // TIBETAN SYLLABLE OM
                                | '\u{0F18}'...'\u{0F19}'    // TIBETAN ASTROLOGICAL SIGN -KHYUD PA...TIBETAN ASTROLOGICAL SIGN SDONG TSHUGS
                                | '\u{0F20}'...'\u{0F29}'    // TIBETAN DIGIT ZERO...TIBETAN DIGIT NINE
                                | '\u{0F35}'                 // TIBETAN MARK NGAS BZUNG NYI ZLA
                                | '\u{0F37}'                 // TIBETAN MARK NGAS BZUNG SGOR RTAGS
                                | '\u{0F39}'                 // TIBETAN MARK TSA -PHRU
                                | '\u{0F3E}'...'\u{0F3F}'    // TIBETAN SIGN YAR TSHES...TIBETAN SIGN MAR TSHES
                                | '\u{0F40}'...'\u{0F47}'    // TIBETAN LETTER KA...TIBETAN LETTER JA
                                | '\u{0F49}'...'\u{0F6C}'    // TIBETAN LETTER NYA...TIBETAN LETTER RRA
                                | '\u{0F71}'...'\u{0F7E}'    // TIBETAN VOWEL SIGN AA...TIBETAN SIGN RJES SU NGA RO
                                | '\u{0F7F}'                 // TIBETAN SIGN RNAM BCAD
                                | '\u{0F80}'...'\u{0F84}'    // TIBETAN VOWEL SIGN REVERSED I...TIBETAN MARK HALANTA
                                | '\u{0F86}'...'\u{0F87}'    // TIBETAN SIGN LCI RTAGS...TIBETAN SIGN YANG RTAGS
                                | '\u{0F88}'...'\u{0F8C}'    // TIBETAN SIGN LCE TSA CAN...TIBETAN SIGN INVERTED MCHU CAN
                                | '\u{0F8D}'...'\u{0F97}'    // TIBETAN SUBJOINED SIGN LCE TSA CAN...TIBETAN SUBJOINED LETTER JA
                                | '\u{0F99}'...'\u{0FBC}'    // TIBETAN SUBJOINED LETTER NYA...TIBETAN SUBJOINED LETTER FIXED-FORM RA
                                | '\u{0FC6}'                 // TIBETAN SYMBOL PADMA GDAN
                                | '\u{1000}'...'\u{102A}'    // MYANMAR LETTER KA...MYANMAR LETTER AU
                                | '\u{102B}'...'\u{102C}'    // MYANMAR VOWEL SIGN TALL AA...MYANMAR VOWEL SIGN AA
                                | '\u{102D}'...'\u{1030}'    // MYANMAR VOWEL SIGN I...MYANMAR VOWEL SIGN UU
                                | '\u{1031}'                 // MYANMAR VOWEL SIGN E
                                | '\u{1032}'...'\u{1037}'    // MYANMAR VOWEL SIGN AI...MYANMAR SIGN DOT BELOW
                                | '\u{1038}'                 // MYANMAR SIGN VISARGA
                                | '\u{1039}'...'\u{103A}'    // MYANMAR SIGN VIRAMA...MYANMAR SIGN ASAT
                                | '\u{103B}'...'\u{103C}'    // MYANMAR CONSONANT SIGN MEDIAL YA...MYANMAR CONSONANT SIGN MEDIAL RA
                                | '\u{103D}'...'\u{103E}'    // MYANMAR CONSONANT SIGN MEDIAL WA...MYANMAR CONSONANT SIGN MEDIAL HA
                                | '\u{103F}'                 // MYANMAR LETTER GREAT SA
                                | '\u{1040}'...'\u{1049}'    // MYANMAR DIGIT ZERO...MYANMAR DIGIT NINE
                                | '\u{1050}'...'\u{1055}'    // MYANMAR LETTER SHA...MYANMAR LETTER VOCALIC LL
                                | '\u{1056}'...'\u{1057}'    // MYANMAR VOWEL SIGN VOCALIC R...MYANMAR VOWEL SIGN VOCALIC RR
                                | '\u{1058}'...'\u{1059}'    // MYANMAR VOWEL SIGN VOCALIC L...MYANMAR VOWEL SIGN VOCALIC LL
                                | '\u{105A}'...'\u{105D}'    // MYANMAR LETTER MON NGA...MYANMAR LETTER MON BBE
                                | '\u{105E}'...'\u{1060}'    // MYANMAR CONSONANT SIGN MON MEDIAL NA...MYANMAR CONSONANT SIGN MON MEDIAL LA
                                | '\u{1061}'                 // MYANMAR LETTER SGAW KAREN SHA
                                | '\u{1062}'...'\u{1064}'    // MYANMAR VOWEL SIGN SGAW KAREN EU...MYANMAR TONE MARK SGAW KAREN KE PHO
                                | '\u{1065}'...'\u{1066}'    // MYANMAR LETTER WESTERN PWO KAREN THA...MYANMAR LETTER WESTERN PWO KAREN PWA
                                | '\u{1067}'...'\u{106D}'    // MYANMAR VOWEL SIGN WESTERN PWO KAREN EU...MYANMAR SIGN WESTERN PWO KAREN TONE-5
                                | '\u{106E}'...'\u{1070}'    // MYANMAR LETTER EASTERN PWO KAREN NNA...MYANMAR LETTER EASTERN PWO KAREN GHWA
                                | '\u{1071}'...'\u{1074}'    // MYANMAR VOWEL SIGN GEBA KAREN I...MYANMAR VOWEL SIGN KAYAH EE
                                | '\u{1075}'...'\u{1081}'    // MYANMAR LETTER SHAN KA...MYANMAR LETTER SHAN HA
                                | '\u{1082}'                 // MYANMAR CONSONANT SIGN SHAN MEDIAL WA
                                | '\u{1083}'...'\u{1084}'    // MYANMAR VOWEL SIGN SHAN AA...MYANMAR VOWEL SIGN SHAN E
                                | '\u{1085}'...'\u{1086}'    // MYANMAR VOWEL SIGN SHAN E ABOVE...MYANMAR VOWEL SIGN SHAN FINAL Y
                                | '\u{1087}'...'\u{108C}'    // MYANMAR SIGN SHAN TONE-2...MYANMAR SIGN SHAN COUNCIL TONE-3
                                | '\u{108D}'                 // MYANMAR SIGN SHAN COUNCIL EMPHATIC TONE
                                | '\u{108E}'                 // MYANMAR LETTER RUMAI PALAUNG FA
                                | '\u{108F}'                 // MYANMAR SIGN RUMAI PALAUNG TONE-5
                                | '\u{1090}'...'\u{1099}'    // MYANMAR SHAN DIGIT ZERO...MYANMAR SHAN DIGIT NINE
                                | '\u{109A}'...'\u{109C}'    // MYANMAR SIGN KHAMTI TONE-1...MYANMAR VOWEL SIGN AITON A
                                | '\u{109D}'                 // MYANMAR VOWEL SIGN AITON AI
                                | '\u{10A0}'...'\u{10C5}'    // GEORGIAN CAPITAL LETTER AN...GEORGIAN CAPITAL LETTER HOE
                                | '\u{10C7}'                 // GEORGIAN CAPITAL LETTER YN
                                | '\u{10CD}'                 // GEORGIAN CAPITAL LETTER AEN
                                | '\u{10D0}'...'\u{10FA}'    // GEORGIAN LETTER AN...GEORGIAN LETTER AIN
                                | '\u{10FC}'                 // MODIFIER LETTER GEORGIAN NAR
                                | '\u{10FD}'...'\u{1248}'    // GEORGIAN LETTER AEN...ETHIOPIC SYLLABLE QWA
                                | '\u{124A}'...'\u{124D}'    // ETHIOPIC SYLLABLE QWI...ETHIOPIC SYLLABLE QWE
                                | '\u{1250}'...'\u{1256}'    // ETHIOPIC SYLLABLE QHA...ETHIOPIC SYLLABLE QHO
                                | '\u{1258}'                 // ETHIOPIC SYLLABLE QHWA
                                | '\u{125A}'...'\u{125D}'    // ETHIOPIC SYLLABLE QHWI...ETHIOPIC SYLLABLE QHWE
                                | '\u{1260}'...'\u{1288}'    // ETHIOPIC SYLLABLE BA...ETHIOPIC SYLLABLE XWA
                                | '\u{128A}'...'\u{128D}'    // ETHIOPIC SYLLABLE XWI...ETHIOPIC SYLLABLE XWE
                                | '\u{1290}'...'\u{12B0}'    // ETHIOPIC SYLLABLE NA...ETHIOPIC SYLLABLE KWA
                                | '\u{12B2}'...'\u{12B5}'    // ETHIOPIC SYLLABLE KWI...ETHIOPIC SYLLABLE KWE
                                | '\u{12B8}'...'\u{12BE}'    // ETHIOPIC SYLLABLE KXA...ETHIOPIC SYLLABLE KXO
                                | '\u{12C0}'                 // ETHIOPIC SYLLABLE KXWA
                                | '\u{12C2}'...'\u{12C5}'    // ETHIOPIC SYLLABLE KXWI...ETHIOPIC SYLLABLE KXWE
                                | '\u{12C8}'...'\u{12D6}'    // ETHIOPIC SYLLABLE WA...ETHIOPIC SYLLABLE PHARYNGEAL O
                                | '\u{12D8}'...'\u{1310}'    // ETHIOPIC SYLLABLE ZA...ETHIOPIC SYLLABLE GWA
                                | '\u{1312}'...'\u{1315}'    // ETHIOPIC SYLLABLE GWI...ETHIOPIC SYLLABLE GWE
                                | '\u{1318}'...'\u{135A}'    // ETHIOPIC SYLLABLE GGA...ETHIOPIC SYLLABLE FYA
                                | '\u{135D}'...'\u{135F}'    // ETHIOPIC COMBINING GEMINATION AND VOWEL LENGTH MARK...ETHIOPIC COMBINING GEMINATION MARK
                                | '\u{1369}'...'\u{1371}'    // ETHIOPIC DIGIT ONE...ETHIOPIC DIGIT NINE
                                | '\u{1380}'...'\u{138F}'    // ETHIOPIC SYLLABLE SEBATBEIT MWA...ETHIOPIC SYLLABLE PWE
                                | '\u{13A0}'...'\u{13F5}'    // CHEROKEE LETTER A...CHEROKEE LETTER MV
                                | '\u{13F8}'...'\u{13FD}'    // CHEROKEE SMALL LETTER YE...CHEROKEE SMALL LETTER MV
                                | '\u{1401}'...'\u{166C}'    // CANADIAN SYLLABICS E...CANADIAN SYLLABICS CARRIER TTSA
                                | '\u{166F}'...'\u{167F}'    // CANADIAN SYLLABICS QAI...CANADIAN SYLLABICS BLACKFOOT W
                                | '\u{1681}'...'\u{169A}'    // OGHAM LETTER BEITH...OGHAM LETTER PEITH
                                | '\u{16A0}'...'\u{16EA}'    // RUNIC LETTER FEHU FEOH FE F...RUNIC LETTER X
                                | '\u{16EE}'...'\u{16F0}'    // RUNIC ARLAUG SYMBOL...RUNIC BELGTHOR SYMBOL
                                | '\u{16F1}'...'\u{16F8}'    // RUNIC LETTER K...RUNIC LETTER FRANKS CASKET AESC
                                | '\u{1700}'...'\u{170C}'    // TAGALOG LETTER A...TAGALOG LETTER YA
                                | '\u{170E}'...'\u{1711}'    // TAGALOG LETTER LA...TAGALOG LETTER HA
                                | '\u{1712}'...'\u{1714}'    // TAGALOG VOWEL SIGN I...TAGALOG SIGN VIRAMA
                                | '\u{1720}'...'\u{1731}'    // HANUNOO LETTER A...HANUNOO LETTER HA
                                | '\u{1732}'...'\u{1734}'    // HANUNOO VOWEL SIGN I...HANUNOO SIGN PAMUDPOD
                                | '\u{1740}'...'\u{1751}'    // BUHID LETTER A...BUHID LETTER HA
                                | '\u{1752}'...'\u{1753}'    // BUHID VOWEL SIGN I...BUHID VOWEL SIGN U
                                | '\u{1760}'...'\u{176C}'    // TAGBANWA LETTER A...TAGBANWA LETTER YA
                                | '\u{176E}'...'\u{1770}'    // TAGBANWA LETTER LA...TAGBANWA LETTER SA
                                | '\u{1772}'...'\u{1773}'    // TAGBANWA VOWEL SIGN I...TAGBANWA VOWEL SIGN U
                                | '\u{1780}'...'\u{17B3}'    // KHMER LETTER KA...KHMER INDEPENDENT VOWEL QAU
                                | '\u{17B4}'...'\u{17B5}'    // KHMER VOWEL INHERENT AQ...KHMER VOWEL INHERENT AA
                                | '\u{17B6}'                 // KHMER VOWEL SIGN AA
                                | '\u{17B7}'...'\u{17BD}'    // KHMER VOWEL SIGN I...KHMER VOWEL SIGN UA
                                | '\u{17BE}'...'\u{17C5}'    // KHMER VOWEL SIGN OE...KHMER VOWEL SIGN AU
                                | '\u{17C6}'                 // KHMER SIGN NIKAHIT
                                | '\u{17C7}'...'\u{17C8}'    // KHMER SIGN REAHMUK...KHMER SIGN YUUKALEAPINTU
                                | '\u{17C9}'...'\u{17D3}'    // KHMER SIGN MUUSIKATOAN...KHMER SIGN BATHAMASAT
                                | '\u{17D7}'                 // KHMER SIGN LEK TOO
                                | '\u{17DC}'                 // KHMER SIGN AVAKRAHASANYA
                                | '\u{17DD}'                 // KHMER SIGN ATTHACAN
                                | '\u{17E0}'...'\u{17E9}'    // KHMER DIGIT ZERO...KHMER DIGIT NINE
                                | '\u{180B}'...'\u{180D}'    // MONGOLIAN FREE VARIATION SELECTOR ONE...MONGOLIAN FREE VARIATION SELECTOR THREE
                                | '\u{1810}'...'\u{1819}'    // MONGOLIAN DIGIT ZERO...MONGOLIAN DIGIT NINE
                                | '\u{1820}'...'\u{1842}'    // MONGOLIAN LETTER A...MONGOLIAN LETTER CHI
                                | '\u{1843}'                 // MONGOLIAN LETTER TODO LONG VOWEL SIGN
                                | '\u{1844}'...'\u{1877}'    // MONGOLIAN LETTER TODO E...MONGOLIAN LETTER MANCHU ZHA
                                | '\u{1880}'...'\u{1884}'    // MONGOLIAN LETTER ALI GALI ANUSVARA ONE...MONGOLIAN LETTER ALI GALI INVERTED UBADAMA
                                | '\u{1885}'...'\u{1886}'    // MONGOLIAN LETTER ALI GALI BALUDA...MONGOLIAN LETTER ALI GALI THREE BALUDA
                                | '\u{1887}'...'\u{18A8}'    // MONGOLIAN LETTER ALI GALI A...MONGOLIAN LETTER MANCHU ALI GALI BHA
                                | '\u{18A9}'                 // MONGOLIAN LETTER ALI GALI DAGALGA
                                | '\u{18AA}'                 // MONGOLIAN LETTER MANCHU ALI GALI LHA
                                | '\u{18B0}'...'\u{18F5}'    // CANADIAN SYLLABICS OY...CANADIAN SYLLABICS CARRIER DENTAL S
                                | '\u{1900}'...'\u{191E}'    // LIMBU VOWEL-CARRIER LETTER...LIMBU LETTER TRA
                                | '\u{1920}'...'\u{1922}'    // LIMBU VOWEL SIGN A...LIMBU VOWEL SIGN U
                                | '\u{1923}'...'\u{1926}'    // LIMBU VOWEL SIGN EE...LIMBU VOWEL SIGN AU
                                | '\u{1927}'...'\u{1928}'    // LIMBU VOWEL SIGN E...LIMBU VOWEL SIGN O
                                | '\u{1929}'...'\u{192B}'    // LIMBU SUBJOINED LETTER YA...LIMBU SUBJOINED LETTER WA
                                | '\u{1930}'...'\u{1931}'    // LIMBU SMALL LETTER KA...LIMBU SMALL LETTER NGA
                                | '\u{1932}'                 // LIMBU SMALL LETTER ANUSVARA
                                | '\u{1933}'...'\u{1938}'    // LIMBU SMALL LETTER TA...LIMBU SMALL LETTER LA
                                | '\u{1939}'...'\u{193B}'    // LIMBU SIGN MUKPHRENG...LIMBU SIGN SA-I
                                | '\u{1946}'...'\u{194F}'    // LIMBU DIGIT ZERO...LIMBU DIGIT NINE
                                | '\u{1950}'...'\u{196D}'    // TAI LE LETTER KA...TAI LE LETTER AI
                                | '\u{1970}'...'\u{1974}'    // TAI LE LETTER TONE-2...TAI LE LETTER TONE-6
                                | '\u{1980}'...'\u{19AB}'    // NEW TAI LUE LETTER HIGH QA...NEW TAI LUE LETTER LOW SUA
                                | '\u{19B0}'...'\u{19C9}'    // NEW TAI LUE VOWEL SIGN VOWEL SHORTENER...NEW TAI LUE TONE MARK-2
                                | '\u{19D0}'...'\u{19D9}'    // NEW TAI LUE DIGIT ZERO...NEW TAI LUE DIGIT NINE
                                | '\u{19DA}'                 // NEW TAI LUE THAM DIGIT ONE
                                | '\u{1A00}'...'\u{1A16}'    // BUGINESE LETTER KA...BUGINESE LETTER HA
                                | '\u{1A17}'...'\u{1A18}'    // BUGINESE VOWEL SIGN I...BUGINESE VOWEL SIGN U
                                | '\u{1A19}'...'\u{1A1A}'    // BUGINESE VOWEL SIGN E...BUGINESE VOWEL SIGN O
                                | '\u{1A1B}'                 // BUGINESE VOWEL SIGN AE
                                | '\u{1A20}'...'\u{1A54}'    // TAI THAM LETTER HIGH KA...TAI THAM LETTER GREAT SA
                                | '\u{1A55}'                 // TAI THAM CONSONANT SIGN MEDIAL RA
                                | '\u{1A56}'                 // TAI THAM CONSONANT SIGN MEDIAL LA
                                | '\u{1A57}'                 // TAI THAM CONSONANT SIGN LA TANG LAI
                                | '\u{1A58}'...'\u{1A5E}'    // TAI THAM SIGN MAI KANG LAI...TAI THAM CONSONANT SIGN SA
                                | '\u{1A60}'                 // TAI THAM SIGN SAKOT
                                | '\u{1A61}'                 // TAI THAM VOWEL SIGN A
                                | '\u{1A62}'                 // TAI THAM VOWEL SIGN MAI SAT
                                | '\u{1A63}'...'\u{1A64}'    // TAI THAM VOWEL SIGN AA...TAI THAM VOWEL SIGN TALL AA
                                | '\u{1A65}'...'\u{1A6C}'    // TAI THAM VOWEL SIGN I...TAI THAM VOWEL SIGN OA BELOW
                                | '\u{1A6D}'...'\u{1A72}'    // TAI THAM VOWEL SIGN OY...TAI THAM VOWEL SIGN THAM AI
                                | '\u{1A73}'...'\u{1A7C}'    // TAI THAM VOWEL SIGN OA ABOVE...TAI THAM SIGN KHUEN-LUE KARAN
                                | '\u{1A7F}'                 // TAI THAM COMBINING CRYPTOGRAMMIC DOT
                                | '\u{1A80}'...'\u{1A89}'    // TAI THAM HORA DIGIT ZERO...TAI THAM HORA DIGIT NINE
                                | '\u{1A90}'...'\u{1A99}'    // TAI THAM THAM DIGIT ZERO...TAI THAM THAM DIGIT NINE
                                | '\u{1AA7}'                 // TAI THAM SIGN MAI YAMOK
                                | '\u{1AB0}'...'\u{1ABD}'    // COMBINING DOUBLED CIRCUMFLEX ACCENT...COMBINING PARENTHESES BELOW
                                | '\u{1B00}'...'\u{1B03}'    // BALINESE SIGN ULU RICEM...BALINESE SIGN SURANG
                                | '\u{1B04}'                 // BALINESE SIGN BISAH
                                | '\u{1B05}'...'\u{1B33}'    // BALINESE LETTER AKARA...BALINESE LETTER HA
                                | '\u{1B34}'                 // BALINESE SIGN REREKAN
                                | '\u{1B35}'                 // BALINESE VOWEL SIGN TEDUNG
                                | '\u{1B36}'...'\u{1B3A}'    // BALINESE VOWEL SIGN ULU...BALINESE VOWEL SIGN RA REPA
                                | '\u{1B3B}'                 // BALINESE VOWEL SIGN RA REPA TEDUNG
                                | '\u{1B3C}'                 // BALINESE VOWEL SIGN LA LENGA
                                | '\u{1B3D}'...'\u{1B41}'    // BALINESE VOWEL SIGN LA LENGA TEDUNG...BALINESE VOWEL SIGN TALING REPA TEDUNG
                                | '\u{1B42}'                 // BALINESE VOWEL SIGN PEPET
                                | '\u{1B43}'...'\u{1B44}'    // BALINESE VOWEL SIGN PEPET TEDUNG...BALINESE ADEG ADEG
                                | '\u{1B45}'...'\u{1B4B}'    // BALINESE LETTER KAF SASAK...BALINESE LETTER ASYURA SASAK
                                | '\u{1B50}'...'\u{1B59}'    // BALINESE DIGIT ZERO...BALINESE DIGIT NINE
                                | '\u{1B6B}'...'\u{1B73}'    // BALINESE MUSICAL SYMBOL COMBINING TEGEH...BALINESE MUSICAL SYMBOL COMBINING GONG
                                | '\u{1B80}'...'\u{1B81}'    // SUNDANESE SIGN PANYECEK...SUNDANESE SIGN PANGLAYAR
                                | '\u{1B82}'                 // SUNDANESE SIGN PANGWISAD
                                | '\u{1B83}'...'\u{1BA0}'    // SUNDANESE LETTER A...SUNDANESE LETTER HA
                                | '\u{1BA1}'                 // SUNDANESE CONSONANT SIGN PAMINGKAL
                                | '\u{1BA2}'...'\u{1BA5}'    // SUNDANESE CONSONANT SIGN PANYAKRA...SUNDANESE VOWEL SIGN PANYUKU
                                | '\u{1BA6}'...'\u{1BA7}'    // SUNDANESE VOWEL SIGN PANAELAENG...SUNDANESE VOWEL SIGN PANOLONG
                                | '\u{1BA8}'...'\u{1BA9}'    // SUNDANESE VOWEL SIGN PAMEPET...SUNDANESE VOWEL SIGN PANEULEUNG
                                | '\u{1BAA}'                 // SUNDANESE SIGN PAMAAEH
                                | '\u{1BAB}'...'\u{1BAD}'    // SUNDANESE SIGN VIRAMA...SUNDANESE CONSONANT SIGN PASANGAN WA
                                | '\u{1BAE}'...'\u{1BAF}'    // SUNDANESE LETTER KHA...SUNDANESE LETTER SYA
                                | '\u{1BB0}'...'\u{1BB9}'    // SUNDANESE DIGIT ZERO...SUNDANESE DIGIT NINE
                                | '\u{1BBA}'...'\u{1BE5}'    // SUNDANESE AVAGRAHA...BATAK LETTER U
                                | '\u{1BE6}'                 // BATAK SIGN TOMPI
                                | '\u{1BE7}'                 // BATAK VOWEL SIGN E
                                | '\u{1BE8}'...'\u{1BE9}'    // BATAK VOWEL SIGN PAKPAK E...BATAK VOWEL SIGN EE
                                | '\u{1BEA}'...'\u{1BEC}'    // BATAK VOWEL SIGN I...BATAK VOWEL SIGN O
                                | '\u{1BED}'                 // BATAK VOWEL SIGN KARO O
                                | '\u{1BEE}'                 // BATAK VOWEL SIGN U
                                | '\u{1BEF}'...'\u{1BF1}'    // BATAK VOWEL SIGN U FOR SIMALUNGUN SA...BATAK CONSONANT SIGN H
                                | '\u{1BF2}'...'\u{1BF3}'    // BATAK PANGOLAT...BATAK PANONGONAN
                                | '\u{1C00}'...'\u{1C23}'    // LEPCHA LETTER KA...LEPCHA LETTER A
                                | '\u{1C24}'...'\u{1C2B}'    // LEPCHA SUBJOINED LETTER YA...LEPCHA VOWEL SIGN UU
                                | '\u{1C2C}'...'\u{1C33}'    // LEPCHA VOWEL SIGN E...LEPCHA CONSONANT SIGN T
                                | '\u{1C34}'...'\u{1C35}'    // LEPCHA CONSONANT SIGN NYIN-DO...LEPCHA CONSONANT SIGN KANG
                                | '\u{1C36}'...'\u{1C37}'    // LEPCHA SIGN RAN...LEPCHA SIGN NUKTA
                                | '\u{1C40}'...'\u{1C49}'    // LEPCHA DIGIT ZERO...LEPCHA DIGIT NINE
                                | '\u{1C4D}'...'\u{1C4F}'    // LEPCHA LETTER TTA...LEPCHA LETTER DDA
                                | '\u{1C50}'...'\u{1C59}'    // OL CHIKI DIGIT ZERO...OL CHIKI DIGIT NINE
                                | '\u{1C5A}'...'\u{1C77}'    // OL CHIKI LETTER LA...OL CHIKI LETTER OH
                                | '\u{1C78}'...'\u{1C7D}'    // OL CHIKI MU TTUDDAG...OL CHIKI AHAD
                                | '\u{1C80}'...'\u{1C88}'    // CYRILLIC SMALL LETTER ROUNDED VE...CYRILLIC SMALL LETTER UNBLENDED UK
                                | '\u{1CD0}'...'\u{1CD2}'    // VEDIC TONE KARSHANA...VEDIC TONE PRENKHA
                                | '\u{1CD4}'...'\u{1CE0}'    // VEDIC SIGN YAJURVEDIC MIDLINE SVARITA...VEDIC TONE RIGVEDIC KASHMIRI INDEPENDENT SVARITA
                                | '\u{1CE1}'                 // VEDIC TONE ATHARVAVEDIC INDEPENDENT SVARITA
                                | '\u{1CE2}'...'\u{1CE8}'    // VEDIC SIGN VISARGA SVARITA...VEDIC SIGN VISARGA ANUDATTA WITH TAIL
                                | '\u{1CE9}'...'\u{1CEC}'    // VEDIC SIGN ANUSVARA ANTARGOMUKHA...VEDIC SIGN ANUSVARA VAMAGOMUKHA WITH TAIL
                                | '\u{1CED}'                 // VEDIC SIGN TIRYAK
                                | '\u{1CEE}'...'\u{1CF1}'    // VEDIC SIGN HEXIFORM LONG ANUSVARA...VEDIC SIGN ANUSVARA UBHAYATO MUKHA
                                | '\u{1CF2}'...'\u{1CF3}'    // VEDIC SIGN ARDHAVISARGA...VEDIC SIGN ROTATED ARDHAVISARGA
                                | '\u{1CF4}'                 // VEDIC TONE CANDRA ABOVE
                                | '\u{1CF5}'...'\u{1CF6}'    // VEDIC SIGN JIHVAMULIYA...VEDIC SIGN UPADHMANIYA
                                | '\u{1CF7}'                 // VEDIC SIGN ATIKRAMA
                                | '\u{1CF8}'...'\u{1CF9}'    // VEDIC TONE RING ABOVE...VEDIC TONE DOUBLE RING ABOVE
                                | '\u{1D00}'...'\u{1D2B}'    // LATIN LETTER SMALL CAPITAL A...CYRILLIC LETTER SMALL CAPITAL EL
                                | '\u{1D2C}'...'\u{1D6A}'    // MODIFIER LETTER CAPITAL A...GREEK SUBSCRIPT SMALL LETTER CHI
                                | '\u{1D6B}'...'\u{1D77}'    // LATIN SMALL LETTER UE...LATIN SMALL LETTER TURNED G
                                | '\u{1D78}'                 // MODIFIER LETTER CYRILLIC EN
                                | '\u{1D79}'...'\u{1D9A}'    // LATIN SMALL LETTER INSULAR G...LATIN SMALL LETTER EZH WITH RETROFLEX HOOK
                                | '\u{1D9B}'...'\u{1DBF}'    // MODIFIER LETTER SMALL TURNED ALPHA...MODIFIER LETTER SMALL THETA
                                | '\u{1DC0}'...'\u{1DF9}'    // COMBINING DOTTED GRAVE ACCENT...COMBINING WIDE INVERTED BRIDGE BELOW
                                | '\u{1DFB}'...'\u{1DFF}'    // COMBINING DELETION MARK...COMBINING RIGHT ARROWHEAD AND DOWN ARROWHEAD BELOW
                                | '\u{1E00}'...'\u{1F15}'    // LATIN CAPITAL LETTER A WITH RING BELOW...GREEK SMALL LETTER EPSILON WITH DASIA AND OXIA
                                | '\u{1F18}'...'\u{1F1D}'    // GREEK CAPITAL LETTER EPSILON WITH PSILI...GREEK CAPITAL LETTER EPSILON WITH DASIA AND OXIA
                                | '\u{1F20}'...'\u{1F45}'    // GREEK SMALL LETTER ETA WITH PSILI...GREEK SMALL LETTER OMICRON WITH DASIA AND OXIA
                                | '\u{1F48}'...'\u{1F4D}'    // GREEK CAPITAL LETTER OMICRON WITH PSILI...GREEK CAPITAL LETTER OMICRON WITH DASIA AND OXIA
                                | '\u{1F50}'...'\u{1F57}'    // GREEK SMALL LETTER UPSILON WITH PSILI...GREEK SMALL LETTER UPSILON WITH DASIA AND PERISPOMENI
                                | '\u{1F59}'                 // GREEK CAPITAL LETTER UPSILON WITH DASIA
                                | '\u{1F5B}'                 // GREEK CAPITAL LETTER UPSILON WITH DASIA AND VARIA
                                | '\u{1F5D}'                 // GREEK CAPITAL LETTER UPSILON WITH DASIA AND OXIA
                                | '\u{1F5F}'...'\u{1F7D}'    // GREEK CAPITAL LETTER UPSILON WITH DASIA AND PERISPOMENI...GREEK SMALL LETTER OMEGA WITH OXIA
                                | '\u{1F80}'...'\u{1FB4}'    // GREEK SMALL LETTER ALPHA WITH PSILI AND YPOGEGRAMMENI...GREEK SMALL LETTER ALPHA WITH OXIA AND YPOGEGRAMMENI
                                | '\u{1FB6}'...'\u{1FBC}'    // GREEK SMALL LETTER ALPHA WITH PERISPOMENI...GREEK CAPITAL LETTER ALPHA WITH PROSGEGRAMMENI
                                | '\u{1FBE}'                 // GREEK PROSGEGRAMMENI
                                | '\u{1FC2}'...'\u{1FC4}'    // GREEK SMALL LETTER ETA WITH VARIA AND YPOGEGRAMMENI...GREEK SMALL LETTER ETA WITH OXIA AND YPOGEGRAMMENI
                                | '\u{1FC6}'...'\u{1FCC}'    // GREEK SMALL LETTER ETA WITH PERISPOMENI...GREEK CAPITAL LETTER ETA WITH PROSGEGRAMMENI
                                | '\u{1FD0}'...'\u{1FD3}'    // GREEK SMALL LETTER IOTA WITH VRACHY...GREEK SMALL LETTER IOTA WITH DIALYTIKA AND OXIA
                                | '\u{1FD6}'...'\u{1FDB}'    // GREEK SMALL LETTER IOTA WITH PERISPOMENI...GREEK CAPITAL LETTER IOTA WITH OXIA
                                | '\u{1FE0}'...'\u{1FEC}'    // GREEK SMALL LETTER UPSILON WITH VRACHY...GREEK CAPITAL LETTER RHO WITH DASIA
                                | '\u{1FF2}'...'\u{1FF4}'    // GREEK SMALL LETTER OMEGA WITH VARIA AND YPOGEGRAMMENI...GREEK SMALL LETTER OMEGA WITH OXIA AND YPOGEGRAMMENI
                                | '\u{1FF6}'...'\u{1FFC}'    // GREEK SMALL LETTER OMEGA WITH PERISPOMENI...GREEK CAPITAL LETTER OMEGA WITH PROSGEGRAMMENI
                                | '\u{203F}'...'\u{2040}'    // UNDERTIE...CHARACTER TIE
                                | '\u{2054}'                 // INVERTED UNDERTIE
                                | '\u{2071}'                 // SUPERSCRIPT LATIN SMALL LETTER I
                                | '\u{207F}'                 // SUPERSCRIPT LATIN SMALL LETTER N
                                | '\u{2090}'...'\u{209C}'    // LATIN SUBSCRIPT SMALL LETTER A...LATIN SUBSCRIPT SMALL LETTER T
                                | '\u{20D0}'...'\u{20DC}'    // COMBINING LEFT HARPOON ABOVE...COMBINING FOUR DOTS ABOVE
                                | '\u{20E1}'                 // COMBINING LEFT RIGHT ARROW ABOVE
                                | '\u{20E5}'...'\u{20F0}'    // COMBINING REVERSE SOLIDUS OVERLAY...COMBINING ASTERISK ABOVE
                                | '\u{2102}'                 // DOUBLE-STRUCK CAPITAL C
                                | '\u{2107}'                 // EULER CONSTANT
                                | '\u{210A}'...'\u{2113}'    // SCRIPT SMALL G...SCRIPT SMALL L
                                | '\u{2115}'                 // DOUBLE-STRUCK CAPITAL N
                                | '\u{2118}'                 // SCRIPT CAPITAL P
                                | '\u{2119}'...'\u{211D}'    // DOUBLE-STRUCK CAPITAL P...DOUBLE-STRUCK CAPITAL R
                                | '\u{2124}'                 // DOUBLE-STRUCK CAPITAL Z
                                | '\u{2126}'                 // OHM SIGN
                                | '\u{2128}'                 // BLACK-LETTER CAPITAL Z
                                | '\u{212A}'...'\u{212D}'    // KELVIN SIGN...BLACK-LETTER CAPITAL C
                                | '\u{212E}'                 // ESTIMATED SYMBOL
                                | '\u{212F}'...'\u{2134}'    // SCRIPT SMALL E...SCRIPT SMALL O
                                | '\u{2135}'...'\u{2138}'    // ALEF SYMBOL...DALET SYMBOL
                                | '\u{2139}'                 // INFORMATION SOURCE
                                | '\u{213C}'...'\u{213F}'    // DOUBLE-STRUCK SMALL PI...DOUBLE-STRUCK CAPITAL PI
                                | '\u{2145}'...'\u{2149}'    // DOUBLE-STRUCK ITALIC CAPITAL D...DOUBLE-STRUCK ITALIC SMALL J
                                | '\u{214E}'                 // TURNED SMALL F
                                | '\u{2160}'...'\u{2182}'    // ROMAN NUMERAL ONE...ROMAN NUMERAL TEN THOUSAND
                                | '\u{2183}'...'\u{2184}'    // ROMAN NUMERAL REVERSED ONE HUNDRED...LATIN SMALL LETTER REVERSED C
                                | '\u{2185}'...'\u{2188}'    // ROMAN NUMERAL SIX LATE FORM...ROMAN NUMERAL ONE HUNDRED THOUSAND
                                | '\u{2C00}'...'\u{2C2E}'    // GLAGOLITIC CAPITAL LETTER AZU...GLAGOLITIC CAPITAL LETTER LATINATE MYSLITE
                                | '\u{2C30}'...'\u{2C5E}'    // GLAGOLITIC SMALL LETTER AZU...GLAGOLITIC SMALL LETTER LATINATE MYSLITE
                                | '\u{2C60}'...'\u{2C7B}'    // LATIN CAPITAL LETTER L WITH DOUBLE BAR...LATIN LETTER SMALL CAPITAL TURNED E
                                | '\u{2C7C}'...'\u{2C7D}'    // LATIN SUBSCRIPT SMALL LETTER J...MODIFIER LETTER CAPITAL V
                                | '\u{2C7E}'...'\u{2CE4}'    // LATIN CAPITAL LETTER S WITH SWASH TAIL...COPTIC SYMBOL KAI
                                | '\u{2CEB}'...'\u{2CEE}'    // COPTIC CAPITAL LETTER CRYPTOGRAMMIC SHEI...COPTIC SMALL LETTER CRYPTOGRAMMIC GANGIA
                                | '\u{2CEF}'...'\u{2CF1}'    // COPTIC COMBINING NI ABOVE...COPTIC COMBINING SPIRITUS LENIS
                                | '\u{2CF2}'...'\u{2CF3}'    // COPTIC CAPITAL LETTER BOHAIRIC KHEI...COPTIC SMALL LETTER BOHAIRIC KHEI
                                | '\u{2D00}'...'\u{2D25}'    // GEORGIAN SMALL LETTER AN...GEORGIAN SMALL LETTER HOE
                                | '\u{2D27}'                 // GEORGIAN SMALL LETTER YN
                                | '\u{2D2D}'                 // GEORGIAN SMALL LETTER AEN
                                | '\u{2D30}'...'\u{2D67}'    // TIFINAGH LETTER YA...TIFINAGH LETTER YO
                                | '\u{2D6F}'                 // TIFINAGH MODIFIER LETTER LABIALIZATION MARK
                                | '\u{2D7F}'                 // TIFINAGH CONSONANT JOINER
                                | '\u{2D80}'...'\u{2D96}'    // ETHIOPIC SYLLABLE LOA...ETHIOPIC SYLLABLE GGWE
                                | '\u{2DA0}'...'\u{2DA6}'    // ETHIOPIC SYLLABLE SSA...ETHIOPIC SYLLABLE SSO
                                | '\u{2DA8}'...'\u{2DAE}'    // ETHIOPIC SYLLABLE CCA...ETHIOPIC SYLLABLE CCO
                                | '\u{2DB0}'...'\u{2DB6}'    // ETHIOPIC SYLLABLE ZZA...ETHIOPIC SYLLABLE ZZO
                                | '\u{2DB8}'...'\u{2DBE}'    // ETHIOPIC SYLLABLE CCHA...ETHIOPIC SYLLABLE CCHO
                                | '\u{2DC0}'...'\u{2DC6}'    // ETHIOPIC SYLLABLE QYA...ETHIOPIC SYLLABLE QYO
                                | '\u{2DC8}'...'\u{2DCE}'    // ETHIOPIC SYLLABLE KYA...ETHIOPIC SYLLABLE KYO
                                | '\u{2DD0}'...'\u{2DD6}'    // ETHIOPIC SYLLABLE XYA...ETHIOPIC SYLLABLE XYO
                                | '\u{2DD8}'...'\u{2DDE}'    // ETHIOPIC SYLLABLE GYA...ETHIOPIC SYLLABLE GYO
                                | '\u{2DE0}'...'\u{2DFF}'    // COMBINING CYRILLIC LETTER BE...COMBINING CYRILLIC LETTER IOTIFIED BIG YUS
                                | '\u{3005}'                 // IDEOGRAPHIC ITERATION MARK
                                | '\u{3006}'                 // IDEOGRAPHIC CLOSING MARK
                                | '\u{3007}'                 // IDEOGRAPHIC NUMBER ZERO
                                | '\u{3021}'...'\u{3029}'    // HANGZHOU NUMERAL ONE...HANGZHOU NUMERAL NINE
                                | '\u{302A}'...'\u{302D}'    // IDEOGRAPHIC LEVEL TONE MARK...IDEOGRAPHIC ENTERING TONE MARK
                                | '\u{302E}'...'\u{302F}'    // HANGUL SINGLE DOT TONE MARK...HANGUL DOUBLE DOT TONE MARK
                                | '\u{3031}'...'\u{3035}'    // VERTICAL KANA REPEAT MARK...VERTICAL KANA REPEAT MARK LOWER HALF
                                | '\u{3038}'...'\u{303A}'    // HANGZHOU NUMERAL TEN...HANGZHOU NUMERAL THIRTY
                                | '\u{303B}'                 // VERTICAL IDEOGRAPHIC ITERATION MARK
                                | '\u{303C}'                 // MASU MARK
                                | '\u{3041}'...'\u{3096}'    // HIRAGANA LETTER SMALL A...HIRAGANA LETTER SMALL KE
                                | '\u{3099}'...'\u{309A}'    // COMBINING KATAKANA-HIRAGANA VOICED SOUND MARK...COMBINING KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK
                                | '\u{309B}'...'\u{309C}'    // KATAKANA-HIRAGANA VOICED SOUND MARK...KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK
                                | '\u{309D}'...'\u{309E}'    // HIRAGANA ITERATION MARK...HIRAGANA VOICED ITERATION MARK
                                | '\u{309F}'                 // HIRAGANA DIGRAPH YORI
                                | '\u{30A1}'...'\u{30FA}'    // KATAKANA LETTER SMALL A...KATAKANA LETTER VO
                                | '\u{30FC}'...'\u{30FE}'    // KATAKANA-HIRAGANA PROLONGED SOUND MARK...KATAKANA VOICED ITERATION MARK
                                | '\u{30FF}'                 // KATAKANA DIGRAPH KOTO
                                | '\u{3105}'...'\u{312E}'    // BOPOMOFO LETTER B...BOPOMOFO LETTER O WITH DOT ABOVE
                                | '\u{3131}'...'\u{318E}'    // HANGUL LETTER KIYEOK...HANGUL LETTER ARAEAE
                                | '\u{31A0}'...'\u{31BA}'    // BOPOMOFO LETTER BU...BOPOMOFO LETTER ZY
                                | '\u{31F0}'...'\u{31FF}'    // KATAKANA LETTER SMALL KU...KATAKANA LETTER SMALL RO
                                | '\u{3400}'...'\u{4DB5}'    // CJK UNIFIED IDEOGRAPH-3400...CJK UNIFIED IDEOGRAPH-4DB5
                                | '\u{4E00}'...'\u{9FEA}'    // CJK UNIFIED IDEOGRAPH-4E00...CJK UNIFIED IDEOGRAPH-9FEA
                                | '\u{A000}'...'\u{A014}'    // YI SYLLABLE IT...YI SYLLABLE E
                                | '\u{A015}'                 // YI SYLLABLE WU
                                | '\u{A016}'...'\u{A48C}'    // YI SYLLABLE BIT...YI SYLLABLE YYR
                                | '\u{A4D0}'...'\u{A4F7}'    // LISU LETTER BA...LISU LETTER OE
                                | '\u{A4F8}'...'\u{A4FD}'    // LISU LETTER TONE MYA TI...LISU LETTER TONE MYA JEU
                                | '\u{A500}'...'\u{A60B}'    // VAI SYLLABLE EE...VAI SYLLABLE NG
                                | '\u{A60C}'                 // VAI SYLLABLE LENGTHENER
                                | '\u{A610}'...'\u{A61F}'    // VAI SYLLABLE NDOLE FA...VAI SYMBOL JONG
                                | '\u{A620}'...'\u{A629}'    // VAI DIGIT ZERO...VAI DIGIT NINE
                                | '\u{A62A}'...'\u{A62B}'    // VAI SYLLABLE NDOLE MA...VAI SYLLABLE NDOLE DO
                                | '\u{A640}'...'\u{A66D}'    // CYRILLIC CAPITAL LETTER ZEMLYA...CYRILLIC SMALL LETTER DOUBLE MONOCULAR O
                                | '\u{A66E}'                 // CYRILLIC LETTER MULTIOCULAR O
                                | '\u{A66F}'                 // COMBINING CYRILLIC VZMET
                                | '\u{A674}'...'\u{A67D}'    // COMBINING CYRILLIC LETTER UKRAINIAN IE...COMBINING CYRILLIC PAYEROK
                                | '\u{A67F}'                 // CYRILLIC PAYEROK
                                | '\u{A680}'...'\u{A69B}'    // CYRILLIC CAPITAL LETTER DWE...CYRILLIC SMALL LETTER CROSSED O
                                | '\u{A69C}'...'\u{A69D}'    // MODIFIER LETTER CYRILLIC HARD SIGN...MODIFIER LETTER CYRILLIC SOFT SIGN
                                | '\u{A69E}'...'\u{A69F}'    // COMBINING CYRILLIC LETTER EF...COMBINING CYRILLIC LETTER IOTIFIED E
                                | '\u{A6A0}'...'\u{A6E5}'    // BAMUM LETTER A...BAMUM LETTER KI
                                | '\u{A6E6}'...'\u{A6EF}'    // BAMUM LETTER MO...BAMUM LETTER KOGHOM
                                | '\u{A6F0}'...'\u{A6F1}'    // BAMUM COMBINING MARK KOQNDON...BAMUM COMBINING MARK TUKWENTIS
                                | '\u{A717}'...'\u{A71F}'    // MODIFIER LETTER DOT VERTICAL BAR...MODIFIER LETTER LOW INVERTED EXCLAMATION MARK
                                | '\u{A722}'...'\u{A76F}'    // LATIN CAPITAL LETTER EGYPTOLOGICAL ALEF...LATIN SMALL LETTER CON
                                | '\u{A770}'                 // MODIFIER LETTER US
                                | '\u{A771}'...'\u{A787}'    // LATIN SMALL LETTER DUM...LATIN SMALL LETTER INSULAR T
                                | '\u{A788}'                 // MODIFIER LETTER LOW CIRCUMFLEX ACCENT
                                | '\u{A78B}'...'\u{A78E}'    // LATIN CAPITAL LETTER SALTILLO...LATIN SMALL LETTER L WITH RETROFLEX HOOK AND BELT
                                | '\u{A78F}'                 // LATIN LETTER SINOLOGICAL DOT
                                | '\u{A790}'...'\u{A7AE}'    // LATIN CAPITAL LETTER N WITH DESCENDER...LATIN CAPITAL LETTER SMALL CAPITAL I
                                | '\u{A7B0}'...'\u{A7B7}'    // LATIN CAPITAL LETTER TURNED K...LATIN SMALL LETTER OMEGA
                                | '\u{A7F7}'                 // LATIN EPIGRAPHIC LETTER SIDEWAYS I
                                | '\u{A7F8}'...'\u{A7F9}'    // MODIFIER LETTER CAPITAL H WITH STROKE...MODIFIER LETTER SMALL LIGATURE OE
                                | '\u{A7FA}'                 // LATIN LETTER SMALL CAPITAL TURNED M
                                | '\u{A7FB}'...'\u{A801}'    // LATIN EPIGRAPHIC LETTER REVERSED F...SYLOTI NAGRI LETTER I
                                | '\u{A802}'                 // SYLOTI NAGRI SIGN DVISVARA
                                | '\u{A803}'...'\u{A805}'    // SYLOTI NAGRI LETTER U...SYLOTI NAGRI LETTER O
                                | '\u{A806}'                 // SYLOTI NAGRI SIGN HASANTA
                                | '\u{A807}'...'\u{A80A}'    // SYLOTI NAGRI LETTER KO...SYLOTI NAGRI LETTER GHO
                                | '\u{A80B}'                 // SYLOTI NAGRI SIGN ANUSVARA
                                | '\u{A80C}'...'\u{A822}'    // SYLOTI NAGRI LETTER CO...SYLOTI NAGRI LETTER HO
                                | '\u{A823}'...'\u{A824}'    // SYLOTI NAGRI VOWEL SIGN A...SYLOTI NAGRI VOWEL SIGN I
                                | '\u{A825}'...'\u{A826}'    // SYLOTI NAGRI VOWEL SIGN U...SYLOTI NAGRI VOWEL SIGN E
                                | '\u{A827}'                 // SYLOTI NAGRI VOWEL SIGN OO
                                | '\u{A840}'...'\u{A873}'    // PHAGS-PA LETTER KA...PHAGS-PA LETTER CANDRABINDU
                                | '\u{A880}'...'\u{A881}'    // SAURASHTRA SIGN ANUSVARA...SAURASHTRA SIGN VISARGA
                                | '\u{A882}'...'\u{A8B3}'    // SAURASHTRA LETTER A...SAURASHTRA LETTER LLA
                                | '\u{A8B4}'...'\u{A8C3}'    // SAURASHTRA CONSONANT SIGN HAARU...SAURASHTRA VOWEL SIGN AU
                                | '\u{A8C4}'...'\u{A8C5}'    // SAURASHTRA SIGN VIRAMA...SAURASHTRA SIGN CANDRABINDU
                                | '\u{A8D0}'...'\u{A8D9}'    // SAURASHTRA DIGIT ZERO...SAURASHTRA DIGIT NINE
                                | '\u{A8E0}'...'\u{A8F1}'    // COMBINING DEVANAGARI DIGIT ZERO...COMBINING DEVANAGARI SIGN AVAGRAHA
                                | '\u{A8F2}'...'\u{A8F7}'    // DEVANAGARI SIGN SPACING CANDRABINDU...DEVANAGARI SIGN CANDRABINDU AVAGRAHA
                                | '\u{A8FB}'                 // DEVANAGARI HEADSTROKE
                                | '\u{A8FD}'                 // DEVANAGARI JAIN OM
                                | '\u{A900}'...'\u{A909}'    // KAYAH LI DIGIT ZERO...KAYAH LI DIGIT NINE
                                | '\u{A90A}'...'\u{A925}'    // KAYAH LI LETTER KA...KAYAH LI LETTER OO
                                | '\u{A926}'...'\u{A92D}'    // KAYAH LI VOWEL UE...KAYAH LI TONE CALYA PLOPHU
                                | '\u{A930}'...'\u{A946}'    // REJANG LETTER KA...REJANG LETTER A
                                | '\u{A947}'...'\u{A951}'    // REJANG VOWEL SIGN I...REJANG CONSONANT SIGN R
                                | '\u{A952}'...'\u{A953}'    // REJANG CONSONANT SIGN H...REJANG VIRAMA
                                | '\u{A960}'...'\u{A97C}'    // HANGUL CHOSEONG TIKEUT-MIEUM...HANGUL CHOSEONG SSANGYEORINHIEUH
                                | '\u{A980}'...'\u{A982}'    // JAVANESE SIGN PANYANGGA...JAVANESE SIGN LAYAR
                                | '\u{A983}'                 // JAVANESE SIGN WIGNYAN
                                | '\u{A984}'...'\u{A9B2}'    // JAVANESE LETTER A...JAVANESE LETTER HA
                                | '\u{A9B3}'                 // JAVANESE SIGN CECAK TELU
                                | '\u{A9B4}'...'\u{A9B5}'    // JAVANESE VOWEL SIGN TARUNG...JAVANESE VOWEL SIGN TOLONG
                                | '\u{A9B6}'...'\u{A9B9}'    // JAVANESE VOWEL SIGN WULU...JAVANESE VOWEL SIGN SUKU MENDUT
                                | '\u{A9BA}'...'\u{A9BB}'    // JAVANESE VOWEL SIGN TALING...JAVANESE VOWEL SIGN DIRGA MURE
                                | '\u{A9BC}'                 // JAVANESE VOWEL SIGN PEPET
                                | '\u{A9BD}'...'\u{A9C0}'    // JAVANESE CONSONANT SIGN KERET...JAVANESE PANGKON
                                | '\u{A9CF}'                 // JAVANESE PANGRANGKEP
                                | '\u{A9D0}'...'\u{A9D9}'    // JAVANESE DIGIT ZERO...JAVANESE DIGIT NINE
                                | '\u{A9E0}'...'\u{A9E4}'    // MYANMAR LETTER SHAN GHA...MYANMAR LETTER SHAN BHA
                                | '\u{A9E5}'                 // MYANMAR SIGN SHAN SAW
                                | '\u{A9E6}'                 // MYANMAR MODIFIER LETTER SHAN REDUPLICATION
                                | '\u{A9E7}'...'\u{A9EF}'    // MYANMAR LETTER TAI LAING NYA...MYANMAR LETTER TAI LAING NNA
                                | '\u{A9F0}'...'\u{A9F9}'    // MYANMAR TAI LAING DIGIT ZERO...MYANMAR TAI LAING DIGIT NINE
                                | '\u{A9FA}'...'\u{A9FE}'    // MYANMAR LETTER TAI LAING LLA...MYANMAR LETTER TAI LAING BHA
                                | '\u{AA00}'...'\u{AA28}'    // CHAM LETTER A...CHAM LETTER HA
                                | '\u{AA29}'...'\u{AA2E}'    // CHAM VOWEL SIGN AA...CHAM VOWEL SIGN OE
                                | '\u{AA2F}'...'\u{AA30}'    // CHAM VOWEL SIGN O...CHAM VOWEL SIGN AI
                                | '\u{AA31}'...'\u{AA32}'    // CHAM VOWEL SIGN AU...CHAM VOWEL SIGN UE
                                | '\u{AA33}'...'\u{AA34}'    // CHAM CONSONANT SIGN YA...CHAM CONSONANT SIGN RA
                                | '\u{AA35}'...'\u{AA36}'    // CHAM CONSONANT SIGN LA...CHAM CONSONANT SIGN WA
                                | '\u{AA40}'...'\u{AA42}'    // CHAM LETTER FINAL K...CHAM LETTER FINAL NG
                                | '\u{AA43}'                 // CHAM CONSONANT SIGN FINAL NG
                                | '\u{AA44}'...'\u{AA4B}'    // CHAM LETTER FINAL CH...CHAM LETTER FINAL SS
                                | '\u{AA4C}'                 // CHAM CONSONANT SIGN FINAL M
                                | '\u{AA4D}'                 // CHAM CONSONANT SIGN FINAL H
                                | '\u{AA50}'...'\u{AA59}'    // CHAM DIGIT ZERO...CHAM DIGIT NINE
                                | '\u{AA60}'...'\u{AA6F}'    // MYANMAR LETTER KHAMTI GA...MYANMAR LETTER KHAMTI FA
                                | '\u{AA70}'                 // MYANMAR MODIFIER LETTER KHAMTI REDUPLICATION
                                | '\u{AA71}'...'\u{AA76}'    // MYANMAR LETTER KHAMTI XA...MYANMAR LOGOGRAM KHAMTI HM
                                | '\u{AA7A}'                 // MYANMAR LETTER AITON RA
                                | '\u{AA7B}'                 // MYANMAR SIGN PAO KAREN TONE
                                | '\u{AA7C}'                 // MYANMAR SIGN TAI LAING TONE-2
                                | '\u{AA7D}'                 // MYANMAR SIGN TAI LAING TONE-5
                                | '\u{AA7E}'...'\u{AAAF}'    // MYANMAR LETTER SHWE PALAUNG CHA...TAI VIET LETTER HIGH O
                                | '\u{AAB0}'                 // TAI VIET MAI KANG
                                | '\u{AAB1}'                 // TAI VIET VOWEL AA
                                | '\u{AAB2}'...'\u{AAB4}'    // TAI VIET VOWEL I...TAI VIET VOWEL U
                                | '\u{AAB5}'...'\u{AAB6}'    // TAI VIET VOWEL E...TAI VIET VOWEL O
                                | '\u{AAB7}'...'\u{AAB8}'    // TAI VIET MAI KHIT...TAI VIET VOWEL IA
                                | '\u{AAB9}'...'\u{AABD}'    // TAI VIET VOWEL UEA...TAI VIET VOWEL AN
                                | '\u{AABE}'...'\u{AABF}'    // TAI VIET VOWEL AM...TAI VIET TONE MAI EK
                                | '\u{AAC0}'                 // TAI VIET TONE MAI NUENG
                                | '\u{AAC1}'                 // TAI VIET TONE MAI THO
                                | '\u{AAC2}'                 // TAI VIET TONE MAI SONG
                                | '\u{AADB}'...'\u{AADC}'    // TAI VIET SYMBOL KON...TAI VIET SYMBOL NUENG
                                | '\u{AADD}'                 // TAI VIET SYMBOL SAM
                                | '\u{AAE0}'...'\u{AAEA}'    // MEETEI MAYEK LETTER E...MEETEI MAYEK LETTER SSA
                                | '\u{AAEB}'                 // MEETEI MAYEK VOWEL SIGN II
                                | '\u{AAEC}'...'\u{AAED}'    // MEETEI MAYEK VOWEL SIGN UU...MEETEI MAYEK VOWEL SIGN AAI
                                | '\u{AAEE}'...'\u{AAEF}'    // MEETEI MAYEK VOWEL SIGN AU...MEETEI MAYEK VOWEL SIGN AAU
                                | '\u{AAF2}'                 // MEETEI MAYEK ANJI
                                | '\u{AAF3}'...'\u{AAF4}'    // MEETEI MAYEK SYLLABLE REPETITION MARK...MEETEI MAYEK WORD REPETITION MARK
                                | '\u{AAF5}'                 // MEETEI MAYEK VOWEL SIGN VISARGA
                                | '\u{AAF6}'                 // MEETEI MAYEK VIRAMA
                                | '\u{AB01}'...'\u{AB06}'    // ETHIOPIC SYLLABLE TTHU...ETHIOPIC SYLLABLE TTHO
                                | '\u{AB09}'...'\u{AB0E}'    // ETHIOPIC SYLLABLE DDHU...ETHIOPIC SYLLABLE DDHO
                                | '\u{AB11}'...'\u{AB16}'    // ETHIOPIC SYLLABLE DZU...ETHIOPIC SYLLABLE DZO
                                | '\u{AB20}'...'\u{AB26}'    // ETHIOPIC SYLLABLE CCHHA...ETHIOPIC SYLLABLE CCHHO
                                | '\u{AB28}'...'\u{AB2E}'    // ETHIOPIC SYLLABLE BBA...ETHIOPIC SYLLABLE BBO
                                | '\u{AB30}'...'\u{AB5A}'    // LATIN SMALL LETTER BARRED ALPHA...LATIN SMALL LETTER Y WITH SHORT RIGHT LEG
                                | '\u{AB5C}'...'\u{AB5F}'    // MODIFIER LETTER SMALL HENG...MODIFIER LETTER SMALL U WITH LEFT HOOK
                                | '\u{AB60}'...'\u{AB65}'    // LATIN SMALL LETTER SAKHA YAT...GREEK LETTER SMALL CAPITAL OMEGA
                                | '\u{AB70}'...'\u{ABBF}'    // CHEROKEE SMALL LETTER A...CHEROKEE SMALL LETTER YA
                                | '\u{ABC0}'...'\u{ABE2}'    // MEETEI MAYEK LETTER KOK...MEETEI MAYEK LETTER I LONSUM
                                | '\u{ABE3}'...'\u{ABE4}'    // MEETEI MAYEK VOWEL SIGN ONAP...MEETEI MAYEK VOWEL SIGN INAP
                                | '\u{ABE5}'                 // MEETEI MAYEK VOWEL SIGN ANAP
                                | '\u{ABE6}'...'\u{ABE7}'    // MEETEI MAYEK VOWEL SIGN YENAP...MEETEI MAYEK VOWEL SIGN SOUNAP
                                | '\u{ABE8}'                 // MEETEI MAYEK VOWEL SIGN UNAP
                                | '\u{ABE9}'...'\u{ABEA}'    // MEETEI MAYEK VOWEL SIGN CHEINAP...MEETEI MAYEK VOWEL SIGN NUNG
                                | '\u{ABEC}'                 // MEETEI MAYEK LUM IYEK
                                | '\u{ABED}'                 // MEETEI MAYEK APUN IYEK
                                | '\u{ABF0}'...'\u{ABF9}'    // MEETEI MAYEK DIGIT ZERO...MEETEI MAYEK DIGIT NINE
                                | '\u{AC00}'...'\u{D7A3}'    // HANGUL SYLLABLE GA...HANGUL SYLLABLE HIH
                                | '\u{D7B0}'...'\u{D7C6}'    // HANGUL JUNGSEONG O-YEO...HANGUL JUNGSEONG ARAEA-E
                                | '\u{D7CB}'...'\u{D7FB}'    // HANGUL JONGSEONG NIEUN-RIEUL...HANGUL JONGSEONG PHIEUPH-THIEUTH
                                | '\u{F900}'...'\u{FA6D}'    // CJK COMPATIBILITY IDEOGRAPH-F900...CJK COMPATIBILITY IDEOGRAPH-FA6D
                                | '\u{FA70}'...'\u{FAD9}'    // CJK COMPATIBILITY IDEOGRAPH-FA70...CJK COMPATIBILITY IDEOGRAPH-FAD9
                                | '\u{FB00}'...'\u{FB06}'    // LATIN SMALL LIGATURE FF...LATIN SMALL LIGATURE ST
                                | '\u{FB13}'...'\u{FB17}'    // ARMENIAN SMALL LIGATURE MEN NOW...ARMENIAN SMALL LIGATURE MEN XEH
                                | '\u{FB1D}'                 // HEBREW LETTER YOD WITH HIRIQ
                                | '\u{FB1E}'                 // HEBREW POINT JUDEO-SPANISH VARIKA
                                | '\u{FB1F}'...'\u{FB28}'    // HEBREW LIGATURE YIDDISH YOD YOD PATAH...HEBREW LETTER WIDE TAV
                                | '\u{FB2A}'...'\u{FB36}'    // HEBREW LETTER SHIN WITH SHIN DOT...HEBREW LETTER ZAYIN WITH DAGESH
                                | '\u{FB38}'...'\u{FB3C}'    // HEBREW LETTER TET WITH DAGESH...HEBREW LETTER LAMED WITH DAGESH
                                | '\u{FB3E}'                 // HEBREW LETTER MEM WITH DAGESH
                                | '\u{FB40}'...'\u{FB41}'    // HEBREW LETTER NUN WITH DAGESH...HEBREW LETTER SAMEKH WITH DAGESH
                                | '\u{FB43}'...'\u{FB44}'    // HEBREW LETTER FINAL PE WITH DAGESH...HEBREW LETTER PE WITH DAGESH
                                | '\u{FB46}'...'\u{FBB1}'    // HEBREW LETTER TSADI WITH DAGESH...ARABIC LETTER YEH BARREE WITH HAMZA ABOVE FINAL FORM
                                | '\u{FBD3}'...'\u{FD3D}'    // ARABIC LETTER NG ISOLATED FORM...ARABIC LIGATURE ALEF WITH FATHATAN ISOLATED FORM
                                | '\u{FD50}'...'\u{FD8F}'    // ARABIC LIGATURE TEH WITH JEEM WITH MEEM INITIAL FORM...ARABIC LIGATURE MEEM WITH KHAH WITH MEEM INITIAL FORM
                                | '\u{FD92}'...'\u{FDC7}'    // ARABIC LIGATURE MEEM WITH JEEM WITH KHAH INITIAL FORM...ARABIC LIGATURE NOON WITH JEEM WITH YEH FINAL FORM
                                | '\u{FDF0}'...'\u{FDFB}'    // ARABIC LIGATURE SALLA USED AS KORANIC STOP SIGN ISOLATED FORM...ARABIC LIGATURE JALLAJALALOUHOU
                                | '\u{FE00}'...'\u{FE0F}'    // VARIATION SELECTOR-1...VARIATION SELECTOR-16
                                | '\u{FE20}'...'\u{FE2F}'    // COMBINING LIGATURE LEFT HALF...COMBINING CYRILLIC TITLO RIGHT HALF
                                | '\u{FE33}'...'\u{FE34}'    // PRESENTATION FORM FOR VERTICAL LOW LINE...PRESENTATION FORM FOR VERTICAL WAVY LOW LINE
                                | '\u{FE4D}'...'\u{FE4F}'    // DASHED LOW LINE...WAVY LOW LINE
                                | '\u{FE70}'...'\u{FE74}'    // ARABIC FATHATAN ISOLATED FORM...ARABIC KASRATAN ISOLATED FORM
                                | '\u{FE76}'...'\u{FEFC}'    // ARABIC FATHA ISOLATED FORM...ARABIC LIGATURE LAM WITH ALEF FINAL FORM
                                | '\u{FF10}'...'\u{FF19}'    // FULLWIDTH DIGIT ZERO...FULLWIDTH DIGIT NINE
                                | '\u{FF21}'...'\u{FF3A}'    // FULLWIDTH LATIN CAPITAL LETTER A...FULLWIDTH LATIN CAPITAL LETTER Z
                                | '\u{FF3F}'                 // FULLWIDTH LOW LINE
                                | '\u{FF41}'...'\u{FF5A}'    // FULLWIDTH LATIN SMALL LETTER A...FULLWIDTH LATIN SMALL LETTER Z
                                | '\u{FF66}'...'\u{FF6F}'    // HALFWIDTH KATAKANA LETTER WO...HALFWIDTH KATAKANA LETTER SMALL TU
                                | '\u{FF70}'                 // HALFWIDTH KATAKANA-HIRAGANA PROLONGED SOUND MARK
                                | '\u{FF71}'...'\u{FF9D}'    // HALFWIDTH KATAKANA LETTER A...HALFWIDTH KATAKANA LETTER N
                                | '\u{FF9E}'...'\u{FF9F}'    // HALFWIDTH KATAKANA VOICED SOUND MARK...HALFWIDTH KATAKANA SEMI-VOICED SOUND MARK
                                | '\u{FFA0}'...'\u{FFBE}'    // HALFWIDTH HANGUL FILLER...HALFWIDTH HANGUL LETTER HIEUH
                                | '\u{FFC2}'...'\u{FFC7}'    // HALFWIDTH HANGUL LETTER A...HALFWIDTH HANGUL LETTER E
                                | '\u{FFCA}'...'\u{FFCF}'    // HALFWIDTH HANGUL LETTER YEO...HALFWIDTH HANGUL LETTER OE
                                | '\u{FFD2}'...'\u{FFD7}'    // HALFWIDTH HANGUL LETTER YO...HALFWIDTH HANGUL LETTER YU
                                | '\u{FFDA}'...'\u{FFDC}'    // HALFWIDTH HANGUL LETTER EU...HALFWIDTH HANGUL LETTER I
                                | '\u{10000}'...'\u{1000B}'  // LINEAR B SYLLABLE B008 A...LINEAR B SYLLABLE B046 JE
                                | '\u{1000D}'...'\u{10026}'  // LINEAR B SYLLABLE B036 JO...LINEAR B SYLLABLE B032 QO
                                | '\u{10028}'...'\u{1003A}'  // LINEAR B SYLLABLE B060 RA...LINEAR B SYLLABLE B042 WO
                                | '\u{1003C}'...'\u{1003D}'  // LINEAR B SYLLABLE B017 ZA...LINEAR B SYLLABLE B074 ZE
                                | '\u{1003F}'...'\u{1004D}'  // LINEAR B SYLLABLE B020 ZO...LINEAR B SYLLABLE B091 TWO
                                | '\u{10050}'...'\u{1005D}'  // LINEAR B SYMBOL B018...LINEAR B SYMBOL B089
                                | '\u{10080}'...'\u{100FA}'  // LINEAR B IDEOGRAM B100 MAN...LINEAR B IDEOGRAM VESSEL B305
                                | '\u{10140}'...'\u{10174}'  // GREEK ACROPHONIC ATTIC ONE QUARTER...GREEK ACROPHONIC STRATIAN FIFTY MNAS
                                | '\u{101FD}'                // PHAISTOS DISC SIGN COMBINING OBLIQUE STROKE
                                | '\u{10280}'...'\u{1029C}'  // LYCIAN LETTER A...LYCIAN LETTER X
                                | '\u{102A0}'...'\u{102D0}'  // CARIAN LETTER A...CARIAN LETTER UUU3
                                | '\u{102E0}'                // COPTIC EPACT THOUSANDS MARK
                                | '\u{10300}'...'\u{1031F}'  // OLD ITALIC LETTER A...OLD ITALIC LETTER ESS
                                | '\u{1032D}'...'\u{10340}'  // OLD ITALIC LETTER YE...GOTHIC LETTER PAIRTHRA
                                | '\u{10341}'                // GOTHIC LETTER NINETY
                                | '\u{10342}'...'\u{10349}'  // GOTHIC LETTER RAIDA...GOTHIC LETTER OTHAL
                                | '\u{1034A}'                // GOTHIC LETTER NINE HUNDRED
                                | '\u{10350}'...'\u{10375}'  // OLD PERMIC LETTER AN...OLD PERMIC LETTER IA
                                | '\u{10376}'...'\u{1037A}'  // COMBINING OLD PERMIC LETTER AN...COMBINING OLD PERMIC LETTER SII
                                | '\u{10380}'...'\u{1039D}'  // UGARITIC LETTER ALPA...UGARITIC LETTER SSU
                                | '\u{103A0}'...'\u{103C3}'  // OLD PERSIAN SIGN A...OLD PERSIAN SIGN HA
                                | '\u{103C8}'...'\u{103CF}'  // OLD PERSIAN SIGN AURAMAZDAA...OLD PERSIAN SIGN BUUMISH
                                | '\u{103D1}'...'\u{103D5}'  // OLD PERSIAN NUMBER ONE...OLD PERSIAN NUMBER HUNDRED
                                | '\u{10400}'...'\u{1044F}'  // DESERET CAPITAL LETTER LONG I...DESERET SMALL LETTER EW
                                | '\u{10450}'...'\u{1049D}'  // SHAVIAN LETTER PEEP...OSMANYA LETTER OO
                                | '\u{104A0}'...'\u{104A9}'  // OSMANYA DIGIT ZERO...OSMANYA DIGIT NINE
                                | '\u{104B0}'...'\u{104D3}'  // OSAGE CAPITAL LETTER A...OSAGE CAPITAL LETTER ZHA
                                | '\u{104D8}'...'\u{104FB}'  // OSAGE SMALL LETTER A...OSAGE SMALL LETTER ZHA
                                | '\u{10500}'...'\u{10527}'  // ELBASAN LETTER A...ELBASAN LETTER KHE
                                | '\u{10530}'...'\u{10563}'  // CAUCASIAN ALBANIAN LETTER ALT...CAUCASIAN ALBANIAN LETTER KIW
                                | '\u{10600}'...'\u{10736}'  // LINEAR A SIGN AB001...LINEAR A SIGN A664
                                | '\u{10740}'...'\u{10755}'  // LINEAR A SIGN A701 A...LINEAR A SIGN A732 JE
                                | '\u{10760}'...'\u{10767}'  // LINEAR A SIGN A800...LINEAR A SIGN A807
                                | '\u{10800}'...'\u{10805}'  // CYPRIOT SYLLABLE A...CYPRIOT SYLLABLE JA
                                | '\u{10808}'                // CYPRIOT SYLLABLE JO
                                | '\u{1080A}'...'\u{10835}'  // CYPRIOT SYLLABLE KA...CYPRIOT SYLLABLE WO
                                | '\u{10837}'...'\u{10838}'  // CYPRIOT SYLLABLE XA...CYPRIOT SYLLABLE XE
                                | '\u{1083C}'                // CYPRIOT SYLLABLE ZA
                                | '\u{1083F}'...'\u{10855}'  // CYPRIOT SYLLABLE ZO...IMPERIAL ARAMAIC LETTER TAW
                                | '\u{10860}'...'\u{10876}'  // PALMYRENE LETTER ALEPH...PALMYRENE LETTER TAW
                                | '\u{10880}'...'\u{1089E}'  // NABATAEAN LETTER FINAL ALEPH...NABATAEAN LETTER TAW
                                | '\u{108E0}'...'\u{108F2}'  // HATRAN LETTER ALEPH...HATRAN LETTER QOPH
                                | '\u{108F4}'...'\u{108F5}'  // HATRAN LETTER SHIN...HATRAN LETTER TAW
                                | '\u{10900}'...'\u{10915}'  // PHOENICIAN LETTER ALF...PHOENICIAN LETTER TAU
                                | '\u{10920}'...'\u{10939}'  // LYDIAN LETTER A...LYDIAN LETTER C
                                | '\u{10980}'...'\u{109B7}'  // MEROITIC HIEROGLYPHIC LETTER A...MEROITIC CURSIVE LETTER DA
                                | '\u{109BE}'...'\u{109BF}'  // MEROITIC CURSIVE LOGOGRAM RMT...MEROITIC CURSIVE LOGOGRAM IMN
                                | '\u{10A00}'                // KHAROSHTHI LETTER A
                                | '\u{10A01}'...'\u{10A03}'  // KHAROSHTHI VOWEL SIGN I...KHAROSHTHI VOWEL SIGN VOCALIC R
                                | '\u{10A05}'...'\u{10A06}'  // KHAROSHTHI VOWEL SIGN E...KHAROSHTHI VOWEL SIGN O
                                | '\u{10A0C}'...'\u{10A0F}'  // KHAROSHTHI VOWEL LENGTH MARK...KHAROSHTHI SIGN VISARGA
                                | '\u{10A10}'...'\u{10A13}'  // KHAROSHTHI LETTER KA...KHAROSHTHI LETTER GHA
                                | '\u{10A15}'...'\u{10A17}'  // KHAROSHTHI LETTER CA...KHAROSHTHI LETTER JA
                                | '\u{10A19}'...'\u{10A33}'  // KHAROSHTHI LETTER NYA...KHAROSHTHI LETTER TTTHA
                                | '\u{10A38}'...'\u{10A3A}'  // KHAROSHTHI SIGN BAR ABOVE...KHAROSHTHI SIGN DOT BELOW
                                | '\u{10A3F}'                // KHAROSHTHI VIRAMA
                                | '\u{10A60}'...'\u{10A7C}'  // OLD SOUTH ARABIAN LETTER HE...OLD SOUTH ARABIAN LETTER THETH
                                | '\u{10A80}'...'\u{10A9C}'  // OLD NORTH ARABIAN LETTER HEH...OLD NORTH ARABIAN LETTER ZAH
                                | '\u{10AC0}'...'\u{10AC7}'  // MANICHAEAN LETTER ALEPH...MANICHAEAN LETTER WAW
                                | '\u{10AC9}'...'\u{10AE4}'  // MANICHAEAN LETTER ZAYIN...MANICHAEAN LETTER TAW
                                | '\u{10AE5}'...'\u{10AE6}'  // MANICHAEAN ABBREVIATION MARK ABOVE...MANICHAEAN ABBREVIATION MARK BELOW
                                | '\u{10B00}'...'\u{10B35}'  // AVESTAN LETTER A...AVESTAN LETTER HE
                                | '\u{10B40}'...'\u{10B55}'  // INSCRIPTIONAL PARTHIAN LETTER ALEPH...INSCRIPTIONAL PARTHIAN LETTER TAW
                                | '\u{10B60}'...'\u{10B72}'  // INSCRIPTIONAL PAHLAVI LETTER ALEPH...INSCRIPTIONAL PAHLAVI LETTER TAW
                                | '\u{10B80}'...'\u{10B91}'  // PSALTER PAHLAVI LETTER ALEPH...PSALTER PAHLAVI LETTER TAW
                                | '\u{10C00}'...'\u{10C48}'  // OLD TURKIC LETTER ORKHON A...OLD TURKIC LETTER ORKHON BASH
                                | '\u{10C80}'...'\u{10CB2}'  // OLD HUNGARIAN CAPITAL LETTER A...OLD HUNGARIAN CAPITAL LETTER US
                                | '\u{10CC0}'...'\u{10CF2}'  // OLD HUNGARIAN SMALL LETTER A...OLD HUNGARIAN SMALL LETTER US
                                | '\u{11000}'                // BRAHMI SIGN CANDRABINDU
                                | '\u{11001}'                // BRAHMI SIGN ANUSVARA
                                | '\u{11002}'                // BRAHMI SIGN VISARGA
                                | '\u{11003}'...'\u{11037}'  // BRAHMI SIGN JIHVAMULIYA...BRAHMI LETTER OLD TAMIL NNNA
                                | '\u{11038}'...'\u{11046}'  // BRAHMI VOWEL SIGN AA...BRAHMI VIRAMA
                                | '\u{11066}'...'\u{1106F}'  // BRAHMI DIGIT ZERO...BRAHMI DIGIT NINE
                                | '\u{1107F}'...'\u{11081}'  // BRAHMI NUMBER JOINER...KAITHI SIGN ANUSVARA
                                | '\u{11082}'                // KAITHI SIGN VISARGA
                                | '\u{11083}'...'\u{110AF}'  // KAITHI LETTER A...KAITHI LETTER HA
                                | '\u{110B0}'...'\u{110B2}'  // KAITHI VOWEL SIGN AA...KAITHI VOWEL SIGN II
                                | '\u{110B3}'...'\u{110B6}'  // KAITHI VOWEL SIGN U...KAITHI VOWEL SIGN AI
                                | '\u{110B7}'...'\u{110B8}'  // KAITHI VOWEL SIGN O...KAITHI VOWEL SIGN AU
                                | '\u{110B9}'...'\u{110BA}'  // KAITHI SIGN VIRAMA...KAITHI SIGN NUKTA
                                | '\u{110D0}'...'\u{110E8}'  // SORA SOMPENG LETTER SAH...SORA SOMPENG LETTER MAE
                                | '\u{110F0}'...'\u{110F9}'  // SORA SOMPENG DIGIT ZERO...SORA SOMPENG DIGIT NINE
                                | '\u{11100}'...'\u{11102}'  // CHAKMA SIGN CANDRABINDU...CHAKMA SIGN VISARGA
                                | '\u{11103}'...'\u{11126}'  // CHAKMA LETTER AA...CHAKMA LETTER HAA
                                | '\u{11127}'...'\u{1112B}'  // CHAKMA VOWEL SIGN A...CHAKMA VOWEL SIGN UU
                                | '\u{1112C}'                // CHAKMA VOWEL SIGN E
                                | '\u{1112D}'...'\u{11134}'  // CHAKMA VOWEL SIGN AI...CHAKMA MAAYYAA
                                | '\u{11136}'...'\u{1113F}'  // CHAKMA DIGIT ZERO...CHAKMA DIGIT NINE
                                | '\u{11150}'...'\u{11172}'  // MAHAJANI LETTER A...MAHAJANI LETTER RRA
                                | '\u{11173}'                // MAHAJANI SIGN NUKTA
                                | '\u{11176}'                // MAHAJANI LIGATURE SHRI
                                | '\u{11180}'...'\u{11181}'  // SHARADA SIGN CANDRABINDU...SHARADA SIGN ANUSVARA
                                | '\u{11182}'                // SHARADA SIGN VISARGA
                                | '\u{11183}'...'\u{111B2}'  // SHARADA LETTER A...SHARADA LETTER HA
                                | '\u{111B3}'...'\u{111B5}'  // SHARADA VOWEL SIGN AA...SHARADA VOWEL SIGN II
                                | '\u{111B6}'...'\u{111BE}'  // SHARADA VOWEL SIGN U...SHARADA VOWEL SIGN O
                                | '\u{111BF}'...'\u{111C0}'  // SHARADA VOWEL SIGN AU...SHARADA SIGN VIRAMA
                                | '\u{111C1}'...'\u{111C4}'  // SHARADA SIGN AVAGRAHA...SHARADA OM
                                | '\u{111CA}'...'\u{111CC}'  // SHARADA SIGN NUKTA...SHARADA EXTRA SHORT VOWEL MARK
                                | '\u{111D0}'...'\u{111D9}'  // SHARADA DIGIT ZERO...SHARADA DIGIT NINE
                                | '\u{111DA}'                // SHARADA EKAM
                                | '\u{111DC}'                // SHARADA HEADSTROKE
                                | '\u{11200}'...'\u{11211}'  // KHOJKI LETTER A...KHOJKI LETTER JJA
                                | '\u{11213}'...'\u{1122B}'  // KHOJKI LETTER NYA...KHOJKI LETTER LLA
                                | '\u{1122C}'...'\u{1122E}'  // KHOJKI VOWEL SIGN AA...KHOJKI VOWEL SIGN II
                                | '\u{1122F}'...'\u{11231}'  // KHOJKI VOWEL SIGN U...KHOJKI VOWEL SIGN AI
                                | '\u{11232}'...'\u{11233}'  // KHOJKI VOWEL SIGN O...KHOJKI VOWEL SIGN AU
                                | '\u{11234}'                // KHOJKI SIGN ANUSVARA
                                | '\u{11235}'                // KHOJKI SIGN VIRAMA
                                | '\u{11236}'...'\u{11237}'  // KHOJKI SIGN NUKTA...KHOJKI SIGN SHADDA
                                | '\u{1123E}'                // KHOJKI SIGN SUKUN
                                | '\u{11280}'...'\u{11286}'  // MULTANI LETTER A...MULTANI LETTER GA
                                | '\u{11288}'                // MULTANI LETTER GHA
                                | '\u{1128A}'...'\u{1128D}'  // MULTANI LETTER CA...MULTANI LETTER JJA
                                | '\u{1128F}'...'\u{1129D}'  // MULTANI LETTER NYA...MULTANI LETTER BA
                                | '\u{1129F}'...'\u{112A8}'  // MULTANI LETTER BHA...MULTANI LETTER RHA
                                | '\u{112B0}'...'\u{112DE}'  // KHUDAWADI LETTER A...KHUDAWADI LETTER HA
                                | '\u{112DF}'                // KHUDAWADI SIGN ANUSVARA
                                | '\u{112E0}'...'\u{112E2}'  // KHUDAWADI VOWEL SIGN AA...KHUDAWADI VOWEL SIGN II
                                | '\u{112E3}'...'\u{112EA}'  // KHUDAWADI VOWEL SIGN U...KHUDAWADI SIGN VIRAMA
                                | '\u{112F0}'...'\u{112F9}'  // KHUDAWADI DIGIT ZERO...KHUDAWADI DIGIT NINE
                                | '\u{11300}'...'\u{11301}'  // GRANTHA SIGN COMBINING ANUSVARA ABOVE...GRANTHA SIGN CANDRABINDU
                                | '\u{11302}'...'\u{11303}'  // GRANTHA SIGN ANUSVARA...GRANTHA SIGN VISARGA
                                | '\u{11305}'...'\u{1130C}'  // GRANTHA LETTER A...GRANTHA LETTER VOCALIC L
                                | '\u{1130F}'...'\u{11310}'  // GRANTHA LETTER EE...GRANTHA LETTER AI
                                | '\u{11313}'...'\u{11328}'  // GRANTHA LETTER OO...GRANTHA LETTER NA
                                | '\u{1132A}'...'\u{11330}'  // GRANTHA LETTER PA...GRANTHA LETTER RA
                                | '\u{11332}'...'\u{11333}'  // GRANTHA LETTER LA...GRANTHA LETTER LLA
                                | '\u{11335}'...'\u{11339}'  // GRANTHA LETTER VA...GRANTHA LETTER HA
                                | '\u{1133C}'                // GRANTHA SIGN NUKTA
                                | '\u{1133D}'                // GRANTHA SIGN AVAGRAHA
                                | '\u{1133E}'...'\u{1133F}'  // GRANTHA VOWEL SIGN AA...GRANTHA VOWEL SIGN I
                                | '\u{11340}'                // GRANTHA VOWEL SIGN II
                                | '\u{11341}'...'\u{11344}'  // GRANTHA VOWEL SIGN U...GRANTHA VOWEL SIGN VOCALIC RR
                                | '\u{11347}'...'\u{11348}'  // GRANTHA VOWEL SIGN EE...GRANTHA VOWEL SIGN AI
                                | '\u{1134B}'...'\u{1134D}'  // GRANTHA VOWEL SIGN OO...GRANTHA SIGN VIRAMA
                                | '\u{11350}'                // GRANTHA OM
                                | '\u{11357}'                // GRANTHA AU LENGTH MARK
                                | '\u{1135D}'...'\u{11361}'  // GRANTHA SIGN PLUTA...GRANTHA LETTER VOCALIC LL
                                | '\u{11362}'...'\u{11363}'  // GRANTHA VOWEL SIGN VOCALIC L...GRANTHA VOWEL SIGN VOCALIC LL
                                | '\u{11366}'...'\u{1136C}'  // COMBINING GRANTHA DIGIT ZERO...COMBINING GRANTHA DIGIT SIX
                                | '\u{11370}'...'\u{11374}'  // COMBINING GRANTHA LETTER A...COMBINING GRANTHA LETTER PA
                                | '\u{11400}'...'\u{11434}'  // NEWA LETTER A...NEWA LETTER HA
                                | '\u{11435}'...'\u{11437}'  // NEWA VOWEL SIGN AA...NEWA VOWEL SIGN II
                                | '\u{11438}'...'\u{1143F}'  // NEWA VOWEL SIGN U...NEWA VOWEL SIGN AI
                                | '\u{11440}'...'\u{11441}'  // NEWA VOWEL SIGN O...NEWA VOWEL SIGN AU
                                | '\u{11442}'...'\u{11444}'  // NEWA SIGN VIRAMA...NEWA SIGN ANUSVARA
                                | '\u{11445}'                // NEWA SIGN VISARGA
                                | '\u{11446}'                // NEWA SIGN NUKTA
                                | '\u{11447}'...'\u{1144A}'  // NEWA SIGN AVAGRAHA...NEWA SIDDHI
                                | '\u{11450}'...'\u{11459}'  // NEWA DIGIT ZERO...NEWA DIGIT NINE
                                | '\u{11480}'...'\u{114AF}'  // TIRHUTA ANJI...TIRHUTA LETTER HA
                                | '\u{114B0}'...'\u{114B2}'  // TIRHUTA VOWEL SIGN AA...TIRHUTA VOWEL SIGN II
                                | '\u{114B3}'...'\u{114B8}'  // TIRHUTA VOWEL SIGN U...TIRHUTA VOWEL SIGN VOCALIC LL
                                | '\u{114B9}'                // TIRHUTA VOWEL SIGN E
                                | '\u{114BA}'                // TIRHUTA VOWEL SIGN SHORT E
                                | '\u{114BB}'...'\u{114BE}'  // TIRHUTA VOWEL SIGN AI...TIRHUTA VOWEL SIGN AU
                                | '\u{114BF}'...'\u{114C0}'  // TIRHUTA SIGN CANDRABINDU...TIRHUTA SIGN ANUSVARA
                                | '\u{114C1}'                // TIRHUTA SIGN VISARGA
                                | '\u{114C2}'...'\u{114C3}'  // TIRHUTA SIGN VIRAMA...TIRHUTA SIGN NUKTA
                                | '\u{114C4}'...'\u{114C5}'  // TIRHUTA SIGN AVAGRAHA...TIRHUTA GVANG
                                | '\u{114C7}'                // TIRHUTA OM
                                | '\u{114D0}'...'\u{114D9}'  // TIRHUTA DIGIT ZERO...TIRHUTA DIGIT NINE
                                | '\u{11580}'...'\u{115AE}'  // SIDDHAM LETTER A...SIDDHAM LETTER HA
                                | '\u{115AF}'...'\u{115B1}'  // SIDDHAM VOWEL SIGN AA...SIDDHAM VOWEL SIGN II
                                | '\u{115B2}'...'\u{115B5}'  // SIDDHAM VOWEL SIGN U...SIDDHAM VOWEL SIGN VOCALIC RR
                                | '\u{115B8}'...'\u{115BB}'  // SIDDHAM VOWEL SIGN E...SIDDHAM VOWEL SIGN AU
                                | '\u{115BC}'...'\u{115BD}'  // SIDDHAM SIGN CANDRABINDU...SIDDHAM SIGN ANUSVARA
                                | '\u{115BE}'                // SIDDHAM SIGN VISARGA
                                | '\u{115BF}'...'\u{115C0}'  // SIDDHAM SIGN VIRAMA...SIDDHAM SIGN NUKTA
                                | '\u{115D8}'...'\u{115DB}'  // SIDDHAM LETTER THREE-CIRCLE ALTERNATE I...SIDDHAM LETTER ALTERNATE U
                                | '\u{115DC}'...'\u{115DD}'  // SIDDHAM VOWEL SIGN ALTERNATE U...SIDDHAM VOWEL SIGN ALTERNATE UU
                                | '\u{11600}'...'\u{1162F}'  // MODI LETTER A...MODI LETTER LLA
                                | '\u{11630}'...'\u{11632}'  // MODI VOWEL SIGN AA...MODI VOWEL SIGN II
                                | '\u{11633}'...'\u{1163A}'  // MODI VOWEL SIGN U...MODI VOWEL SIGN AI
                                | '\u{1163B}'...'\u{1163C}'  // MODI VOWEL SIGN O...MODI VOWEL SIGN AU
                                | '\u{1163D}'                // MODI SIGN ANUSVARA
                                | '\u{1163E}'                // MODI SIGN VISARGA
                                | '\u{1163F}'...'\u{11640}'  // MODI SIGN VIRAMA...MODI SIGN ARDHACANDRA
                                | '\u{11644}'                // MODI SIGN HUVA
                                | '\u{11650}'...'\u{11659}'  // MODI DIGIT ZERO...MODI DIGIT NINE
                                | '\u{11680}'...'\u{116AA}'  // TAKRI LETTER A...TAKRI LETTER RRA
                                | '\u{116AB}'                // TAKRI SIGN ANUSVARA
                                | '\u{116AC}'                // TAKRI SIGN VISARGA
                                | '\u{116AD}'                // TAKRI VOWEL SIGN AA
                                | '\u{116AE}'...'\u{116AF}'  // TAKRI VOWEL SIGN I...TAKRI VOWEL SIGN II
                                | '\u{116B0}'...'\u{116B5}'  // TAKRI VOWEL SIGN U...TAKRI VOWEL SIGN AU
                                | '\u{116B6}'                // TAKRI SIGN VIRAMA
                                | '\u{116B7}'                // TAKRI SIGN NUKTA
                                | '\u{116C0}'...'\u{116C9}'  // TAKRI DIGIT ZERO...TAKRI DIGIT NINE
                                | '\u{11700}'...'\u{11719}'  // AHOM LETTER KA...AHOM LETTER JHA
                                | '\u{1171D}'...'\u{1171F}'  // AHOM CONSONANT SIGN MEDIAL LA...AHOM CONSONANT SIGN MEDIAL LIGATING RA
                                | '\u{11720}'...'\u{11721}'  // AHOM VOWEL SIGN A...AHOM VOWEL SIGN AA
                                | '\u{11722}'...'\u{11725}'  // AHOM VOWEL SIGN I...AHOM VOWEL SIGN UU
                                | '\u{11726}'                // AHOM VOWEL SIGN E
                                | '\u{11727}'...'\u{1172B}'  // AHOM VOWEL SIGN AW...AHOM SIGN KILLER
                                | '\u{11730}'...'\u{11739}'  // AHOM DIGIT ZERO...AHOM DIGIT NINE
                                | '\u{118A0}'...'\u{118DF}'  // WARANG CITI CAPITAL LETTER NGAA...WARANG CITI SMALL LETTER VIYO
                                | '\u{118E0}'...'\u{118E9}'  // WARANG CITI DIGIT ZERO...WARANG CITI DIGIT NINE
                                | '\u{118FF}'                // WARANG CITI OM
                                | '\u{11A00}'                // ZANABAZAR SQUARE LETTER A
                                | '\u{11A01}'...'\u{11A06}'  // ZANABAZAR SQUARE VOWEL SIGN I...ZANABAZAR SQUARE VOWEL SIGN O
                                | '\u{11A07}'...'\u{11A08}'  // ZANABAZAR SQUARE VOWEL SIGN AI...ZANABAZAR SQUARE VOWEL SIGN AU
                                | '\u{11A09}'...'\u{11A0A}'  // ZANABAZAR SQUARE VOWEL SIGN REVERSED I...ZANABAZAR SQUARE VOWEL LENGTH MARK
                                | '\u{11A0B}'...'\u{11A32}'  // ZANABAZAR SQUARE LETTER KA...ZANABAZAR SQUARE LETTER KSSA
                                | '\u{11A33}'...'\u{11A38}'  // ZANABAZAR SQUARE FINAL CONSONANT MARK...ZANABAZAR SQUARE SIGN ANUSVARA
                                | '\u{11A39}'                // ZANABAZAR SQUARE SIGN VISARGA
                                | '\u{11A3A}'                // ZANABAZAR SQUARE CLUSTER-INITIAL LETTER RA
                                | '\u{11A3B}'...'\u{11A3E}'  // ZANABAZAR SQUARE CLUSTER-FINAL LETTER YA...ZANABAZAR SQUARE CLUSTER-FINAL LETTER VA
                                | '\u{11A47}'                // ZANABAZAR SQUARE SUBJOINER
                                | '\u{11A50}'                // SOYOMBO LETTER A
                                | '\u{11A51}'...'\u{11A56}'  // SOYOMBO VOWEL SIGN I...SOYOMBO VOWEL SIGN OE
                                | '\u{11A57}'...'\u{11A58}'  // SOYOMBO VOWEL SIGN AI...SOYOMBO VOWEL SIGN AU
                                | '\u{11A59}'...'\u{11A5B}'  // SOYOMBO VOWEL SIGN VOCALIC R...SOYOMBO VOWEL LENGTH MARK
                                | '\u{11A5C}'...'\u{11A83}'  // SOYOMBO LETTER KA...SOYOMBO LETTER KSSA
                                | '\u{11A86}'...'\u{11A89}'  // SOYOMBO CLUSTER-INITIAL LETTER RA...SOYOMBO CLUSTER-INITIAL LETTER SA
                                | '\u{11A8A}'...'\u{11A96}'  // SOYOMBO FINAL CONSONANT SIGN G...SOYOMBO SIGN ANUSVARA
                                | '\u{11A97}'                // SOYOMBO SIGN VISARGA
                                | '\u{11A98}'...'\u{11A99}'  // SOYOMBO GEMINATION MARK...SOYOMBO SUBJOINER
                                | '\u{11AC0}'...'\u{11AF8}'  // PAU CIN HAU LETTER PA...PAU CIN HAU GLOTTAL STOP FINAL
                                | '\u{11C00}'...'\u{11C08}'  // BHAIKSUKI LETTER A...BHAIKSUKI LETTER VOCALIC L
                                | '\u{11C0A}'...'\u{11C2E}'  // BHAIKSUKI LETTER E...BHAIKSUKI LETTER HA
                                | '\u{11C2F}'                // BHAIKSUKI VOWEL SIGN AA
                                | '\u{11C30}'...'\u{11C36}'  // BHAIKSUKI VOWEL SIGN I...BHAIKSUKI VOWEL SIGN VOCALIC L
                                | '\u{11C38}'...'\u{11C3D}'  // BHAIKSUKI VOWEL SIGN E...BHAIKSUKI SIGN ANUSVARA
                                | '\u{11C3E}'                // BHAIKSUKI SIGN VISARGA
                                | '\u{11C3F}'                // BHAIKSUKI SIGN VIRAMA
                                | '\u{11C40}'                // BHAIKSUKI SIGN AVAGRAHA
                                | '\u{11C50}'...'\u{11C59}'  // BHAIKSUKI DIGIT ZERO...BHAIKSUKI DIGIT NINE
                                | '\u{11C72}'...'\u{11C8F}'  // MARCHEN LETTER KA...MARCHEN LETTER A
                                | '\u{11C92}'...'\u{11CA7}'  // MARCHEN SUBJOINED LETTER KA...MARCHEN SUBJOINED LETTER ZA
                                | '\u{11CA9}'                // MARCHEN SUBJOINED LETTER YA
                                | '\u{11CAA}'...'\u{11CB0}'  // MARCHEN SUBJOINED LETTER RA...MARCHEN VOWEL SIGN AA
                                | '\u{11CB1}'                // MARCHEN VOWEL SIGN I
                                | '\u{11CB2}'...'\u{11CB3}'  // MARCHEN VOWEL SIGN U...MARCHEN VOWEL SIGN E
                                | '\u{11CB4}'                // MARCHEN VOWEL SIGN O
                                | '\u{11CB5}'...'\u{11CB6}'  // MARCHEN SIGN ANUSVARA...MARCHEN SIGN CANDRABINDU
                                | '\u{11D00}'...'\u{11D06}'  // MASARAM GONDI LETTER A...MASARAM GONDI LETTER E
                                | '\u{11D08}'...'\u{11D09}'  // MASARAM GONDI LETTER AI...MASARAM GONDI LETTER O
                                | '\u{11D0B}'...'\u{11D30}'  // MASARAM GONDI LETTER AU...MASARAM GONDI LETTER TRA
                                | '\u{11D31}'...'\u{11D36}'  // MASARAM GONDI VOWEL SIGN AA...MASARAM GONDI VOWEL SIGN VOCALIC R
                                | '\u{11D3A}'                // MASARAM GONDI VOWEL SIGN E
                                | '\u{11D3C}'...'\u{11D3D}'  // MASARAM GONDI VOWEL SIGN AI...MASARAM GONDI VOWEL SIGN O
                                | '\u{11D3F}'...'\u{11D45}'  // MASARAM GONDI VOWEL SIGN AU...MASARAM GONDI VIRAMA
                                | '\u{11D46}'                // MASARAM GONDI REPHA
                                | '\u{11D47}'                // MASARAM GONDI RA-KARA
                                | '\u{11D50}'...'\u{11D59}'  // MASARAM GONDI DIGIT ZERO...MASARAM GONDI DIGIT NINE
                                | '\u{12000}'...'\u{12399}'  // CUNEIFORM SIGN A...CUNEIFORM SIGN U U
                                | '\u{12400}'...'\u{1246E}'  // CUNEIFORM NUMERIC SIGN TWO ASH...CUNEIFORM NUMERIC SIGN NINE U VARIANT FORM
                                | '\u{12480}'...'\u{12543}'  // CUNEIFORM SIGN AB TIMES NUN TENU...CUNEIFORM SIGN ZU5 TIMES THREE DISH TENU
                                | '\u{13000}'...'\u{1342E}'  // EGYPTIAN HIEROGLYPH A001...EGYPTIAN HIEROGLYPH AA032
                                | '\u{14400}'...'\u{14646}'  // ANATOLIAN HIEROGLYPH A001...ANATOLIAN HIEROGLYPH A530
                                | '\u{16800}'...'\u{16A38}'  // BAMUM LETTER PHASE-A NGKUE MFON...BAMUM LETTER PHASE-F VUEQ
                                | '\u{16A40}'...'\u{16A5E}'  // MRO LETTER TA...MRO LETTER TEK
                                | '\u{16A60}'...'\u{16A69}'  // MRO DIGIT ZERO...MRO DIGIT NINE
                                | '\u{16AD0}'...'\u{16AED}'  // BASSA VAH LETTER ENNI...BASSA VAH LETTER I
                                | '\u{16AF0}'...'\u{16AF4}'  // BASSA VAH COMBINING HIGH TONE...BASSA VAH COMBINING HIGH-LOW TONE
                                | '\u{16B00}'...'\u{16B2F}'  // PAHAWH HMONG VOWEL KEEB...PAHAWH HMONG CONSONANT CAU
                                | '\u{16B30}'...'\u{16B36}'  // PAHAWH HMONG MARK CIM TUB...PAHAWH HMONG MARK CIM TAUM
                                | '\u{16B40}'...'\u{16B43}'  // PAHAWH HMONG SIGN VOS SEEV...PAHAWH HMONG SIGN IB YAM
                                | '\u{16B50}'...'\u{16B59}'  // PAHAWH HMONG DIGIT ZERO...PAHAWH HMONG DIGIT NINE
                                | '\u{16B63}'...'\u{16B77}'  // PAHAWH HMONG SIGN VOS LUB...PAHAWH HMONG SIGN CIM NRES TOS
                                | '\u{16B7D}'...'\u{16B8F}'  // PAHAWH HMONG CLAN SIGN TSHEEJ...PAHAWH HMONG CLAN SIGN VWJ
                                | '\u{16F00}'...'\u{16F44}'  // MIAO LETTER PA...MIAO LETTER HHA
                                | '\u{16F50}'                // MIAO LETTER NASALIZATION
                                | '\u{16F51}'...'\u{16F7E}'  // MIAO SIGN ASPIRATION...MIAO VOWEL SIGN NG
                                | '\u{16F8F}'...'\u{16F92}'  // MIAO TONE RIGHT...MIAO TONE BELOW
                                | '\u{16F93}'...'\u{16F9F}'  // MIAO LETTER TONE-2...MIAO LETTER REFORMED TONE-8
                                | '\u{16FE0}'...'\u{16FE1}'  // TANGUT ITERATION MARK...NUSHU ITERATION MARK
                                | '\u{17000}'...'\u{187EC}'  // TANGUT IDEOGRAPH-17000...TANGUT IDEOGRAPH-187EC
                                | '\u{18800}'...'\u{18AF2}'  // TANGUT COMPONENT-001...TANGUT COMPONENT-755
                                | '\u{1B000}'...'\u{1B11E}'  // KATAKANA LETTER ARCHAIC E...HENTAIGANA LETTER N-MU-MO-2
                                | '\u{1B170}'...'\u{1B2FB}'  // NUSHU CHARACTER-1B170...NUSHU CHARACTER-1B2FB
                                | '\u{1BC00}'...'\u{1BC6A}'  // DUPLOYAN LETTER H...DUPLOYAN LETTER VOCALIC M
                                | '\u{1BC70}'...'\u{1BC7C}'  // DUPLOYAN AFFIX LEFT HORIZONTAL SECANT...DUPLOYAN AFFIX ATTACHED TANGENT HOOK
                                | '\u{1BC80}'...'\u{1BC88}'  // DUPLOYAN AFFIX HIGH ACUTE...DUPLOYAN AFFIX HIGH VERTICAL
                                | '\u{1BC90}'...'\u{1BC99}'  // DUPLOYAN AFFIX LOW ACUTE...DUPLOYAN AFFIX LOW ARROW
                                | '\u{1BC9D}'...'\u{1BC9E}'  // DUPLOYAN THICK LETTER SELECTOR...DUPLOYAN DOUBLE MARK
                                | '\u{1D165}'...'\u{1D166}'  // MUSICAL SYMBOL COMBINING STEM...MUSICAL SYMBOL COMBINING SPRECHGESANG STEM
                                | '\u{1D167}'...'\u{1D169}'  // MUSICAL SYMBOL COMBINING TREMOLO-1...MUSICAL SYMBOL COMBINING TREMOLO-3
                                | '\u{1D16D}'...'\u{1D172}'  // MUSICAL SYMBOL COMBINING AUGMENTATION DOT...MUSICAL SYMBOL COMBINING FLAG-5
                                | '\u{1D17B}'...'\u{1D182}'  // MUSICAL SYMBOL COMBINING ACCENT...MUSICAL SYMBOL COMBINING LOURE
                                | '\u{1D185}'...'\u{1D18B}'  // MUSICAL SYMBOL COMBINING DOIT...MUSICAL SYMBOL COMBINING TRIPLE TONGUE
                                | '\u{1D1AA}'...'\u{1D1AD}'  // MUSICAL SYMBOL COMBINING DOWN BOW...MUSICAL SYMBOL COMBINING SNAP PIZZICATO
                                | '\u{1D242}'...'\u{1D244}'  // COMBINING GREEK MUSICAL TRISEME...COMBINING GREEK MUSICAL PENTASEME
                                | '\u{1D400}'...'\u{1D454}'  // MATHEMATICAL BOLD CAPITAL A...MATHEMATICAL ITALIC SMALL G
                                | '\u{1D456}'...'\u{1D49C}'  // MATHEMATICAL ITALIC SMALL I...MATHEMATICAL SCRIPT CAPITAL A
                                | '\u{1D49E}'...'\u{1D49F}'  // MATHEMATICAL SCRIPT CAPITAL C...MATHEMATICAL SCRIPT CAPITAL D
                                | '\u{1D4A2}'                // MATHEMATICAL SCRIPT CAPITAL G
                                | '\u{1D4A5}'...'\u{1D4A6}'  // MATHEMATICAL SCRIPT CAPITAL J...MATHEMATICAL SCRIPT CAPITAL K
                                | '\u{1D4A9}'...'\u{1D4AC}'  // MATHEMATICAL SCRIPT CAPITAL N...MATHEMATICAL SCRIPT CAPITAL Q
                                | '\u{1D4AE}'...'\u{1D4B9}'  // MATHEMATICAL SCRIPT CAPITAL S...MATHEMATICAL SCRIPT SMALL D
                                | '\u{1D4BB}'                // MATHEMATICAL SCRIPT SMALL F
                                | '\u{1D4BD}'...'\u{1D4C3}'  // MATHEMATICAL SCRIPT SMALL H...MATHEMATICAL SCRIPT SMALL N
                                | '\u{1D4C5}'...'\u{1D505}'  // MATHEMATICAL SCRIPT SMALL P...MATHEMATICAL FRAKTUR CAPITAL B
                                | '\u{1D507}'...'\u{1D50A}'  // MATHEMATICAL FRAKTUR CAPITAL D...MATHEMATICAL FRAKTUR CAPITAL G
                                | '\u{1D50D}'...'\u{1D514}'  // MATHEMATICAL FRAKTUR CAPITAL J...MATHEMATICAL FRAKTUR CAPITAL Q
                                | '\u{1D516}'...'\u{1D51C}'  // MATHEMATICAL FRAKTUR CAPITAL S...MATHEMATICAL FRAKTUR CAPITAL Y
                                | '\u{1D51E}'...'\u{1D539}'  // MATHEMATICAL FRAKTUR SMALL A...MATHEMATICAL DOUBLE-STRUCK CAPITAL B
                                | '\u{1D53B}'...'\u{1D53E}'  // MATHEMATICAL DOUBLE-STRUCK CAPITAL D...MATHEMATICAL DOUBLE-STRUCK CAPITAL G
                                | '\u{1D540}'...'\u{1D544}'  // MATHEMATICAL DOUBLE-STRUCK CAPITAL I...MATHEMATICAL DOUBLE-STRUCK CAPITAL M
                                | '\u{1D546}'                // MATHEMATICAL DOUBLE-STRUCK CAPITAL O
                                | '\u{1D54A}'...'\u{1D550}'  // MATHEMATICAL DOUBLE-STRUCK CAPITAL S...MATHEMATICAL DOUBLE-STRUCK CAPITAL Y
                                | '\u{1D552}'...'\u{1D6A5}'  // MATHEMATICAL DOUBLE-STRUCK SMALL A...MATHEMATICAL ITALIC SMALL DOTLESS J
                                | '\u{1D6A8}'...'\u{1D6C0}'  // MATHEMATICAL BOLD CAPITAL ALPHA...MATHEMATICAL BOLD CAPITAL OMEGA
                                | '\u{1D6C2}'...'\u{1D6DA}'  // MATHEMATICAL BOLD SMALL ALPHA...MATHEMATICAL BOLD SMALL OMEGA
                                | '\u{1D6DC}'...'\u{1D6FA}'  // MATHEMATICAL BOLD EPSILON SYMBOL...MATHEMATICAL ITALIC CAPITAL OMEGA
                                | '\u{1D6FC}'...'\u{1D714}'  // MATHEMATICAL ITALIC SMALL ALPHA...MATHEMATICAL ITALIC SMALL OMEGA
                                | '\u{1D716}'...'\u{1D734}'  // MATHEMATICAL ITALIC EPSILON SYMBOL...MATHEMATICAL BOLD ITALIC CAPITAL OMEGA
                                | '\u{1D736}'...'\u{1D74E}'  // MATHEMATICAL BOLD ITALIC SMALL ALPHA...MATHEMATICAL BOLD ITALIC SMALL OMEGA
                                | '\u{1D750}'...'\u{1D76E}'  // MATHEMATICAL BOLD ITALIC EPSILON SYMBOL...MATHEMATICAL SANS-SERIF BOLD CAPITAL OMEGA
                                | '\u{1D770}'...'\u{1D788}'  // MATHEMATICAL SANS-SERIF BOLD SMALL ALPHA...MATHEMATICAL SANS-SERIF BOLD SMALL OMEGA
                                | '\u{1D78A}'...'\u{1D7A8}'  // MATHEMATICAL SANS-SERIF BOLD EPSILON SYMBOL...MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL OMEGA
                                | '\u{1D7AA}'...'\u{1D7C2}'  // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL ALPHA...MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL OMEGA
                                | '\u{1D7C4}'...'\u{1D7CB}'  // MATHEMATICAL SANS-SERIF BOLD ITALIC EPSILON SYMBOL...MATHEMATICAL BOLD SMALL DIGAMMA
                                | '\u{1D7CE}'...'\u{1D7FF}'  // MATHEMATICAL BOLD DIGIT ZERO...MATHEMATICAL MONOSPACE DIGIT NINE
                                | '\u{1DA00}'...'\u{1DA36}'  // SIGNWRITING HEAD RIM...SIGNWRITING AIR SUCKING IN
                                | '\u{1DA3B}'...'\u{1DA6C}'  // SIGNWRITING MOUTH CLOSED NEUTRAL...SIGNWRITING EXCITEMENT
                                | '\u{1DA75}'                // SIGNWRITING UPPER BODY TILTING FROM HIP JOINTS
                                | '\u{1DA84}'                // SIGNWRITING LOCATION HEAD NECK
                                | '\u{1DA9B}'...'\u{1DA9F}'  // SIGNWRITING FILL MODIFIER-2...SIGNWRITING FILL MODIFIER-6
                                | '\u{1DAA1}'...'\u{1DAAF}'  // SIGNWRITING ROTATION MODIFIER-2...SIGNWRITING ROTATION MODIFIER-16
                                | '\u{1E000}'...'\u{1E006}'  // COMBINING GLAGOLITIC LETTER AZU...COMBINING GLAGOLITIC LETTER ZHIVETE
                                | '\u{1E008}'...'\u{1E018}'  // COMBINING GLAGOLITIC LETTER ZEMLJA...COMBINING GLAGOLITIC LETTER HERU
                                | '\u{1E01B}'...'\u{1E021}'  // COMBINING GLAGOLITIC LETTER SHTA...COMBINING GLAGOLITIC LETTER YATI
                                | '\u{1E023}'...'\u{1E024}'  // COMBINING GLAGOLITIC LETTER YU...COMBINING GLAGOLITIC LETTER SMALL YUS
                                | '\u{1E026}'...'\u{1E02A}'  // COMBINING GLAGOLITIC LETTER YO...COMBINING GLAGOLITIC LETTER FITA
                                | '\u{1E800}'...'\u{1E8C4}'  // MENDE KIKAKUI SYLLABLE M001 KI...MENDE KIKAKUI SYLLABLE M060 NYON
                                | '\u{1E8D0}'...'\u{1E8D6}'  // MENDE KIKAKUI COMBINING NUMBER TEENS...MENDE KIKAKUI COMBINING NUMBER MILLIONS
                                | '\u{1E900}'...'\u{1E943}'  // ADLAM CAPITAL LETTER ALIF...ADLAM SMALL LETTER SHA
                                | '\u{1E944}'...'\u{1E94A}'  // ADLAM ALIF LENGTHENER...ADLAM NUKTA
                                | '\u{1E950}'...'\u{1E959}'  // ADLAM DIGIT ZERO...ADLAM DIGIT NINE
                                | '\u{1EE00}'...'\u{1EE03}'  // ARABIC MATHEMATICAL ALEF...ARABIC MATHEMATICAL DAL
                                | '\u{1EE05}'...'\u{1EE1F}'  // ARABIC MATHEMATICAL WAW...ARABIC MATHEMATICAL DOTLESS QAF
                                | '\u{1EE21}'...'\u{1EE22}'  // ARABIC MATHEMATICAL INITIAL BEH...ARABIC MATHEMATICAL INITIAL JEEM
                                | '\u{1EE24}'                // ARABIC MATHEMATICAL INITIAL HEH
                                | '\u{1EE27}'                // ARABIC MATHEMATICAL INITIAL HAH
                                | '\u{1EE29}'...'\u{1EE32}'  // ARABIC MATHEMATICAL INITIAL YEH...ARABIC MATHEMATICAL INITIAL QAF
                                | '\u{1EE34}'...'\u{1EE37}'  // ARABIC MATHEMATICAL INITIAL SHEEN...ARABIC MATHEMATICAL INITIAL KHAH
                                | '\u{1EE39}'                // ARABIC MATHEMATICAL INITIAL DAD
                                | '\u{1EE3B}'                // ARABIC MATHEMATICAL INITIAL GHAIN
                                | '\u{1EE42}'                // ARABIC MATHEMATICAL TAILED JEEM
                                | '\u{1EE47}'                // ARABIC MATHEMATICAL TAILED HAH
                                | '\u{1EE49}'                // ARABIC MATHEMATICAL TAILED YEH
                                | '\u{1EE4B}'                // ARABIC MATHEMATICAL TAILED LAM
                                | '\u{1EE4D}'...'\u{1EE4F}'  // ARABIC MATHEMATICAL TAILED NOON...ARABIC MATHEMATICAL TAILED AIN
                                | '\u{1EE51}'...'\u{1EE52}'  // ARABIC MATHEMATICAL TAILED SAD...ARABIC MATHEMATICAL TAILED QAF
                                | '\u{1EE54}'                // ARABIC MATHEMATICAL TAILED SHEEN
                                | '\u{1EE57}'                // ARABIC MATHEMATICAL TAILED KHAH
                                | '\u{1EE59}'                // ARABIC MATHEMATICAL TAILED DAD
                                | '\u{1EE5B}'                // ARABIC MATHEMATICAL TAILED GHAIN
                                | '\u{1EE5D}'                // ARABIC MATHEMATICAL TAILED DOTLESS NOON
                                | '\u{1EE5F}'                // ARABIC MATHEMATICAL TAILED DOTLESS QAF
                                | '\u{1EE61}'...'\u{1EE62}'  // ARABIC MATHEMATICAL STRETCHED BEH...ARABIC MATHEMATICAL STRETCHED JEEM
                                | '\u{1EE64}'                // ARABIC MATHEMATICAL STRETCHED HEH
                                | '\u{1EE67}'...'\u{1EE6A}'  // ARABIC MATHEMATICAL STRETCHED HAH...ARABIC MATHEMATICAL STRETCHED KAF
                                | '\u{1EE6C}'...'\u{1EE72}'  // ARABIC MATHEMATICAL STRETCHED MEEM...ARABIC MATHEMATICAL STRETCHED QAF
                                | '\u{1EE74}'...'\u{1EE77}'  // ARABIC MATHEMATICAL STRETCHED SHEEN...ARABIC MATHEMATICAL STRETCHED KHAH
                                | '\u{1EE79}'...'\u{1EE7C}'  // ARABIC MATHEMATICAL STRETCHED DAD...ARABIC MATHEMATICAL STRETCHED DOTLESS BEH
                                | '\u{1EE7E}'                // ARABIC MATHEMATICAL STRETCHED DOTLESS FEH
                                | '\u{1EE80}'...'\u{1EE89}'  // ARABIC MATHEMATICAL LOOPED ALEF...ARABIC MATHEMATICAL LOOPED YEH
                                | '\u{1EE8B}'...'\u{1EE9B}'  // ARABIC MATHEMATICAL LOOPED LAM...ARABIC MATHEMATICAL LOOPED GHAIN
                                | '\u{1EEA1}'...'\u{1EEA3}'  // ARABIC MATHEMATICAL DOUBLE-STRUCK BEH...ARABIC MATHEMATICAL DOUBLE-STRUCK DAL
                                | '\u{1EEA5}'...'\u{1EEA9}'  // ARABIC MATHEMATICAL DOUBLE-STRUCK WAW...ARABIC MATHEMATICAL DOUBLE-STRUCK YEH
                                | '\u{1EEAB}'...'\u{1EEBB}'  // ARABIC MATHEMATICAL DOUBLE-STRUCK LAM...ARABIC MATHEMATICAL DOUBLE-STRUCK GHAIN
                                | '\u{20000}'...'\u{2A6D6}'  // CJK UNIFIED IDEOGRAPH-20000...CJK UNIFIED IDEOGRAPH-2A6D6
                                | '\u{2A700}'...'\u{2B734}'  // CJK UNIFIED IDEOGRAPH-2A700...CJK UNIFIED IDEOGRAPH-2B734
                                | '\u{2B740}'...'\u{2B81D}'  // CJK UNIFIED IDEOGRAPH-2B740...CJK UNIFIED IDEOGRAPH-2B81D
                                | '\u{2B820}'...'\u{2CEA1}'  // CJK UNIFIED IDEOGRAPH-2B820...CJK UNIFIED IDEOGRAPH-2CEA1
                                | '\u{2CEB0}'...'\u{2EBE0}'  // CJK UNIFIED IDEOGRAPH-2CEB0...CJK UNIFIED IDEOGRAPH-2EBE0
                                | '\u{2F800}'...'\u{2FA1D}'  // CJK COMPATIBILITY IDEOGRAPH-2F800...CJK COMPATIBILITY IDEOGRAPH-2FA1D
                                | '\u{E0100}'...'\u{E01EF}'  // VARIATION SELECTOR-17...VARIATION SELECTOR-256
                            => {
                                self.stream.advance();
                            }
                            // TODO '\\' |
                            _ => break,
                        },
                    }
                }
                let id = self.stream.str_from(start.pos);
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
            }

            _ => {
                panic!("unexpected {} @ {}:{} (col {})", here, self.file_name, start.row + 1, start.col + 1)
            },
        };
        Tok {
            tt,
            span: Span::new(self.file_name, start, self.stream.loc()),
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

/// Generic stream structure for source code.
///
/// A `Stream` advances over its input one character at a time, tracking line and column information and providing two characters of lookahead.
#[derive(Debug)]
pub struct Stream<'s> {
    input: &'s str,

    loc: Loc,
    here: Option<char>,

    next_pos: usize,
    next_width: usize,
    next: Option<char>,
}

impl<'s> Stream<'s> {
    /// Creates a new `Stream` on the given input.
    pub fn new(input: &'s str) -> Self {
        let mut stream = Stream {
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

    /// `true` if and only if the current character is `c`.
    #[inline]
    pub fn is(&self, c: char) -> bool {
        self.here.map_or(false, |cc| c == cc)
    }

    /// Advances to the next character if and only if the current character is `c`.
    #[inline]
    pub fn eat(&mut self, c: char) -> bool {
        match self.here {
            Some(cc) if c == cc => {
                self.advance();
                true
            }
            _ => false,
        }
    }

    /// Advances by two characters if and only if the current character is `c` and the next character is `d`.
    #[inline]
    pub fn eat2(&mut self, c: char, d: char) -> bool {
        match self.here {
            Some(cc) if c == cc => {
                match self.next {
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
    pub fn skip_while<F>(&mut self, mut f: F) where
    F: FnMut(char) -> bool {
        loop {
            match self.here {
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
    // pub fn skip_str_dbl_chars(&mut self) {
    //     self.skip_while(|c| match c {
    //         '\\' | '"' => false,
    //         _ => true,
    //     });
    // }

    // #[inline]
    // pub fn skip_str_sgl_chars(&mut self) {
    //     self.skip_while(|c| match c {
    //         '\\' | '\'' => false,
    //         _ => true,
    //     });
    // }

    /// Advances past any binary digits (`0` or `1`).
    #[inline]
    pub fn skip_bin_digits(&mut self) {
        self.skip_while(|c| match c {
            '0'...'1' => true,
            _ => false,
        });
    }

    /// Advances past any octal digits (`0` through `7`).
    #[inline]
    pub fn skip_oct_digits(&mut self) {
        self.skip_while(|c| match c {
            '0'...'7' => true,
            _ => false,
        });
    }

    /// Advances past any decimal digits (`0` through `9`).
    #[inline]
    pub fn skip_dec_digits(&mut self) {
        self.skip_while(|c| match c {
            '0'...'9' => true,
            _ => false,
        });
    }

    /// Advances past any hexadecimal digits (`0` through `9` and `a` through `f`, case insensitive).
    #[inline]
    pub fn skip_hex_digits(&mut self) {
        self.skip_while(|c| match c {
            '0'...'9' | 'a'...'f' | 'A'...'F' => true,
            _ => false,
        });
    }

    /// Advances past any characters in the Unicode category [ID_Continue](http://unicode.org/reports/tr31/).
    #[inline]
    pub fn skip_id_continue_chars(&mut self) {
        self.skip_while(|c| match c {
              '$'
            | '_'
            | '\u{200C}' // ZERO WIDTH NON-JOINER
            | '\u{200D}' // ZERO WIDTH JOINER
            // ID_Continue:
                | '\u{0030}'...'\u{0039}'    // DIGIT ZERO...DIGIT NINE
                | '\u{0041}'...'\u{005A}'    // LATIN CAPITAL LETTER A...LATIN CAPITAL LETTER Z
                // | '\u{005F}'                 // LOW LINE
                | '\u{0061}'...'\u{007A}'    // LATIN SMALL LETTER A...LATIN SMALL LETTER Z
                | '\u{00AA}'                 // FEMININE ORDINAL INDICATOR
                | '\u{00B5}'                 // MICRO SIGN
                | '\u{00B7}'                 // MIDDLE DOT
                | '\u{00BA}'                 // MASCULINE ORDINAL INDICATOR
                | '\u{00C0}'...'\u{00D6}'    // LATIN CAPITAL LETTER A WITH GRAVE...LATIN CAPITAL LETTER O WITH DIAERESIS
                | '\u{00D8}'...'\u{00F6}'    // LATIN CAPITAL LETTER O WITH STROKE...LATIN SMALL LETTER O WITH DIAERESIS
                | '\u{00F8}'...'\u{01BA}'    // LATIN SMALL LETTER O WITH STROKE...LATIN SMALL LETTER EZH WITH TAIL
                | '\u{01BB}'                 // LATIN LETTER TWO WITH STROKE
                | '\u{01BC}'...'\u{01BF}'    // LATIN CAPITAL LETTER TONE FIVE...LATIN LETTER WYNN
                | '\u{01C0}'...'\u{01C3}'    // LATIN LETTER DENTAL CLICK...LATIN LETTER RETROFLEX CLICK
                | '\u{01C4}'...'\u{0293}'    // LATIN CAPITAL LETTER DZ WITH CARON...LATIN SMALL LETTER EZH WITH CURL
                | '\u{0294}'                 // LATIN LETTER GLOTTAL STOP
                | '\u{0295}'...'\u{02AF}'    // LATIN LETTER PHARYNGEAL VOICED FRICATIVE...LATIN SMALL LETTER TURNED H WITH FISHHOOK AND TAIL
                | '\u{02B0}'...'\u{02C1}'    // MODIFIER LETTER SMALL H...MODIFIER LETTER REVERSED GLOTTAL STOP
                | '\u{02C6}'...'\u{02D1}'    // MODIFIER LETTER CIRCUMFLEX ACCENT...MODIFIER LETTER HALF TRIANGULAR COLON
                | '\u{02E0}'...'\u{02E4}'    // MODIFIER LETTER SMALL GAMMA...MODIFIER LETTER SMALL REVERSED GLOTTAL STOP
                | '\u{02EC}'                 // MODIFIER LETTER VOICING
                | '\u{02EE}'                 // MODIFIER LETTER DOUBLE APOSTROPHE
                | '\u{0300}'...'\u{036F}'    // COMBINING GRAVE ACCENT...COMBINING LATIN SMALL LETTER X
                | '\u{0370}'...'\u{0373}'    // GREEK CAPITAL LETTER HETA...GREEK SMALL LETTER ARCHAIC SAMPI
                | '\u{0374}'                 // GREEK NUMERAL SIGN
                | '\u{0376}'...'\u{0377}'    // GREEK CAPITAL LETTER PAMPHYLIAN DIGAMMA...GREEK SMALL LETTER PAMPHYLIAN DIGAMMA
                | '\u{037A}'                 // GREEK YPOGEGRAMMENI
                | '\u{037B}'...'\u{037D}'    // GREEK SMALL REVERSED LUNATE SIGMA SYMBOL...GREEK SMALL REVERSED DOTTED LUNATE SIGMA SYMBOL
                | '\u{037F}'                 // GREEK CAPITAL LETTER YOT
                | '\u{0386}'                 // GREEK CAPITAL LETTER ALPHA WITH TONOS
                | '\u{0387}'                 // GREEK ANO TELEIA
                | '\u{0388}'...'\u{038A}'    // GREEK CAPITAL LETTER EPSILON WITH TONOS...GREEK CAPITAL LETTER IOTA WITH TONOS
                | '\u{038C}'                 // GREEK CAPITAL LETTER OMICRON WITH TONOS
                | '\u{038E}'...'\u{03A1}'    // GREEK CAPITAL LETTER UPSILON WITH TONOS...GREEK CAPITAL LETTER RHO
                | '\u{03A3}'...'\u{03F5}'    // GREEK CAPITAL LETTER SIGMA...GREEK LUNATE EPSILON SYMBOL
                | '\u{03F7}'...'\u{0481}'    // GREEK CAPITAL LETTER SHO...CYRILLIC SMALL LETTER KOPPA
                | '\u{0483}'...'\u{0487}'    // COMBINING CYRILLIC TITLO...COMBINING CYRILLIC POKRYTIE
                | '\u{048A}'...'\u{052F}'    // CYRILLIC CAPITAL LETTER SHORT I WITH TAIL...CYRILLIC SMALL LETTER EL WITH DESCENDER
                | '\u{0531}'...'\u{0556}'    // ARMENIAN CAPITAL LETTER AYB...ARMENIAN CAPITAL LETTER FEH
                | '\u{0559}'                 // ARMENIAN MODIFIER LETTER LEFT HALF RING
                | '\u{0561}'...'\u{0587}'    // ARMENIAN SMALL LETTER AYB...ARMENIAN SMALL LIGATURE ECH YIWN
                | '\u{0591}'...'\u{05BD}'    // HEBREW ACCENT ETNAHTA...HEBREW POINT METEG
                | '\u{05BF}'                 // HEBREW POINT RAFE
                | '\u{05C1}'...'\u{05C2}'    // HEBREW POINT SHIN DOT...HEBREW POINT SIN DOT
                | '\u{05C4}'...'\u{05C5}'    // HEBREW MARK UPPER DOT...HEBREW MARK LOWER DOT
                | '\u{05C7}'                 // HEBREW POINT QAMATS QATAN
                | '\u{05D0}'...'\u{05EA}'    // HEBREW LETTER ALEF...HEBREW LETTER TAV
                | '\u{05F0}'...'\u{05F2}'    // HEBREW LIGATURE YIDDISH DOUBLE VAV...HEBREW LIGATURE YIDDISH DOUBLE YOD
                | '\u{0610}'...'\u{061A}'    // ARABIC SIGN SALLALLAHOU ALAYHE WASSALLAM...ARABIC SMALL KASRA
                | '\u{0620}'...'\u{063F}'    // ARABIC LETTER KASHMIRI YEH...ARABIC LETTER FARSI YEH WITH THREE DOTS ABOVE
                | '\u{0640}'                 // ARABIC TATWEEL
                | '\u{0641}'...'\u{064A}'    // ARABIC LETTER FEH...ARABIC LETTER YEH
                | '\u{064B}'...'\u{065F}'    // ARABIC FATHATAN...ARABIC WAVY HAMZA BELOW
                | '\u{0660}'...'\u{0669}'    // ARABIC-INDIC DIGIT ZERO...ARABIC-INDIC DIGIT NINE
                | '\u{066E}'...'\u{066F}'    // ARABIC LETTER DOTLESS BEH...ARABIC LETTER DOTLESS QAF
                | '\u{0670}'                 // ARABIC LETTER SUPERSCRIPT ALEF
                | '\u{0671}'...'\u{06D3}'    // ARABIC LETTER ALEF WASLA...ARABIC LETTER YEH BARREE WITH HAMZA ABOVE
                | '\u{06D5}'                 // ARABIC LETTER AE
                | '\u{06D6}'...'\u{06DC}'    // ARABIC SMALL HIGH LIGATURE SAD WITH LAM WITH ALEF MAKSURA...ARABIC SMALL HIGH SEEN
                | '\u{06DF}'...'\u{06E4}'    // ARABIC SMALL HIGH ROUNDED ZERO...ARABIC SMALL HIGH MADDA
                | '\u{06E5}'...'\u{06E6}'    // ARABIC SMALL WAW...ARABIC SMALL YEH
                | '\u{06E7}'...'\u{06E8}'    // ARABIC SMALL HIGH YEH...ARABIC SMALL HIGH NOON
                | '\u{06EA}'...'\u{06ED}'    // ARABIC EMPTY CENTRE LOW STOP...ARABIC SMALL LOW MEEM
                | '\u{06EE}'...'\u{06EF}'    // ARABIC LETTER DAL WITH INVERTED V...ARABIC LETTER REH WITH INVERTED V
                | '\u{06F0}'...'\u{06F9}'    // EXTENDED ARABIC-INDIC DIGIT ZERO...EXTENDED ARABIC-INDIC DIGIT NINE
                | '\u{06FA}'...'\u{06FC}'    // ARABIC LETTER SHEEN WITH DOT BELOW...ARABIC LETTER GHAIN WITH DOT BELOW
                | '\u{06FF}'                 // ARABIC LETTER HEH WITH INVERTED V
                | '\u{0710}'                 // SYRIAC LETTER ALAPH
                | '\u{0711}'                 // SYRIAC LETTER SUPERSCRIPT ALAPH
                | '\u{0712}'...'\u{072F}'    // SYRIAC LETTER BETH...SYRIAC LETTER PERSIAN DHALATH
                | '\u{0730}'...'\u{074A}'    // SYRIAC PTHAHA ABOVE...SYRIAC BARREKH
                | '\u{074D}'...'\u{07A5}'    // SYRIAC LETTER SOGDIAN ZHAIN...THAANA LETTER WAAVU
                | '\u{07A6}'...'\u{07B0}'    // THAANA ABAFILI...THAANA SUKUN
                | '\u{07B1}'                 // THAANA LETTER NAA
                | '\u{07C0}'...'\u{07C9}'    // NKO DIGIT ZERO...NKO DIGIT NINE
                | '\u{07CA}'...'\u{07EA}'    // NKO LETTER A...NKO LETTER JONA RA
                | '\u{07EB}'...'\u{07F3}'    // NKO COMBINING SHORT HIGH TONE...NKO COMBINING DOUBLE DOT ABOVE
                | '\u{07F4}'...'\u{07F5}'    // NKO HIGH TONE APOSTROPHE...NKO LOW TONE APOSTROPHE
                | '\u{07FA}'                 // NKO LAJANYALAN
                | '\u{0800}'...'\u{0815}'    // SAMARITAN LETTER ALAF...SAMARITAN LETTER TAAF
                | '\u{0816}'...'\u{0819}'    // SAMARITAN MARK IN...SAMARITAN MARK DAGESH
                | '\u{081A}'                 // SAMARITAN MODIFIER LETTER EPENTHETIC YUT
                | '\u{081B}'...'\u{0823}'    // SAMARITAN MARK EPENTHETIC YUT...SAMARITAN VOWEL SIGN A
                | '\u{0824}'                 // SAMARITAN MODIFIER LETTER SHORT A
                | '\u{0825}'...'\u{0827}'    // SAMARITAN VOWEL SIGN SHORT A...SAMARITAN VOWEL SIGN U
                | '\u{0828}'                 // SAMARITAN MODIFIER LETTER I
                | '\u{0829}'...'\u{082D}'    // SAMARITAN VOWEL SIGN LONG I...SAMARITAN MARK NEQUDAA
                | '\u{0840}'...'\u{0858}'    // MANDAIC LETTER HALQA...MANDAIC LETTER AIN
                | '\u{0859}'...'\u{085B}'    // MANDAIC AFFRICATION MARK...MANDAIC GEMINATION MARK
                | '\u{0860}'...'\u{086A}'    // SYRIAC LETTER MALAYALAM NGA...SYRIAC LETTER MALAYALAM SSA
                | '\u{08A0}'...'\u{08B4}'    // ARABIC LETTER BEH WITH SMALL V BELOW...ARABIC LETTER KAF WITH DOT BELOW
                | '\u{08B6}'...'\u{08BD}'    // ARABIC LETTER BEH WITH SMALL MEEM ABOVE...ARABIC LETTER AFRICAN NOON
                | '\u{08D4}'...'\u{08E1}'    // ARABIC SMALL HIGH WORD AR-RUB...ARABIC SMALL HIGH SIGN SAFHA
                | '\u{08E3}'...'\u{0902}'    // ARABIC TURNED DAMMA BELOW...DEVANAGARI SIGN ANUSVARA
                | '\u{0903}'                 // DEVANAGARI SIGN VISARGA
                | '\u{0904}'...'\u{0939}'    // DEVANAGARI LETTER SHORT A...DEVANAGARI LETTER HA
                | '\u{093A}'                 // DEVANAGARI VOWEL SIGN OE
                | '\u{093B}'                 // DEVANAGARI VOWEL SIGN OOE
                | '\u{093C}'                 // DEVANAGARI SIGN NUKTA
                | '\u{093D}'                 // DEVANAGARI SIGN AVAGRAHA
                | '\u{093E}'...'\u{0940}'    // DEVANAGARI VOWEL SIGN AA...DEVANAGARI VOWEL SIGN II
                | '\u{0941}'...'\u{0948}'    // DEVANAGARI VOWEL SIGN U...DEVANAGARI VOWEL SIGN AI
                | '\u{0949}'...'\u{094C}'    // DEVANAGARI VOWEL SIGN CANDRA O...DEVANAGARI VOWEL SIGN AU
                | '\u{094D}'                 // DEVANAGARI SIGN VIRAMA
                | '\u{094E}'...'\u{094F}'    // DEVANAGARI VOWEL SIGN PRISHTHAMATRA E...DEVANAGARI VOWEL SIGN AW
                | '\u{0950}'                 // DEVANAGARI OM
                | '\u{0951}'...'\u{0957}'    // DEVANAGARI STRESS SIGN UDATTA...DEVANAGARI VOWEL SIGN UUE
                | '\u{0958}'...'\u{0961}'    // DEVANAGARI LETTER QA...DEVANAGARI LETTER VOCALIC LL
                | '\u{0962}'...'\u{0963}'    // DEVANAGARI VOWEL SIGN VOCALIC L...DEVANAGARI VOWEL SIGN VOCALIC LL
                | '\u{0966}'...'\u{096F}'    // DEVANAGARI DIGIT ZERO...DEVANAGARI DIGIT NINE
                | '\u{0971}'                 // DEVANAGARI SIGN HIGH SPACING DOT
                | '\u{0972}'...'\u{0980}'    // DEVANAGARI LETTER CANDRA A...BENGALI ANJI
                | '\u{0981}'                 // BENGALI SIGN CANDRABINDU
                | '\u{0982}'...'\u{0983}'    // BENGALI SIGN ANUSVARA...BENGALI SIGN VISARGA
                | '\u{0985}'...'\u{098C}'    // BENGALI LETTER A...BENGALI LETTER VOCALIC L
                | '\u{098F}'...'\u{0990}'    // BENGALI LETTER E...BENGALI LETTER AI
                | '\u{0993}'...'\u{09A8}'    // BENGALI LETTER O...BENGALI LETTER NA
                | '\u{09AA}'...'\u{09B0}'    // BENGALI LETTER PA...BENGALI LETTER RA
                | '\u{09B2}'                 // BENGALI LETTER LA
                | '\u{09B6}'...'\u{09B9}'    // BENGALI LETTER SHA...BENGALI LETTER HA
                | '\u{09BC}'                 // BENGALI SIGN NUKTA
                | '\u{09BD}'                 // BENGALI SIGN AVAGRAHA
                | '\u{09BE}'...'\u{09C0}'    // BENGALI VOWEL SIGN AA...BENGALI VOWEL SIGN II
                | '\u{09C1}'...'\u{09C4}'    // BENGALI VOWEL SIGN U...BENGALI VOWEL SIGN VOCALIC RR
                | '\u{09C7}'...'\u{09C8}'    // BENGALI VOWEL SIGN E...BENGALI VOWEL SIGN AI
                | '\u{09CB}'...'\u{09CC}'    // BENGALI VOWEL SIGN O...BENGALI VOWEL SIGN AU
                | '\u{09CD}'                 // BENGALI SIGN VIRAMA
                | '\u{09CE}'                 // BENGALI LETTER KHANDA TA
                | '\u{09D7}'                 // BENGALI AU LENGTH MARK
                | '\u{09DC}'...'\u{09DD}'    // BENGALI LETTER RRA...BENGALI LETTER RHA
                | '\u{09DF}'...'\u{09E1}'    // BENGALI LETTER YYA...BENGALI LETTER VOCALIC LL
                | '\u{09E2}'...'\u{09E3}'    // BENGALI VOWEL SIGN VOCALIC L...BENGALI VOWEL SIGN VOCALIC LL
                | '\u{09E6}'...'\u{09EF}'    // BENGALI DIGIT ZERO...BENGALI DIGIT NINE
                | '\u{09F0}'...'\u{09F1}'    // BENGALI LETTER RA WITH MIDDLE DIAGONAL...BENGALI LETTER RA WITH LOWER DIAGONAL
                | '\u{09FC}'                 // BENGALI LETTER VEDIC ANUSVARA
                | '\u{0A01}'...'\u{0A02}'    // GURMUKHI SIGN ADAK BINDI...GURMUKHI SIGN BINDI
                | '\u{0A03}'                 // GURMUKHI SIGN VISARGA
                | '\u{0A05}'...'\u{0A0A}'    // GURMUKHI LETTER A...GURMUKHI LETTER UU
                | '\u{0A0F}'...'\u{0A10}'    // GURMUKHI LETTER EE...GURMUKHI LETTER AI
                | '\u{0A13}'...'\u{0A28}'    // GURMUKHI LETTER OO...GURMUKHI LETTER NA
                | '\u{0A2A}'...'\u{0A30}'    // GURMUKHI LETTER PA...GURMUKHI LETTER RA
                | '\u{0A32}'...'\u{0A33}'    // GURMUKHI LETTER LA...GURMUKHI LETTER LLA
                | '\u{0A35}'...'\u{0A36}'    // GURMUKHI LETTER VA...GURMUKHI LETTER SHA
                | '\u{0A38}'...'\u{0A39}'    // GURMUKHI LETTER SA...GURMUKHI LETTER HA
                | '\u{0A3C}'                 // GURMUKHI SIGN NUKTA
                | '\u{0A3E}'...'\u{0A40}'    // GURMUKHI VOWEL SIGN AA...GURMUKHI VOWEL SIGN II
                | '\u{0A41}'...'\u{0A42}'    // GURMUKHI VOWEL SIGN U...GURMUKHI VOWEL SIGN UU
                | '\u{0A47}'...'\u{0A48}'    // GURMUKHI VOWEL SIGN EE...GURMUKHI VOWEL SIGN AI
                | '\u{0A4B}'...'\u{0A4D}'    // GURMUKHI VOWEL SIGN OO...GURMUKHI SIGN VIRAMA
                | '\u{0A51}'                 // GURMUKHI SIGN UDAAT
                | '\u{0A59}'...'\u{0A5C}'    // GURMUKHI LETTER KHHA...GURMUKHI LETTER RRA
                | '\u{0A5E}'                 // GURMUKHI LETTER FA
                | '\u{0A66}'...'\u{0A6F}'    // GURMUKHI DIGIT ZERO...GURMUKHI DIGIT NINE
                | '\u{0A70}'...'\u{0A71}'    // GURMUKHI TIPPI...GURMUKHI ADDAK
                | '\u{0A72}'...'\u{0A74}'    // GURMUKHI IRI...GURMUKHI EK ONKAR
                | '\u{0A75}'                 // GURMUKHI SIGN YAKASH
                | '\u{0A81}'...'\u{0A82}'    // GUJARATI SIGN CANDRABINDU...GUJARATI SIGN ANUSVARA
                | '\u{0A83}'                 // GUJARATI SIGN VISARGA
                | '\u{0A85}'...'\u{0A8D}'    // GUJARATI LETTER A...GUJARATI VOWEL CANDRA E
                | '\u{0A8F}'...'\u{0A91}'    // GUJARATI LETTER E...GUJARATI VOWEL CANDRA O
                | '\u{0A93}'...'\u{0AA8}'    // GUJARATI LETTER O...GUJARATI LETTER NA
                | '\u{0AAA}'...'\u{0AB0}'    // GUJARATI LETTER PA...GUJARATI LETTER RA
                | '\u{0AB2}'...'\u{0AB3}'    // GUJARATI LETTER LA...GUJARATI LETTER LLA
                | '\u{0AB5}'...'\u{0AB9}'    // GUJARATI LETTER VA...GUJARATI LETTER HA
                | '\u{0ABC}'                 // GUJARATI SIGN NUKTA
                | '\u{0ABD}'                 // GUJARATI SIGN AVAGRAHA
                | '\u{0ABE}'...'\u{0AC0}'    // GUJARATI VOWEL SIGN AA...GUJARATI VOWEL SIGN II
                | '\u{0AC1}'...'\u{0AC5}'    // GUJARATI VOWEL SIGN U...GUJARATI VOWEL SIGN CANDRA E
                | '\u{0AC7}'...'\u{0AC8}'    // GUJARATI VOWEL SIGN E...GUJARATI VOWEL SIGN AI
                | '\u{0AC9}'                 // GUJARATI VOWEL SIGN CANDRA O
                | '\u{0ACB}'...'\u{0ACC}'    // GUJARATI VOWEL SIGN O...GUJARATI VOWEL SIGN AU
                | '\u{0ACD}'                 // GUJARATI SIGN VIRAMA
                | '\u{0AD0}'                 // GUJARATI OM
                | '\u{0AE0}'...'\u{0AE1}'    // GUJARATI LETTER VOCALIC RR...GUJARATI LETTER VOCALIC LL
                | '\u{0AE2}'...'\u{0AE3}'    // GUJARATI VOWEL SIGN VOCALIC L...GUJARATI VOWEL SIGN VOCALIC LL
                | '\u{0AE6}'...'\u{0AEF}'    // GUJARATI DIGIT ZERO...GUJARATI DIGIT NINE
                | '\u{0AF9}'                 // GUJARATI LETTER ZHA
                | '\u{0AFA}'...'\u{0AFF}'    // GUJARATI SIGN SUKUN...GUJARATI SIGN TWO-CIRCLE NUKTA ABOVE
                | '\u{0B01}'                 // ORIYA SIGN CANDRABINDU
                | '\u{0B02}'...'\u{0B03}'    // ORIYA SIGN ANUSVARA...ORIYA SIGN VISARGA
                | '\u{0B05}'...'\u{0B0C}'    // ORIYA LETTER A...ORIYA LETTER VOCALIC L
                | '\u{0B0F}'...'\u{0B10}'    // ORIYA LETTER E...ORIYA LETTER AI
                | '\u{0B13}'...'\u{0B28}'    // ORIYA LETTER O...ORIYA LETTER NA
                | '\u{0B2A}'...'\u{0B30}'    // ORIYA LETTER PA...ORIYA LETTER RA
                | '\u{0B32}'...'\u{0B33}'    // ORIYA LETTER LA...ORIYA LETTER LLA
                | '\u{0B35}'...'\u{0B39}'    // ORIYA LETTER VA...ORIYA LETTER HA
                | '\u{0B3C}'                 // ORIYA SIGN NUKTA
                | '\u{0B3D}'                 // ORIYA SIGN AVAGRAHA
                | '\u{0B3E}'                 // ORIYA VOWEL SIGN AA
                | '\u{0B3F}'                 // ORIYA VOWEL SIGN I
                | '\u{0B40}'                 // ORIYA VOWEL SIGN II
                | '\u{0B41}'...'\u{0B44}'    // ORIYA VOWEL SIGN U...ORIYA VOWEL SIGN VOCALIC RR
                | '\u{0B47}'...'\u{0B48}'    // ORIYA VOWEL SIGN E...ORIYA VOWEL SIGN AI
                | '\u{0B4B}'...'\u{0B4C}'    // ORIYA VOWEL SIGN O...ORIYA VOWEL SIGN AU
                | '\u{0B4D}'                 // ORIYA SIGN VIRAMA
                | '\u{0B56}'                 // ORIYA AI LENGTH MARK
                | '\u{0B57}'                 // ORIYA AU LENGTH MARK
                | '\u{0B5C}'...'\u{0B5D}'    // ORIYA LETTER RRA...ORIYA LETTER RHA
                | '\u{0B5F}'...'\u{0B61}'    // ORIYA LETTER YYA...ORIYA LETTER VOCALIC LL
                | '\u{0B62}'...'\u{0B63}'    // ORIYA VOWEL SIGN VOCALIC L...ORIYA VOWEL SIGN VOCALIC LL
                | '\u{0B66}'...'\u{0B6F}'    // ORIYA DIGIT ZERO...ORIYA DIGIT NINE
                | '\u{0B71}'                 // ORIYA LETTER WA
                | '\u{0B82}'                 // TAMIL SIGN ANUSVARA
                | '\u{0B83}'                 // TAMIL SIGN VISARGA
                | '\u{0B85}'...'\u{0B8A}'    // TAMIL LETTER A...TAMIL LETTER UU
                | '\u{0B8E}'...'\u{0B90}'    // TAMIL LETTER E...TAMIL LETTER AI
                | '\u{0B92}'...'\u{0B95}'    // TAMIL LETTER O...TAMIL LETTER KA
                | '\u{0B99}'...'\u{0B9A}'    // TAMIL LETTER NGA...TAMIL LETTER CA
                | '\u{0B9C}'                 // TAMIL LETTER JA
                | '\u{0B9E}'...'\u{0B9F}'    // TAMIL LETTER NYA...TAMIL LETTER TTA
                | '\u{0BA3}'...'\u{0BA4}'    // TAMIL LETTER NNA...TAMIL LETTER TA
                | '\u{0BA8}'...'\u{0BAA}'    // TAMIL LETTER NA...TAMIL LETTER PA
                | '\u{0BAE}'...'\u{0BB9}'    // TAMIL LETTER MA...TAMIL LETTER HA
                | '\u{0BBE}'...'\u{0BBF}'    // TAMIL VOWEL SIGN AA...TAMIL VOWEL SIGN I
                | '\u{0BC0}'                 // TAMIL VOWEL SIGN II
                | '\u{0BC1}'...'\u{0BC2}'    // TAMIL VOWEL SIGN U...TAMIL VOWEL SIGN UU
                | '\u{0BC6}'...'\u{0BC8}'    // TAMIL VOWEL SIGN E...TAMIL VOWEL SIGN AI
                | '\u{0BCA}'...'\u{0BCC}'    // TAMIL VOWEL SIGN O...TAMIL VOWEL SIGN AU
                | '\u{0BCD}'                 // TAMIL SIGN VIRAMA
                | '\u{0BD0}'                 // TAMIL OM
                | '\u{0BD7}'                 // TAMIL AU LENGTH MARK
                | '\u{0BE6}'...'\u{0BEF}'    // TAMIL DIGIT ZERO...TAMIL DIGIT NINE
                | '\u{0C00}'                 // TELUGU SIGN COMBINING CANDRABINDU ABOVE
                | '\u{0C01}'...'\u{0C03}'    // TELUGU SIGN CANDRABINDU...TELUGU SIGN VISARGA
                | '\u{0C05}'...'\u{0C0C}'    // TELUGU LETTER A...TELUGU LETTER VOCALIC L
                | '\u{0C0E}'...'\u{0C10}'    // TELUGU LETTER E...TELUGU LETTER AI
                | '\u{0C12}'...'\u{0C28}'    // TELUGU LETTER O...TELUGU LETTER NA
                | '\u{0C2A}'...'\u{0C39}'    // TELUGU LETTER PA...TELUGU LETTER HA
                | '\u{0C3D}'                 // TELUGU SIGN AVAGRAHA
                | '\u{0C3E}'...'\u{0C40}'    // TELUGU VOWEL SIGN AA...TELUGU VOWEL SIGN II
                | '\u{0C41}'...'\u{0C44}'    // TELUGU VOWEL SIGN U...TELUGU VOWEL SIGN VOCALIC RR
                | '\u{0C46}'...'\u{0C48}'    // TELUGU VOWEL SIGN E...TELUGU VOWEL SIGN AI
                | '\u{0C4A}'...'\u{0C4D}'    // TELUGU VOWEL SIGN O...TELUGU SIGN VIRAMA
                | '\u{0C55}'...'\u{0C56}'    // TELUGU LENGTH MARK...TELUGU AI LENGTH MARK
                | '\u{0C58}'...'\u{0C5A}'    // TELUGU LETTER TSA...TELUGU LETTER RRRA
                | '\u{0C60}'...'\u{0C61}'    // TELUGU LETTER VOCALIC RR...TELUGU LETTER VOCALIC LL
                | '\u{0C62}'...'\u{0C63}'    // TELUGU VOWEL SIGN VOCALIC L...TELUGU VOWEL SIGN VOCALIC LL
                | '\u{0C66}'...'\u{0C6F}'    // TELUGU DIGIT ZERO...TELUGU DIGIT NINE
                | '\u{0C80}'                 // KANNADA SIGN SPACING CANDRABINDU
                | '\u{0C81}'                 // KANNADA SIGN CANDRABINDU
                | '\u{0C82}'...'\u{0C83}'    // KANNADA SIGN ANUSVARA...KANNADA SIGN VISARGA
                | '\u{0C85}'...'\u{0C8C}'    // KANNADA LETTER A...KANNADA LETTER VOCALIC L
                | '\u{0C8E}'...'\u{0C90}'    // KANNADA LETTER E...KANNADA LETTER AI
                | '\u{0C92}'...'\u{0CA8}'    // KANNADA LETTER O...KANNADA LETTER NA
                | '\u{0CAA}'...'\u{0CB3}'    // KANNADA LETTER PA...KANNADA LETTER LLA
                | '\u{0CB5}'...'\u{0CB9}'    // KANNADA LETTER VA...KANNADA LETTER HA
                | '\u{0CBC}'                 // KANNADA SIGN NUKTA
                | '\u{0CBD}'                 // KANNADA SIGN AVAGRAHA
                | '\u{0CBE}'                 // KANNADA VOWEL SIGN AA
                | '\u{0CBF}'                 // KANNADA VOWEL SIGN I
                | '\u{0CC0}'...'\u{0CC4}'    // KANNADA VOWEL SIGN II...KANNADA VOWEL SIGN VOCALIC RR
                | '\u{0CC6}'                 // KANNADA VOWEL SIGN E
                | '\u{0CC7}'...'\u{0CC8}'    // KANNADA VOWEL SIGN EE...KANNADA VOWEL SIGN AI
                | '\u{0CCA}'...'\u{0CCB}'    // KANNADA VOWEL SIGN O...KANNADA VOWEL SIGN OO
                | '\u{0CCC}'...'\u{0CCD}'    // KANNADA VOWEL SIGN AU...KANNADA SIGN VIRAMA
                | '\u{0CD5}'...'\u{0CD6}'    // KANNADA LENGTH MARK...KANNADA AI LENGTH MARK
                | '\u{0CDE}'                 // KANNADA LETTER FA
                | '\u{0CE0}'...'\u{0CE1}'    // KANNADA LETTER VOCALIC RR...KANNADA LETTER VOCALIC LL
                | '\u{0CE2}'...'\u{0CE3}'    // KANNADA VOWEL SIGN VOCALIC L...KANNADA VOWEL SIGN VOCALIC LL
                | '\u{0CE6}'...'\u{0CEF}'    // KANNADA DIGIT ZERO...KANNADA DIGIT NINE
                | '\u{0CF1}'...'\u{0CF2}'    // KANNADA SIGN JIHVAMULIYA...KANNADA SIGN UPADHMANIYA
                | '\u{0D00}'...'\u{0D01}'    // MALAYALAM SIGN COMBINING ANUSVARA ABOVE...MALAYALAM SIGN CANDRABINDU
                | '\u{0D02}'...'\u{0D03}'    // MALAYALAM SIGN ANUSVARA...MALAYALAM SIGN VISARGA
                | '\u{0D05}'...'\u{0D0C}'    // MALAYALAM LETTER A...MALAYALAM LETTER VOCALIC L
                | '\u{0D0E}'...'\u{0D10}'    // MALAYALAM LETTER E...MALAYALAM LETTER AI
                | '\u{0D12}'...'\u{0D3A}'    // MALAYALAM LETTER O...MALAYALAM LETTER TTTA
                | '\u{0D3B}'...'\u{0D3C}'    // MALAYALAM SIGN VERTICAL BAR VIRAMA...MALAYALAM SIGN CIRCULAR VIRAMA
                | '\u{0D3D}'                 // MALAYALAM SIGN AVAGRAHA
                | '\u{0D3E}'...'\u{0D40}'    // MALAYALAM VOWEL SIGN AA...MALAYALAM VOWEL SIGN II
                | '\u{0D41}'...'\u{0D44}'    // MALAYALAM VOWEL SIGN U...MALAYALAM VOWEL SIGN VOCALIC RR
                | '\u{0D46}'...'\u{0D48}'    // MALAYALAM VOWEL SIGN E...MALAYALAM VOWEL SIGN AI
                | '\u{0D4A}'...'\u{0D4C}'    // MALAYALAM VOWEL SIGN O...MALAYALAM VOWEL SIGN AU
                | '\u{0D4D}'                 // MALAYALAM SIGN VIRAMA
                | '\u{0D4E}'                 // MALAYALAM LETTER DOT REPH
                | '\u{0D54}'...'\u{0D56}'    // MALAYALAM LETTER CHILLU M...MALAYALAM LETTER CHILLU LLL
                | '\u{0D57}'                 // MALAYALAM AU LENGTH MARK
                | '\u{0D5F}'...'\u{0D61}'    // MALAYALAM LETTER ARCHAIC II...MALAYALAM LETTER VOCALIC LL
                | '\u{0D62}'...'\u{0D63}'    // MALAYALAM VOWEL SIGN VOCALIC L...MALAYALAM VOWEL SIGN VOCALIC LL
                | '\u{0D66}'...'\u{0D6F}'    // MALAYALAM DIGIT ZERO...MALAYALAM DIGIT NINE
                | '\u{0D7A}'...'\u{0D7F}'    // MALAYALAM LETTER CHILLU NN...MALAYALAM LETTER CHILLU K
                | '\u{0D82}'...'\u{0D83}'    // SINHALA SIGN ANUSVARAYA...SINHALA SIGN VISARGAYA
                | '\u{0D85}'...'\u{0D96}'    // SINHALA LETTER AYANNA...SINHALA LETTER AUYANNA
                | '\u{0D9A}'...'\u{0DB1}'    // SINHALA LETTER ALPAPRAANA KAYANNA...SINHALA LETTER DANTAJA NAYANNA
                | '\u{0DB3}'...'\u{0DBB}'    // SINHALA LETTER SANYAKA DAYANNA...SINHALA LETTER RAYANNA
                | '\u{0DBD}'                 // SINHALA LETTER DANTAJA LAYANNA
                | '\u{0DC0}'...'\u{0DC6}'    // SINHALA LETTER VAYANNA...SINHALA LETTER FAYANNA
                | '\u{0DCA}'                 // SINHALA SIGN AL-LAKUNA
                | '\u{0DCF}'...'\u{0DD1}'    // SINHALA VOWEL SIGN AELA-PILLA...SINHALA VOWEL SIGN DIGA AEDA-PILLA
                | '\u{0DD2}'...'\u{0DD4}'    // SINHALA VOWEL SIGN KETTI IS-PILLA...SINHALA VOWEL SIGN KETTI PAA-PILLA
                | '\u{0DD6}'                 // SINHALA VOWEL SIGN DIGA PAA-PILLA
                | '\u{0DD8}'...'\u{0DDF}'    // SINHALA VOWEL SIGN GAETTA-PILLA...SINHALA VOWEL SIGN GAYANUKITTA
                | '\u{0DE6}'...'\u{0DEF}'    // SINHALA LITH DIGIT ZERO...SINHALA LITH DIGIT NINE
                | '\u{0DF2}'...'\u{0DF3}'    // SINHALA VOWEL SIGN DIGA GAETTA-PILLA...SINHALA VOWEL SIGN DIGA GAYANUKITTA
                | '\u{0E01}'...'\u{0E30}'    // THAI CHARACTER KO KAI...THAI CHARACTER SARA A
                | '\u{0E31}'                 // THAI CHARACTER MAI HAN-AKAT
                | '\u{0E32}'...'\u{0E33}'    // THAI CHARACTER SARA AA...THAI CHARACTER SARA AM
                | '\u{0E34}'...'\u{0E3A}'    // THAI CHARACTER SARA I...THAI CHARACTER PHINTHU
                | '\u{0E40}'...'\u{0E45}'    // THAI CHARACTER SARA E...THAI CHARACTER LAKKHANGYAO
                | '\u{0E46}'                 // THAI CHARACTER MAIYAMOK
                | '\u{0E47}'...'\u{0E4E}'    // THAI CHARACTER MAITAIKHU...THAI CHARACTER YAMAKKAN
                | '\u{0E50}'...'\u{0E59}'    // THAI DIGIT ZERO...THAI DIGIT NINE
                | '\u{0E81}'...'\u{0E82}'    // LAO LETTER KO...LAO LETTER KHO SUNG
                | '\u{0E84}'                 // LAO LETTER KHO TAM
                | '\u{0E87}'...'\u{0E88}'    // LAO LETTER NGO...LAO LETTER CO
                | '\u{0E8A}'                 // LAO LETTER SO TAM
                | '\u{0E8D}'                 // LAO LETTER NYO
                | '\u{0E94}'...'\u{0E97}'    // LAO LETTER DO...LAO LETTER THO TAM
                | '\u{0E99}'...'\u{0E9F}'    // LAO LETTER NO...LAO LETTER FO SUNG
                | '\u{0EA1}'...'\u{0EA3}'    // LAO LETTER MO...LAO LETTER LO LING
                | '\u{0EA5}'                 // LAO LETTER LO LOOT
                | '\u{0EA7}'                 // LAO LETTER WO
                | '\u{0EAA}'...'\u{0EAB}'    // LAO LETTER SO SUNG...LAO LETTER HO SUNG
                | '\u{0EAD}'...'\u{0EB0}'    // LAO LETTER O...LAO VOWEL SIGN A
                | '\u{0EB1}'                 // LAO VOWEL SIGN MAI KAN
                | '\u{0EB2}'...'\u{0EB3}'    // LAO VOWEL SIGN AA...LAO VOWEL SIGN AM
                | '\u{0EB4}'...'\u{0EB9}'    // LAO VOWEL SIGN I...LAO VOWEL SIGN UU
                | '\u{0EBB}'...'\u{0EBC}'    // LAO VOWEL SIGN MAI KON...LAO SEMIVOWEL SIGN LO
                | '\u{0EBD}'                 // LAO SEMIVOWEL SIGN NYO
                | '\u{0EC0}'...'\u{0EC4}'    // LAO VOWEL SIGN E...LAO VOWEL SIGN AI
                | '\u{0EC6}'                 // LAO KO LA
                | '\u{0EC8}'...'\u{0ECD}'    // LAO TONE MAI EK...LAO NIGGAHITA
                | '\u{0ED0}'...'\u{0ED9}'    // LAO DIGIT ZERO...LAO DIGIT NINE
                | '\u{0EDC}'...'\u{0EDF}'    // LAO HO NO...LAO LETTER KHMU NYO
                | '\u{0F00}'                 // TIBETAN SYLLABLE OM
                | '\u{0F18}'...'\u{0F19}'    // TIBETAN ASTROLOGICAL SIGN -KHYUD PA...TIBETAN ASTROLOGICAL SIGN SDONG TSHUGS
                | '\u{0F20}'...'\u{0F29}'    // TIBETAN DIGIT ZERO...TIBETAN DIGIT NINE
                | '\u{0F35}'                 // TIBETAN MARK NGAS BZUNG NYI ZLA
                | '\u{0F37}'                 // TIBETAN MARK NGAS BZUNG SGOR RTAGS
                | '\u{0F39}'                 // TIBETAN MARK TSA -PHRU
                | '\u{0F3E}'...'\u{0F3F}'    // TIBETAN SIGN YAR TSHES...TIBETAN SIGN MAR TSHES
                | '\u{0F40}'...'\u{0F47}'    // TIBETAN LETTER KA...TIBETAN LETTER JA
                | '\u{0F49}'...'\u{0F6C}'    // TIBETAN LETTER NYA...TIBETAN LETTER RRA
                | '\u{0F71}'...'\u{0F7E}'    // TIBETAN VOWEL SIGN AA...TIBETAN SIGN RJES SU NGA RO
                | '\u{0F7F}'                 // TIBETAN SIGN RNAM BCAD
                | '\u{0F80}'...'\u{0F84}'    // TIBETAN VOWEL SIGN REVERSED I...TIBETAN MARK HALANTA
                | '\u{0F86}'...'\u{0F87}'    // TIBETAN SIGN LCI RTAGS...TIBETAN SIGN YANG RTAGS
                | '\u{0F88}'...'\u{0F8C}'    // TIBETAN SIGN LCE TSA CAN...TIBETAN SIGN INVERTED MCHU CAN
                | '\u{0F8D}'...'\u{0F97}'    // TIBETAN SUBJOINED SIGN LCE TSA CAN...TIBETAN SUBJOINED LETTER JA
                | '\u{0F99}'...'\u{0FBC}'    // TIBETAN SUBJOINED LETTER NYA...TIBETAN SUBJOINED LETTER FIXED-FORM RA
                | '\u{0FC6}'                 // TIBETAN SYMBOL PADMA GDAN
                | '\u{1000}'...'\u{102A}'    // MYANMAR LETTER KA...MYANMAR LETTER AU
                | '\u{102B}'...'\u{102C}'    // MYANMAR VOWEL SIGN TALL AA...MYANMAR VOWEL SIGN AA
                | '\u{102D}'...'\u{1030}'    // MYANMAR VOWEL SIGN I...MYANMAR VOWEL SIGN UU
                | '\u{1031}'                 // MYANMAR VOWEL SIGN E
                | '\u{1032}'...'\u{1037}'    // MYANMAR VOWEL SIGN AI...MYANMAR SIGN DOT BELOW
                | '\u{1038}'                 // MYANMAR SIGN VISARGA
                | '\u{1039}'...'\u{103A}'    // MYANMAR SIGN VIRAMA...MYANMAR SIGN ASAT
                | '\u{103B}'...'\u{103C}'    // MYANMAR CONSONANT SIGN MEDIAL YA...MYANMAR CONSONANT SIGN MEDIAL RA
                | '\u{103D}'...'\u{103E}'    // MYANMAR CONSONANT SIGN MEDIAL WA...MYANMAR CONSONANT SIGN MEDIAL HA
                | '\u{103F}'                 // MYANMAR LETTER GREAT SA
                | '\u{1040}'...'\u{1049}'    // MYANMAR DIGIT ZERO...MYANMAR DIGIT NINE
                | '\u{1050}'...'\u{1055}'    // MYANMAR LETTER SHA...MYANMAR LETTER VOCALIC LL
                | '\u{1056}'...'\u{1057}'    // MYANMAR VOWEL SIGN VOCALIC R...MYANMAR VOWEL SIGN VOCALIC RR
                | '\u{1058}'...'\u{1059}'    // MYANMAR VOWEL SIGN VOCALIC L...MYANMAR VOWEL SIGN VOCALIC LL
                | '\u{105A}'...'\u{105D}'    // MYANMAR LETTER MON NGA...MYANMAR LETTER MON BBE
                | '\u{105E}'...'\u{1060}'    // MYANMAR CONSONANT SIGN MON MEDIAL NA...MYANMAR CONSONANT SIGN MON MEDIAL LA
                | '\u{1061}'                 // MYANMAR LETTER SGAW KAREN SHA
                | '\u{1062}'...'\u{1064}'    // MYANMAR VOWEL SIGN SGAW KAREN EU...MYANMAR TONE MARK SGAW KAREN KE PHO
                | '\u{1065}'...'\u{1066}'    // MYANMAR LETTER WESTERN PWO KAREN THA...MYANMAR LETTER WESTERN PWO KAREN PWA
                | '\u{1067}'...'\u{106D}'    // MYANMAR VOWEL SIGN WESTERN PWO KAREN EU...MYANMAR SIGN WESTERN PWO KAREN TONE-5
                | '\u{106E}'...'\u{1070}'    // MYANMAR LETTER EASTERN PWO KAREN NNA...MYANMAR LETTER EASTERN PWO KAREN GHWA
                | '\u{1071}'...'\u{1074}'    // MYANMAR VOWEL SIGN GEBA KAREN I...MYANMAR VOWEL SIGN KAYAH EE
                | '\u{1075}'...'\u{1081}'    // MYANMAR LETTER SHAN KA...MYANMAR LETTER SHAN HA
                | '\u{1082}'                 // MYANMAR CONSONANT SIGN SHAN MEDIAL WA
                | '\u{1083}'...'\u{1084}'    // MYANMAR VOWEL SIGN SHAN AA...MYANMAR VOWEL SIGN SHAN E
                | '\u{1085}'...'\u{1086}'    // MYANMAR VOWEL SIGN SHAN E ABOVE...MYANMAR VOWEL SIGN SHAN FINAL Y
                | '\u{1087}'...'\u{108C}'    // MYANMAR SIGN SHAN TONE-2...MYANMAR SIGN SHAN COUNCIL TONE-3
                | '\u{108D}'                 // MYANMAR SIGN SHAN COUNCIL EMPHATIC TONE
                | '\u{108E}'                 // MYANMAR LETTER RUMAI PALAUNG FA
                | '\u{108F}'                 // MYANMAR SIGN RUMAI PALAUNG TONE-5
                | '\u{1090}'...'\u{1099}'    // MYANMAR SHAN DIGIT ZERO...MYANMAR SHAN DIGIT NINE
                | '\u{109A}'...'\u{109C}'    // MYANMAR SIGN KHAMTI TONE-1...MYANMAR VOWEL SIGN AITON A
                | '\u{109D}'                 // MYANMAR VOWEL SIGN AITON AI
                | '\u{10A0}'...'\u{10C5}'    // GEORGIAN CAPITAL LETTER AN...GEORGIAN CAPITAL LETTER HOE
                | '\u{10C7}'                 // GEORGIAN CAPITAL LETTER YN
                | '\u{10CD}'                 // GEORGIAN CAPITAL LETTER AEN
                | '\u{10D0}'...'\u{10FA}'    // GEORGIAN LETTER AN...GEORGIAN LETTER AIN
                | '\u{10FC}'                 // MODIFIER LETTER GEORGIAN NAR
                | '\u{10FD}'...'\u{1248}'    // GEORGIAN LETTER AEN...ETHIOPIC SYLLABLE QWA
                | '\u{124A}'...'\u{124D}'    // ETHIOPIC SYLLABLE QWI...ETHIOPIC SYLLABLE QWE
                | '\u{1250}'...'\u{1256}'    // ETHIOPIC SYLLABLE QHA...ETHIOPIC SYLLABLE QHO
                | '\u{1258}'                 // ETHIOPIC SYLLABLE QHWA
                | '\u{125A}'...'\u{125D}'    // ETHIOPIC SYLLABLE QHWI...ETHIOPIC SYLLABLE QHWE
                | '\u{1260}'...'\u{1288}'    // ETHIOPIC SYLLABLE BA...ETHIOPIC SYLLABLE XWA
                | '\u{128A}'...'\u{128D}'    // ETHIOPIC SYLLABLE XWI...ETHIOPIC SYLLABLE XWE
                | '\u{1290}'...'\u{12B0}'    // ETHIOPIC SYLLABLE NA...ETHIOPIC SYLLABLE KWA
                | '\u{12B2}'...'\u{12B5}'    // ETHIOPIC SYLLABLE KWI...ETHIOPIC SYLLABLE KWE
                | '\u{12B8}'...'\u{12BE}'    // ETHIOPIC SYLLABLE KXA...ETHIOPIC SYLLABLE KXO
                | '\u{12C0}'                 // ETHIOPIC SYLLABLE KXWA
                | '\u{12C2}'...'\u{12C5}'    // ETHIOPIC SYLLABLE KXWI...ETHIOPIC SYLLABLE KXWE
                | '\u{12C8}'...'\u{12D6}'    // ETHIOPIC SYLLABLE WA...ETHIOPIC SYLLABLE PHARYNGEAL O
                | '\u{12D8}'...'\u{1310}'    // ETHIOPIC SYLLABLE ZA...ETHIOPIC SYLLABLE GWA
                | '\u{1312}'...'\u{1315}'    // ETHIOPIC SYLLABLE GWI...ETHIOPIC SYLLABLE GWE
                | '\u{1318}'...'\u{135A}'    // ETHIOPIC SYLLABLE GGA...ETHIOPIC SYLLABLE FYA
                | '\u{135D}'...'\u{135F}'    // ETHIOPIC COMBINING GEMINATION AND VOWEL LENGTH MARK...ETHIOPIC COMBINING GEMINATION MARK
                | '\u{1369}'...'\u{1371}'    // ETHIOPIC DIGIT ONE...ETHIOPIC DIGIT NINE
                | '\u{1380}'...'\u{138F}'    // ETHIOPIC SYLLABLE SEBATBEIT MWA...ETHIOPIC SYLLABLE PWE
                | '\u{13A0}'...'\u{13F5}'    // CHEROKEE LETTER A...CHEROKEE LETTER MV
                | '\u{13F8}'...'\u{13FD}'    // CHEROKEE SMALL LETTER YE...CHEROKEE SMALL LETTER MV
                | '\u{1401}'...'\u{166C}'    // CANADIAN SYLLABICS E...CANADIAN SYLLABICS CARRIER TTSA
                | '\u{166F}'...'\u{167F}'    // CANADIAN SYLLABICS QAI...CANADIAN SYLLABICS BLACKFOOT W
                | '\u{1681}'...'\u{169A}'    // OGHAM LETTER BEITH...OGHAM LETTER PEITH
                | '\u{16A0}'...'\u{16EA}'    // RUNIC LETTER FEHU FEOH FE F...RUNIC LETTER X
                | '\u{16EE}'...'\u{16F0}'    // RUNIC ARLAUG SYMBOL...RUNIC BELGTHOR SYMBOL
                | '\u{16F1}'...'\u{16F8}'    // RUNIC LETTER K...RUNIC LETTER FRANKS CASKET AESC
                | '\u{1700}'...'\u{170C}'    // TAGALOG LETTER A...TAGALOG LETTER YA
                | '\u{170E}'...'\u{1711}'    // TAGALOG LETTER LA...TAGALOG LETTER HA
                | '\u{1712}'...'\u{1714}'    // TAGALOG VOWEL SIGN I...TAGALOG SIGN VIRAMA
                | '\u{1720}'...'\u{1731}'    // HANUNOO LETTER A...HANUNOO LETTER HA
                | '\u{1732}'...'\u{1734}'    // HANUNOO VOWEL SIGN I...HANUNOO SIGN PAMUDPOD
                | '\u{1740}'...'\u{1751}'    // BUHID LETTER A...BUHID LETTER HA
                | '\u{1752}'...'\u{1753}'    // BUHID VOWEL SIGN I...BUHID VOWEL SIGN U
                | '\u{1760}'...'\u{176C}'    // TAGBANWA LETTER A...TAGBANWA LETTER YA
                | '\u{176E}'...'\u{1770}'    // TAGBANWA LETTER LA...TAGBANWA LETTER SA
                | '\u{1772}'...'\u{1773}'    // TAGBANWA VOWEL SIGN I...TAGBANWA VOWEL SIGN U
                | '\u{1780}'...'\u{17B3}'    // KHMER LETTER KA...KHMER INDEPENDENT VOWEL QAU
                | '\u{17B4}'...'\u{17B5}'    // KHMER VOWEL INHERENT AQ...KHMER VOWEL INHERENT AA
                | '\u{17B6}'                 // KHMER VOWEL SIGN AA
                | '\u{17B7}'...'\u{17BD}'    // KHMER VOWEL SIGN I...KHMER VOWEL SIGN UA
                | '\u{17BE}'...'\u{17C5}'    // KHMER VOWEL SIGN OE...KHMER VOWEL SIGN AU
                | '\u{17C6}'                 // KHMER SIGN NIKAHIT
                | '\u{17C7}'...'\u{17C8}'    // KHMER SIGN REAHMUK...KHMER SIGN YUUKALEAPINTU
                | '\u{17C9}'...'\u{17D3}'    // KHMER SIGN MUUSIKATOAN...KHMER SIGN BATHAMASAT
                | '\u{17D7}'                 // KHMER SIGN LEK TOO
                | '\u{17DC}'                 // KHMER SIGN AVAKRAHASANYA
                | '\u{17DD}'                 // KHMER SIGN ATTHACAN
                | '\u{17E0}'...'\u{17E9}'    // KHMER DIGIT ZERO...KHMER DIGIT NINE
                | '\u{180B}'...'\u{180D}'    // MONGOLIAN FREE VARIATION SELECTOR ONE...MONGOLIAN FREE VARIATION SELECTOR THREE
                | '\u{1810}'...'\u{1819}'    // MONGOLIAN DIGIT ZERO...MONGOLIAN DIGIT NINE
                | '\u{1820}'...'\u{1842}'    // MONGOLIAN LETTER A...MONGOLIAN LETTER CHI
                | '\u{1843}'                 // MONGOLIAN LETTER TODO LONG VOWEL SIGN
                | '\u{1844}'...'\u{1877}'    // MONGOLIAN LETTER TODO E...MONGOLIAN LETTER MANCHU ZHA
                | '\u{1880}'...'\u{1884}'    // MONGOLIAN LETTER ALI GALI ANUSVARA ONE...MONGOLIAN LETTER ALI GALI INVERTED UBADAMA
                | '\u{1885}'...'\u{1886}'    // MONGOLIAN LETTER ALI GALI BALUDA...MONGOLIAN LETTER ALI GALI THREE BALUDA
                | '\u{1887}'...'\u{18A8}'    // MONGOLIAN LETTER ALI GALI A...MONGOLIAN LETTER MANCHU ALI GALI BHA
                | '\u{18A9}'                 // MONGOLIAN LETTER ALI GALI DAGALGA
                | '\u{18AA}'                 // MONGOLIAN LETTER MANCHU ALI GALI LHA
                | '\u{18B0}'...'\u{18F5}'    // CANADIAN SYLLABICS OY...CANADIAN SYLLABICS CARRIER DENTAL S
                | '\u{1900}'...'\u{191E}'    // LIMBU VOWEL-CARRIER LETTER...LIMBU LETTER TRA
                | '\u{1920}'...'\u{1922}'    // LIMBU VOWEL SIGN A...LIMBU VOWEL SIGN U
                | '\u{1923}'...'\u{1926}'    // LIMBU VOWEL SIGN EE...LIMBU VOWEL SIGN AU
                | '\u{1927}'...'\u{1928}'    // LIMBU VOWEL SIGN E...LIMBU VOWEL SIGN O
                | '\u{1929}'...'\u{192B}'    // LIMBU SUBJOINED LETTER YA...LIMBU SUBJOINED LETTER WA
                | '\u{1930}'...'\u{1931}'    // LIMBU SMALL LETTER KA...LIMBU SMALL LETTER NGA
                | '\u{1932}'                 // LIMBU SMALL LETTER ANUSVARA
                | '\u{1933}'...'\u{1938}'    // LIMBU SMALL LETTER TA...LIMBU SMALL LETTER LA
                | '\u{1939}'...'\u{193B}'    // LIMBU SIGN MUKPHRENG...LIMBU SIGN SA-I
                | '\u{1946}'...'\u{194F}'    // LIMBU DIGIT ZERO...LIMBU DIGIT NINE
                | '\u{1950}'...'\u{196D}'    // TAI LE LETTER KA...TAI LE LETTER AI
                | '\u{1970}'...'\u{1974}'    // TAI LE LETTER TONE-2...TAI LE LETTER TONE-6
                | '\u{1980}'...'\u{19AB}'    // NEW TAI LUE LETTER HIGH QA...NEW TAI LUE LETTER LOW SUA
                | '\u{19B0}'...'\u{19C9}'    // NEW TAI LUE VOWEL SIGN VOWEL SHORTENER...NEW TAI LUE TONE MARK-2
                | '\u{19D0}'...'\u{19D9}'    // NEW TAI LUE DIGIT ZERO...NEW TAI LUE DIGIT NINE
                | '\u{19DA}'                 // NEW TAI LUE THAM DIGIT ONE
                | '\u{1A00}'...'\u{1A16}'    // BUGINESE LETTER KA...BUGINESE LETTER HA
                | '\u{1A17}'...'\u{1A18}'    // BUGINESE VOWEL SIGN I...BUGINESE VOWEL SIGN U
                | '\u{1A19}'...'\u{1A1A}'    // BUGINESE VOWEL SIGN E...BUGINESE VOWEL SIGN O
                | '\u{1A1B}'                 // BUGINESE VOWEL SIGN AE
                | '\u{1A20}'...'\u{1A54}'    // TAI THAM LETTER HIGH KA...TAI THAM LETTER GREAT SA
                | '\u{1A55}'                 // TAI THAM CONSONANT SIGN MEDIAL RA
                | '\u{1A56}'                 // TAI THAM CONSONANT SIGN MEDIAL LA
                | '\u{1A57}'                 // TAI THAM CONSONANT SIGN LA TANG LAI
                | '\u{1A58}'...'\u{1A5E}'    // TAI THAM SIGN MAI KANG LAI...TAI THAM CONSONANT SIGN SA
                | '\u{1A60}'                 // TAI THAM SIGN SAKOT
                | '\u{1A61}'                 // TAI THAM VOWEL SIGN A
                | '\u{1A62}'                 // TAI THAM VOWEL SIGN MAI SAT
                | '\u{1A63}'...'\u{1A64}'    // TAI THAM VOWEL SIGN AA...TAI THAM VOWEL SIGN TALL AA
                | '\u{1A65}'...'\u{1A6C}'    // TAI THAM VOWEL SIGN I...TAI THAM VOWEL SIGN OA BELOW
                | '\u{1A6D}'...'\u{1A72}'    // TAI THAM VOWEL SIGN OY...TAI THAM VOWEL SIGN THAM AI
                | '\u{1A73}'...'\u{1A7C}'    // TAI THAM VOWEL SIGN OA ABOVE...TAI THAM SIGN KHUEN-LUE KARAN
                | '\u{1A7F}'                 // TAI THAM COMBINING CRYPTOGRAMMIC DOT
                | '\u{1A80}'...'\u{1A89}'    // TAI THAM HORA DIGIT ZERO...TAI THAM HORA DIGIT NINE
                | '\u{1A90}'...'\u{1A99}'    // TAI THAM THAM DIGIT ZERO...TAI THAM THAM DIGIT NINE
                | '\u{1AA7}'                 // TAI THAM SIGN MAI YAMOK
                | '\u{1AB0}'...'\u{1ABD}'    // COMBINING DOUBLED CIRCUMFLEX ACCENT...COMBINING PARENTHESES BELOW
                | '\u{1B00}'...'\u{1B03}'    // BALINESE SIGN ULU RICEM...BALINESE SIGN SURANG
                | '\u{1B04}'                 // BALINESE SIGN BISAH
                | '\u{1B05}'...'\u{1B33}'    // BALINESE LETTER AKARA...BALINESE LETTER HA
                | '\u{1B34}'                 // BALINESE SIGN REREKAN
                | '\u{1B35}'                 // BALINESE VOWEL SIGN TEDUNG
                | '\u{1B36}'...'\u{1B3A}'    // BALINESE VOWEL SIGN ULU...BALINESE VOWEL SIGN RA REPA
                | '\u{1B3B}'                 // BALINESE VOWEL SIGN RA REPA TEDUNG
                | '\u{1B3C}'                 // BALINESE VOWEL SIGN LA LENGA
                | '\u{1B3D}'...'\u{1B41}'    // BALINESE VOWEL SIGN LA LENGA TEDUNG...BALINESE VOWEL SIGN TALING REPA TEDUNG
                | '\u{1B42}'                 // BALINESE VOWEL SIGN PEPET
                | '\u{1B43}'...'\u{1B44}'    // BALINESE VOWEL SIGN PEPET TEDUNG...BALINESE ADEG ADEG
                | '\u{1B45}'...'\u{1B4B}'    // BALINESE LETTER KAF SASAK...BALINESE LETTER ASYURA SASAK
                | '\u{1B50}'...'\u{1B59}'    // BALINESE DIGIT ZERO...BALINESE DIGIT NINE
                | '\u{1B6B}'...'\u{1B73}'    // BALINESE MUSICAL SYMBOL COMBINING TEGEH...BALINESE MUSICAL SYMBOL COMBINING GONG
                | '\u{1B80}'...'\u{1B81}'    // SUNDANESE SIGN PANYECEK...SUNDANESE SIGN PANGLAYAR
                | '\u{1B82}'                 // SUNDANESE SIGN PANGWISAD
                | '\u{1B83}'...'\u{1BA0}'    // SUNDANESE LETTER A...SUNDANESE LETTER HA
                | '\u{1BA1}'                 // SUNDANESE CONSONANT SIGN PAMINGKAL
                | '\u{1BA2}'...'\u{1BA5}'    // SUNDANESE CONSONANT SIGN PANYAKRA...SUNDANESE VOWEL SIGN PANYUKU
                | '\u{1BA6}'...'\u{1BA7}'    // SUNDANESE VOWEL SIGN PANAELAENG...SUNDANESE VOWEL SIGN PANOLONG
                | '\u{1BA8}'...'\u{1BA9}'    // SUNDANESE VOWEL SIGN PAMEPET...SUNDANESE VOWEL SIGN PANEULEUNG
                | '\u{1BAA}'                 // SUNDANESE SIGN PAMAAEH
                | '\u{1BAB}'...'\u{1BAD}'    // SUNDANESE SIGN VIRAMA...SUNDANESE CONSONANT SIGN PASANGAN WA
                | '\u{1BAE}'...'\u{1BAF}'    // SUNDANESE LETTER KHA...SUNDANESE LETTER SYA
                | '\u{1BB0}'...'\u{1BB9}'    // SUNDANESE DIGIT ZERO...SUNDANESE DIGIT NINE
                | '\u{1BBA}'...'\u{1BE5}'    // SUNDANESE AVAGRAHA...BATAK LETTER U
                | '\u{1BE6}'                 // BATAK SIGN TOMPI
                | '\u{1BE7}'                 // BATAK VOWEL SIGN E
                | '\u{1BE8}'...'\u{1BE9}'    // BATAK VOWEL SIGN PAKPAK E...BATAK VOWEL SIGN EE
                | '\u{1BEA}'...'\u{1BEC}'    // BATAK VOWEL SIGN I...BATAK VOWEL SIGN O
                | '\u{1BED}'                 // BATAK VOWEL SIGN KARO O
                | '\u{1BEE}'                 // BATAK VOWEL SIGN U
                | '\u{1BEF}'...'\u{1BF1}'    // BATAK VOWEL SIGN U FOR SIMALUNGUN SA...BATAK CONSONANT SIGN H
                | '\u{1BF2}'...'\u{1BF3}'    // BATAK PANGOLAT...BATAK PANONGONAN
                | '\u{1C00}'...'\u{1C23}'    // LEPCHA LETTER KA...LEPCHA LETTER A
                | '\u{1C24}'...'\u{1C2B}'    // LEPCHA SUBJOINED LETTER YA...LEPCHA VOWEL SIGN UU
                | '\u{1C2C}'...'\u{1C33}'    // LEPCHA VOWEL SIGN E...LEPCHA CONSONANT SIGN T
                | '\u{1C34}'...'\u{1C35}'    // LEPCHA CONSONANT SIGN NYIN-DO...LEPCHA CONSONANT SIGN KANG
                | '\u{1C36}'...'\u{1C37}'    // LEPCHA SIGN RAN...LEPCHA SIGN NUKTA
                | '\u{1C40}'...'\u{1C49}'    // LEPCHA DIGIT ZERO...LEPCHA DIGIT NINE
                | '\u{1C4D}'...'\u{1C4F}'    // LEPCHA LETTER TTA...LEPCHA LETTER DDA
                | '\u{1C50}'...'\u{1C59}'    // OL CHIKI DIGIT ZERO...OL CHIKI DIGIT NINE
                | '\u{1C5A}'...'\u{1C77}'    // OL CHIKI LETTER LA...OL CHIKI LETTER OH
                | '\u{1C78}'...'\u{1C7D}'    // OL CHIKI MU TTUDDAG...OL CHIKI AHAD
                | '\u{1C80}'...'\u{1C88}'    // CYRILLIC SMALL LETTER ROUNDED VE...CYRILLIC SMALL LETTER UNBLENDED UK
                | '\u{1CD0}'...'\u{1CD2}'    // VEDIC TONE KARSHANA...VEDIC TONE PRENKHA
                | '\u{1CD4}'...'\u{1CE0}'    // VEDIC SIGN YAJURVEDIC MIDLINE SVARITA...VEDIC TONE RIGVEDIC KASHMIRI INDEPENDENT SVARITA
                | '\u{1CE1}'                 // VEDIC TONE ATHARVAVEDIC INDEPENDENT SVARITA
                | '\u{1CE2}'...'\u{1CE8}'    // VEDIC SIGN VISARGA SVARITA...VEDIC SIGN VISARGA ANUDATTA WITH TAIL
                | '\u{1CE9}'...'\u{1CEC}'    // VEDIC SIGN ANUSVARA ANTARGOMUKHA...VEDIC SIGN ANUSVARA VAMAGOMUKHA WITH TAIL
                | '\u{1CED}'                 // VEDIC SIGN TIRYAK
                | '\u{1CEE}'...'\u{1CF1}'    // VEDIC SIGN HEXIFORM LONG ANUSVARA...VEDIC SIGN ANUSVARA UBHAYATO MUKHA
                | '\u{1CF2}'...'\u{1CF3}'    // VEDIC SIGN ARDHAVISARGA...VEDIC SIGN ROTATED ARDHAVISARGA
                | '\u{1CF4}'                 // VEDIC TONE CANDRA ABOVE
                | '\u{1CF5}'...'\u{1CF6}'    // VEDIC SIGN JIHVAMULIYA...VEDIC SIGN UPADHMANIYA
                | '\u{1CF7}'                 // VEDIC SIGN ATIKRAMA
                | '\u{1CF8}'...'\u{1CF9}'    // VEDIC TONE RING ABOVE...VEDIC TONE DOUBLE RING ABOVE
                | '\u{1D00}'...'\u{1D2B}'    // LATIN LETTER SMALL CAPITAL A...CYRILLIC LETTER SMALL CAPITAL EL
                | '\u{1D2C}'...'\u{1D6A}'    // MODIFIER LETTER CAPITAL A...GREEK SUBSCRIPT SMALL LETTER CHI
                | '\u{1D6B}'...'\u{1D77}'    // LATIN SMALL LETTER UE...LATIN SMALL LETTER TURNED G
                | '\u{1D78}'                 // MODIFIER LETTER CYRILLIC EN
                | '\u{1D79}'...'\u{1D9A}'    // LATIN SMALL LETTER INSULAR G...LATIN SMALL LETTER EZH WITH RETROFLEX HOOK
                | '\u{1D9B}'...'\u{1DBF}'    // MODIFIER LETTER SMALL TURNED ALPHA...MODIFIER LETTER SMALL THETA
                | '\u{1DC0}'...'\u{1DF9}'    // COMBINING DOTTED GRAVE ACCENT...COMBINING WIDE INVERTED BRIDGE BELOW
                | '\u{1DFB}'...'\u{1DFF}'    // COMBINING DELETION MARK...COMBINING RIGHT ARROWHEAD AND DOWN ARROWHEAD BELOW
                | '\u{1E00}'...'\u{1F15}'    // LATIN CAPITAL LETTER A WITH RING BELOW...GREEK SMALL LETTER EPSILON WITH DASIA AND OXIA
                | '\u{1F18}'...'\u{1F1D}'    // GREEK CAPITAL LETTER EPSILON WITH PSILI...GREEK CAPITAL LETTER EPSILON WITH DASIA AND OXIA
                | '\u{1F20}'...'\u{1F45}'    // GREEK SMALL LETTER ETA WITH PSILI...GREEK SMALL LETTER OMICRON WITH DASIA AND OXIA
                | '\u{1F48}'...'\u{1F4D}'    // GREEK CAPITAL LETTER OMICRON WITH PSILI...GREEK CAPITAL LETTER OMICRON WITH DASIA AND OXIA
                | '\u{1F50}'...'\u{1F57}'    // GREEK SMALL LETTER UPSILON WITH PSILI...GREEK SMALL LETTER UPSILON WITH DASIA AND PERISPOMENI
                | '\u{1F59}'                 // GREEK CAPITAL LETTER UPSILON WITH DASIA
                | '\u{1F5B}'                 // GREEK CAPITAL LETTER UPSILON WITH DASIA AND VARIA
                | '\u{1F5D}'                 // GREEK CAPITAL LETTER UPSILON WITH DASIA AND OXIA
                | '\u{1F5F}'...'\u{1F7D}'    // GREEK CAPITAL LETTER UPSILON WITH DASIA AND PERISPOMENI...GREEK SMALL LETTER OMEGA WITH OXIA
                | '\u{1F80}'...'\u{1FB4}'    // GREEK SMALL LETTER ALPHA WITH PSILI AND YPOGEGRAMMENI...GREEK SMALL LETTER ALPHA WITH OXIA AND YPOGEGRAMMENI
                | '\u{1FB6}'...'\u{1FBC}'    // GREEK SMALL LETTER ALPHA WITH PERISPOMENI...GREEK CAPITAL LETTER ALPHA WITH PROSGEGRAMMENI
                | '\u{1FBE}'                 // GREEK PROSGEGRAMMENI
                | '\u{1FC2}'...'\u{1FC4}'    // GREEK SMALL LETTER ETA WITH VARIA AND YPOGEGRAMMENI...GREEK SMALL LETTER ETA WITH OXIA AND YPOGEGRAMMENI
                | '\u{1FC6}'...'\u{1FCC}'    // GREEK SMALL LETTER ETA WITH PERISPOMENI...GREEK CAPITAL LETTER ETA WITH PROSGEGRAMMENI
                | '\u{1FD0}'...'\u{1FD3}'    // GREEK SMALL LETTER IOTA WITH VRACHY...GREEK SMALL LETTER IOTA WITH DIALYTIKA AND OXIA
                | '\u{1FD6}'...'\u{1FDB}'    // GREEK SMALL LETTER IOTA WITH PERISPOMENI...GREEK CAPITAL LETTER IOTA WITH OXIA
                | '\u{1FE0}'...'\u{1FEC}'    // GREEK SMALL LETTER UPSILON WITH VRACHY...GREEK CAPITAL LETTER RHO WITH DASIA
                | '\u{1FF2}'...'\u{1FF4}'    // GREEK SMALL LETTER OMEGA WITH VARIA AND YPOGEGRAMMENI...GREEK SMALL LETTER OMEGA WITH OXIA AND YPOGEGRAMMENI
                | '\u{1FF6}'...'\u{1FFC}'    // GREEK SMALL LETTER OMEGA WITH PERISPOMENI...GREEK CAPITAL LETTER OMEGA WITH PROSGEGRAMMENI
                | '\u{203F}'...'\u{2040}'    // UNDERTIE...CHARACTER TIE
                | '\u{2054}'                 // INVERTED UNDERTIE
                | '\u{2071}'                 // SUPERSCRIPT LATIN SMALL LETTER I
                | '\u{207F}'                 // SUPERSCRIPT LATIN SMALL LETTER N
                | '\u{2090}'...'\u{209C}'    // LATIN SUBSCRIPT SMALL LETTER A...LATIN SUBSCRIPT SMALL LETTER T
                | '\u{20D0}'...'\u{20DC}'    // COMBINING LEFT HARPOON ABOVE...COMBINING FOUR DOTS ABOVE
                | '\u{20E1}'                 // COMBINING LEFT RIGHT ARROW ABOVE
                | '\u{20E5}'...'\u{20F0}'    // COMBINING REVERSE SOLIDUS OVERLAY...COMBINING ASTERISK ABOVE
                | '\u{2102}'                 // DOUBLE-STRUCK CAPITAL C
                | '\u{2107}'                 // EULER CONSTANT
                | '\u{210A}'...'\u{2113}'    // SCRIPT SMALL G...SCRIPT SMALL L
                | '\u{2115}'                 // DOUBLE-STRUCK CAPITAL N
                | '\u{2118}'                 // SCRIPT CAPITAL P
                | '\u{2119}'...'\u{211D}'    // DOUBLE-STRUCK CAPITAL P...DOUBLE-STRUCK CAPITAL R
                | '\u{2124}'                 // DOUBLE-STRUCK CAPITAL Z
                | '\u{2126}'                 // OHM SIGN
                | '\u{2128}'                 // BLACK-LETTER CAPITAL Z
                | '\u{212A}'...'\u{212D}'    // KELVIN SIGN...BLACK-LETTER CAPITAL C
                | '\u{212E}'                 // ESTIMATED SYMBOL
                | '\u{212F}'...'\u{2134}'    // SCRIPT SMALL E...SCRIPT SMALL O
                | '\u{2135}'...'\u{2138}'    // ALEF SYMBOL...DALET SYMBOL
                | '\u{2139}'                 // INFORMATION SOURCE
                | '\u{213C}'...'\u{213F}'    // DOUBLE-STRUCK SMALL PI...DOUBLE-STRUCK CAPITAL PI
                | '\u{2145}'...'\u{2149}'    // DOUBLE-STRUCK ITALIC CAPITAL D...DOUBLE-STRUCK ITALIC SMALL J
                | '\u{214E}'                 // TURNED SMALL F
                | '\u{2160}'...'\u{2182}'    // ROMAN NUMERAL ONE...ROMAN NUMERAL TEN THOUSAND
                | '\u{2183}'...'\u{2184}'    // ROMAN NUMERAL REVERSED ONE HUNDRED...LATIN SMALL LETTER REVERSED C
                | '\u{2185}'...'\u{2188}'    // ROMAN NUMERAL SIX LATE FORM...ROMAN NUMERAL ONE HUNDRED THOUSAND
                | '\u{2C00}'...'\u{2C2E}'    // GLAGOLITIC CAPITAL LETTER AZU...GLAGOLITIC CAPITAL LETTER LATINATE MYSLITE
                | '\u{2C30}'...'\u{2C5E}'    // GLAGOLITIC SMALL LETTER AZU...GLAGOLITIC SMALL LETTER LATINATE MYSLITE
                | '\u{2C60}'...'\u{2C7B}'    // LATIN CAPITAL LETTER L WITH DOUBLE BAR...LATIN LETTER SMALL CAPITAL TURNED E
                | '\u{2C7C}'...'\u{2C7D}'    // LATIN SUBSCRIPT SMALL LETTER J...MODIFIER LETTER CAPITAL V
                | '\u{2C7E}'...'\u{2CE4}'    // LATIN CAPITAL LETTER S WITH SWASH TAIL...COPTIC SYMBOL KAI
                | '\u{2CEB}'...'\u{2CEE}'    // COPTIC CAPITAL LETTER CRYPTOGRAMMIC SHEI...COPTIC SMALL LETTER CRYPTOGRAMMIC GANGIA
                | '\u{2CEF}'...'\u{2CF1}'    // COPTIC COMBINING NI ABOVE...COPTIC COMBINING SPIRITUS LENIS
                | '\u{2CF2}'...'\u{2CF3}'    // COPTIC CAPITAL LETTER BOHAIRIC KHEI...COPTIC SMALL LETTER BOHAIRIC KHEI
                | '\u{2D00}'...'\u{2D25}'    // GEORGIAN SMALL LETTER AN...GEORGIAN SMALL LETTER HOE
                | '\u{2D27}'                 // GEORGIAN SMALL LETTER YN
                | '\u{2D2D}'                 // GEORGIAN SMALL LETTER AEN
                | '\u{2D30}'...'\u{2D67}'    // TIFINAGH LETTER YA...TIFINAGH LETTER YO
                | '\u{2D6F}'                 // TIFINAGH MODIFIER LETTER LABIALIZATION MARK
                | '\u{2D7F}'                 // TIFINAGH CONSONANT JOINER
                | '\u{2D80}'...'\u{2D96}'    // ETHIOPIC SYLLABLE LOA...ETHIOPIC SYLLABLE GGWE
                | '\u{2DA0}'...'\u{2DA6}'    // ETHIOPIC SYLLABLE SSA...ETHIOPIC SYLLABLE SSO
                | '\u{2DA8}'...'\u{2DAE}'    // ETHIOPIC SYLLABLE CCA...ETHIOPIC SYLLABLE CCO
                | '\u{2DB0}'...'\u{2DB6}'    // ETHIOPIC SYLLABLE ZZA...ETHIOPIC SYLLABLE ZZO
                | '\u{2DB8}'...'\u{2DBE}'    // ETHIOPIC SYLLABLE CCHA...ETHIOPIC SYLLABLE CCHO
                | '\u{2DC0}'...'\u{2DC6}'    // ETHIOPIC SYLLABLE QYA...ETHIOPIC SYLLABLE QYO
                | '\u{2DC8}'...'\u{2DCE}'    // ETHIOPIC SYLLABLE KYA...ETHIOPIC SYLLABLE KYO
                | '\u{2DD0}'...'\u{2DD6}'    // ETHIOPIC SYLLABLE XYA...ETHIOPIC SYLLABLE XYO
                | '\u{2DD8}'...'\u{2DDE}'    // ETHIOPIC SYLLABLE GYA...ETHIOPIC SYLLABLE GYO
                | '\u{2DE0}'...'\u{2DFF}'    // COMBINING CYRILLIC LETTER BE...COMBINING CYRILLIC LETTER IOTIFIED BIG YUS
                | '\u{3005}'                 // IDEOGRAPHIC ITERATION MARK
                | '\u{3006}'                 // IDEOGRAPHIC CLOSING MARK
                | '\u{3007}'                 // IDEOGRAPHIC NUMBER ZERO
                | '\u{3021}'...'\u{3029}'    // HANGZHOU NUMERAL ONE...HANGZHOU NUMERAL NINE
                | '\u{302A}'...'\u{302D}'    // IDEOGRAPHIC LEVEL TONE MARK...IDEOGRAPHIC ENTERING TONE MARK
                | '\u{302E}'...'\u{302F}'    // HANGUL SINGLE DOT TONE MARK...HANGUL DOUBLE DOT TONE MARK
                | '\u{3031}'...'\u{3035}'    // VERTICAL KANA REPEAT MARK...VERTICAL KANA REPEAT MARK LOWER HALF
                | '\u{3038}'...'\u{303A}'    // HANGZHOU NUMERAL TEN...HANGZHOU NUMERAL THIRTY
                | '\u{303B}'                 // VERTICAL IDEOGRAPHIC ITERATION MARK
                | '\u{303C}'                 // MASU MARK
                | '\u{3041}'...'\u{3096}'    // HIRAGANA LETTER SMALL A...HIRAGANA LETTER SMALL KE
                | '\u{3099}'...'\u{309A}'    // COMBINING KATAKANA-HIRAGANA VOICED SOUND MARK...COMBINING KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK
                | '\u{309B}'...'\u{309C}'    // KATAKANA-HIRAGANA VOICED SOUND MARK...KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK
                | '\u{309D}'...'\u{309E}'    // HIRAGANA ITERATION MARK...HIRAGANA VOICED ITERATION MARK
                | '\u{309F}'                 // HIRAGANA DIGRAPH YORI
                | '\u{30A1}'...'\u{30FA}'    // KATAKANA LETTER SMALL A...KATAKANA LETTER VO
                | '\u{30FC}'...'\u{30FE}'    // KATAKANA-HIRAGANA PROLONGED SOUND MARK...KATAKANA VOICED ITERATION MARK
                | '\u{30FF}'                 // KATAKANA DIGRAPH KOTO
                | '\u{3105}'...'\u{312E}'    // BOPOMOFO LETTER B...BOPOMOFO LETTER O WITH DOT ABOVE
                | '\u{3131}'...'\u{318E}'    // HANGUL LETTER KIYEOK...HANGUL LETTER ARAEAE
                | '\u{31A0}'...'\u{31BA}'    // BOPOMOFO LETTER BU...BOPOMOFO LETTER ZY
                | '\u{31F0}'...'\u{31FF}'    // KATAKANA LETTER SMALL KU...KATAKANA LETTER SMALL RO
                | '\u{3400}'...'\u{4DB5}'    // CJK UNIFIED IDEOGRAPH-3400...CJK UNIFIED IDEOGRAPH-4DB5
                | '\u{4E00}'...'\u{9FEA}'    // CJK UNIFIED IDEOGRAPH-4E00...CJK UNIFIED IDEOGRAPH-9FEA
                | '\u{A000}'...'\u{A014}'    // YI SYLLABLE IT...YI SYLLABLE E
                | '\u{A015}'                 // YI SYLLABLE WU
                | '\u{A016}'...'\u{A48C}'    // YI SYLLABLE BIT...YI SYLLABLE YYR
                | '\u{A4D0}'...'\u{A4F7}'    // LISU LETTER BA...LISU LETTER OE
                | '\u{A4F8}'...'\u{A4FD}'    // LISU LETTER TONE MYA TI...LISU LETTER TONE MYA JEU
                | '\u{A500}'...'\u{A60B}'    // VAI SYLLABLE EE...VAI SYLLABLE NG
                | '\u{A60C}'                 // VAI SYLLABLE LENGTHENER
                | '\u{A610}'...'\u{A61F}'    // VAI SYLLABLE NDOLE FA...VAI SYMBOL JONG
                | '\u{A620}'...'\u{A629}'    // VAI DIGIT ZERO...VAI DIGIT NINE
                | '\u{A62A}'...'\u{A62B}'    // VAI SYLLABLE NDOLE MA...VAI SYLLABLE NDOLE DO
                | '\u{A640}'...'\u{A66D}'    // CYRILLIC CAPITAL LETTER ZEMLYA...CYRILLIC SMALL LETTER DOUBLE MONOCULAR O
                | '\u{A66E}'                 // CYRILLIC LETTER MULTIOCULAR O
                | '\u{A66F}'                 // COMBINING CYRILLIC VZMET
                | '\u{A674}'...'\u{A67D}'    // COMBINING CYRILLIC LETTER UKRAINIAN IE...COMBINING CYRILLIC PAYEROK
                | '\u{A67F}'                 // CYRILLIC PAYEROK
                | '\u{A680}'...'\u{A69B}'    // CYRILLIC CAPITAL LETTER DWE...CYRILLIC SMALL LETTER CROSSED O
                | '\u{A69C}'...'\u{A69D}'    // MODIFIER LETTER CYRILLIC HARD SIGN...MODIFIER LETTER CYRILLIC SOFT SIGN
                | '\u{A69E}'...'\u{A69F}'    // COMBINING CYRILLIC LETTER EF...COMBINING CYRILLIC LETTER IOTIFIED E
                | '\u{A6A0}'...'\u{A6E5}'    // BAMUM LETTER A...BAMUM LETTER KI
                | '\u{A6E6}'...'\u{A6EF}'    // BAMUM LETTER MO...BAMUM LETTER KOGHOM
                | '\u{A6F0}'...'\u{A6F1}'    // BAMUM COMBINING MARK KOQNDON...BAMUM COMBINING MARK TUKWENTIS
                | '\u{A717}'...'\u{A71F}'    // MODIFIER LETTER DOT VERTICAL BAR...MODIFIER LETTER LOW INVERTED EXCLAMATION MARK
                | '\u{A722}'...'\u{A76F}'    // LATIN CAPITAL LETTER EGYPTOLOGICAL ALEF...LATIN SMALL LETTER CON
                | '\u{A770}'                 // MODIFIER LETTER US
                | '\u{A771}'...'\u{A787}'    // LATIN SMALL LETTER DUM...LATIN SMALL LETTER INSULAR T
                | '\u{A788}'                 // MODIFIER LETTER LOW CIRCUMFLEX ACCENT
                | '\u{A78B}'...'\u{A78E}'    // LATIN CAPITAL LETTER SALTILLO...LATIN SMALL LETTER L WITH RETROFLEX HOOK AND BELT
                | '\u{A78F}'                 // LATIN LETTER SINOLOGICAL DOT
                | '\u{A790}'...'\u{A7AE}'    // LATIN CAPITAL LETTER N WITH DESCENDER...LATIN CAPITAL LETTER SMALL CAPITAL I
                | '\u{A7B0}'...'\u{A7B7}'    // LATIN CAPITAL LETTER TURNED K...LATIN SMALL LETTER OMEGA
                | '\u{A7F7}'                 // LATIN EPIGRAPHIC LETTER SIDEWAYS I
                | '\u{A7F8}'...'\u{A7F9}'    // MODIFIER LETTER CAPITAL H WITH STROKE...MODIFIER LETTER SMALL LIGATURE OE
                | '\u{A7FA}'                 // LATIN LETTER SMALL CAPITAL TURNED M
                | '\u{A7FB}'...'\u{A801}'    // LATIN EPIGRAPHIC LETTER REVERSED F...SYLOTI NAGRI LETTER I
                | '\u{A802}'                 // SYLOTI NAGRI SIGN DVISVARA
                | '\u{A803}'...'\u{A805}'    // SYLOTI NAGRI LETTER U...SYLOTI NAGRI LETTER O
                | '\u{A806}'                 // SYLOTI NAGRI SIGN HASANTA
                | '\u{A807}'...'\u{A80A}'    // SYLOTI NAGRI LETTER KO...SYLOTI NAGRI LETTER GHO
                | '\u{A80B}'                 // SYLOTI NAGRI SIGN ANUSVARA
                | '\u{A80C}'...'\u{A822}'    // SYLOTI NAGRI LETTER CO...SYLOTI NAGRI LETTER HO
                | '\u{A823}'...'\u{A824}'    // SYLOTI NAGRI VOWEL SIGN A...SYLOTI NAGRI VOWEL SIGN I
                | '\u{A825}'...'\u{A826}'    // SYLOTI NAGRI VOWEL SIGN U...SYLOTI NAGRI VOWEL SIGN E
                | '\u{A827}'                 // SYLOTI NAGRI VOWEL SIGN OO
                | '\u{A840}'...'\u{A873}'    // PHAGS-PA LETTER KA...PHAGS-PA LETTER CANDRABINDU
                | '\u{A880}'...'\u{A881}'    // SAURASHTRA SIGN ANUSVARA...SAURASHTRA SIGN VISARGA
                | '\u{A882}'...'\u{A8B3}'    // SAURASHTRA LETTER A...SAURASHTRA LETTER LLA
                | '\u{A8B4}'...'\u{A8C3}'    // SAURASHTRA CONSONANT SIGN HAARU...SAURASHTRA VOWEL SIGN AU
                | '\u{A8C4}'...'\u{A8C5}'    // SAURASHTRA SIGN VIRAMA...SAURASHTRA SIGN CANDRABINDU
                | '\u{A8D0}'...'\u{A8D9}'    // SAURASHTRA DIGIT ZERO...SAURASHTRA DIGIT NINE
                | '\u{A8E0}'...'\u{A8F1}'    // COMBINING DEVANAGARI DIGIT ZERO...COMBINING DEVANAGARI SIGN AVAGRAHA
                | '\u{A8F2}'...'\u{A8F7}'    // DEVANAGARI SIGN SPACING CANDRABINDU...DEVANAGARI SIGN CANDRABINDU AVAGRAHA
                | '\u{A8FB}'                 // DEVANAGARI HEADSTROKE
                | '\u{A8FD}'                 // DEVANAGARI JAIN OM
                | '\u{A900}'...'\u{A909}'    // KAYAH LI DIGIT ZERO...KAYAH LI DIGIT NINE
                | '\u{A90A}'...'\u{A925}'    // KAYAH LI LETTER KA...KAYAH LI LETTER OO
                | '\u{A926}'...'\u{A92D}'    // KAYAH LI VOWEL UE...KAYAH LI TONE CALYA PLOPHU
                | '\u{A930}'...'\u{A946}'    // REJANG LETTER KA...REJANG LETTER A
                | '\u{A947}'...'\u{A951}'    // REJANG VOWEL SIGN I...REJANG CONSONANT SIGN R
                | '\u{A952}'...'\u{A953}'    // REJANG CONSONANT SIGN H...REJANG VIRAMA
                | '\u{A960}'...'\u{A97C}'    // HANGUL CHOSEONG TIKEUT-MIEUM...HANGUL CHOSEONG SSANGYEORINHIEUH
                | '\u{A980}'...'\u{A982}'    // JAVANESE SIGN PANYANGGA...JAVANESE SIGN LAYAR
                | '\u{A983}'                 // JAVANESE SIGN WIGNYAN
                | '\u{A984}'...'\u{A9B2}'    // JAVANESE LETTER A...JAVANESE LETTER HA
                | '\u{A9B3}'                 // JAVANESE SIGN CECAK TELU
                | '\u{A9B4}'...'\u{A9B5}'    // JAVANESE VOWEL SIGN TARUNG...JAVANESE VOWEL SIGN TOLONG
                | '\u{A9B6}'...'\u{A9B9}'    // JAVANESE VOWEL SIGN WULU...JAVANESE VOWEL SIGN SUKU MENDUT
                | '\u{A9BA}'...'\u{A9BB}'    // JAVANESE VOWEL SIGN TALING...JAVANESE VOWEL SIGN DIRGA MURE
                | '\u{A9BC}'                 // JAVANESE VOWEL SIGN PEPET
                | '\u{A9BD}'...'\u{A9C0}'    // JAVANESE CONSONANT SIGN KERET...JAVANESE PANGKON
                | '\u{A9CF}'                 // JAVANESE PANGRANGKEP
                | '\u{A9D0}'...'\u{A9D9}'    // JAVANESE DIGIT ZERO...JAVANESE DIGIT NINE
                | '\u{A9E0}'...'\u{A9E4}'    // MYANMAR LETTER SHAN GHA...MYANMAR LETTER SHAN BHA
                | '\u{A9E5}'                 // MYANMAR SIGN SHAN SAW
                | '\u{A9E6}'                 // MYANMAR MODIFIER LETTER SHAN REDUPLICATION
                | '\u{A9E7}'...'\u{A9EF}'    // MYANMAR LETTER TAI LAING NYA...MYANMAR LETTER TAI LAING NNA
                | '\u{A9F0}'...'\u{A9F9}'    // MYANMAR TAI LAING DIGIT ZERO...MYANMAR TAI LAING DIGIT NINE
                | '\u{A9FA}'...'\u{A9FE}'    // MYANMAR LETTER TAI LAING LLA...MYANMAR LETTER TAI LAING BHA
                | '\u{AA00}'...'\u{AA28}'    // CHAM LETTER A...CHAM LETTER HA
                | '\u{AA29}'...'\u{AA2E}'    // CHAM VOWEL SIGN AA...CHAM VOWEL SIGN OE
                | '\u{AA2F}'...'\u{AA30}'    // CHAM VOWEL SIGN O...CHAM VOWEL SIGN AI
                | '\u{AA31}'...'\u{AA32}'    // CHAM VOWEL SIGN AU...CHAM VOWEL SIGN UE
                | '\u{AA33}'...'\u{AA34}'    // CHAM CONSONANT SIGN YA...CHAM CONSONANT SIGN RA
                | '\u{AA35}'...'\u{AA36}'    // CHAM CONSONANT SIGN LA...CHAM CONSONANT SIGN WA
                | '\u{AA40}'...'\u{AA42}'    // CHAM LETTER FINAL K...CHAM LETTER FINAL NG
                | '\u{AA43}'                 // CHAM CONSONANT SIGN FINAL NG
                | '\u{AA44}'...'\u{AA4B}'    // CHAM LETTER FINAL CH...CHAM LETTER FINAL SS
                | '\u{AA4C}'                 // CHAM CONSONANT SIGN FINAL M
                | '\u{AA4D}'                 // CHAM CONSONANT SIGN FINAL H
                | '\u{AA50}'...'\u{AA59}'    // CHAM DIGIT ZERO...CHAM DIGIT NINE
                | '\u{AA60}'...'\u{AA6F}'    // MYANMAR LETTER KHAMTI GA...MYANMAR LETTER KHAMTI FA
                | '\u{AA70}'                 // MYANMAR MODIFIER LETTER KHAMTI REDUPLICATION
                | '\u{AA71}'...'\u{AA76}'    // MYANMAR LETTER KHAMTI XA...MYANMAR LOGOGRAM KHAMTI HM
                | '\u{AA7A}'                 // MYANMAR LETTER AITON RA
                | '\u{AA7B}'                 // MYANMAR SIGN PAO KAREN TONE
                | '\u{AA7C}'                 // MYANMAR SIGN TAI LAING TONE-2
                | '\u{AA7D}'                 // MYANMAR SIGN TAI LAING TONE-5
                | '\u{AA7E}'...'\u{AAAF}'    // MYANMAR LETTER SHWE PALAUNG CHA...TAI VIET LETTER HIGH O
                | '\u{AAB0}'                 // TAI VIET MAI KANG
                | '\u{AAB1}'                 // TAI VIET VOWEL AA
                | '\u{AAB2}'...'\u{AAB4}'    // TAI VIET VOWEL I...TAI VIET VOWEL U
                | '\u{AAB5}'...'\u{AAB6}'    // TAI VIET VOWEL E...TAI VIET VOWEL O
                | '\u{AAB7}'...'\u{AAB8}'    // TAI VIET MAI KHIT...TAI VIET VOWEL IA
                | '\u{AAB9}'...'\u{AABD}'    // TAI VIET VOWEL UEA...TAI VIET VOWEL AN
                | '\u{AABE}'...'\u{AABF}'    // TAI VIET VOWEL AM...TAI VIET TONE MAI EK
                | '\u{AAC0}'                 // TAI VIET TONE MAI NUENG
                | '\u{AAC1}'                 // TAI VIET TONE MAI THO
                | '\u{AAC2}'                 // TAI VIET TONE MAI SONG
                | '\u{AADB}'...'\u{AADC}'    // TAI VIET SYMBOL KON...TAI VIET SYMBOL NUENG
                | '\u{AADD}'                 // TAI VIET SYMBOL SAM
                | '\u{AAE0}'...'\u{AAEA}'    // MEETEI MAYEK LETTER E...MEETEI MAYEK LETTER SSA
                | '\u{AAEB}'                 // MEETEI MAYEK VOWEL SIGN II
                | '\u{AAEC}'...'\u{AAED}'    // MEETEI MAYEK VOWEL SIGN UU...MEETEI MAYEK VOWEL SIGN AAI
                | '\u{AAEE}'...'\u{AAEF}'    // MEETEI MAYEK VOWEL SIGN AU...MEETEI MAYEK VOWEL SIGN AAU
                | '\u{AAF2}'                 // MEETEI MAYEK ANJI
                | '\u{AAF3}'...'\u{AAF4}'    // MEETEI MAYEK SYLLABLE REPETITION MARK...MEETEI MAYEK WORD REPETITION MARK
                | '\u{AAF5}'                 // MEETEI MAYEK VOWEL SIGN VISARGA
                | '\u{AAF6}'                 // MEETEI MAYEK VIRAMA
                | '\u{AB01}'...'\u{AB06}'    // ETHIOPIC SYLLABLE TTHU...ETHIOPIC SYLLABLE TTHO
                | '\u{AB09}'...'\u{AB0E}'    // ETHIOPIC SYLLABLE DDHU...ETHIOPIC SYLLABLE DDHO
                | '\u{AB11}'...'\u{AB16}'    // ETHIOPIC SYLLABLE DZU...ETHIOPIC SYLLABLE DZO
                | '\u{AB20}'...'\u{AB26}'    // ETHIOPIC SYLLABLE CCHHA...ETHIOPIC SYLLABLE CCHHO
                | '\u{AB28}'...'\u{AB2E}'    // ETHIOPIC SYLLABLE BBA...ETHIOPIC SYLLABLE BBO
                | '\u{AB30}'...'\u{AB5A}'    // LATIN SMALL LETTER BARRED ALPHA...LATIN SMALL LETTER Y WITH SHORT RIGHT LEG
                | '\u{AB5C}'...'\u{AB5F}'    // MODIFIER LETTER SMALL HENG...MODIFIER LETTER SMALL U WITH LEFT HOOK
                | '\u{AB60}'...'\u{AB65}'    // LATIN SMALL LETTER SAKHA YAT...GREEK LETTER SMALL CAPITAL OMEGA
                | '\u{AB70}'...'\u{ABBF}'    // CHEROKEE SMALL LETTER A...CHEROKEE SMALL LETTER YA
                | '\u{ABC0}'...'\u{ABE2}'    // MEETEI MAYEK LETTER KOK...MEETEI MAYEK LETTER I LONSUM
                | '\u{ABE3}'...'\u{ABE4}'    // MEETEI MAYEK VOWEL SIGN ONAP...MEETEI MAYEK VOWEL SIGN INAP
                | '\u{ABE5}'                 // MEETEI MAYEK VOWEL SIGN ANAP
                | '\u{ABE6}'...'\u{ABE7}'    // MEETEI MAYEK VOWEL SIGN YENAP...MEETEI MAYEK VOWEL SIGN SOUNAP
                | '\u{ABE8}'                 // MEETEI MAYEK VOWEL SIGN UNAP
                | '\u{ABE9}'...'\u{ABEA}'    // MEETEI MAYEK VOWEL SIGN CHEINAP...MEETEI MAYEK VOWEL SIGN NUNG
                | '\u{ABEC}'                 // MEETEI MAYEK LUM IYEK
                | '\u{ABED}'                 // MEETEI MAYEK APUN IYEK
                | '\u{ABF0}'...'\u{ABF9}'    // MEETEI MAYEK DIGIT ZERO...MEETEI MAYEK DIGIT NINE
                | '\u{AC00}'...'\u{D7A3}'    // HANGUL SYLLABLE GA...HANGUL SYLLABLE HIH
                | '\u{D7B0}'...'\u{D7C6}'    // HANGUL JUNGSEONG O-YEO...HANGUL JUNGSEONG ARAEA-E
                | '\u{D7CB}'...'\u{D7FB}'    // HANGUL JONGSEONG NIEUN-RIEUL...HANGUL JONGSEONG PHIEUPH-THIEUTH
                | '\u{F900}'...'\u{FA6D}'    // CJK COMPATIBILITY IDEOGRAPH-F900...CJK COMPATIBILITY IDEOGRAPH-FA6D
                | '\u{FA70}'...'\u{FAD9}'    // CJK COMPATIBILITY IDEOGRAPH-FA70...CJK COMPATIBILITY IDEOGRAPH-FAD9
                | '\u{FB00}'...'\u{FB06}'    // LATIN SMALL LIGATURE FF...LATIN SMALL LIGATURE ST
                | '\u{FB13}'...'\u{FB17}'    // ARMENIAN SMALL LIGATURE MEN NOW...ARMENIAN SMALL LIGATURE MEN XEH
                | '\u{FB1D}'                 // HEBREW LETTER YOD WITH HIRIQ
                | '\u{FB1E}'                 // HEBREW POINT JUDEO-SPANISH VARIKA
                | '\u{FB1F}'...'\u{FB28}'    // HEBREW LIGATURE YIDDISH YOD YOD PATAH...HEBREW LETTER WIDE TAV
                | '\u{FB2A}'...'\u{FB36}'    // HEBREW LETTER SHIN WITH SHIN DOT...HEBREW LETTER ZAYIN WITH DAGESH
                | '\u{FB38}'...'\u{FB3C}'    // HEBREW LETTER TET WITH DAGESH...HEBREW LETTER LAMED WITH DAGESH
                | '\u{FB3E}'                 // HEBREW LETTER MEM WITH DAGESH
                | '\u{FB40}'...'\u{FB41}'    // HEBREW LETTER NUN WITH DAGESH...HEBREW LETTER SAMEKH WITH DAGESH
                | '\u{FB43}'...'\u{FB44}'    // HEBREW LETTER FINAL PE WITH DAGESH...HEBREW LETTER PE WITH DAGESH
                | '\u{FB46}'...'\u{FBB1}'    // HEBREW LETTER TSADI WITH DAGESH...ARABIC LETTER YEH BARREE WITH HAMZA ABOVE FINAL FORM
                | '\u{FBD3}'...'\u{FD3D}'    // ARABIC LETTER NG ISOLATED FORM...ARABIC LIGATURE ALEF WITH FATHATAN ISOLATED FORM
                | '\u{FD50}'...'\u{FD8F}'    // ARABIC LIGATURE TEH WITH JEEM WITH MEEM INITIAL FORM...ARABIC LIGATURE MEEM WITH KHAH WITH MEEM INITIAL FORM
                | '\u{FD92}'...'\u{FDC7}'    // ARABIC LIGATURE MEEM WITH JEEM WITH KHAH INITIAL FORM...ARABIC LIGATURE NOON WITH JEEM WITH YEH FINAL FORM
                | '\u{FDF0}'...'\u{FDFB}'    // ARABIC LIGATURE SALLA USED AS KORANIC STOP SIGN ISOLATED FORM...ARABIC LIGATURE JALLAJALALOUHOU
                | '\u{FE00}'...'\u{FE0F}'    // VARIATION SELECTOR-1...VARIATION SELECTOR-16
                | '\u{FE20}'...'\u{FE2F}'    // COMBINING LIGATURE LEFT HALF...COMBINING CYRILLIC TITLO RIGHT HALF
                | '\u{FE33}'...'\u{FE34}'    // PRESENTATION FORM FOR VERTICAL LOW LINE...PRESENTATION FORM FOR VERTICAL WAVY LOW LINE
                | '\u{FE4D}'...'\u{FE4F}'    // DASHED LOW LINE...WAVY LOW LINE
                | '\u{FE70}'...'\u{FE74}'    // ARABIC FATHATAN ISOLATED FORM...ARABIC KASRATAN ISOLATED FORM
                | '\u{FE76}'...'\u{FEFC}'    // ARABIC FATHA ISOLATED FORM...ARABIC LIGATURE LAM WITH ALEF FINAL FORM
                | '\u{FF10}'...'\u{FF19}'    // FULLWIDTH DIGIT ZERO...FULLWIDTH DIGIT NINE
                | '\u{FF21}'...'\u{FF3A}'    // FULLWIDTH LATIN CAPITAL LETTER A...FULLWIDTH LATIN CAPITAL LETTER Z
                | '\u{FF3F}'                 // FULLWIDTH LOW LINE
                | '\u{FF41}'...'\u{FF5A}'    // FULLWIDTH LATIN SMALL LETTER A...FULLWIDTH LATIN SMALL LETTER Z
                | '\u{FF66}'...'\u{FF6F}'    // HALFWIDTH KATAKANA LETTER WO...HALFWIDTH KATAKANA LETTER SMALL TU
                | '\u{FF70}'                 // HALFWIDTH KATAKANA-HIRAGANA PROLONGED SOUND MARK
                | '\u{FF71}'...'\u{FF9D}'    // HALFWIDTH KATAKANA LETTER A...HALFWIDTH KATAKANA LETTER N
                | '\u{FF9E}'...'\u{FF9F}'    // HALFWIDTH KATAKANA VOICED SOUND MARK...HALFWIDTH KATAKANA SEMI-VOICED SOUND MARK
                | '\u{FFA0}'...'\u{FFBE}'    // HALFWIDTH HANGUL FILLER...HALFWIDTH HANGUL LETTER HIEUH
                | '\u{FFC2}'...'\u{FFC7}'    // HALFWIDTH HANGUL LETTER A...HALFWIDTH HANGUL LETTER E
                | '\u{FFCA}'...'\u{FFCF}'    // HALFWIDTH HANGUL LETTER YEO...HALFWIDTH HANGUL LETTER OE
                | '\u{FFD2}'...'\u{FFD7}'    // HALFWIDTH HANGUL LETTER YO...HALFWIDTH HANGUL LETTER YU
                | '\u{FFDA}'...'\u{FFDC}'    // HALFWIDTH HANGUL LETTER EU...HALFWIDTH HANGUL LETTER I
                | '\u{10000}'...'\u{1000B}'  // LINEAR B SYLLABLE B008 A...LINEAR B SYLLABLE B046 JE
                | '\u{1000D}'...'\u{10026}'  // LINEAR B SYLLABLE B036 JO...LINEAR B SYLLABLE B032 QO
                | '\u{10028}'...'\u{1003A}'  // LINEAR B SYLLABLE B060 RA...LINEAR B SYLLABLE B042 WO
                | '\u{1003C}'...'\u{1003D}'  // LINEAR B SYLLABLE B017 ZA...LINEAR B SYLLABLE B074 ZE
                | '\u{1003F}'...'\u{1004D}'  // LINEAR B SYLLABLE B020 ZO...LINEAR B SYLLABLE B091 TWO
                | '\u{10050}'...'\u{1005D}'  // LINEAR B SYMBOL B018...LINEAR B SYMBOL B089
                | '\u{10080}'...'\u{100FA}'  // LINEAR B IDEOGRAM B100 MAN...LINEAR B IDEOGRAM VESSEL B305
                | '\u{10140}'...'\u{10174}'  // GREEK ACROPHONIC ATTIC ONE QUARTER...GREEK ACROPHONIC STRATIAN FIFTY MNAS
                | '\u{101FD}'                // PHAISTOS DISC SIGN COMBINING OBLIQUE STROKE
                | '\u{10280}'...'\u{1029C}'  // LYCIAN LETTER A...LYCIAN LETTER X
                | '\u{102A0}'...'\u{102D0}'  // CARIAN LETTER A...CARIAN LETTER UUU3
                | '\u{102E0}'                // COPTIC EPACT THOUSANDS MARK
                | '\u{10300}'...'\u{1031F}'  // OLD ITALIC LETTER A...OLD ITALIC LETTER ESS
                | '\u{1032D}'...'\u{10340}'  // OLD ITALIC LETTER YE...GOTHIC LETTER PAIRTHRA
                | '\u{10341}'                // GOTHIC LETTER NINETY
                | '\u{10342}'...'\u{10349}'  // GOTHIC LETTER RAIDA...GOTHIC LETTER OTHAL
                | '\u{1034A}'                // GOTHIC LETTER NINE HUNDRED
                | '\u{10350}'...'\u{10375}'  // OLD PERMIC LETTER AN...OLD PERMIC LETTER IA
                | '\u{10376}'...'\u{1037A}'  // COMBINING OLD PERMIC LETTER AN...COMBINING OLD PERMIC LETTER SII
                | '\u{10380}'...'\u{1039D}'  // UGARITIC LETTER ALPA...UGARITIC LETTER SSU
                | '\u{103A0}'...'\u{103C3}'  // OLD PERSIAN SIGN A...OLD PERSIAN SIGN HA
                | '\u{103C8}'...'\u{103CF}'  // OLD PERSIAN SIGN AURAMAZDAA...OLD PERSIAN SIGN BUUMISH
                | '\u{103D1}'...'\u{103D5}'  // OLD PERSIAN NUMBER ONE...OLD PERSIAN NUMBER HUNDRED
                | '\u{10400}'...'\u{1044F}'  // DESERET CAPITAL LETTER LONG I...DESERET SMALL LETTER EW
                | '\u{10450}'...'\u{1049D}'  // SHAVIAN LETTER PEEP...OSMANYA LETTER OO
                | '\u{104A0}'...'\u{104A9}'  // OSMANYA DIGIT ZERO...OSMANYA DIGIT NINE
                | '\u{104B0}'...'\u{104D3}'  // OSAGE CAPITAL LETTER A...OSAGE CAPITAL LETTER ZHA
                | '\u{104D8}'...'\u{104FB}'  // OSAGE SMALL LETTER A...OSAGE SMALL LETTER ZHA
                | '\u{10500}'...'\u{10527}'  // ELBASAN LETTER A...ELBASAN LETTER KHE
                | '\u{10530}'...'\u{10563}'  // CAUCASIAN ALBANIAN LETTER ALT...CAUCASIAN ALBANIAN LETTER KIW
                | '\u{10600}'...'\u{10736}'  // LINEAR A SIGN AB001...LINEAR A SIGN A664
                | '\u{10740}'...'\u{10755}'  // LINEAR A SIGN A701 A...LINEAR A SIGN A732 JE
                | '\u{10760}'...'\u{10767}'  // LINEAR A SIGN A800...LINEAR A SIGN A807
                | '\u{10800}'...'\u{10805}'  // CYPRIOT SYLLABLE A...CYPRIOT SYLLABLE JA
                | '\u{10808}'                // CYPRIOT SYLLABLE JO
                | '\u{1080A}'...'\u{10835}'  // CYPRIOT SYLLABLE KA...CYPRIOT SYLLABLE WO
                | '\u{10837}'...'\u{10838}'  // CYPRIOT SYLLABLE XA...CYPRIOT SYLLABLE XE
                | '\u{1083C}'                // CYPRIOT SYLLABLE ZA
                | '\u{1083F}'...'\u{10855}'  // CYPRIOT SYLLABLE ZO...IMPERIAL ARAMAIC LETTER TAW
                | '\u{10860}'...'\u{10876}'  // PALMYRENE LETTER ALEPH...PALMYRENE LETTER TAW
                | '\u{10880}'...'\u{1089E}'  // NABATAEAN LETTER FINAL ALEPH...NABATAEAN LETTER TAW
                | '\u{108E0}'...'\u{108F2}'  // HATRAN LETTER ALEPH...HATRAN LETTER QOPH
                | '\u{108F4}'...'\u{108F5}'  // HATRAN LETTER SHIN...HATRAN LETTER TAW
                | '\u{10900}'...'\u{10915}'  // PHOENICIAN LETTER ALF...PHOENICIAN LETTER TAU
                | '\u{10920}'...'\u{10939}'  // LYDIAN LETTER A...LYDIAN LETTER C
                | '\u{10980}'...'\u{109B7}'  // MEROITIC HIEROGLYPHIC LETTER A...MEROITIC CURSIVE LETTER DA
                | '\u{109BE}'...'\u{109BF}'  // MEROITIC CURSIVE LOGOGRAM RMT...MEROITIC CURSIVE LOGOGRAM IMN
                | '\u{10A00}'                // KHAROSHTHI LETTER A
                | '\u{10A01}'...'\u{10A03}'  // KHAROSHTHI VOWEL SIGN I...KHAROSHTHI VOWEL SIGN VOCALIC R
                | '\u{10A05}'...'\u{10A06}'  // KHAROSHTHI VOWEL SIGN E...KHAROSHTHI VOWEL SIGN O
                | '\u{10A0C}'...'\u{10A0F}'  // KHAROSHTHI VOWEL LENGTH MARK...KHAROSHTHI SIGN VISARGA
                | '\u{10A10}'...'\u{10A13}'  // KHAROSHTHI LETTER KA...KHAROSHTHI LETTER GHA
                | '\u{10A15}'...'\u{10A17}'  // KHAROSHTHI LETTER CA...KHAROSHTHI LETTER JA
                | '\u{10A19}'...'\u{10A33}'  // KHAROSHTHI LETTER NYA...KHAROSHTHI LETTER TTTHA
                | '\u{10A38}'...'\u{10A3A}'  // KHAROSHTHI SIGN BAR ABOVE...KHAROSHTHI SIGN DOT BELOW
                | '\u{10A3F}'                // KHAROSHTHI VIRAMA
                | '\u{10A60}'...'\u{10A7C}'  // OLD SOUTH ARABIAN LETTER HE...OLD SOUTH ARABIAN LETTER THETH
                | '\u{10A80}'...'\u{10A9C}'  // OLD NORTH ARABIAN LETTER HEH...OLD NORTH ARABIAN LETTER ZAH
                | '\u{10AC0}'...'\u{10AC7}'  // MANICHAEAN LETTER ALEPH...MANICHAEAN LETTER WAW
                | '\u{10AC9}'...'\u{10AE4}'  // MANICHAEAN LETTER ZAYIN...MANICHAEAN LETTER TAW
                | '\u{10AE5}'...'\u{10AE6}'  // MANICHAEAN ABBREVIATION MARK ABOVE...MANICHAEAN ABBREVIATION MARK BELOW
                | '\u{10B00}'...'\u{10B35}'  // AVESTAN LETTER A...AVESTAN LETTER HE
                | '\u{10B40}'...'\u{10B55}'  // INSCRIPTIONAL PARTHIAN LETTER ALEPH...INSCRIPTIONAL PARTHIAN LETTER TAW
                | '\u{10B60}'...'\u{10B72}'  // INSCRIPTIONAL PAHLAVI LETTER ALEPH...INSCRIPTIONAL PAHLAVI LETTER TAW
                | '\u{10B80}'...'\u{10B91}'  // PSALTER PAHLAVI LETTER ALEPH...PSALTER PAHLAVI LETTER TAW
                | '\u{10C00}'...'\u{10C48}'  // OLD TURKIC LETTER ORKHON A...OLD TURKIC LETTER ORKHON BASH
                | '\u{10C80}'...'\u{10CB2}'  // OLD HUNGARIAN CAPITAL LETTER A...OLD HUNGARIAN CAPITAL LETTER US
                | '\u{10CC0}'...'\u{10CF2}'  // OLD HUNGARIAN SMALL LETTER A...OLD HUNGARIAN SMALL LETTER US
                | '\u{11000}'                // BRAHMI SIGN CANDRABINDU
                | '\u{11001}'                // BRAHMI SIGN ANUSVARA
                | '\u{11002}'                // BRAHMI SIGN VISARGA
                | '\u{11003}'...'\u{11037}'  // BRAHMI SIGN JIHVAMULIYA...BRAHMI LETTER OLD TAMIL NNNA
                | '\u{11038}'...'\u{11046}'  // BRAHMI VOWEL SIGN AA...BRAHMI VIRAMA
                | '\u{11066}'...'\u{1106F}'  // BRAHMI DIGIT ZERO...BRAHMI DIGIT NINE
                | '\u{1107F}'...'\u{11081}'  // BRAHMI NUMBER JOINER...KAITHI SIGN ANUSVARA
                | '\u{11082}'                // KAITHI SIGN VISARGA
                | '\u{11083}'...'\u{110AF}'  // KAITHI LETTER A...KAITHI LETTER HA
                | '\u{110B0}'...'\u{110B2}'  // KAITHI VOWEL SIGN AA...KAITHI VOWEL SIGN II
                | '\u{110B3}'...'\u{110B6}'  // KAITHI VOWEL SIGN U...KAITHI VOWEL SIGN AI
                | '\u{110B7}'...'\u{110B8}'  // KAITHI VOWEL SIGN O...KAITHI VOWEL SIGN AU
                | '\u{110B9}'...'\u{110BA}'  // KAITHI SIGN VIRAMA...KAITHI SIGN NUKTA
                | '\u{110D0}'...'\u{110E8}'  // SORA SOMPENG LETTER SAH...SORA SOMPENG LETTER MAE
                | '\u{110F0}'...'\u{110F9}'  // SORA SOMPENG DIGIT ZERO...SORA SOMPENG DIGIT NINE
                | '\u{11100}'...'\u{11102}'  // CHAKMA SIGN CANDRABINDU...CHAKMA SIGN VISARGA
                | '\u{11103}'...'\u{11126}'  // CHAKMA LETTER AA...CHAKMA LETTER HAA
                | '\u{11127}'...'\u{1112B}'  // CHAKMA VOWEL SIGN A...CHAKMA VOWEL SIGN UU
                | '\u{1112C}'                // CHAKMA VOWEL SIGN E
                | '\u{1112D}'...'\u{11134}'  // CHAKMA VOWEL SIGN AI...CHAKMA MAAYYAA
                | '\u{11136}'...'\u{1113F}'  // CHAKMA DIGIT ZERO...CHAKMA DIGIT NINE
                | '\u{11150}'...'\u{11172}'  // MAHAJANI LETTER A...MAHAJANI LETTER RRA
                | '\u{11173}'                // MAHAJANI SIGN NUKTA
                | '\u{11176}'                // MAHAJANI LIGATURE SHRI
                | '\u{11180}'...'\u{11181}'  // SHARADA SIGN CANDRABINDU...SHARADA SIGN ANUSVARA
                | '\u{11182}'                // SHARADA SIGN VISARGA
                | '\u{11183}'...'\u{111B2}'  // SHARADA LETTER A...SHARADA LETTER HA
                | '\u{111B3}'...'\u{111B5}'  // SHARADA VOWEL SIGN AA...SHARADA VOWEL SIGN II
                | '\u{111B6}'...'\u{111BE}'  // SHARADA VOWEL SIGN U...SHARADA VOWEL SIGN O
                | '\u{111BF}'...'\u{111C0}'  // SHARADA VOWEL SIGN AU...SHARADA SIGN VIRAMA
                | '\u{111C1}'...'\u{111C4}'  // SHARADA SIGN AVAGRAHA...SHARADA OM
                | '\u{111CA}'...'\u{111CC}'  // SHARADA SIGN NUKTA...SHARADA EXTRA SHORT VOWEL MARK
                | '\u{111D0}'...'\u{111D9}'  // SHARADA DIGIT ZERO...SHARADA DIGIT NINE
                | '\u{111DA}'                // SHARADA EKAM
                | '\u{111DC}'                // SHARADA HEADSTROKE
                | '\u{11200}'...'\u{11211}'  // KHOJKI LETTER A...KHOJKI LETTER JJA
                | '\u{11213}'...'\u{1122B}'  // KHOJKI LETTER NYA...KHOJKI LETTER LLA
                | '\u{1122C}'...'\u{1122E}'  // KHOJKI VOWEL SIGN AA...KHOJKI VOWEL SIGN II
                | '\u{1122F}'...'\u{11231}'  // KHOJKI VOWEL SIGN U...KHOJKI VOWEL SIGN AI
                | '\u{11232}'...'\u{11233}'  // KHOJKI VOWEL SIGN O...KHOJKI VOWEL SIGN AU
                | '\u{11234}'                // KHOJKI SIGN ANUSVARA
                | '\u{11235}'                // KHOJKI SIGN VIRAMA
                | '\u{11236}'...'\u{11237}'  // KHOJKI SIGN NUKTA...KHOJKI SIGN SHADDA
                | '\u{1123E}'                // KHOJKI SIGN SUKUN
                | '\u{11280}'...'\u{11286}'  // MULTANI LETTER A...MULTANI LETTER GA
                | '\u{11288}'                // MULTANI LETTER GHA
                | '\u{1128A}'...'\u{1128D}'  // MULTANI LETTER CA...MULTANI LETTER JJA
                | '\u{1128F}'...'\u{1129D}'  // MULTANI LETTER NYA...MULTANI LETTER BA
                | '\u{1129F}'...'\u{112A8}'  // MULTANI LETTER BHA...MULTANI LETTER RHA
                | '\u{112B0}'...'\u{112DE}'  // KHUDAWADI LETTER A...KHUDAWADI LETTER HA
                | '\u{112DF}'                // KHUDAWADI SIGN ANUSVARA
                | '\u{112E0}'...'\u{112E2}'  // KHUDAWADI VOWEL SIGN AA...KHUDAWADI VOWEL SIGN II
                | '\u{112E3}'...'\u{112EA}'  // KHUDAWADI VOWEL SIGN U...KHUDAWADI SIGN VIRAMA
                | '\u{112F0}'...'\u{112F9}'  // KHUDAWADI DIGIT ZERO...KHUDAWADI DIGIT NINE
                | '\u{11300}'...'\u{11301}'  // GRANTHA SIGN COMBINING ANUSVARA ABOVE...GRANTHA SIGN CANDRABINDU
                | '\u{11302}'...'\u{11303}'  // GRANTHA SIGN ANUSVARA...GRANTHA SIGN VISARGA
                | '\u{11305}'...'\u{1130C}'  // GRANTHA LETTER A...GRANTHA LETTER VOCALIC L
                | '\u{1130F}'...'\u{11310}'  // GRANTHA LETTER EE...GRANTHA LETTER AI
                | '\u{11313}'...'\u{11328}'  // GRANTHA LETTER OO...GRANTHA LETTER NA
                | '\u{1132A}'...'\u{11330}'  // GRANTHA LETTER PA...GRANTHA LETTER RA
                | '\u{11332}'...'\u{11333}'  // GRANTHA LETTER LA...GRANTHA LETTER LLA
                | '\u{11335}'...'\u{11339}'  // GRANTHA LETTER VA...GRANTHA LETTER HA
                | '\u{1133C}'                // GRANTHA SIGN NUKTA
                | '\u{1133D}'                // GRANTHA SIGN AVAGRAHA
                | '\u{1133E}'...'\u{1133F}'  // GRANTHA VOWEL SIGN AA...GRANTHA VOWEL SIGN I
                | '\u{11340}'                // GRANTHA VOWEL SIGN II
                | '\u{11341}'...'\u{11344}'  // GRANTHA VOWEL SIGN U...GRANTHA VOWEL SIGN VOCALIC RR
                | '\u{11347}'...'\u{11348}'  // GRANTHA VOWEL SIGN EE...GRANTHA VOWEL SIGN AI
                | '\u{1134B}'...'\u{1134D}'  // GRANTHA VOWEL SIGN OO...GRANTHA SIGN VIRAMA
                | '\u{11350}'                // GRANTHA OM
                | '\u{11357}'                // GRANTHA AU LENGTH MARK
                | '\u{1135D}'...'\u{11361}'  // GRANTHA SIGN PLUTA...GRANTHA LETTER VOCALIC LL
                | '\u{11362}'...'\u{11363}'  // GRANTHA VOWEL SIGN VOCALIC L...GRANTHA VOWEL SIGN VOCALIC LL
                | '\u{11366}'...'\u{1136C}'  // COMBINING GRANTHA DIGIT ZERO...COMBINING GRANTHA DIGIT SIX
                | '\u{11370}'...'\u{11374}'  // COMBINING GRANTHA LETTER A...COMBINING GRANTHA LETTER PA
                | '\u{11400}'...'\u{11434}'  // NEWA LETTER A...NEWA LETTER HA
                | '\u{11435}'...'\u{11437}'  // NEWA VOWEL SIGN AA...NEWA VOWEL SIGN II
                | '\u{11438}'...'\u{1143F}'  // NEWA VOWEL SIGN U...NEWA VOWEL SIGN AI
                | '\u{11440}'...'\u{11441}'  // NEWA VOWEL SIGN O...NEWA VOWEL SIGN AU
                | '\u{11442}'...'\u{11444}'  // NEWA SIGN VIRAMA...NEWA SIGN ANUSVARA
                | '\u{11445}'                // NEWA SIGN VISARGA
                | '\u{11446}'                // NEWA SIGN NUKTA
                | '\u{11447}'...'\u{1144A}'  // NEWA SIGN AVAGRAHA...NEWA SIDDHI
                | '\u{11450}'...'\u{11459}'  // NEWA DIGIT ZERO...NEWA DIGIT NINE
                | '\u{11480}'...'\u{114AF}'  // TIRHUTA ANJI...TIRHUTA LETTER HA
                | '\u{114B0}'...'\u{114B2}'  // TIRHUTA VOWEL SIGN AA...TIRHUTA VOWEL SIGN II
                | '\u{114B3}'...'\u{114B8}'  // TIRHUTA VOWEL SIGN U...TIRHUTA VOWEL SIGN VOCALIC LL
                | '\u{114B9}'                // TIRHUTA VOWEL SIGN E
                | '\u{114BA}'                // TIRHUTA VOWEL SIGN SHORT E
                | '\u{114BB}'...'\u{114BE}'  // TIRHUTA VOWEL SIGN AI...TIRHUTA VOWEL SIGN AU
                | '\u{114BF}'...'\u{114C0}'  // TIRHUTA SIGN CANDRABINDU...TIRHUTA SIGN ANUSVARA
                | '\u{114C1}'                // TIRHUTA SIGN VISARGA
                | '\u{114C2}'...'\u{114C3}'  // TIRHUTA SIGN VIRAMA...TIRHUTA SIGN NUKTA
                | '\u{114C4}'...'\u{114C5}'  // TIRHUTA SIGN AVAGRAHA...TIRHUTA GVANG
                | '\u{114C7}'                // TIRHUTA OM
                | '\u{114D0}'...'\u{114D9}'  // TIRHUTA DIGIT ZERO...TIRHUTA DIGIT NINE
                | '\u{11580}'...'\u{115AE}'  // SIDDHAM LETTER A...SIDDHAM LETTER HA
                | '\u{115AF}'...'\u{115B1}'  // SIDDHAM VOWEL SIGN AA...SIDDHAM VOWEL SIGN II
                | '\u{115B2}'...'\u{115B5}'  // SIDDHAM VOWEL SIGN U...SIDDHAM VOWEL SIGN VOCALIC RR
                | '\u{115B8}'...'\u{115BB}'  // SIDDHAM VOWEL SIGN E...SIDDHAM VOWEL SIGN AU
                | '\u{115BC}'...'\u{115BD}'  // SIDDHAM SIGN CANDRABINDU...SIDDHAM SIGN ANUSVARA
                | '\u{115BE}'                // SIDDHAM SIGN VISARGA
                | '\u{115BF}'...'\u{115C0}'  // SIDDHAM SIGN VIRAMA...SIDDHAM SIGN NUKTA
                | '\u{115D8}'...'\u{115DB}'  // SIDDHAM LETTER THREE-CIRCLE ALTERNATE I...SIDDHAM LETTER ALTERNATE U
                | '\u{115DC}'...'\u{115DD}'  // SIDDHAM VOWEL SIGN ALTERNATE U...SIDDHAM VOWEL SIGN ALTERNATE UU
                | '\u{11600}'...'\u{1162F}'  // MODI LETTER A...MODI LETTER LLA
                | '\u{11630}'...'\u{11632}'  // MODI VOWEL SIGN AA...MODI VOWEL SIGN II
                | '\u{11633}'...'\u{1163A}'  // MODI VOWEL SIGN U...MODI VOWEL SIGN AI
                | '\u{1163B}'...'\u{1163C}'  // MODI VOWEL SIGN O...MODI VOWEL SIGN AU
                | '\u{1163D}'                // MODI SIGN ANUSVARA
                | '\u{1163E}'                // MODI SIGN VISARGA
                | '\u{1163F}'...'\u{11640}'  // MODI SIGN VIRAMA...MODI SIGN ARDHACANDRA
                | '\u{11644}'                // MODI SIGN HUVA
                | '\u{11650}'...'\u{11659}'  // MODI DIGIT ZERO...MODI DIGIT NINE
                | '\u{11680}'...'\u{116AA}'  // TAKRI LETTER A...TAKRI LETTER RRA
                | '\u{116AB}'                // TAKRI SIGN ANUSVARA
                | '\u{116AC}'                // TAKRI SIGN VISARGA
                | '\u{116AD}'                // TAKRI VOWEL SIGN AA
                | '\u{116AE}'...'\u{116AF}'  // TAKRI VOWEL SIGN I...TAKRI VOWEL SIGN II
                | '\u{116B0}'...'\u{116B5}'  // TAKRI VOWEL SIGN U...TAKRI VOWEL SIGN AU
                | '\u{116B6}'                // TAKRI SIGN VIRAMA
                | '\u{116B7}'                // TAKRI SIGN NUKTA
                | '\u{116C0}'...'\u{116C9}'  // TAKRI DIGIT ZERO...TAKRI DIGIT NINE
                | '\u{11700}'...'\u{11719}'  // AHOM LETTER KA...AHOM LETTER JHA
                | '\u{1171D}'...'\u{1171F}'  // AHOM CONSONANT SIGN MEDIAL LA...AHOM CONSONANT SIGN MEDIAL LIGATING RA
                | '\u{11720}'...'\u{11721}'  // AHOM VOWEL SIGN A...AHOM VOWEL SIGN AA
                | '\u{11722}'...'\u{11725}'  // AHOM VOWEL SIGN I...AHOM VOWEL SIGN UU
                | '\u{11726}'                // AHOM VOWEL SIGN E
                | '\u{11727}'...'\u{1172B}'  // AHOM VOWEL SIGN AW...AHOM SIGN KILLER
                | '\u{11730}'...'\u{11739}'  // AHOM DIGIT ZERO...AHOM DIGIT NINE
                | '\u{118A0}'...'\u{118DF}'  // WARANG CITI CAPITAL LETTER NGAA...WARANG CITI SMALL LETTER VIYO
                | '\u{118E0}'...'\u{118E9}'  // WARANG CITI DIGIT ZERO...WARANG CITI DIGIT NINE
                | '\u{118FF}'                // WARANG CITI OM
                | '\u{11A00}'                // ZANABAZAR SQUARE LETTER A
                | '\u{11A01}'...'\u{11A06}'  // ZANABAZAR SQUARE VOWEL SIGN I...ZANABAZAR SQUARE VOWEL SIGN O
                | '\u{11A07}'...'\u{11A08}'  // ZANABAZAR SQUARE VOWEL SIGN AI...ZANABAZAR SQUARE VOWEL SIGN AU
                | '\u{11A09}'...'\u{11A0A}'  // ZANABAZAR SQUARE VOWEL SIGN REVERSED I...ZANABAZAR SQUARE VOWEL LENGTH MARK
                | '\u{11A0B}'...'\u{11A32}'  // ZANABAZAR SQUARE LETTER KA...ZANABAZAR SQUARE LETTER KSSA
                | '\u{11A33}'...'\u{11A38}'  // ZANABAZAR SQUARE FINAL CONSONANT MARK...ZANABAZAR SQUARE SIGN ANUSVARA
                | '\u{11A39}'                // ZANABAZAR SQUARE SIGN VISARGA
                | '\u{11A3A}'                // ZANABAZAR SQUARE CLUSTER-INITIAL LETTER RA
                | '\u{11A3B}'...'\u{11A3E}'  // ZANABAZAR SQUARE CLUSTER-FINAL LETTER YA...ZANABAZAR SQUARE CLUSTER-FINAL LETTER VA
                | '\u{11A47}'                // ZANABAZAR SQUARE SUBJOINER
                | '\u{11A50}'                // SOYOMBO LETTER A
                | '\u{11A51}'...'\u{11A56}'  // SOYOMBO VOWEL SIGN I...SOYOMBO VOWEL SIGN OE
                | '\u{11A57}'...'\u{11A58}'  // SOYOMBO VOWEL SIGN AI...SOYOMBO VOWEL SIGN AU
                | '\u{11A59}'...'\u{11A5B}'  // SOYOMBO VOWEL SIGN VOCALIC R...SOYOMBO VOWEL LENGTH MARK
                | '\u{11A5C}'...'\u{11A83}'  // SOYOMBO LETTER KA...SOYOMBO LETTER KSSA
                | '\u{11A86}'...'\u{11A89}'  // SOYOMBO CLUSTER-INITIAL LETTER RA...SOYOMBO CLUSTER-INITIAL LETTER SA
                | '\u{11A8A}'...'\u{11A96}'  // SOYOMBO FINAL CONSONANT SIGN G...SOYOMBO SIGN ANUSVARA
                | '\u{11A97}'                // SOYOMBO SIGN VISARGA
                | '\u{11A98}'...'\u{11A99}'  // SOYOMBO GEMINATION MARK...SOYOMBO SUBJOINER
                | '\u{11AC0}'...'\u{11AF8}'  // PAU CIN HAU LETTER PA...PAU CIN HAU GLOTTAL STOP FINAL
                | '\u{11C00}'...'\u{11C08}'  // BHAIKSUKI LETTER A...BHAIKSUKI LETTER VOCALIC L
                | '\u{11C0A}'...'\u{11C2E}'  // BHAIKSUKI LETTER E...BHAIKSUKI LETTER HA
                | '\u{11C2F}'                // BHAIKSUKI VOWEL SIGN AA
                | '\u{11C30}'...'\u{11C36}'  // BHAIKSUKI VOWEL SIGN I...BHAIKSUKI VOWEL SIGN VOCALIC L
                | '\u{11C38}'...'\u{11C3D}'  // BHAIKSUKI VOWEL SIGN E...BHAIKSUKI SIGN ANUSVARA
                | '\u{11C3E}'                // BHAIKSUKI SIGN VISARGA
                | '\u{11C3F}'                // BHAIKSUKI SIGN VIRAMA
                | '\u{11C40}'                // BHAIKSUKI SIGN AVAGRAHA
                | '\u{11C50}'...'\u{11C59}'  // BHAIKSUKI DIGIT ZERO...BHAIKSUKI DIGIT NINE
                | '\u{11C72}'...'\u{11C8F}'  // MARCHEN LETTER KA...MARCHEN LETTER A
                | '\u{11C92}'...'\u{11CA7}'  // MARCHEN SUBJOINED LETTER KA...MARCHEN SUBJOINED LETTER ZA
                | '\u{11CA9}'                // MARCHEN SUBJOINED LETTER YA
                | '\u{11CAA}'...'\u{11CB0}'  // MARCHEN SUBJOINED LETTER RA...MARCHEN VOWEL SIGN AA
                | '\u{11CB1}'                // MARCHEN VOWEL SIGN I
                | '\u{11CB2}'...'\u{11CB3}'  // MARCHEN VOWEL SIGN U...MARCHEN VOWEL SIGN E
                | '\u{11CB4}'                // MARCHEN VOWEL SIGN O
                | '\u{11CB5}'...'\u{11CB6}'  // MARCHEN SIGN ANUSVARA...MARCHEN SIGN CANDRABINDU
                | '\u{11D00}'...'\u{11D06}'  // MASARAM GONDI LETTER A...MASARAM GONDI LETTER E
                | '\u{11D08}'...'\u{11D09}'  // MASARAM GONDI LETTER AI...MASARAM GONDI LETTER O
                | '\u{11D0B}'...'\u{11D30}'  // MASARAM GONDI LETTER AU...MASARAM GONDI LETTER TRA
                | '\u{11D31}'...'\u{11D36}'  // MASARAM GONDI VOWEL SIGN AA...MASARAM GONDI VOWEL SIGN VOCALIC R
                | '\u{11D3A}'                // MASARAM GONDI VOWEL SIGN E
                | '\u{11D3C}'...'\u{11D3D}'  // MASARAM GONDI VOWEL SIGN AI...MASARAM GONDI VOWEL SIGN O
                | '\u{11D3F}'...'\u{11D45}'  // MASARAM GONDI VOWEL SIGN AU...MASARAM GONDI VIRAMA
                | '\u{11D46}'                // MASARAM GONDI REPHA
                | '\u{11D47}'                // MASARAM GONDI RA-KARA
                | '\u{11D50}'...'\u{11D59}'  // MASARAM GONDI DIGIT ZERO...MASARAM GONDI DIGIT NINE
                | '\u{12000}'...'\u{12399}'  // CUNEIFORM SIGN A...CUNEIFORM SIGN U U
                | '\u{12400}'...'\u{1246E}'  // CUNEIFORM NUMERIC SIGN TWO ASH...CUNEIFORM NUMERIC SIGN NINE U VARIANT FORM
                | '\u{12480}'...'\u{12543}'  // CUNEIFORM SIGN AB TIMES NUN TENU...CUNEIFORM SIGN ZU5 TIMES THREE DISH TENU
                | '\u{13000}'...'\u{1342E}'  // EGYPTIAN HIEROGLYPH A001...EGYPTIAN HIEROGLYPH AA032
                | '\u{14400}'...'\u{14646}'  // ANATOLIAN HIEROGLYPH A001...ANATOLIAN HIEROGLYPH A530
                | '\u{16800}'...'\u{16A38}'  // BAMUM LETTER PHASE-A NGKUE MFON...BAMUM LETTER PHASE-F VUEQ
                | '\u{16A40}'...'\u{16A5E}'  // MRO LETTER TA...MRO LETTER TEK
                | '\u{16A60}'...'\u{16A69}'  // MRO DIGIT ZERO...MRO DIGIT NINE
                | '\u{16AD0}'...'\u{16AED}'  // BASSA VAH LETTER ENNI...BASSA VAH LETTER I
                | '\u{16AF0}'...'\u{16AF4}'  // BASSA VAH COMBINING HIGH TONE...BASSA VAH COMBINING HIGH-LOW TONE
                | '\u{16B00}'...'\u{16B2F}'  // PAHAWH HMONG VOWEL KEEB...PAHAWH HMONG CONSONANT CAU
                | '\u{16B30}'...'\u{16B36}'  // PAHAWH HMONG MARK CIM TUB...PAHAWH HMONG MARK CIM TAUM
                | '\u{16B40}'...'\u{16B43}'  // PAHAWH HMONG SIGN VOS SEEV...PAHAWH HMONG SIGN IB YAM
                | '\u{16B50}'...'\u{16B59}'  // PAHAWH HMONG DIGIT ZERO...PAHAWH HMONG DIGIT NINE
                | '\u{16B63}'...'\u{16B77}'  // PAHAWH HMONG SIGN VOS LUB...PAHAWH HMONG SIGN CIM NRES TOS
                | '\u{16B7D}'...'\u{16B8F}'  // PAHAWH HMONG CLAN SIGN TSHEEJ...PAHAWH HMONG CLAN SIGN VWJ
                | '\u{16F00}'...'\u{16F44}'  // MIAO LETTER PA...MIAO LETTER HHA
                | '\u{16F50}'                // MIAO LETTER NASALIZATION
                | '\u{16F51}'...'\u{16F7E}'  // MIAO SIGN ASPIRATION...MIAO VOWEL SIGN NG
                | '\u{16F8F}'...'\u{16F92}'  // MIAO TONE RIGHT...MIAO TONE BELOW
                | '\u{16F93}'...'\u{16F9F}'  // MIAO LETTER TONE-2...MIAO LETTER REFORMED TONE-8
                | '\u{16FE0}'...'\u{16FE1}'  // TANGUT ITERATION MARK...NUSHU ITERATION MARK
                | '\u{17000}'...'\u{187EC}'  // TANGUT IDEOGRAPH-17000...TANGUT IDEOGRAPH-187EC
                | '\u{18800}'...'\u{18AF2}'  // TANGUT COMPONENT-001...TANGUT COMPONENT-755
                | '\u{1B000}'...'\u{1B11E}'  // KATAKANA LETTER ARCHAIC E...HENTAIGANA LETTER N-MU-MO-2
                | '\u{1B170}'...'\u{1B2FB}'  // NUSHU CHARACTER-1B170...NUSHU CHARACTER-1B2FB
                | '\u{1BC00}'...'\u{1BC6A}'  // DUPLOYAN LETTER H...DUPLOYAN LETTER VOCALIC M
                | '\u{1BC70}'...'\u{1BC7C}'  // DUPLOYAN AFFIX LEFT HORIZONTAL SECANT...DUPLOYAN AFFIX ATTACHED TANGENT HOOK
                | '\u{1BC80}'...'\u{1BC88}'  // DUPLOYAN AFFIX HIGH ACUTE...DUPLOYAN AFFIX HIGH VERTICAL
                | '\u{1BC90}'...'\u{1BC99}'  // DUPLOYAN AFFIX LOW ACUTE...DUPLOYAN AFFIX LOW ARROW
                | '\u{1BC9D}'...'\u{1BC9E}'  // DUPLOYAN THICK LETTER SELECTOR...DUPLOYAN DOUBLE MARK
                | '\u{1D165}'...'\u{1D166}'  // MUSICAL SYMBOL COMBINING STEM...MUSICAL SYMBOL COMBINING SPRECHGESANG STEM
                | '\u{1D167}'...'\u{1D169}'  // MUSICAL SYMBOL COMBINING TREMOLO-1...MUSICAL SYMBOL COMBINING TREMOLO-3
                | '\u{1D16D}'...'\u{1D172}'  // MUSICAL SYMBOL COMBINING AUGMENTATION DOT...MUSICAL SYMBOL COMBINING FLAG-5
                | '\u{1D17B}'...'\u{1D182}'  // MUSICAL SYMBOL COMBINING ACCENT...MUSICAL SYMBOL COMBINING LOURE
                | '\u{1D185}'...'\u{1D18B}'  // MUSICAL SYMBOL COMBINING DOIT...MUSICAL SYMBOL COMBINING TRIPLE TONGUE
                | '\u{1D1AA}'...'\u{1D1AD}'  // MUSICAL SYMBOL COMBINING DOWN BOW...MUSICAL SYMBOL COMBINING SNAP PIZZICATO
                | '\u{1D242}'...'\u{1D244}'  // COMBINING GREEK MUSICAL TRISEME...COMBINING GREEK MUSICAL PENTASEME
                | '\u{1D400}'...'\u{1D454}'  // MATHEMATICAL BOLD CAPITAL A...MATHEMATICAL ITALIC SMALL G
                | '\u{1D456}'...'\u{1D49C}'  // MATHEMATICAL ITALIC SMALL I...MATHEMATICAL SCRIPT CAPITAL A
                | '\u{1D49E}'...'\u{1D49F}'  // MATHEMATICAL SCRIPT CAPITAL C...MATHEMATICAL SCRIPT CAPITAL D
                | '\u{1D4A2}'                // MATHEMATICAL SCRIPT CAPITAL G
                | '\u{1D4A5}'...'\u{1D4A6}'  // MATHEMATICAL SCRIPT CAPITAL J...MATHEMATICAL SCRIPT CAPITAL K
                | '\u{1D4A9}'...'\u{1D4AC}'  // MATHEMATICAL SCRIPT CAPITAL N...MATHEMATICAL SCRIPT CAPITAL Q
                | '\u{1D4AE}'...'\u{1D4B9}'  // MATHEMATICAL SCRIPT CAPITAL S...MATHEMATICAL SCRIPT SMALL D
                | '\u{1D4BB}'                // MATHEMATICAL SCRIPT SMALL F
                | '\u{1D4BD}'...'\u{1D4C3}'  // MATHEMATICAL SCRIPT SMALL H...MATHEMATICAL SCRIPT SMALL N
                | '\u{1D4C5}'...'\u{1D505}'  // MATHEMATICAL SCRIPT SMALL P...MATHEMATICAL FRAKTUR CAPITAL B
                | '\u{1D507}'...'\u{1D50A}'  // MATHEMATICAL FRAKTUR CAPITAL D...MATHEMATICAL FRAKTUR CAPITAL G
                | '\u{1D50D}'...'\u{1D514}'  // MATHEMATICAL FRAKTUR CAPITAL J...MATHEMATICAL FRAKTUR CAPITAL Q
                | '\u{1D516}'...'\u{1D51C}'  // MATHEMATICAL FRAKTUR CAPITAL S...MATHEMATICAL FRAKTUR CAPITAL Y
                | '\u{1D51E}'...'\u{1D539}'  // MATHEMATICAL FRAKTUR SMALL A...MATHEMATICAL DOUBLE-STRUCK CAPITAL B
                | '\u{1D53B}'...'\u{1D53E}'  // MATHEMATICAL DOUBLE-STRUCK CAPITAL D...MATHEMATICAL DOUBLE-STRUCK CAPITAL G
                | '\u{1D540}'...'\u{1D544}'  // MATHEMATICAL DOUBLE-STRUCK CAPITAL I...MATHEMATICAL DOUBLE-STRUCK CAPITAL M
                | '\u{1D546}'                // MATHEMATICAL DOUBLE-STRUCK CAPITAL O
                | '\u{1D54A}'...'\u{1D550}'  // MATHEMATICAL DOUBLE-STRUCK CAPITAL S...MATHEMATICAL DOUBLE-STRUCK CAPITAL Y
                | '\u{1D552}'...'\u{1D6A5}'  // MATHEMATICAL DOUBLE-STRUCK SMALL A...MATHEMATICAL ITALIC SMALL DOTLESS J
                | '\u{1D6A8}'...'\u{1D6C0}'  // MATHEMATICAL BOLD CAPITAL ALPHA...MATHEMATICAL BOLD CAPITAL OMEGA
                | '\u{1D6C2}'...'\u{1D6DA}'  // MATHEMATICAL BOLD SMALL ALPHA...MATHEMATICAL BOLD SMALL OMEGA
                | '\u{1D6DC}'...'\u{1D6FA}'  // MATHEMATICAL BOLD EPSILON SYMBOL...MATHEMATICAL ITALIC CAPITAL OMEGA
                | '\u{1D6FC}'...'\u{1D714}'  // MATHEMATICAL ITALIC SMALL ALPHA...MATHEMATICAL ITALIC SMALL OMEGA
                | '\u{1D716}'...'\u{1D734}'  // MATHEMATICAL ITALIC EPSILON SYMBOL...MATHEMATICAL BOLD ITALIC CAPITAL OMEGA
                | '\u{1D736}'...'\u{1D74E}'  // MATHEMATICAL BOLD ITALIC SMALL ALPHA...MATHEMATICAL BOLD ITALIC SMALL OMEGA
                | '\u{1D750}'...'\u{1D76E}'  // MATHEMATICAL BOLD ITALIC EPSILON SYMBOL...MATHEMATICAL SANS-SERIF BOLD CAPITAL OMEGA
                | '\u{1D770}'...'\u{1D788}'  // MATHEMATICAL SANS-SERIF BOLD SMALL ALPHA...MATHEMATICAL SANS-SERIF BOLD SMALL OMEGA
                | '\u{1D78A}'...'\u{1D7A8}'  // MATHEMATICAL SANS-SERIF BOLD EPSILON SYMBOL...MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL OMEGA
                | '\u{1D7AA}'...'\u{1D7C2}'  // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL ALPHA...MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL OMEGA
                | '\u{1D7C4}'...'\u{1D7CB}'  // MATHEMATICAL SANS-SERIF BOLD ITALIC EPSILON SYMBOL...MATHEMATICAL BOLD SMALL DIGAMMA
                | '\u{1D7CE}'...'\u{1D7FF}'  // MATHEMATICAL BOLD DIGIT ZERO...MATHEMATICAL MONOSPACE DIGIT NINE
                | '\u{1DA00}'...'\u{1DA36}'  // SIGNWRITING HEAD RIM...SIGNWRITING AIR SUCKING IN
                | '\u{1DA3B}'...'\u{1DA6C}'  // SIGNWRITING MOUTH CLOSED NEUTRAL...SIGNWRITING EXCITEMENT
                | '\u{1DA75}'                // SIGNWRITING UPPER BODY TILTING FROM HIP JOINTS
                | '\u{1DA84}'                // SIGNWRITING LOCATION HEAD NECK
                | '\u{1DA9B}'...'\u{1DA9F}'  // SIGNWRITING FILL MODIFIER-2...SIGNWRITING FILL MODIFIER-6
                | '\u{1DAA1}'...'\u{1DAAF}'  // SIGNWRITING ROTATION MODIFIER-2...SIGNWRITING ROTATION MODIFIER-16
                | '\u{1E000}'...'\u{1E006}'  // COMBINING GLAGOLITIC LETTER AZU...COMBINING GLAGOLITIC LETTER ZHIVETE
                | '\u{1E008}'...'\u{1E018}'  // COMBINING GLAGOLITIC LETTER ZEMLJA...COMBINING GLAGOLITIC LETTER HERU
                | '\u{1E01B}'...'\u{1E021}'  // COMBINING GLAGOLITIC LETTER SHTA...COMBINING GLAGOLITIC LETTER YATI
                | '\u{1E023}'...'\u{1E024}'  // COMBINING GLAGOLITIC LETTER YU...COMBINING GLAGOLITIC LETTER SMALL YUS
                | '\u{1E026}'...'\u{1E02A}'  // COMBINING GLAGOLITIC LETTER YO...COMBINING GLAGOLITIC LETTER FITA
                | '\u{1E800}'...'\u{1E8C4}'  // MENDE KIKAKUI SYLLABLE M001 KI...MENDE KIKAKUI SYLLABLE M060 NYON
                | '\u{1E8D0}'...'\u{1E8D6}'  // MENDE KIKAKUI COMBINING NUMBER TEENS...MENDE KIKAKUI COMBINING NUMBER MILLIONS
                | '\u{1E900}'...'\u{1E943}'  // ADLAM CAPITAL LETTER ALIF...ADLAM SMALL LETTER SHA
                | '\u{1E944}'...'\u{1E94A}'  // ADLAM ALIF LENGTHENER...ADLAM NUKTA
                | '\u{1E950}'...'\u{1E959}'  // ADLAM DIGIT ZERO...ADLAM DIGIT NINE
                | '\u{1EE00}'...'\u{1EE03}'  // ARABIC MATHEMATICAL ALEF...ARABIC MATHEMATICAL DAL
                | '\u{1EE05}'...'\u{1EE1F}'  // ARABIC MATHEMATICAL WAW...ARABIC MATHEMATICAL DOTLESS QAF
                | '\u{1EE21}'...'\u{1EE22}'  // ARABIC MATHEMATICAL INITIAL BEH...ARABIC MATHEMATICAL INITIAL JEEM
                | '\u{1EE24}'                // ARABIC MATHEMATICAL INITIAL HEH
                | '\u{1EE27}'                // ARABIC MATHEMATICAL INITIAL HAH
                | '\u{1EE29}'...'\u{1EE32}'  // ARABIC MATHEMATICAL INITIAL YEH...ARABIC MATHEMATICAL INITIAL QAF
                | '\u{1EE34}'...'\u{1EE37}'  // ARABIC MATHEMATICAL INITIAL SHEEN...ARABIC MATHEMATICAL INITIAL KHAH
                | '\u{1EE39}'                // ARABIC MATHEMATICAL INITIAL DAD
                | '\u{1EE3B}'                // ARABIC MATHEMATICAL INITIAL GHAIN
                | '\u{1EE42}'                // ARABIC MATHEMATICAL TAILED JEEM
                | '\u{1EE47}'                // ARABIC MATHEMATICAL TAILED HAH
                | '\u{1EE49}'                // ARABIC MATHEMATICAL TAILED YEH
                | '\u{1EE4B}'                // ARABIC MATHEMATICAL TAILED LAM
                | '\u{1EE4D}'...'\u{1EE4F}'  // ARABIC MATHEMATICAL TAILED NOON...ARABIC MATHEMATICAL TAILED AIN
                | '\u{1EE51}'...'\u{1EE52}'  // ARABIC MATHEMATICAL TAILED SAD...ARABIC MATHEMATICAL TAILED QAF
                | '\u{1EE54}'                // ARABIC MATHEMATICAL TAILED SHEEN
                | '\u{1EE57}'                // ARABIC MATHEMATICAL TAILED KHAH
                | '\u{1EE59}'                // ARABIC MATHEMATICAL TAILED DAD
                | '\u{1EE5B}'                // ARABIC MATHEMATICAL TAILED GHAIN
                | '\u{1EE5D}'                // ARABIC MATHEMATICAL TAILED DOTLESS NOON
                | '\u{1EE5F}'                // ARABIC MATHEMATICAL TAILED DOTLESS QAF
                | '\u{1EE61}'...'\u{1EE62}'  // ARABIC MATHEMATICAL STRETCHED BEH...ARABIC MATHEMATICAL STRETCHED JEEM
                | '\u{1EE64}'                // ARABIC MATHEMATICAL STRETCHED HEH
                | '\u{1EE67}'...'\u{1EE6A}'  // ARABIC MATHEMATICAL STRETCHED HAH...ARABIC MATHEMATICAL STRETCHED KAF
                | '\u{1EE6C}'...'\u{1EE72}'  // ARABIC MATHEMATICAL STRETCHED MEEM...ARABIC MATHEMATICAL STRETCHED QAF
                | '\u{1EE74}'...'\u{1EE77}'  // ARABIC MATHEMATICAL STRETCHED SHEEN...ARABIC MATHEMATICAL STRETCHED KHAH
                | '\u{1EE79}'...'\u{1EE7C}'  // ARABIC MATHEMATICAL STRETCHED DAD...ARABIC MATHEMATICAL STRETCHED DOTLESS BEH
                | '\u{1EE7E}'                // ARABIC MATHEMATICAL STRETCHED DOTLESS FEH
                | '\u{1EE80}'...'\u{1EE89}'  // ARABIC MATHEMATICAL LOOPED ALEF...ARABIC MATHEMATICAL LOOPED YEH
                | '\u{1EE8B}'...'\u{1EE9B}'  // ARABIC MATHEMATICAL LOOPED LAM...ARABIC MATHEMATICAL LOOPED GHAIN
                | '\u{1EEA1}'...'\u{1EEA3}'  // ARABIC MATHEMATICAL DOUBLE-STRUCK BEH...ARABIC MATHEMATICAL DOUBLE-STRUCK DAL
                | '\u{1EEA5}'...'\u{1EEA9}'  // ARABIC MATHEMATICAL DOUBLE-STRUCK WAW...ARABIC MATHEMATICAL DOUBLE-STRUCK YEH
                | '\u{1EEAB}'...'\u{1EEBB}'  // ARABIC MATHEMATICAL DOUBLE-STRUCK LAM...ARABIC MATHEMATICAL DOUBLE-STRUCK GHAIN
                | '\u{20000}'...'\u{2A6D6}'  // CJK UNIFIED IDEOGRAPH-20000...CJK UNIFIED IDEOGRAPH-2A6D6
                | '\u{2A700}'...'\u{2B734}'  // CJK UNIFIED IDEOGRAPH-2A700...CJK UNIFIED IDEOGRAPH-2B734
                | '\u{2B740}'...'\u{2B81D}'  // CJK UNIFIED IDEOGRAPH-2B740...CJK UNIFIED IDEOGRAPH-2B81D
                | '\u{2B820}'...'\u{2CEA1}'  // CJK UNIFIED IDEOGRAPH-2B820...CJK UNIFIED IDEOGRAPH-2CEA1
                | '\u{2CEB0}'...'\u{2EBE0}'  // CJK UNIFIED IDEOGRAPH-2CEB0...CJK UNIFIED IDEOGRAPH-2EBE0
                | '\u{2F800}'...'\u{2FA1D}'  // CJK COMPATIBILITY IDEOGRAPH-2F800...CJK COMPATIBILITY IDEOGRAPH-2FA1D
                | '\u{E0100}'...'\u{E01EF}'  // VARIATION SELECTOR-17...VARIATION SELECTOR-256
            => true,
            _ => false,
        })
    }

    /// Advances past any whitespace or JavaScript comments.
    #[inline]
    pub fn skip_ws(&mut self) -> (&'s str, bool) {
        let start = self.loc;
        loop {
            match self.here {
                Some(c) => match c {
                      '\u{0009}' // CHARACTER TABULATION
                    | '\u{000B}' // LINE TABULATION
                    | '\u{000C}' // FORM FEED
                    | '\u{0020}' // SPACE
                    | '\u{00A0}' // NO-BREAK SPACE
                    | '\u{FEFF}' // ZERO WIDTH NO-BREAK SPACE

                    | '\u{000A}' // LINE FEED (LF)          <LF>
                    | '\u{000D}' // CARRIAGE RETURN (CR)    <CR>
                    | '\u{2028}' // LINE SEPARATOR          <LS>
                    | '\u{2029}' // PARAGRAPH SEPARATOR     <PS>

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
                    //   '\u{000A}' // LINE FEED (LF)       <LF>
                    // | '\u{000D}' // CARRIAGE RETURN (CR) <CR>
                    // | '\u{2028}' // LINE SEPARATOR       <LS>
                    // | '\u{2029}' // PARAGRAPH SEPARATOR  <PS>
                    // => {
                    //     self.advance();
                    //     nl = true;
                    // }
                    '/' => match self.next {
                        Some('*') => {
                            self.advance();
                            self.advance();
                            'outer: loop {
                                match self.here {
                                    Some('*') => {
                                        loop {
                                            self.advance();
                                            match self.here {
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
                                                    panic!("unterminated multiline comment")
                                                }
                                            }
                                        }
                                    }
                                    Some(_) => {
                                        self.advance();
                                    }
                                    None => {
                                        panic!("unterminated multiline comment")
                                    }
                                }
                            }
                        },
                        Some('/') => {
                            self.advance();
                            self.advance();
                            self.skip_while(|c| match c {
                                  '\u{000A}' // LINE FEED (LF)          <LF>
                                | '\u{000D}' // CARRIAGE RETURN (CR)    <CR>
                                | '\u{2028}' // LINE SEPARATOR          <LS>
                                | '\u{2029}' // PARAGRAPH SEPARATOR     <PS>
                                => false,
                                _ => true,
                            });
                        },
                        _ => break,
                    },
                    _ => break,
                },
                None => break,
            }
        }
        (self.str_from(start.pos), start.row < self.loc.row)
    }

    /// The bytewise position of the current character.
    #[inline]
    pub fn pos(&self) -> usize {
        self.loc.pos
    }

    /// The location of the current character.
    #[inline]
    pub fn loc(&self) -> Loc {
        self.loc
    }

    /// The current character, or `None` if the stream has reached the end of its input.
    #[inline]
    pub fn here(&self) -> Option<char> {
        self.here
    }

    /// The next character, or `None` if the stream has reached the end of its input.
    #[inline]
    pub fn next(&self) -> Option<char> {
        self.next
    }

    /// The entire input source code.
    #[inline]
    pub fn input(&self) -> &'s str {
        self.input
    }

    /// A slice of the input source code from the byte at `start` up to, but not including the first byte of the current character.
    #[inline]
    pub fn str_from(&self, start: usize) -> &'s str {
        &self.input[start..self.loc.pos]
    }

    /// A slice of the input source code from the byte at `start` up to, but not including the byte at `end`.
    #[inline]
    pub fn str_range(&self, start: usize, end: usize) -> &'s str {
        &self.input[start..end]
    }

    // TODO pub fn advance_by(&mut self, n: usize) -> &'s str {}

    /// Advances the stream by one character, returning the character advanced past or `None` if the stream was already at the end of its input.
    pub fn advance(&mut self) -> Option<char> {
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
            let new_pos = self.next_pos + self.next_width;
            self.loc.pos = mem::replace(&mut self.next_pos, new_pos);

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
mod test {
    extern crate test;

    use super::*;
    use std::{fs, panic};
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
        panic::catch_unwind(|| {
            for _ in Lexer::new_unnamed(source) {}
        }).unwrap_err();
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
            let mut stream = Stream::new(&contents);
            while let Some(_) = stream.here() {
                stream.advance();
            }
        });
    }
}
