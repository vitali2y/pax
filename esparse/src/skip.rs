//! Skip syntactic constructs without retaining their structure.
//!
//! In general, skipping functions are overly permissive, i.e., they may accept invalid constructs, but they will never reject valid ones. They will also never skip too far.

use std::{fmt, error};
use ast;
use lex::{self, Tt};

macro_rules! expected {
    ($lex:expr, $msg:expr) => {{
        return Err(Error {
            kind: ErrorKind::Expected($msg),
            span: $lex.here().span.with_owned(),
        })
    }};
}

/// The result type for skipping functions.
pub type Result<T> = ::std::result::Result<T, Error>;

/// The error type for skipping functions.
#[derive(Debug)]
pub struct Error {
    /// The kind of error.
    pub kind: ErrorKind,
    /// The source code region in which the error appeared.
    pub span: ast::SpanT<String>,
}

/// The specific kind of error that occurred.
#[derive(Debug)]
pub enum ErrorKind {
    /// A syntactic construct was expected, but not found.
    ///
    /// The slice names the expected construct.
    Expected(&'static str),
}

impl error::Error for Error {
    fn description(&self) -> &str {
        match self.kind {
            ErrorKind::Expected(_) => "expected something, but got something else",
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{} at {}", self.kind, self.span)
    }
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ErrorKind::Expected(s) => write!(f, "expected {}", s),
        }
    }
}

/// Binding strength for [`skip::expr`](fn.expr.html).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Prec {
    /// Skip only a [PrimaryExpression](https://tc39.github.io/ecma262/#prod-PrimaryExpression).
    ///
    /// Primary expressions are the atomic units of the expression grammar, e.g., identifiers, literals, functions and classes, templates, etc.
    Primary,
    /// Skip an [AssignmentExpression](https://tc39.github.io/ecma262/#prod-AssignmentExpression).
    ///
    /// Consumes all operators except the comma operator.
    NoComma,
    /// Skip an [Expression](https://tc39.github.io/ecma262/#prod-Expression).
    ///
    /// Consumes all operators.
    Any,
}

/// Skips an expression from `lex` with the given binding strength (precedence).
pub fn expr<'f, 's>(lex: &mut lex::Lexer<'f, 's>, prec: Prec) -> Result<()> {
    // intentionally over-permissive
    let mut had_primary = false;
    loop {
        // skip prefix ops
        eat!(lex,
            Tt::Plus |
            Tt::Minus |
            Tt::Tilde |
            Tt::Bang |
            Tt::Delete |
            Tt::Void |
            Tt::Typeof |
            Tt::DotDotDot |
            Tt::PlusPlus |
            Tt::MinusMinus => {},
            Tt::New => eat!(lex,
                Tt::Dot => eat!(lex,
                    Tt::Id("target") => {
                        had_primary = true;
                        break
                    },
                    _ => expected!(lex, "keyword 'target'"),
                ),
                _ => {},
            ),
            Tt::Id("async") => {
                if lex.here().nl_before {
                    return Ok(())
                }
                eat!(lex,
                    Tt::Function => {
                        eat!(lex,
                            Tt::Id(_) => {},
                            _ => {},
                        );
                        eat!(lex,
                            Tt::Lparen => balanced_parens(lex, 1)?,
                            _ => expected!(lex, "formal parameter list"),
                        );
                        eat!(lex,
                            Tt::Lbrace => balanced_braces(lex, 1)?,
                            _ => expected!(lex, "function body"),
                        );
                        had_primary = true;
                        break
                    },
                    _ => {},
                );
            },
            Tt::Await => {
                println!("at {}", lex.here().tt);
                if lex.here().nl_before {
                    return Ok(())
                }
            },
            Tt::Yield => {
                if lex.here().nl_before {
                    return Ok(())
                }
                eat!(lex,
                    Tt::Star => {},
                    _ => {},
                );
            },
            _ => break,
        );
    }
    // skip primary expr
    if !had_primary {
        eat!(lex,
            Tt::This |
            Tt::Super |
            Tt::Id(_) |
            Tt::Null |
            Tt::True |
            Tt::False |
            Tt::NumLitBin(_) |
            Tt::NumLitOct(_) |
            Tt::NumLitDec(_) |
            Tt::NumLitHex(_) |
            Tt::StrLitDbl(_) |
            Tt::StrLitSgl(_) |
            Tt::RegExpLit(_, _) |
            Tt::TemplateNoSub(_) => {},

            Tt::TemplateStart(_) => balanced_templates(lex, 1)?,
            Tt::Lbracket => balanced_brackets(lex, 1)?,
            Tt::Lbrace => balanced_braces(lex, 1)?,
            Tt::Lparen => balanced_parens(lex, 1)?,

            Tt::Function => {
                eat!(lex,
                    Tt::Star => {},
                    _ => {},
                );
                eat!(lex,
                    Tt::Id(_) => {},
                    _ => {},
                );
                eat!(lex,
                    Tt::Lparen => balanced_parens(lex, 1)?,
                    _ => expected!(lex, "formal parameter list"),
                );
                eat!(lex,
                    Tt::Lbrace => balanced_braces(lex, 1)?,
                    _ => expected!(lex, "function body"),
                );
            },
            Tt::Class => {
                eat!(lex,
                    Tt::Id(_) => {},
                    _ => {},
                );
                eat!(lex,
                    Tt::Extends => expr(lex, Prec::NoComma)?,
                    _ => {},
                );
                eat!(lex,
                    Tt::Lbrace => balanced_braces(lex, 1)?,
                    _ => expected!(lex, "class body"),
                );
            },
            // TODO Tt::Id("async") =>
            _ => expected!(lex, "primary expression"),
        );
    }
    if prec != Prec::Primary {
        // skip postfix and infix ops
        loop {
            eat!(lex => tok,
                Tt::PlusPlus |
                Tt::MinusMinus if !tok.nl_before => {},

                Tt::Dot => eat!(lex,
                    Tt::Id(_) => {},
                    _ => expected!(lex, "member name"),
                ),
                Tt::TemplateNoSub(_) => {},
                Tt::TemplateStart(_) => balanced_templates(lex, 1)?,
                Tt::Lbracket => balanced_brackets(lex, 1)?,
                Tt::Lparen => balanced_parens(lex, 1)?,

                Tt::StarStar |
                Tt::Star |
                Tt::Slash |
                Tt::Percent |
                Tt::Plus |
                Tt::Minus |
                Tt::LtLt |
                Tt::GtGt |
                Tt::GtGtGt |
                Tt::Lt |
                Tt::Gt |
                Tt::LtEq |
                Tt::GtEq |
                Tt::Instanceof |
                Tt::In |
                Tt::EqEq |
                Tt::BangEq |
                Tt::EqEqEq |
                Tt::BangEqEq |
                Tt::And |
                Tt::Or |
                Tt::Circumflex |
                Tt::AndAnd |
                Tt::OrOr |
                Tt::Eq |
                Tt::StarEq |
                Tt::SlashEq |
                Tt::PercentEq |
                Tt::PlusEq |
                Tt::MinusEq |
                Tt::LtLtEq |
                Tt::GtGtEq |
                Tt::GtGtGtEq |
                Tt::AndEq |
                Tt::CircumflexEq |
                Tt::OrEq |
                Tt::StarStarEq |

                // TODO async arrow function
                Tt::EqGt |
                // below might be better for ternary exprs
                Tt::Question |
                Tt::Colon => expr(lex, Prec::Primary)?,

                Tt::Comma if prec == Prec::Any => expr(lex, Prec::Primary)?,

                // Tt::Question => {
                //     expr(lex, prec)?;
                //     eat!(lex,
                //         Tt::Colon => {},
                //         _ => expected!(lex, "':' in ternary expression"),
                //     );
                //     expr(lex, prec::Primary)?;
                // },

                _ => break,
            );
        }
    }
    Ok(())
}

#[inline]
pub fn balanced_templates<'f, 's>(lex: &mut lex::Lexer<'f, 's>, nesting: usize) -> Result<()> {
    balanced(
        lex,
        nesting,
        |tt| matches!(tt, Tt::TemplateStart(_)),
        |tt| matches!(tt, Tt::TemplateEnd(_)),
        "end of template literal",
    )
}

#[inline]
pub fn balanced_braces<'f, 's>(lex: &mut lex::Lexer<'f, 's>, nesting: usize) -> Result<()> {
    balanced(
        lex,
        nesting,
        |tt| tt == Tt::Lbrace,
        |tt| tt == Tt::Rbrace,
        "'}'",
    )
}

#[inline]
pub fn balanced_brackets<'f, 's>(lex: &mut lex::Lexer<'f, 's>, nesting: usize) -> Result<()> {
    balanced(
        lex,
        nesting,
        |tt| tt == Tt::Lbracket,
        |tt| tt == Tt::Rbracket,
        "']'",
    )
}

#[inline]
pub fn balanced_parens<'f, 's>(lex: &mut lex::Lexer<'f, 's>, nesting: usize) -> Result<()> {
    balanced(
        lex,
        nesting,
        |tt| tt == Tt::Lparen,
        |tt| tt == Tt::Rparen,
        "')'",
    )
}

#[inline]
pub fn balanced<'f, 's, L, R>(lex: &mut lex::Lexer<'f, 's>, mut nesting: usize, mut l: L, mut r: R, expect: &'static str) -> Result<()> where
L: FnMut(Tt) -> bool,
R: FnMut(Tt) -> bool {
    debug_assert!(nesting > 0);
    #[cold]
    #[inline(never)]
    fn unbalanced<'f, 's>(lex: &mut lex::Lexer<'f, 's>, expect: &'static str) -> Result<()> {
        expected!(lex, expect)
    }
    while nesting > 0 {
        let tt = lex.advance().tt;
        if l(tt) {
            nesting += 1;
        } else if r(tt) {
            nesting -= 1;
        } else if tt == Tt::Eof {
            return unbalanced(lex, expect)
        }
    }
    Ok(())
}

#[cfg(test)]
mod test {
    use skip::{self, Prec};
    use lex::{self, Tt};

    fn assert_skips_expr(source: &str, prec: Prec) {
        let mut cleaned = String::new();
        let mut iter = source.splitn(2, '@');
        let prefix = iter.next().unwrap();
        let suffix = iter.next().unwrap();
        cleaned.push_str(prefix);
        cleaned.push_str(suffix);

        let mut lexer = lex::Lexer::new_unnamed(&cleaned);
        skip::expr(&mut lexer, prec).unwrap();
        let here = lexer.here();
        let here_pos = here.span.start.pos - here.ws_before.len();
        assert!(
            here_pos <= prefix.len() &&
            prefix.len() <= here.span.start.pos,
            "expected skip::expr to skip to:\n{}@{}\nbut it skipped to:\n{}@{}",
            &cleaned[..prefix.len()], &cleaned[prefix.len()..],
            &cleaned[..here_pos], &cleaned[here_pos..],
        );
    }

    #[test]
    fn test_skip_expr_operator() {
        assert_skips_expr("1@ + 2 - 3 * 4 / 5 % 6 ** 7, next", Prec::Primary);
        assert_skips_expr("1 + 2 - 3 * 4 / 5 % 6 ** 7 @, next", Prec::NoComma);
        assert_skips_expr("1 + 2 - 3 * 4 / 5 % 6 ** 7, next@", Prec::Any);
        assert_skips_expr("b = (x, y, z) => x + y + z @, next", Prec::NoComma);
        assert_skips_expr("1 + 2 @; 3 + 4", Prec::NoComma);
        assert_skips_expr("1 + 2 @; 3 + 4", Prec::Any);
        assert_skips_expr("1 + 2 @\n 3 + 4", Prec::NoComma);
        assert_skips_expr("1 + 2 @\n 3 + 4", Prec::Any);
        assert_skips_expr("a(b, c, d).e < f > g <= h >= i == j != k === l !== m + n - o * p % q ** r++ << s-- >> t >>> u & v | w ^ x && y || z / _ @, next", Prec::NoComma);
        assert_skips_expr("x = x += x -= x *= x %= x **= x <<= x >>= x >>>= x &= x |= x ^= x /= x @, next", Prec::NoComma);
        assert_skips_expr("x in y instanceof z @, next", Prec::NoComma);
    }

    #[test]
    fn test_skip_expr_ternary() {
        assert_skips_expr("a ? b ? c : d : e ? f : g @, next", Prec::NoComma);
    }

    #[test]
    fn test_skip_expr_postfix() {
        assert_skips_expr("a @\n ++b", Prec::NoComma);
        assert_skips_expr("a @\n ++b", Prec::Any);
        assert_skips_expr("a @\n --b", Prec::NoComma);
        assert_skips_expr("a @\n --b", Prec::Any);
        assert_skips_expr("a++ @\n b", Prec::NoComma);
        assert_skips_expr("a++ @\n b", Prec::Any);
        assert_skips_expr("a-- @\n b", Prec::NoComma);
        assert_skips_expr("a-- @\n b", Prec::Any);
    }

    #[test]
    fn test_skip_expr_lhs() {
        assert_skips_expr("a[x, y]@", Prec::NoComma);
        assert_skips_expr("a.b.c[x, y].d@", Prec::NoComma);
        assert_skips_expr("temp`late`@", Prec::NoComma);
        assert_skips_expr("temp`late${with}subs`@", Prec::NoComma);
        assert_skips_expr("super.a()@", Prec::NoComma);
        assert_skips_expr("super['key']@", Prec::NoComma);
        assert_skips_expr("new a.b.C('something')@", Prec::NoComma);
    }

    #[test]
    fn test_skip_expr_primary_prefix() {
        assert_skips_expr("+-~!...++--a@", Prec::Primary);
        assert_skips_expr("void typeof delete a@", Prec::Primary);
    }

    #[test]
    fn test_skip_expr_primary_basic() {
        assert_skips_expr("ident@", Prec::Primary);
        assert_skips_expr("this@", Prec::Primary);
        assert_skips_expr("super@", Prec::Primary);
        assert_skips_expr("new.target@", Prec::Primary);
    }

    #[test]
    fn test_skip_expr_primary_literal() {
        assert_skips_expr("1@", Prec::Primary);
        assert_skips_expr("0b1@", Prec::Primary);
        assert_skips_expr("0o1@", Prec::Primary);
        assert_skips_expr("0x1@", Prec::Primary);
        assert_skips_expr("null@", Prec::Primary);
        assert_skips_expr("true@", Prec::Primary);
        assert_skips_expr("false@", Prec::Primary);
        assert_skips_expr("'test'@", Prec::Primary);
        assert_skips_expr("\"test\"@", Prec::Primary);
        assert_skips_expr("/foo/y@", Prec::Primary);
    }

    #[test]
    fn test_skip_expr_primary_balanced() {
        assert_skips_expr("(1, (2 + 3), a(4, 5))@", Prec::Primary);
        assert_skips_expr("[]@", Prec::Primary);
        assert_skips_expr("[1, [2], 3]@[4]", Prec::Primary);
        assert_skips_expr("{this: {that: {another: (1)}}}@[here]", Prec::Primary);
    }

    #[test]
    fn test_skip_expr_primary_function() {
        assert_skips_expr("function() {with, [some, more], {commas}}@", Prec::Primary);
        assert_skips_expr("function named() {}@", Prec::Primary);
        assert_skips_expr("function*() {}@", Prec::Primary);
        assert_skips_expr("function* generator() {}@", Prec::Primary);
    }

    #[test]
    fn test_skip_expr_primary_class() {
        assert_skips_expr("class { method() {} }@", Prec::Primary);
        assert_skips_expr("class extends Base { method() {} }@", Prec::Primary);
        assert_skips_expr("class Named { method() {} }@", Prec::Primary);
        assert_skips_expr("class Named extends Base { method() {} }@", Prec::Primary);
    }

    #[test]
    fn test_skip_expr_primary_template() {
        assert_skips_expr("`template`@", Prec::Primary);
        assert_skips_expr("`template ${with} some ${subs}`@", Prec::Primary);
        assert_skips_expr("`template ${`with`} some ${`nested ${subs}`}`@", Prec::Primary);
    }

    #[test]
    fn test_skip_expr_arrow() {
        assert_skips_expr("x => x + 1@", Prec::NoComma);
        assert_skips_expr("(x, y, z) => y => z@", Prec::NoComma);
        assert_skips_expr("a => ({})@", Prec::NoComma);
        assert_skips_expr("(x, y, z) => ({})@", Prec::NoComma);
        assert_skips_expr("abc => ({ x: 1, y: 2 })@", Prec::NoComma);
        assert_skips_expr("(x, y, z) => ({ x: 1, y: 2 })@", Prec::NoComma);
        assert_skips_expr("id => {}@", Prec::NoComma);
        assert_skips_expr("(x, y, z) => {}@", Prec::NoComma);
        assert_skips_expr("s => { call(1, 2) }@", Prec::NoComma);
        assert_skips_expr("(x, y, z) => { call(1, 2) }@", Prec::NoComma);
        assert_skips_expr("() => 1@", Prec::NoComma);
    }

    #[test]
    fn test_skip_expr_primary_async() {
        assert_skips_expr("async function() {}@", Prec::Primary);
        assert_skips_expr("async function named() {}@", Prec::Primary);
        assert_skips_expr("async @\n function named() {}", Prec::Primary);
    }

    #[test]
    fn test_skip_expr_async_arrow() {
        assert_skips_expr("async x => x + 1@", Prec::NoComma);
        assert_skips_expr("async (x, y) => x + y@", Prec::NoComma);
        assert_skips_expr("async @\n x => x", Prec::NoComma);
        assert_skips_expr("async @\n (x, y) => x", Prec::NoComma);
    }

    #[test]
    fn test_skip_expr_primary_await() {
        assert_skips_expr("await a@", Prec::Primary);
        assert_skips_expr("await @\n a", Prec::Primary);
    }

    #[test]
    fn test_skip_balanced() {
        let mut lexer = lex::Lexer::new_unnamed("(((-a)())((()(b)())))c)");
        lexer.advance();
        skip::balanced(
            &mut lexer,
            1,
            |tt| tt == Tt::Lparen,
            |tt| tt == Tt::Rparen,
            "')'",
        ).unwrap();
        assert_eq!(lexer.here().tt, Tt::Id("c"));

        let mut lexer = lex::Lexer::new_unnamed("(((-a)())((()(b)())))c)");
        lexer.advance();
        skip::balanced_parens(&mut lexer, 1).unwrap();
        assert_eq!(lexer.here().tt, Tt::Id("c"));

        let mut lexer = lex::Lexer::new_unnamed("[[[-a][]][[[][b][]]]]c]");
        lexer.advance();
        skip::balanced_brackets(&mut lexer, 1).unwrap();
        assert_eq!(lexer.here().tt, Tt::Id("c"));

        let mut lexer = lex::Lexer::new_unnamed("{{{-a}{}}{{{}{b}{}}}}c}");
        lexer.advance();
        skip::balanced_braces(&mut lexer, 1).unwrap();
        assert_eq!(lexer.here().tt, Tt::Id("c"));

        let mut lexer = lex::Lexer::new_unnamed("`${`${`${-a}` + `${v}`}` - `${`${`${x}` * `${b}` * `${z}`}`}`}`c}`");
        lexer.advance(); // skip first Tt::TemplateStart
        skip::balanced_templates(&mut lexer, 1).unwrap();
        assert_eq!(lexer.here().tt, Tt::Id("c"));
    }

    #[test]
    #[should_panic]
    fn test_skip_balanced_eof() {
        let mut lexer = lex::Lexer::new_unnamed("((");
        skip::balanced(
            &mut lexer,
            1,
            |tt| tt == Tt::Lparen,
            |tt| tt == Tt::Rparen,
            "')'",
        ).unwrap();
    }
}
