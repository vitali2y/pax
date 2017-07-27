use std::fmt;
use std::fmt::Write;
use std::borrow::Cow;
use std::collections::HashSet;

use esparse;
use esparse::lex::{self, Tt};

macro_rules! expected {
    ($lex:expr, $msg:expr) => {{
        return Err(Error {
            kind: ErrorKind::Expected($msg),
            span: $lex.here().span.with_owned(),
        })
    }};
}

#[derive(Debug)]
pub enum Export<'s> {
    Default(&'s str),
    AllFrom(Cow<'s, str>),
    Named(Vec<ExportSpec<'s>>),
    NamedFrom(Vec<ExportSpec<'s>>, Cow<'s, str>),
}
#[derive(Debug)]
pub struct ExportSpec<'s> {
    bind: &'s str,
    name: &'s str,
}
impl<'s> ExportSpec<'s> {
    pub fn new(bind: &'s str, name: &'s str) -> Self {
        ExportSpec {
            name,
            bind,
        }
    }
    pub fn same(name: &'s str) -> Self {
        ExportSpec::new(name, name)
    }
}

#[derive(Debug)]
pub struct Import<'s> {
    module_source: &'s str,
    module: Cow<'s, str>,
    default_bind: Option<&'s str>,
    binds: Bindings<'s>,
}
impl<'s> Import<'s> {
    pub fn new(module_source: &'s str, module: Cow<'s, str>) -> Self {
        Import {
            module_source,
            module,
            default_bind: None,
            binds: Bindings::None,
        }
    }
}

#[derive(Debug)]
pub enum Bindings<'s> {
    None,
    NameSpace(&'s str),
    Named(Vec<ImportSpec<'s>>),
}
#[derive(Debug)]
pub struct ImportSpec<'s> {
    name: &'s str,
    bind: &'s str,
}
impl<'s> ImportSpec<'s> {
    pub fn new(name: &'s str, bind: &'s str) -> Self {
        ImportSpec {
            name,
            bind,
        }
    }
    pub fn same(name: &'s str) -> Self {
        ImportSpec::new(name, name)
    }
}

#[derive(Debug)]
pub struct CjsModule<'s> {
    pub source_prefix: String,
    pub source: String,
    pub source_suffix: String,
    pub deps: HashSet<Cow<'s, str>>,
}
pub type Result<T> = ::std::result::Result<T, Error>;
#[derive(Debug)]
pub struct Error {
    kind: ErrorKind,
    span: esparse::ast::SpanT<String>,
}
#[derive(Debug)]
pub enum ErrorKind {
    Expected(&'static str),
    ParseStrLitError(lex::ParseStrLitError),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            ErrorKind::Expected(s) => write!(f, "expected {}", s)?,
            ErrorKind::ParseStrLitError(ref error) => write!(f, "invalid string literal: {}", error)?,
        }
        writeln!(f,
            " at {}:{},{}",
            self.span.file_name,
            self.span.start.row + 1,
            self.span.start.col + 1,
        )
    }
}

pub fn module_to_cjs<'f, 's>(lex: &mut lex::Lexer<'f, 's>, allow_require: bool) -> Result<CjsModule<'s>> {
    let mut source = String::new();
    let mut deps = HashSet::new();
    let mut imports = Vec::new();
    let mut exports = Vec::new();
    // TODO source map lines won't match up when module string literal contains newlines
    loop {
        eat!(lex => tok { source.push_str(tok.ws_before) },
            Tt::Export => {
                let export = parse_export(lex, &mut source)?;
                exports.push(export);
            },
            Tt::Import => {
                let import = parse_import(lex, &mut source)?;
                imports.push(import);
            },
            Tt::Id("require") if allow_require => {
                let start_pos = tok.span.start.pos;
                eat!(lex,
                    Tt::Lparen => eat!(lex,
                        Tt::StrLitSgl(dep_source) |
                        Tt::StrLitDbl(dep_source) => eat!(lex,
                            Tt::Rparen => {
                                deps.insert(match lex::str_lit_value(dep_source) {
                                    Ok(dep) => dep,
                                    Err(error) => return Err(Error {
                                        kind: ErrorKind::ParseStrLitError(error),
                                        span: tok.span.with_owned(),
                                    }),
                                });
                            },
                            _ => {},
                        ),
                        _ => {},
                    ),
                    _ => {},
                );

                let here = lex.here();
                let end_pos = here.span.start.pos - here.ws_before.len();
                source.push_str(&lex.input()[start_pos..end_pos]);
            },
            Tt::Eof => break,
            _ => {
                let tok = lex.advance();
                write!(source, "{}{}", tok.ws_before, tok.tt).unwrap();
            },
        );
    }

    let is_module = !allow_require || !exports.is_empty() || !imports.is_empty();
    let mut source_prefix = String::new();

    if is_module {
        write!(source_prefix, "Object.defineProperty(exports, '__esModule', {{value: true}})\n").unwrap();
    }

    if !imports.is_empty() {
        write!(source_prefix, "with (function() {{").unwrap();
        for (i, import) in imports.iter().enumerate() {
            write!(source_prefix, "\n  const __module{} = require._esModule({})", i, import.module_source).unwrap();
        }
        write!(source_prefix, "\n  return Object.freeze(Object.create(null, {{\n    [Symbol.toStringTag]: {{value: 'ModuleImports'}},").unwrap();
        for (i, import) in imports.iter().enumerate() {
            if let Some(bind) = import.default_bind {
                write!(
                    source_prefix,
                    "\n    {}: {{get() {{return __module{}.default}}, enumerable: true}},",
                    bind,
                    i,
                ).unwrap();
            }
            match import.binds {
                Bindings::None => {}
                Bindings::NameSpace(bind) => {
                    write!(
                        source_prefix,
                        "\n    {}: {{value: __module{}, enumerable: true}},",
                        bind,
                        i,
                    ).unwrap();
                }
                Bindings::Named(ref specs) => {
                    for spec in specs {
                        write!(
                            source_prefix,
                            "\n    {}: {{get() {{return __module{}.{}}}, enumerable: true}},",
                            spec.bind,
                            i,
                            spec.name,
                        ).unwrap();
                    }
                }
            }
        }
        write!(source_prefix, "\n  }}))\n}}()) ").unwrap();
    }

    write!(source_prefix, "~function() {{").unwrap();

    if is_module {
        write!(source_prefix, "\n'use strict';\n").unwrap();
    }

    if !exports.is_empty() {
        write!(source_prefix, "Object.defineProperties(exports, {{").unwrap();
        for export in &exports {
            match *export {
                Export::Default(bind) => {
                    write!(
                        source_prefix,
                        "\n  default: {{get() {{return {}}}, enumerable: true}},",
                        bind,
                    ).unwrap();
                }
                Export::AllFrom(_) => unimplemented!(),
                Export::Named(ref specs) => {
                    for spec in specs {
                        write!(
                            source_prefix,
                            "\n  {}: {{get() {{return {}}}, enumerable: true}},",
                            spec.name,
                            spec.bind,
                        ).unwrap();
                    }
                }
                Export::NamedFrom(_, _) => unimplemented!(),
            }
        }
        write!(source_prefix, "\n}});\n").unwrap();
    }

    for import in imports {
        deps.insert(import.module);
    }
    Ok(CjsModule {
        source_prefix,
        source,
        source_suffix: "}()".to_owned(),
        deps,
    })
}

#[inline(always)]
fn parse_export<'f, 's>(lex: &mut lex::Lexer<'f, 's>, source: &mut String) -> Result<Export<'s>> {
    eat!(lex => tok { source.push_str(tok.ws_before) },
        Tt::Default => {
            eat!(lex => tok { write!(source, "{}{}", tok.ws_before, tok.tt).unwrap(); },
                Tt::Class => {
                    let name = eat!(lex => tok { write!(source, "{}{}", tok.ws_before, tok.tt).unwrap(); },
                        Tt::Id(name) => name,
                        _ => expected!(lex, "class name"),
                    );
                    Ok(Export::Default(name))
                },
                Tt::Function => {
                    eat!(lex => tok { write!(source, "{}{}", tok.ws_before, tok.tt).unwrap(); },
                        Tt::Star => {},
                        _ => {},
                    );
                    let name = eat!(lex => tok { write!(source, "{}{}", tok.ws_before, tok.tt).unwrap(); },
                        Tt::Id(name) => name,
                        _ => expected!(lex, "function name"),
                    );
                    Ok(Export::Default(name))
                },
                _ => {
                    source.push_str("const __default = ");
                    // skip_expr(lex, Prec::NoComma)?;
                    Ok(Export::Default("__default"))
                },
            )
        },
        Tt::Star => eat!(lex => tok { source.push_str(tok.ws_before) },
            Tt::Id("from") => eat!(lex => tok { source.push_str(tok.ws_before) },
                Tt::StrLitSgl(module) |
                Tt::StrLitDbl(module) => {
                    Ok(Export::AllFrom(match lex::str_lit_value(module) {
                        Ok(module) => module,
                        Err(error) => return Err(Error {
                            kind: ErrorKind::ParseStrLitError(error),
                            span: tok.span.with_owned(),
                        }),
                    }))
                },
                _ => expected!(lex, "module name (string literal)"),
            ),
            _ => expected!(lex, "keyword 'from'"),
        ),
        Tt::Lbrace => {
            let mut exports = Vec::new();
            loop {
                // TODO export {default as default}
                eat!(lex => tok { source.push_str(tok.ws_before) },
                    Tt::Id(bind) => eat!(lex => tok { source.push_str(tok.ws_before) },
                        Tt::Id("as") => eat!(lex => tok { source.push_str(tok.ws_before) },
                            Tt::Id(name) => {
                                exports.push(ExportSpec::new(bind, name));
                                eat!(lex => tok { source.push_str(tok.ws_before) },
                                    Tt::Rbrace => break,
                                    Tt::Comma => {},
                                    _ => expected!(lex, "',' or '}'"),
                                );
                            },
                            _ => expected!(lex, "export name after keyword 'as'"),
                        ),
                        Tt::Rbrace => {
                            exports.push(ExportSpec::same(bind));
                            break
                        },
                        Tt::Comma => {
                            exports.push(ExportSpec::same(bind));
                        },
                        _ => expected!(lex, "',' or '}' or keyword 'as'"),
                    ),
                    Tt::Rbrace => break,
                    _ => expected!(lex, "binding name or '}'"),
                );
            }
            eat!(lex => tok { source.push_str(tok.ws_before) },
                Tt::Id("from") => eat!(lex => tok { source.push_str(tok.ws_before) },
                    Tt::StrLitSgl(module) |
                    Tt::StrLitDbl(module) => {
                        Ok(Export::NamedFrom(exports, match lex::str_lit_value(module) {
                            Ok(module) => module,
                            Err(error) => return Err(Error {
                                kind: ErrorKind::ParseStrLitError(error),
                                span: tok.span.with_owned(),
                            }),
                        }))
                    },
                    _ => expected!(lex, "module name (string literal)"),
                ),
                _ => {
                    Ok(Export::Named(exports))
                },
            )
        },
        Tt::Var |
        Tt::Const |
        Tt::Id("let") => {
            let start_pos = tok.span.start.pos;
            let mut exports = Vec::new();
            loop {
                eat!(lex,
                    Tt::Id(name) => {
                        exports.push(ExportSpec::same(name));
                        eat!(lex,
                            Tt::Eq => {
                                skip_expr(lex, Prec::NoComma)?;
                                eat!(lex,
                                    Tt::Comma => continue,
                                    _ => break,
                                )
                            },
                            Tt::Comma => continue,
                            _ => break,
                        );
                    },
                    // TODO Tt::Lbrace =>
                    // TODO Tt::Lbracket =>
                    _ => expected!(lex, "binding name"),
                );
            }

            let here = lex.here();
            let end_pos = here.span.start.pos - here.ws_before.len();
            source.push_str(&lex.input()[start_pos..end_pos]);

            Ok(Export::Named(exports))
        },
        Tt::Function => {
            let start_pos = tok.span.start.pos;

            eat!(lex,
                Tt::Star => {},
                _ => {},
            );
            let name = eat!(lex,
                Tt::Id(name) => name,
                _ => expected!(lex, "function name"),
            );
            // eat!(lex,
            //     Tt::Lparen => skip_balanced_parens(lex, 1)?,
            //     _ => expected!(lex, "formal parameter list"),
            // );
            // eat!(lex,
            //     Tt::Lbrace => skip_balanced_braces(lex, 1)?,
            //     _ => expected!(lex, "function body"),
            // );

            let here = lex.here();
            let end_pos = here.span.start.pos - here.ws_before.len();
            source.push_str(&lex.input()[start_pos..end_pos]);

            Ok(Export::Named(vec![ExportSpec::same(name)]))
        },
        Tt::Class => {
            let start_pos = tok.span.start.pos;

            let name = eat!(lex,
                Tt::Id(name) => name,
                _ => expected!(lex, "class name"),
            );
            // eat!(lex,
            //     Tt::Extends => skip_expr(lex, Prec::NoComma)?,
            //     _ => {},
            // );
            // eat!(lex,
            //     Tt::Lbrace => skip_balanced_braces(lex, 1)?,
            //     _ => expected!(lex, "class body"),
            // );

            let here = lex.here();
            let end_pos = here.span.start.pos - here.ws_before.len();
            source.push_str(&lex.input()[start_pos..end_pos]);

            Ok(Export::Named(vec![ExportSpec::same(name)]))
        },
        // TODO Tt::Id("async") =>
        _ => expected!(lex, "keyword 'default' or '*' or '{' or declaration"),
    )
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Prec {
    Primary,
    NoComma,
    Any,
}
fn skip_expr<'f, 's>(lex: &mut lex::Lexer<'f, 's>, prec: Prec) -> Result<()> {
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
            Tt::Id("await") |
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
            Tt::Yield => {
                if lex.here().nl_before {
                    return Ok(())
                }
                if lex.here().tt == Tt::Star {
                    lex.advance();
                }
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

            Tt::TemplateStart(_) => skip_balanced_templates(lex, 1)?,
            Tt::Lbracket => skip_balanced_brackets(lex, 1)?,
            Tt::Lbrace => skip_balanced_braces(lex, 1)?,
            Tt::Lparen => skip_balanced_parens(lex, 1)?,

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
                    Tt::Lparen => skip_balanced_parens(lex, 1)?,
                    _ => expected!(lex, "formal parameter list"),
                );
                eat!(lex,
                    Tt::Lbrace => skip_balanced_braces(lex, 1)?,
                    _ => expected!(lex, "function body"),
                );
            },
            Tt::Class => {
                eat!(lex,
                    Tt::Id(_) => {},
                    _ => {},
                );
                eat!(lex,
                    Tt::Extends => skip_expr(lex, Prec::NoComma)?,
                    _ => {},
                );
                eat!(lex,
                    Tt::Lbrace => skip_balanced_braces(lex, 1)?,
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
                Tt::TemplateStart(_) => skip_balanced_templates(lex, 1)?,
                Tt::Lbracket => skip_balanced_brackets(lex, 1)?,
                Tt::Lparen => skip_balanced_parens(lex, 1)?,

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
                Tt::Colon => skip_expr(lex, Prec::Primary)?,

                Tt::Comma if prec == Prec::Any => skip_expr(lex, Prec::Primary)?,

                // Tt::Question => {
                //     skip_expr(lex, prec)?;
                //     eat!(lex,
                //         Tt::Colon => {},
                //         _ => expected!(lex, "':' in ternary expression"),
                //     );
                //     skip_expr(lex, prec::Primary)?;
                // },

                _ => break,
            );
        }
    }
    Ok(())
}

#[inline]
fn skip_balanced_templates<'f, 's>(lex: &mut lex::Lexer<'f, 's>, nesting: usize) -> Result<()> {
    skip_balanced(
        lex,
        nesting,
        |tt| matches!(tt, Tt::TemplateStart(_)),
        |tt| matches!(tt, Tt::TemplateEnd(_)),
        "end of template literal",
    )
}

#[inline]
fn skip_balanced_braces<'f, 's>(lex: &mut lex::Lexer<'f, 's>, nesting: usize) -> Result<()> {
    skip_balanced(
        lex,
        nesting,
        |tt| tt == Tt::Lbrace,
        |tt| tt == Tt::Rbrace,
        "'}'",
    )
}

#[inline]
fn skip_balanced_brackets<'f, 's>(lex: &mut lex::Lexer<'f, 's>, nesting: usize) -> Result<()> {
    skip_balanced(
        lex,
        nesting,
        |tt| tt == Tt::Lbracket,
        |tt| tt == Tt::Rbracket,
        "']'",
    )
}

#[inline]
fn skip_balanced_parens<'f, 's>(lex: &mut lex::Lexer<'f, 's>, nesting: usize) -> Result<()> {
    skip_balanced(
        lex,
        nesting,
        |tt| tt == Tt::Lparen,
        |tt| tt == Tt::Rparen,
        "')'",
    )
}

#[inline]
fn skip_balanced<'f, 's, L, R>(lex: &mut lex::Lexer<'f, 's>, mut nesting: usize, mut l: L, mut r: R, expect: &'static str) -> Result<()> where
L: FnMut(Tt) -> bool,
R: FnMut(Tt) -> bool {
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

#[inline(always)]
fn parse_import<'f, 's>(lex: &mut lex::Lexer<'f, 's>, source: &mut String) -> Result<Import<'s>> {
    #[inline(always)]
    fn parse_binds<'f, 's>(lex: &mut lex::Lexer<'f, 's>, source: &mut String, binds: &mut Bindings<'s>, expected: &'static str) -> Result<()> {
        eat!(lex => tok { source.push_str(tok.ws_before) },
            Tt::Star => eat!(lex => tok { source.push_str(tok.ws_before) },
                Tt::Id("as") => eat!(lex => tok { source.push_str(tok.ws_before) },
                    Tt::Id(name_space) => {
                        *binds = Bindings::NameSpace(name_space)
                    },
                    _ => expected!(lex, "name space binding name"),
                ),
                _ => expected!(lex, "keyword 'as'"),
            ),
            Tt::Lbrace => {
                let mut imports = Vec::new();
                loop {
                    eat!(lex => tok { source.push_str(tok.ws_before) },
                        Tt::Id(name) => eat!(lex => tok { source.push_str(tok.ws_before) },
                            Tt::Id("as") => eat!(lex => tok { source.push_str(tok.ws_before) },
                                Tt::Id(bind) => {
                                    imports.push(ImportSpec::new(name, bind));
                                    eat!(lex => tok { source.push_str(tok.ws_before) },
                                        Tt::Rbrace => break,
                                        Tt::Comma => {},
                                        _ => expected!(lex, "',' or '}'"),
                                    );
                                },
                                _ => expected!(lex, "binding name after keyword 'as'"),
                            ),
                            Tt::Rbrace => {
                                imports.push(ImportSpec::same(name));
                                break
                            },
                            Tt::Comma => {
                                imports.push(ImportSpec::same(name));
                            },
                            _ => expected!(lex, "',' or '}' or keyword 'as'"),
                        ),
                        Tt::Rbrace => break,
                        _ => expected!(lex, "import specifier or '}'"),
                    );
                }
                *binds = Bindings::Named(imports);
            },
            _ => expected!(lex, expected),
        );
        Ok(())
    }

    let mut default_bind = None;
    let mut binds = Bindings::None;

    eat!(lex => tok { source.push_str(tok.ws_before) },
        Tt::StrLitSgl(module_source) |
        Tt::StrLitDbl(module_source) => {
            return Ok(Import::new(module_source, match lex::str_lit_value(module_source) {
                Ok(module) => module,
                Err(error) => return Err(Error {
                    kind: ErrorKind::ParseStrLitError(error),
                    span: tok.span.with_owned(),
                }),
            }))
        },
        Tt::Id(default) => {
            default_bind = Some(default);
            eat!(lex => tok { source.push_str(tok.ws_before) },
                Tt::Comma => parse_binds(lex, source, &mut binds, "bindings")?,
                _ => {},
            );
        },
        _ => parse_binds(lex, source, &mut binds, "module name (string literal) or bindings")?,
    );
    eat!(lex => tok { source.push_str(tok.ws_before) },
        Tt::Id("from") => {},
        _ => expected!(lex, "keyword 'from'"),
    );
    eat!(lex => tok { source.push_str(tok.ws_before) },
        Tt::StrLitSgl(module_source) |
        Tt::StrLitDbl(module_source) => {
            Ok(Import {
                module_source,
                module: match lex::str_lit_value(module_source) {
                    Ok(module) => module,
                    Err(error) => return Err(Error {
                        kind: ErrorKind::ParseStrLitError(error),
                        span: tok.span.with_owned(),
                    }),
                },
                default_bind,
                binds,
            })
        },
        _ => expected!(lex, "module name (string literal)"),
    )
}

#[cfg(test)]
mod test {
    use super::*;
    use esparse::lex;

    fn assert_skips_expr(source: &str, prec: Prec) {
        let mut cleaned = String::new();
        let mut iter = source.splitn(2, '@');
        let prefix = iter.next().unwrap();
        let suffix = iter.next().unwrap();
        cleaned.push_str(prefix);
        cleaned.push_str(suffix);

        let mut lexer = lex::Lexer::new_unnamed(&cleaned);
        skip_expr(&mut lexer, prec).unwrap();
        let here = lexer.here();
        assert!(
            here.span.start.pos - here.ws_before.len() <= prefix.len() &&
            prefix.len() <= here.span.start.pos,
            "expected skip_expr to skip to:\n{}@\nbut it skipped to:\n{}@",
            &cleaned[..prefix.len()],
            &cleaned[..here.span.start.pos - here.ws_before.len()],
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
    fn test_skip_expr_primary_arrow() {
        assert_skips_expr("(x, y, z) => y => z@", Prec::Primary);
    }

    #[test]
    fn test_skip_expr_primary_async() {
        assert_skips_expr("async function() {}@", Prec::Primary);
        assert_skips_expr("async function named() {}@", Prec::Primary);
        assert_skips_expr("async @\n function named() {}", Prec::Primary);
    }

    #[test]
    fn test_skip_expr_primary_await() {
        assert_skips_expr("await a@", Prec::Primary);
    }

    #[test]
    fn test_skip_balanced() {
        let mut lexer = lex::Lexer::new_unnamed("(((-a)())((()(b)())))c)");
        lexer.advance();
        skip_balanced(
            &mut lexer,
            1,
            |tt| tt == Tt::Lparen,
            |tt| tt == Tt::Rparen,
            "')'",
        ).unwrap();
        assert_eq!(lexer.here().tt, Tt::Id("c"));

        let mut lexer = lex::Lexer::new_unnamed("(((-a)())((()(b)())))c)");
        lexer.advance();
        skip_balanced_parens(&mut lexer, 1).unwrap();
        assert_eq!(lexer.here().tt, Tt::Id("c"));

        let mut lexer = lex::Lexer::new_unnamed("[[[-a][]][[[][b][]]]]c]");
        lexer.advance();
        skip_balanced_brackets(&mut lexer, 1).unwrap();
        assert_eq!(lexer.here().tt, Tt::Id("c"));

        let mut lexer = lex::Lexer::new_unnamed("{{{-a}{}}{{{}{b}{}}}}c}");
        lexer.advance();
        skip_balanced_braces(&mut lexer, 1).unwrap();
        assert_eq!(lexer.here().tt, Tt::Id("c"));

        let mut lexer = lex::Lexer::new_unnamed("`${`${`${-a}` + `${v}`}` - `${`${`${x}` * `${b}` * `${z}`}`}`}`c}`");
        lexer.advance(); // skip first Tt::TemplateStart
        skip_balanced_templates(&mut lexer, 1).unwrap();
        assert_eq!(lexer.here().tt, Tt::Id("c"));
    }

    #[test]
    #[should_panic]
    fn test_skip_balanced_eof() {
        let mut lexer = lex::Lexer::new_unnamed("((");
        skip_balanced(
            &mut lexer,
            1,
            |tt| tt == Tt::Lparen,
            |tt| tt == Tt::Rparen,
            "')'",
        ).unwrap();
    }
}
