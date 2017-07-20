extern crate esparse;

use std::{env, process, io, fs};
use std::io::prelude::*;
use esparse::lex;

fn run() -> Result<(), CliError> {
    let file_name = env::args().nth(1).ok_or(CliError::MissingFileName)?;
    let file = fs::File::open(&file_name)?;
    let mut buf_reader = io::BufReader::new(file);
    let mut contents = String::new();
    buf_reader.read_to_string(&mut contents)?;

    let lexer = lex::Lexer::new(&file_name, &contents);
    let stdout = io::stdout();
    let mut stdout = stdout.lock();
    for tok in lexer {
        write!(stdout, "{}", tok.ws_before).unwrap();
        match tok.tt {
            lex::Tt::Id(s) |
            lex::Tt::StrLitSgl(s) |
            lex::Tt::StrLitDbl(s) |
            lex::Tt::NumLitBin(s) |
            lex::Tt::NumLitOct(s) |
            lex::Tt::NumLitDec(s) |
            lex::Tt::NumLitHex(s) |
            lex::Tt::TemplateNoSub(s) |
            lex::Tt::TemplateStart(s) |
            lex::Tt::TemplateMiddle(s) |
            lex::Tt::TemplateEnd(s) => write!(stdout, "{}", s),

            lex::Tt::RegExpLit(s, f) => write!(stdout, "{}{}", s, f),

            lex::Tt::Lbrace => write!(stdout, "{{"),
            lex::Tt::Lparen => write!(stdout, "("),
            lex::Tt::Rparen => write!(stdout, ")"),
            lex::Tt::Lbracket => write!(stdout, "["),
            lex::Tt::Rbracket => write!(stdout, "]"),
            lex::Tt::Dot => write!(stdout, "."),
            lex::Tt::DotDotDot => write!(stdout, "..."),
            lex::Tt::Semi => write!(stdout, ";"),
            lex::Tt::Comma => write!(stdout, ","),
            lex::Tt::Lt => write!(stdout, "<"),
            lex::Tt::Gt => write!(stdout, ">"),
            lex::Tt::LtEq => write!(stdout, "<="),
            lex::Tt::GtEq => write!(stdout, ">="),
            lex::Tt::EqEq => write!(stdout, "=="),
            lex::Tt::BangEq => write!(stdout, "!="),
            lex::Tt::EqEqEq => write!(stdout, "==="),
            lex::Tt::BangEqEq => write!(stdout, "!=="),
            lex::Tt::Plus => write!(stdout, "+"),
            lex::Tt::Minus => write!(stdout, "-"),
            lex::Tt::Star => write!(stdout, "*"),
            lex::Tt::Percent => write!(stdout, "%"),
            lex::Tt::StarStar => write!(stdout, "**"),
            lex::Tt::PlusPlus => write!(stdout, "++"),
            lex::Tt::MinusMinus => write!(stdout, "--"),
            lex::Tt::LtLt => write!(stdout, "<<"),
            lex::Tt::GtGt => write!(stdout, ">>"),
            lex::Tt::GtGtGt => write!(stdout, ">>>"),
            lex::Tt::And => write!(stdout, "&"),
            lex::Tt::Or => write!(stdout, "|"),
            lex::Tt::Circumflex => write!(stdout, "^"),
            lex::Tt::Bang => write!(stdout, "!"),
            lex::Tt::Tilde => write!(stdout, "~"),
            lex::Tt::AndAnd => write!(stdout, "&&"),
            lex::Tt::OrOr => write!(stdout, "||"),
            lex::Tt::Question => write!(stdout, "?"),
            lex::Tt::Colon => write!(stdout, ":"),
            lex::Tt::Eq => write!(stdout, "="),
            lex::Tt::PlusEq => write!(stdout, "+="),
            lex::Tt::MinusEq => write!(stdout, "-="),
            lex::Tt::StarEq => write!(stdout, "*="),
            lex::Tt::PercentEq => write!(stdout, "%="),
            lex::Tt::StarStarEq => write!(stdout, "**="),
            lex::Tt::LtLtEq => write!(stdout, "<<="),
            lex::Tt::GtGtEq => write!(stdout, ">>="),
            lex::Tt::GtGtGtEq => write!(stdout, ">>>="),
            lex::Tt::AndEq => write!(stdout, "&="),
            lex::Tt::OrEq => write!(stdout, "|="),
            lex::Tt::CircumflexEq => write!(stdout, "^="),
            lex::Tt::EqGt => write!(stdout, "=>"),
            lex::Tt::Slash => write!(stdout, "/"),
            lex::Tt::SlashEq => write!(stdout, "/="),
            lex::Tt::Rbrace => write!(stdout, "}}"),

            lex::Tt::Null => write!(stdout, "null"),
            lex::Tt::True => write!(stdout, "true"),
            lex::Tt::False => write!(stdout, "false"),
            lex::Tt::Await => write!(stdout, "await"),
            lex::Tt::Break => write!(stdout, "break"),
            lex::Tt::Case => write!(stdout, "case"),
            lex::Tt::Catch => write!(stdout, "catch"),
            lex::Tt::Class => write!(stdout, "class"),
            lex::Tt::Const => write!(stdout, "const"),
            lex::Tt::Continue => write!(stdout, "continue"),
            lex::Tt::Debugger => write!(stdout, "debugger"),
            lex::Tt::Default => write!(stdout, "default"),
            lex::Tt::Delete => write!(stdout, "delete"),
            lex::Tt::Do => write!(stdout, "do"),
            lex::Tt::Else => write!(stdout, "else"),
            lex::Tt::Export => write!(stdout, "export"),
            lex::Tt::Extends => write!(stdout, "extends"),
            lex::Tt::Finally => write!(stdout, "finally"),
            lex::Tt::For => write!(stdout, "for"),
            lex::Tt::Function => write!(stdout, "function"),
            lex::Tt::If => write!(stdout, "if"),
            lex::Tt::Import => write!(stdout, "import"),
            lex::Tt::In => write!(stdout, "in"),
            lex::Tt::Instanceof => write!(stdout, "instanceof"),
            lex::Tt::New => write!(stdout, "new"),
            lex::Tt::Return => write!(stdout, "return"),
            lex::Tt::Super => write!(stdout, "super"),
            lex::Tt::Switch => write!(stdout, "switch"),
            lex::Tt::This => write!(stdout, "this"),
            lex::Tt::Throw => write!(stdout, "throw"),
            lex::Tt::Try => write!(stdout, "try"),
            lex::Tt::Typeof => write!(stdout, "typeof"),
            lex::Tt::Var => write!(stdout, "var"),
            lex::Tt::Void => write!(stdout, "void"),
            lex::Tt::While => write!(stdout, "while"),
            lex::Tt::With => write!(stdout, "with"),
            lex::Tt::Yield => write!(stdout, "yield"),

            lex::Tt::Eof => unreachable!(),
        }.unwrap()
    }
    writeln!(stdout).unwrap();
    Ok(())
}

const APP_NAME: &'static str = env!("CARGO_PKG_NAME");

fn print_usage() {
    println!("usage: {} <file>", APP_NAME);
}

enum CliError {
    MissingFileName,
    Io(io::Error),
}
impl From<io::Error> for CliError {
    fn from(inner: io::Error) -> CliError {
        CliError::Io(inner)
    }
}

fn main() {
    process::exit(match run() {
        Ok(_) => 0,
        Err(kind) => {
            match kind {
                CliError::MissingFileName => print_usage(),
                CliError::Io(inner) => println!("{}: {}", APP_NAME, inner),
            }
            1
        }
    })
}
