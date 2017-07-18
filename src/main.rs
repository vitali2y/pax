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
    for tok in lexer {
        print!("{}", tok.ws_before);
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
            lex::Tt::TemplateEnd(s) => print!("{}", s),

            lex::Tt::RegExpLit(s, f) => print!("{}{}", s, f),

            lex::Tt::Lbrace => print!("{{"),
            lex::Tt::Lparen => print!("("),
            lex::Tt::Rparen => print!(")"),
            lex::Tt::Lbracket => print!("["),
            lex::Tt::Rbracket => print!("]"),
            lex::Tt::Dot => print!("."),
            lex::Tt::DotDotDot => print!("..."),
            lex::Tt::Semi => print!(";"),
            lex::Tt::Comma => print!(","),
            lex::Tt::Lt => print!("<"),
            lex::Tt::Gt => print!(">"),
            lex::Tt::LtEq => print!("<="),
            lex::Tt::GtEq => print!(">="),
            lex::Tt::EqEq => print!("=="),
            lex::Tt::BangEq => print!("!="),
            lex::Tt::EqEqEq => print!("==="),
            lex::Tt::BangEqEq => print!("!=="),
            lex::Tt::Plus => print!("+"),
            lex::Tt::Minus => print!("-"),
            lex::Tt::Star => print!("*"),
            lex::Tt::Percent => print!("%"),
            lex::Tt::StarStar => print!("**"),
            lex::Tt::PlusPlus => print!("++"),
            lex::Tt::MinusMinus => print!("--"),
            lex::Tt::LtLt => print!("<<"),
            lex::Tt::GtGt => print!(">>"),
            lex::Tt::GtGtGt => print!(">>>"),
            lex::Tt::And => print!("&"),
            lex::Tt::Or => print!("|"),
            lex::Tt::Circumflex => print!("^"),
            lex::Tt::Bang => print!("!"),
            lex::Tt::Tilde => print!("~"),
            lex::Tt::AndAnd => print!("&&"),
            lex::Tt::OrOr => print!("||"),
            lex::Tt::Question => print!("?"),
            lex::Tt::Colon => print!(":"),
            lex::Tt::Eq => print!("="),
            lex::Tt::PlusEq => print!("+="),
            lex::Tt::MinusEq => print!("-="),
            lex::Tt::StarEq => print!("*="),
            lex::Tt::PercentEq => print!("%="),
            lex::Tt::StarStarEq => print!("**="),
            lex::Tt::LtLtEq => print!("<<="),
            lex::Tt::GtGtEq => print!(">>="),
            lex::Tt::GtGtGtEq => print!(">>>="),
            lex::Tt::AndEq => print!("&="),
            lex::Tt::OrEq => print!("|="),
            lex::Tt::CircumflexEq => print!("^="),
            lex::Tt::EqGt => print!("=>"),
            lex::Tt::Slash => print!("/"),
            lex::Tt::SlashEq => print!("/="),
            lex::Tt::Rbrace => print!("}}"),

            lex::Tt::Null => print!("null"),
            lex::Tt::True => print!("true"),
            lex::Tt::False => print!("false"),
            lex::Tt::Await => print!("await"),
            lex::Tt::Break => print!("break"),
            lex::Tt::Case => print!("case"),
            lex::Tt::Catch => print!("catch"),
            lex::Tt::Class => print!("class"),
            lex::Tt::Const => print!("const"),
            lex::Tt::Continue => print!("continue"),
            lex::Tt::Debugger => print!("debugger"),
            lex::Tt::Default => print!("default"),
            lex::Tt::Delete => print!("delete"),
            lex::Tt::Do => print!("do"),
            lex::Tt::Else => print!("else"),
            lex::Tt::Export => print!("export"),
            lex::Tt::Extends => print!("extends"),
            lex::Tt::Finally => print!("finally"),
            lex::Tt::For => print!("for"),
            lex::Tt::Function => print!("function"),
            lex::Tt::If => print!("if"),
            lex::Tt::Import => print!("import"),
            lex::Tt::In => print!("in"),
            lex::Tt::Instanceof => print!("instanceof"),
            lex::Tt::New => print!("new"),
            lex::Tt::Return => print!("return"),
            lex::Tt::Super => print!("super"),
            lex::Tt::Switch => print!("switch"),
            lex::Tt::This => print!("this"),
            lex::Tt::Throw => print!("throw"),
            lex::Tt::Try => print!("try"),
            lex::Tt::Typeof => print!("typeof"),
            lex::Tt::Var => print!("var"),
            lex::Tt::Void => print!("void"),
            lex::Tt::While => print!("while"),
            lex::Tt::With => print!("with"),
            lex::Tt::Yield => print!("yield"),

            lex::Tt::Eof => unreachable!(),
        }
    }
    println!();
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
