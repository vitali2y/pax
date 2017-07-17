use std::{char, mem};

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

impl<'f, 's> Lexer<'f, 's> {
    pub fn new(file_name: &'f str, input: &'s str) -> Self {
        Lexer {
            file_name,
            stream: Stream::new(input),
            here: Tok::new(Tt::Eof, Span::zero(file_name)),
        }
    }
}

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

    #[inline]
    pub fn is(&self, c: char) -> bool {
        self.here.map_or(false, |cc| c == cc)
    }

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

    #[inline]
    pub fn skip_while<F>(&mut self, mut f: F) where
    F: FnMut(char) -> bool {
        loop {
            match self.here {
                Some(c) => {
                    if f(c) {
                        self.advance();
                        continue
                    }
                }
                None => {}
            }
            break
        }
    }

    #[inline]
    pub fn pos(&self) -> usize {
        self.loc.pos
    }

    #[inline]
    pub fn loc(&self) -> Loc {
        self.loc
    }

    #[inline]
    pub fn here(&self) -> Option<char> {
        self.here
    }

    #[inline]
    pub fn next(&self) -> Option<char> {
        self.next
    }

    #[inline]
    pub fn str_from(&self, start: usize) -> &str {
        &self.input[start..self.loc.pos]
    }

    #[inline]
    pub fn str_range(&self, start: usize, end: usize) -> &str {
        &self.input[start..end]
    }

    // TODO pub fn advance_by(&mut self, n: usize) -> &str {}

    pub fn advance(&mut self) -> Option<char> {
        match self.here {
            Some('\n') => {
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
    use std::fs;
    use std::io::prelude::*;

    #[bench]
    fn bench_big(b: &mut test::Bencher) {
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
