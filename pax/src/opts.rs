use std::{env, iter};

pub fn args() -> Expand<iter::Skip<env::Args>> {
    expand(env::args().skip(1))
}

pub fn expand<I: IntoIterator<Item = String>>(args: I) -> Expand<I::IntoIter> {
    Expand {
        arg: None,
        args: args.into_iter(),
        state: State::Start,
    }
}

#[derive(Debug)]
pub struct Expand<I> {
    arg: Option<String>,
    args: I,
    state: State,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum State {
    Start,
    ShortOption(usize),
    Raw,
    Done,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Arg {
    Opt(String),
    Pos(String),
}
#[derive(Debug)]
enum Type {
    Short,
    Long,
    Raw,
    Pos,
}

impl<I: Iterator<Item = String>> Expand<I> {
    pub fn next_arg(&mut self) -> Option<String> {
        match self.state {
            State::Done => None,
            State::Start |
            State::Raw |
            State::ShortOption(_) => self.args.next(),
        }
    }
}

impl<I: Iterator<Item = String>> Iterator for Expand<I> {
    type Item = Arg;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.state {
                State::Start => {
                    let arg = match self.args.next() {
                        None => {
                            self.state = State::Done;
                            return None
                        }
                        Some(arg) => arg,
                    };

                    let ty = {
                        let mut chars = arg.chars();
                        match chars.next() {
                            Some('-') => match chars.next() {
                                Some('-') => {
                                    if chars.next().is_none() {
                                        Type::Raw
                                    } else {
                                        Type::Long
                                    }
                                }
                                Some(_) => Type::Short,
                                None => Type::Pos,
                            }
                            _ => Type::Pos,
                        }
                    };
                    match ty {
                        Type::Raw => {
                            self.state = State::Raw;
                        }
                        Type::Short => {
                            self.arg = Some(arg);
                            self.state = State::ShortOption(1);
                        }
                        Type::Long => {
                            return Some(Arg::Opt(arg))
                        }
                        Type::Pos => {
                            return Some(Arg::Pos(arg))
                        }
                    }
                }
                State::Raw => {
                    let arg = self.args.next();
                    if arg.is_none() {
                        self.state = State::Done;
                    }
                    return arg.map(Arg::Pos)
                }
                State::ShortOption(n) => {
                    let c = {
                        let mut indices = self.arg.as_ref().unwrap()[n..].char_indices();
                        match indices.next() {
                            Some((_, c)) => {
                                self.state = match indices.next() {
                                    Some((m, _)) => State::ShortOption(n + m),
                                    None => State::Start,
                                };
                                c
                            }
                            None => unreachable!(),
                        }
                    };
                    if self.state == State::Start {
                        self.arg = None;
                    }
                    return Some(Arg::Opt(format!("-{}", c)))
                }
                State::Done => return None,
            }
        }
    }
}
// impl iter::FusedIterator for Expand {}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parser() {
        let mut args = expand("-f -bz - --a --foo c  -- -f -bz - --a --foo c ".split(" ").into_iter().map(ToOwned::to_owned));
        assert_eq!(args.next(), Some(Arg::Opt("-f".to_owned())));
        assert_eq!(args.next(), Some(Arg::Opt("-b".to_owned())));
        assert_eq!(args.next(), Some(Arg::Opt("-z".to_owned())));
        assert_eq!(args.next(), Some(Arg::Pos("-".to_owned())));
        assert_eq!(args.next(), Some(Arg::Opt("--a".to_owned())));
        assert_eq!(args.next(), Some(Arg::Opt("--foo".to_owned())));
        assert_eq!(args.next(), Some(Arg::Pos("c".to_owned())));
        assert_eq!(args.next(), Some(Arg::Pos("".to_owned())));
        assert_eq!(args.next(), Some(Arg::Pos("-f".to_owned())));
        assert_eq!(args.next(), Some(Arg::Pos("-bz".to_owned())));
        assert_eq!(args.next(), Some(Arg::Pos("-".to_owned())));
        assert_eq!(args.next(), Some(Arg::Pos("--a".to_owned())));
        assert_eq!(args.next(), Some(Arg::Pos("--foo".to_owned())));
        assert_eq!(args.next(), Some(Arg::Pos("c".to_owned())));
        assert_eq!(args.next(), Some(Arg::Pos("".to_owned())));
    }

    #[test]
    fn test_next_arg() {
        let mut args = expand("0 -bz 1 2 --something 3 4".split(" ").into_iter().map(ToOwned::to_owned));
        assert_eq!(args.next_arg(), Some("0".to_owned()));
        assert_eq!(args.next(), Some(Arg::Opt("-b".to_owned())));
        assert_eq!(args.next_arg(), Some("1".to_owned()));
        assert_eq!(args.next(), Some(Arg::Opt("-z".to_owned())));
        assert_eq!(args.next_arg(), Some("2".to_owned()));
        assert_eq!(args.next(), Some(Arg::Opt("--something".to_owned())));
        assert_eq!(args.next_arg(), Some("3".to_owned()));
        assert_eq!(args.next_arg(), Some("4".to_owned()));
    }
}
