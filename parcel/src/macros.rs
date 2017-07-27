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
