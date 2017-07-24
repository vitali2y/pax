macro_rules! eat {
    (@collect $lexer:expr, { $($($p:pat)|+ if $c:expr => $e:expr ,)* }, _ => $else:expr $(,)*) => {
        match $lexer.here().tt {
            $($($p)|+ if $c => {
                $lexer.advance();
                $e
            })*
            _ => $else
        }
    };
    (@collect $lexer:expr, { $($($p:pat)|+ if $c:expr => $e:expr ,)* }, $($q:pat)|+ => $f:expr, $($t:tt)+) => {
        eat!(@collect $lexer, { $($($p)|+ if $c => $e ,)* $($q)|+ if true => $f, }, $($t)+)
    };
    (@collect $lexer:expr, { $($($p:pat)|+ if $c:expr => $e:expr ,)* }, $($q:pat)|+ if $d:expr => $f:expr, $($t:tt)+) => {
        eat!(@collect $lexer, { $($($p)|+ if $c => $e ,)* $($q)|+ if $d => $f, }, $($t)+)
    };
    ($lexer:expr, $($t:tt)+) => {
        eat!(@collect $lexer, {}, $($t)+)
    };
}
