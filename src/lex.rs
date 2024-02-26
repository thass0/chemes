//! The `lex` function in this module turns a string
//! of Scheme source code into a list of tokens that
//! represent the code. It is used in the `parse`
//! module as the first step to the parser.

use alloc::fmt;
use alloc::fmt::Formatter;
use alloc::string::String;
use alloc::vec::Vec;
use core::iter::Peekable;
use core::str::Chars;
use libm::{exp10, floor, log10};

use crate::value::Number;

/// Lex a string of Scheme source code and turn it into a list of tokens.
pub fn lex(src: &str) -> Result<Vec<Token>, InvalidChar> {
    let mut cursor = Cursor::new(src);
    let mut tokens = Vec::new();
    let mut tok = cursor.advance()?;
    while !tok.is_eof() {
        tokens.push(tok);
        tok = cursor.advance()?;
    }
    tokens.push(tok);
    Ok(tokens)
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct InvalidChar(char);

impl fmt::Display for InvalidChar {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "invalid character '{}'", self.0)
    }
}

struct Cursor<'a> {
    chars: Peekable<Chars<'a>>,
    at: Position,
}

impl<'a> Cursor<'a> {
    fn new(src: &'a str) -> Self {
        Cursor {
            chars: src.chars().peekable(),
            at: Position::default(),
        }
    }

    fn advance(&mut self) -> Result<Token, InvalidChar> {
        let first = match self.peek() {
            Some(c) => c,
            None => return Ok(Token::new(TokenKind::Eof, self.at)),
        };

        match first {
            ch if ch.is_whitespace() => {
                self.eat_whitespace();
                self.advance() // Recursive call.
            }
            ';' => {
                self.eat_comment();
                self.advance() // Recursive call.
            }
            '(' => Ok(Token::new(TokenKind::LParen, self.bump())),
            ')' => Ok(Token::new(TokenKind::RParen, self.bump())),
            '\'' => Ok(Token::new(TokenKind::Quote, self.bump())),
            '"' => Ok(self.string()),
            ch if ch.is_ascii_digit() => Ok(self.number()),
            ch if is_symbol_char(ch) => Ok(self.symbol()),
            ch => Err(InvalidChar(ch)),
        }
    }

    fn eat_whitespace(&mut self) {
        self.bump_while(char::is_whitespace);
    }

    fn eat_comment(&mut self) {
        self.bump_while(|ch| ch != '\n');
        self.bump(); // Eat the '\n' that stopped the loop.
    }

    fn string(&mut self) -> Token {
        let fix = self.bump(); // Eat the opening '"'.
        let s = self.bump_while(|ch| ch != '"');
        self.bump(); // Eat the closing '"'.
        Token::new(TokenKind::String(s), fix)
    }

    fn number(&mut self) -> Token {
        let fix = self.at;
        let n1 = self.numeral();

        match self.peek() {
            Some('/') => {
                self.bump();
                let n2 = self.numeral();
                let n = (n1, n2 as u32);
                Token::new(TokenKind::Number(n.into()), fix)
            }
            Some('.') => {
                self.bump();
                let n2 = as_decimal_part(self.numeral());
                let n = n1 as f64 + n2;
                Token::new(TokenKind::Number(n.into()), fix)
            }
            Some(_) | None => Token::new(TokenKind::Number(n1.into()), fix),
        }
    }

    fn numeral(&mut self) -> i32 {
        self.bump_while(|ch| ch.is_ascii_digit())
            .parse::<i32>()
            .unwrap()
    }

    fn symbol(&mut self) -> Token {
        let fix = self.at;
        let sym = self.bump_while(is_symbol_char);
        Token::new(TokenKind::Symbol(sym), fix)
    }

    fn bump(&mut self) -> Position {
        let fix = self.at;
        if let Some(ch) = self.chars.next() {
            if ch == '\n' {
                self.at.next_line();
            } else {
                self.at.next_column();
            }
        }
        fix
    }

    fn bump_while<F>(&mut self, f: F) -> String
    where
        F: Fn(char) -> bool,
    {
        let mut buf = String::new();
        while let Some(ch) = self.peek() {
            if f(ch) {
                buf.push(ch);
                self.bump();
            } else {
                return buf;
            }
        }
        buf
    }

    fn peek(&mut self) -> Option<char> {
        self.chars.peek().copied()
    }
}

/// Turn an i32 into a floating point number
/// that has all the same digits as the i32
/// in the decimal part.
fn as_decimal_part(i: i32) -> f64 {
    let f = i as f64;
    f / exp10(floor(log10(f)) + 1.0)
}

fn is_symbol_char(ch: char) -> bool {
    ch.is_alphanumeric()
        || ch == '?'
        || ch == '.'
        || ch == '-'
        || ch == ','
        || ch == '*'
        || ch == '@'
        || ch == '!'
        || ch == '#'
        || ch == '$'
        || ch == '%'
        || ch == '^'
        || ch == '&'
        || ch == '_'
        || ch == '+'
        || ch == '='
        || ch == '/'
        || ch == '|'
        || ch == ':'
        || ch == '<'
        || ch == '>'
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub at: Position,
}

impl Token {
    fn new(kind: TokenKind, at: Position) -> Self {
        Token { at, kind }
    }

    fn is_eof(&self) -> bool {
        self.kind == TokenKind::Eof
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    LParen,
    RParen,
    Quote,
    Number(Number),
    String(String),
    Symbol(String),
    Eof,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Position {
    line: i32,
    column: i32,
}

impl Position {
    #[cfg(test)]
    fn new(line: i32, column: i32) -> Self {
        Position { line, column }
    }

    fn default() -> Self {
        Position { line: 1, column: 1 }
    }

    fn next_line(&mut self) {
        self.line += 1;
        self.column = 1;
    }

    fn next_column(&mut self) {
        self.column += 1;
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use alloc::borrow::ToOwned;
    use alloc::format;
    use alloc::vec;

    #[test]
    fn example_works() {
        use TokenKind::*;
        assert_eq!(
            lex(r#"
(define (fib n)
    (cond
      ((= n 0) 0)
      ((= n 1) 1)
      (else
        (+ (fib (- n 1))
           (fib (- n 2))))))"#)
            .unwrap(),
            vec![
                Token {
                    kind: LParen,
                    at: Position { line: 2, column: 1 }
                },
                Token {
                    kind: Symbol("define".to_owned()),
                    at: Position { line: 2, column: 2 }
                },
                Token {
                    kind: LParen,
                    at: Position { line: 2, column: 9 }
                },
                Token {
                    kind: Symbol("fib".to_owned()),
                    at: Position {
                        line: 2,
                        column: 10
                    }
                },
                Token {
                    kind: Symbol("n".to_owned()),
                    at: Position {
                        line: 2,
                        column: 14
                    }
                },
                Token {
                    kind: RParen,
                    at: Position {
                        line: 2,
                        column: 15
                    }
                },
                Token {
                    kind: LParen,
                    at: Position { line: 3, column: 5 }
                },
                Token {
                    kind: Symbol("cond".to_owned()),
                    at: Position { line: 3, column: 6 }
                },
                Token {
                    kind: LParen,
                    at: Position { line: 4, column: 7 }
                },
                Token {
                    kind: LParen,
                    at: Position { line: 4, column: 8 }
                },
                Token {
                    kind: Symbol("=".to_owned()),
                    at: Position { line: 4, column: 9 }
                },
                Token {
                    kind: Symbol("n".to_owned()),
                    at: Position {
                        line: 4,
                        column: 11
                    }
                },
                Token {
                    kind: Number(0.into()),
                    at: Position {
                        line: 4,
                        column: 13
                    }
                },
                Token {
                    kind: RParen,
                    at: Position {
                        line: 4,
                        column: 14
                    }
                },
                Token {
                    kind: Number(0.into()),
                    at: Position {
                        line: 4,
                        column: 16
                    }
                },
                Token {
                    kind: RParen,
                    at: Position {
                        line: 4,
                        column: 17
                    }
                },
                Token {
                    kind: LParen,
                    at: Position { line: 5, column: 7 }
                },
                Token {
                    kind: LParen,
                    at: Position { line: 5, column: 8 }
                },
                Token {
                    kind: Symbol("=".to_owned()),
                    at: Position { line: 5, column: 9 }
                },
                Token {
                    kind: Symbol("n".to_owned()),
                    at: Position {
                        line: 5,
                        column: 11
                    }
                },
                Token {
                    kind: Number(1.into()),
                    at: Position {
                        line: 5,
                        column: 13
                    }
                },
                Token {
                    kind: RParen,
                    at: Position {
                        line: 5,
                        column: 14
                    }
                },
                Token {
                    kind: Number(1.into()),
                    at: Position {
                        line: 5,
                        column: 16
                    }
                },
                Token {
                    kind: RParen,
                    at: Position {
                        line: 5,
                        column: 17
                    }
                },
                Token {
                    kind: LParen,
                    at: Position { line: 6, column: 7 }
                },
                Token {
                    kind: Symbol("else".to_owned()),
                    at: Position { line: 6, column: 8 }
                },
                Token {
                    kind: LParen,
                    at: Position { line: 7, column: 9 }
                },
                Token {
                    kind: Symbol("+".to_owned()),
                    at: Position {
                        line: 7,
                        column: 10
                    }
                },
                Token {
                    kind: LParen,
                    at: Position {
                        line: 7,
                        column: 12
                    }
                },
                Token {
                    kind: Symbol("fib".to_owned()),
                    at: Position {
                        line: 7,
                        column: 13
                    }
                },
                Token {
                    kind: LParen,
                    at: Position {
                        line: 7,
                        column: 17
                    }
                },
                Token {
                    kind: Symbol("-".to_owned()),
                    at: Position {
                        line: 7,
                        column: 18
                    }
                },
                Token {
                    kind: Symbol("n".to_owned()),
                    at: Position {
                        line: 7,
                        column: 20
                    }
                },
                Token {
                    kind: Number(1.into()),
                    at: Position {
                        line: 7,
                        column: 22
                    }
                },
                Token {
                    kind: RParen,
                    at: Position {
                        line: 7,
                        column: 23
                    }
                },
                Token {
                    kind: RParen,
                    at: Position {
                        line: 7,
                        column: 24
                    }
                },
                Token {
                    kind: LParen,
                    at: Position {
                        line: 8,
                        column: 12
                    }
                },
                Token {
                    kind: Symbol("fib".to_owned()),
                    at: Position {
                        line: 8,
                        column: 13
                    }
                },
                Token {
                    kind: LParen,
                    at: Position {
                        line: 8,
                        column: 17
                    }
                },
                Token {
                    kind: Symbol("-".to_owned()),
                    at: Position {
                        line: 8,
                        column: 18
                    }
                },
                Token {
                    kind: Symbol("n".to_owned()),
                    at: Position {
                        line: 8,
                        column: 20
                    }
                },
                Token {
                    kind: Number(2.into()),
                    at: Position {
                        line: 8,
                        column: 22
                    }
                },
                Token {
                    kind: RParen,
                    at: Position {
                        line: 8,
                        column: 23
                    }
                },
                Token {
                    kind: RParen,
                    at: Position {
                        line: 8,
                        column: 24
                    }
                },
                Token {
                    kind: RParen,
                    at: Position {
                        line: 8,
                        column: 25
                    }
                },
                Token {
                    kind: RParen,
                    at: Position {
                        line: 8,
                        column: 26
                    }
                },
                Token {
                    kind: RParen,
                    at: Position {
                        line: 8,
                        column: 27
                    }
                },
                Token {
                    kind: RParen,
                    at: Position {
                        line: 8,
                        column: 28
                    }
                },
                Token {
                    kind: Eof,
                    at: Position {
                        line: 8,
                        column: 29
                    }
                }
            ]
        )
    }

    #[test]
    fn eating_whitespace_works() {
        assert_eq!(
            lex("   \t \n \n\t").unwrap(),
            vec![Token::new(TokenKind::Eof, Position::new(3, 2))],
        );

        assert_eq!(
            lex(" ( \n )\t").unwrap(),
            vec![
                Token::new(TokenKind::LParen, Position::new(1, 2)),
                Token::new(TokenKind::RParen, Position::new(2, 2)),
                Token::new(TokenKind::Eof, Position::new(2, 4)),
            ],
        );
    }

    #[test]
    fn eating_comments_works() {
        assert_eq!(
            lex("; blah\n;; blah blah\n;;; blah ;;;").unwrap(),
            vec![Token::new(TokenKind::Eof, Position::new(3, 13)),]
        );
    }

    #[test]
    fn lexing_strings_works() {
        let s1 = "I'm so bad at thinking of examples, I might cry!";

        assert_eq!(
            lex(&format!("\"{s1}\"")).unwrap(),
            vec![
                Token::new(TokenKind::String(s1.to_owned()), Position::new(1, 1)),
                Token::new(TokenKind::Eof, Position::new(1, 51)),
            ],
        );

        assert_eq!(
            lex(&format!("(\"{s1}\")")).unwrap(),
            vec![
                Token::new(TokenKind::LParen, Position::new(1, 1)),
                Token::new(TokenKind::String(s1.to_owned()), Position::new(1, 2)),
                Token::new(TokenKind::RParen, Position::new(1, 52)),
                Token::new(TokenKind::Eof, Position::new(1, 53)),
            ],
        );

        let s2 = "In a string,\nthere can be anything: \t even: ().\n";

        assert_eq!(
            lex(&format!("\"{s2}\"")).unwrap(),
            vec![
                Token::new(TokenKind::String(s2.to_owned()), Position::new(1, 1)),
                Token::new(TokenKind::Eof, Position::new(3, 2)),
            ],
        );
    }

    #[test]
    fn lexing_numbers_works() {
        assert_eq!(
            lex("515 69343575 020000 32.3426 32/3 9/2").unwrap(),
            vec![
                Token::new(TokenKind::Number(515.into()), Position::new(1, 1)),
                Token::new(TokenKind::Number(69343575.into()), Position::new(1, 5)),
                Token::new(TokenKind::Number(20000.into()), Position::new(1, 14)),
                Token::new(TokenKind::Number((32.3426).into()), Position::new(1, 21)),
                Token::new(TokenKind::Number((32, 3).into()), Position::new(1, 29)),
                Token::new(TokenKind::Number((9, 2).into()), Position::new(1, 34)),
                Token::new(TokenKind::Eof, Position::new(1, 37)),
            ],
        );
    }

    #[test]
    fn as_decimal_works() {
        // The exact values are meaningless.
        assert_eq!(as_decimal_part(51235), 0.51235);
        assert_eq!(as_decimal_part(684), 0.684);
        assert_eq!(as_decimal_part(1), 0.1);
        assert_eq!(as_decimal_part(100), 0.1);
        assert_eq!(as_decimal_part(67), 0.67);
    }

    #[test]
    fn lexing_symbols_works() {
        assert_eq!(
            lex("abc? blah-symbol symbols,are.special!").unwrap(),
            vec![
                Token::new(TokenKind::Symbol("abc?".to_owned()), Position::new(1, 1),),
                Token::new(
                    TokenKind::Symbol("blah-symbol".to_owned()),
                    Position::new(1, 6),
                ),
                Token::new(
                    TokenKind::Symbol("symbols,are.special!".to_owned()),
                    Position::new(1, 18)
                ),
                Token::new(TokenKind::Eof, Position::new(1, 38)),
            ]
        )
    }

    #[test]
    fn lexing_quotes_works() {
        assert_eq!(
            lex("''('('blah'wow)").unwrap(),
            vec![
                Token::new(TokenKind::Quote, Position::new(1, 1)),
                Token::new(TokenKind::Quote, Position::new(1, 2)),
                Token::new(TokenKind::LParen, Position::new(1, 3)),
                Token::new(TokenKind::Quote, Position::new(1, 4)),
                Token::new(TokenKind::LParen, Position::new(1, 5)),
                Token::new(TokenKind::Quote, Position::new(1, 6)),
                Token::new(TokenKind::Symbol("blah".to_owned()), Position::new(1, 7)),
                Token::new(TokenKind::Quote, Position::new(1, 11)),
                Token::new(TokenKind::Symbol("wow".to_owned()), Position::new(1, 12)),
                Token::new(TokenKind::RParen, Position::new(1, 15)),
                Token::new(TokenKind::Eof, Position::new(1, 16)),
            ]
        )
    }
}
