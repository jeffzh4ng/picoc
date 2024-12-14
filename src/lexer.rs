use serde::{Deserialize, Serialize};
use std::io;
use std::iter;

// todo. remove allocations.
// iterate on slices and iterators.
// todo: change to iterative

#[rustfmt::skip]
#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct Token { pub lexeme: String, pub typ: TT }

#[rustfmt::skip]
#[derive(Copy, Clone, PartialEq, Serialize, Deserialize, Debug)]
pub enum TT {
    LiteralInt, Alias, // introductions (values) RE: [0-9]+ and [a-zA-Z][a-zA-Z0-9]*
    KeywordInt, KeywordChar, KeywordVoid, KeywordRet, KeywordIf, KeywordEls, KeywordFor, KeywordWhile, KeywordTrue, KeywordFalse, // keywords ⊂ identifiers
    Plus, Minus, Star, Slash, LeftAngleBracket, RightAngleBracket, Equals, Bang, Amp, Bar, // eliminations (ops)
    PuncLeftParen, PuncRightParen, PuncLeftBrace, PuncRightBrace, PuncSemiColon, PuncComma,// punctuation
}

//  1. variations are explicitly typed. Collapsing categories like keywords
//     into one variant will lose information since lexeme : String, which
//     will produce redundant work for the parser during syntactic analysis
//  2. non-tokens: comments, preprocessor directives, macros, whitespace

pub fn lex(input: &[char]) -> Result<Vec<Token>, io::Error> {
    let cs = skip_ws(input);

    // literals and identifiers have arbitrary length
    // operations and punctuations are single ASCII characters
    match cs {
        [] => Ok(vec![]),
        [f, r @ ..] => match f {
            '0'..='9' => scan_int(cs),
            'a'..='z' | 'A'..='Z' => scan_id(cs),
            '+' => {
                #[rustfmt::skip]
                let t = Token { lexeme: String::from("+"), typ: TT::Plus };
                Ok(iter::once(t).chain(lex(r)?).collect())
            }
            '-' => {
                #[rustfmt::skip]
                let t = Token { lexeme: String::from("-"), typ: TT::Minus };
                Ok(iter::once(t).chain(lex(r)?).collect())
            }
            '*' => {
                #[rustfmt::skip]
                let t = Token { lexeme: String::from("*"), typ: TT::Star };
                Ok(iter::once(t).chain(lex(r)?).collect())
            }
            '/' => {
                #[rustfmt::skip]
                let t = Token { lexeme: String::from("/"), typ: TT::Slash };
                Ok(iter::once(t).chain(lex(r)?).collect())
            }
            '<' => {
                #[rustfmt::skip]
                let t = Token { lexeme: String::from("<"), typ: TT::LeftAngleBracket };
                Ok(iter::once(t).chain(lex(r)?).collect())
            }
            '>' => {
                #[rustfmt::skip]
                let t = Token { lexeme: String::from(">"), typ: TT::RightAngleBracket };
                Ok(iter::once(t).chain(lex(r)?).collect())
            }
            '=' => {
                #[rustfmt::skip]
                let t = Token { lexeme: String::from("="), typ: TT::Equals };
                Ok(iter::once(t).chain(lex(r)?).collect())
            }
            '!' => {
                #[rustfmt::skip]
                let t = Token { lexeme: String::from("!"), typ: TT::Bang };
                Ok(iter::once(t).chain(lex(r)?).collect())
            }
            '&' => {
                #[rustfmt::skip]
                let t = Token { lexeme: String::from("&"), typ: TT::Amp };
                Ok(iter::once(t).chain(lex(r)?).collect())
            }
            '|' => {
                #[rustfmt::skip]
                let t = Token { lexeme: String::from("|"), typ: TT::Bar };
                Ok(iter::once(t).chain(lex(r)?).collect())
            }
            '(' => {
                #[rustfmt::skip]
                let t = Token { lexeme: String::from("("), typ: TT::PuncLeftParen };
                Ok(iter::once(t).chain(lex(r)?).collect())
            }
            ')' => {
                #[rustfmt::skip]
                let t = Token { lexeme: String::from(")"), typ: TT::PuncRightParen };
                Ok(iter::once(t).chain(lex(r)?).collect())
            }
            '{' => {
                #[rustfmt::skip]
                let t = Token { lexeme: String::from("{"), typ: TT::PuncLeftBrace };
                Ok(iter::once(t).chain(lex(r)?).collect())
            }
            '}' => {
                #[rustfmt::skip]
                let t = Token { lexeme: String::from("}"), typ: TT::PuncRightBrace };
                Ok(iter::once(t).chain(lex(r)?).collect())
            }
            ';' => {
                #[rustfmt::skip]
                let t = Token { lexeme: String::from(";"), typ: TT::PuncSemiColon };
                Ok(iter::once(t).chain(lex(r)?).collect())
            }
            ',' => {
                #[rustfmt::skip]
                let t = Token { lexeme: String::from(","), typ: TT::PuncComma };
                Ok(iter::once(t).chain(lex(r)?).collect())
            }
            _ => Err(io::Error::new(
                io::ErrorKind::Other,
                format!("unexpected token: {:?}", f),
            )),
        },
    }
}

fn scan_int(input: &[char]) -> Result<Vec<Token>, io::Error> {
    // scan_int calls skip_whitespace too to remain idempotent
    let cs = skip_ws(input);

    match cs {
        [] => Ok(vec![]),
        [f, _r @ ..] => match f {
            '0'..='9' => {
                #[rustfmt::skip]
                let i = _r
                    .iter()
                    .take_while(|&&c| c.is_numeric())
                    .count();

                let f = cs[..=i].iter().collect::<String>();
                let r = &cs[i + 1..];

                let t = Token {
                    lexeme: f,
                    typ: TT::LiteralInt,
                };

                Ok(iter::once(t).chain(lex(r)?).collect())
            }
            _ => Err(io::Error::new(
                io::ErrorKind::Other,
                format!("unexpected token: {:?}", f),
            )),
        },
    }
}

// TODO: support identifiers with alpha*numeric* characters after first alphabetic
fn scan_id(input: &[char]) -> Result<Vec<Token>, io::Error> {
    // scan_id calls skip_whitespace too to remain idempotent
    let cs = skip_ws(input);

    match cs {
        [] => Ok(vec![]),
        [f, r @ ..] => match f {
            'a'..='z' => {
                // Find the index where the alphabetic characters end
                #[rustfmt::skip]
                let i = r
                    .iter()
                    .take_while(|&&c| c.is_alphabetic())
                    .count();

                let f = (cs[..=i].iter()).collect::<String>();
                let new_r = &cs[i + 1..];

                let keyword = match f.as_str() {
                    "int" => Some(Token {
                        lexeme: f.to_string(),
                        typ: TT::KeywordInt,
                    }),
                    "if" => Some(Token {
                        lexeme: f.to_string(),
                        typ: TT::KeywordIf,
                    }),
                    "else" => Some(Token {
                        lexeme: f.to_string(),
                        typ: TT::KeywordEls,
                    }),
                    "for" => Some(Token {
                        lexeme: f.to_string(),
                        typ: TT::KeywordFor,
                    }),
                    "while" => Some(Token {
                        lexeme: f.to_string(),
                        typ: TT::KeywordWhile,
                    }),
                    "return" => Some(Token {
                        lexeme: f.to_string(),
                        typ: TT::KeywordRet,
                    }),
                    "true" => Some(Token {
                        lexeme: f.to_string(),
                        typ: TT::KeywordTrue,
                    }),
                    "false" => Some(Token {
                        lexeme: f.to_string(),
                        typ: TT::KeywordFalse,
                    }),
                    _ => None,
                };

                let t = match keyword {
                    Some(k) => k,
                    None => Token {
                        lexeme: f,
                        typ: TT::Alias,
                    },
                };

                Ok(iter::once(t).chain(lex(new_r)?).collect())
            }
            _ => Err(io::Error::new(
                io::ErrorKind::Other,
                format!("unexpected token: {:?}", f),
            )),
        },
    }
}

fn skip_ws(input: &[char]) -> &[char] {
    match input {
        [] => input,
        [f, r @ ..] => {
            if f.is_whitespace() {
                skip_ws(r)
            } else {
                input
            }
        }
    }
}

#[cfg(test)]
mod test_arith {
    use std::fs;
    const TEST_DIR: &str = "tests/fixtures/snap/shared/arith";

    #[test]
    fn lit() {
        #[rustfmt::skip]
        let input = fs::read(format!("{TEST_DIR}/lit.c"))
            .expect("file dne")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let output = super::lex(input.as_slice()).unwrap();
        insta::assert_yaml_snapshot!(output, @r###"
        ---
        - lexeme: int
          typ: KeywordInt
        - lexeme: main
          typ: Alias
        - lexeme: (
          typ: PuncLeftParen
        - lexeme: )
          typ: PuncRightParen
        - lexeme: "{"
          typ: PuncLeftBrace
        - lexeme: return
          typ: KeywordRet
        - lexeme: "8"
          typ: LiteralInt
        - lexeme: ;
          typ: PuncSemiColon
        - lexeme: "}"
          typ: PuncRightBrace
        "###);
    }

    #[test]
    fn add() {
        #[rustfmt::skip]
        let input = fs::read(format!("{TEST_DIR}/add.c"))
            .expect("file dne")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let output = super::lex(input.as_slice()).unwrap();
        insta::assert_yaml_snapshot!(output, @r###"
        ---
        - lexeme: int
          typ: KeywordInt
        - lexeme: main
          typ: Alias
        - lexeme: (
          typ: PuncLeftParen
        - lexeme: )
          typ: PuncRightParen
        - lexeme: "{"
          typ: PuncLeftBrace
        - lexeme: return
          typ: KeywordRet
        - lexeme: "9"
          typ: LiteralInt
        - lexeme: +
          typ: Plus
        - lexeme: "10"
          typ: LiteralInt
        - lexeme: ;
          typ: PuncSemiColon
        - lexeme: "}"
          typ: PuncRightBrace
        "###);
    }

    #[test]
    fn add_multi() {
        #[rustfmt::skip]
        let input = fs::read(format!("{TEST_DIR}/add_multi.c"))
            .expect("file dne")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let output = super::lex(input.as_slice()).unwrap();
        insta::assert_yaml_snapshot!(output, @r###"
        ---
        - lexeme: int
          typ: KeywordInt
        - lexeme: main
          typ: Alias
        - lexeme: (
          typ: PuncLeftParen
        - lexeme: )
          typ: PuncRightParen
        - lexeme: "{"
          typ: PuncLeftBrace
        - lexeme: return
          typ: KeywordRet
        - lexeme: "9"
          typ: LiteralInt
        - lexeme: +
          typ: Plus
        - lexeme: "10"
          typ: LiteralInt
        - lexeme: +
          typ: Plus
        - lexeme: "11"
          typ: LiteralInt
        - lexeme: ;
          typ: PuncSemiColon
        - lexeme: "}"
          typ: PuncRightBrace
        "###);
    }

    #[test]
    fn sub() {
        #[rustfmt::skip]
        let input = fs::read(format!("{}/sub.c", TEST_DIR))
            .expect("file dne")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let output = super::lex(input.as_slice()).unwrap();
        insta::assert_yaml_snapshot!(output, @r###"
        ---
        - lexeme: int
          typ: KeywordInt
        - lexeme: main
          typ: Alias
        - lexeme: (
          typ: PuncLeftParen
        - lexeme: )
          typ: PuncRightParen
        - lexeme: "{"
          typ: PuncLeftBrace
        - lexeme: return
          typ: KeywordRet
        - lexeme: "88"
          typ: LiteralInt
        - lexeme: "-"
          typ: Minus
        - lexeme: "32"
          typ: LiteralInt
        - lexeme: ;
          typ: PuncSemiColon
        - lexeme: "}"
          typ: PuncRightBrace
        "###);
    }

    #[test]
    fn mult() {
        #[rustfmt::skip]
        let input = fs::read(format!("{TEST_DIR}/mult.c"))
            .expect("file dne")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let output = super::lex(input.as_slice()).unwrap();
        insta::assert_yaml_snapshot!(output, @r###"
        ---
        - lexeme: int
          typ: KeywordInt
        - lexeme: main
          typ: Alias
        - lexeme: (
          typ: PuncLeftParen
        - lexeme: )
          typ: PuncRightParen
        - lexeme: "{"
          typ: PuncLeftBrace
        - lexeme: return
          typ: KeywordRet
        - lexeme: "9"
          typ: LiteralInt
        - lexeme: "*"
          typ: Star
        - lexeme: "10"
          typ: LiteralInt
        - lexeme: ;
          typ: PuncSemiColon
        - lexeme: "}"
          typ: PuncRightBrace
        "###);
    }

    #[test]
    fn div() {
        #[rustfmt::skip]
        let input = fs::read(format!("{TEST_DIR}/div.c"))
            .expect("file dne")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let output = super::lex(input.as_slice()).unwrap();
        insta::assert_yaml_snapshot!(output, @r###"
        ---
        - lexeme: int
          typ: KeywordInt
        - lexeme: main
          typ: Alias
        - lexeme: (
          typ: PuncLeftParen
        - lexeme: )
          typ: PuncRightParen
        - lexeme: "{"
          typ: PuncLeftBrace
        - lexeme: return
          typ: KeywordRet
        - lexeme: "100"
          typ: LiteralInt
        - lexeme: /
          typ: Slash
        - lexeme: "9"
          typ: LiteralInt
        - lexeme: ;
          typ: PuncSemiColon
        - lexeme: "}"
          typ: PuncRightBrace
        "###);
    }
}

#[cfg(test)]
mod test_bindings {
    use std::fs;
    const TEST_DIR: &str = "tests/fixtures/snap/shared/bindings";

    #[test]
    fn asnmt() {
        #[rustfmt::skip]
        let input = fs::read(format!("{TEST_DIR}/asnmt.c"))
            .expect("file dne")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let output = super::lex(input.as_slice()).unwrap();
        insta::assert_yaml_snapshot!(output, @r###"
        ---
        - lexeme: int
          typ: KeywordInt
        - lexeme: main
          typ: Alias
        - lexeme: (
          typ: PuncLeftParen
        - lexeme: )
          typ: PuncRightParen
        - lexeme: "{"
          typ: PuncLeftBrace
        - lexeme: int
          typ: KeywordInt
        - lexeme: x
          typ: Alias
        - lexeme: "="
          typ: Equals
        - lexeme: "8"
          typ: LiteralInt
        - lexeme: ;
          typ: PuncSemiColon
        - lexeme: return
          typ: KeywordRet
        - lexeme: x
          typ: Alias
        - lexeme: ;
          typ: PuncSemiColon
        - lexeme: "}"
          typ: PuncRightBrace
        "###);
    }

    #[test]
    fn composition() {
        #[rustfmt::skip]
        let input = fs::read(format!("{TEST_DIR}/composition.c"))
            .expect("file dne")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let output = super::lex(input.as_slice()).unwrap();
        insta::assert_yaml_snapshot!(output, @r###"
        ---
        - lexeme: int
          typ: KeywordInt
        - lexeme: h
          typ: Alias
        - lexeme: (
          typ: PuncLeftParen
        - lexeme: )
          typ: PuncRightParen
        - lexeme: "{"
          typ: PuncLeftBrace
        - lexeme: return
          typ: KeywordRet
        - lexeme: "11"
          typ: LiteralInt
        - lexeme: ;
          typ: PuncSemiColon
        - lexeme: "}"
          typ: PuncRightBrace
        - lexeme: int
          typ: KeywordInt
        - lexeme: g
          typ: Alias
        - lexeme: (
          typ: PuncLeftParen
        - lexeme: )
          typ: PuncRightParen
        - lexeme: "{"
          typ: PuncLeftBrace
        - lexeme: return
          typ: KeywordRet
        - lexeme: "10"
          typ: LiteralInt
        - lexeme: +
          typ: Plus
        - lexeme: h
          typ: Alias
        - lexeme: (
          typ: PuncLeftParen
        - lexeme: )
          typ: PuncRightParen
        - lexeme: ;
          typ: PuncSemiColon
        - lexeme: "}"
          typ: PuncRightBrace
        - lexeme: int
          typ: KeywordInt
        - lexeme: f
          typ: Alias
        - lexeme: (
          typ: PuncLeftParen
        - lexeme: )
          typ: PuncRightParen
        - lexeme: "{"
          typ: PuncLeftBrace
        - lexeme: return
          typ: KeywordRet
        - lexeme: "9"
          typ: LiteralInt
        - lexeme: +
          typ: Plus
        - lexeme: g
          typ: Alias
        - lexeme: (
          typ: PuncLeftParen
        - lexeme: )
          typ: PuncRightParen
        - lexeme: ;
          typ: PuncSemiColon
        - lexeme: "}"
          typ: PuncRightBrace
        - lexeme: int
          typ: KeywordInt
        - lexeme: main
          typ: Alias
        - lexeme: (
          typ: PuncLeftParen
        - lexeme: )
          typ: PuncRightParen
        - lexeme: "{"
          typ: PuncLeftBrace
        - lexeme: return
          typ: KeywordRet
        - lexeme: f
          typ: Alias
        - lexeme: (
          typ: PuncLeftParen
        - lexeme: )
          typ: PuncRightParen
        - lexeme: ;
          typ: PuncSemiColon
        - lexeme: "}"
          typ: PuncRightBrace
        "###);
    }
}

#[cfg(test)]
mod test_control {
    use std::fs;
    const TEST_DIR: &str = "tests/fixtures/snap/shared/control";

    #[test]
    fn lit() {
        #[rustfmt::skip]
        let input = fs::read(format!("{TEST_DIR}/ifels_then.c"))
            .expect("file dne")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let output = super::lex(input.as_slice()).unwrap();
        insta::assert_yaml_snapshot!(output, @r###"
        ---
        - lexeme: int
          typ: KeywordInt
        - lexeme: main
          typ: Alias
        - lexeme: (
          typ: PuncLeftParen
        - lexeme: )
          typ: PuncRightParen
        - lexeme: "{"
          typ: PuncLeftBrace
        - lexeme: if
          typ: KeywordIf
        - lexeme: (
          typ: PuncLeftParen
        - lexeme: "1"
          typ: LiteralInt
        - lexeme: )
          typ: PuncRightParen
        - lexeme: "{"
          typ: PuncLeftBrace
        - lexeme: return
          typ: KeywordRet
        - lexeme: "9"
          typ: LiteralInt
        - lexeme: ;
          typ: PuncSemiColon
        - lexeme: "}"
          typ: PuncRightBrace
        - lexeme: else
          typ: KeywordEls
        - lexeme: "{"
          typ: PuncLeftBrace
        - lexeme: return
          typ: KeywordRet
        - lexeme: "10"
          typ: LiteralInt
        - lexeme: ;
          typ: PuncSemiColon
        - lexeme: "}"
          typ: PuncRightBrace
        - lexeme: "}"
          typ: PuncRightBrace
        "###);
    }
}
