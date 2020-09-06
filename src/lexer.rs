use crate::token::{lookup_ident, Token, Type};

const ZERO_CHAR: char = '\u{0}';

#[derive(Debug, Default)]
pub struct Lexer {
    input: Vec<char>,
    position: usize,
    read_position: usize,
    ch: char,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut l = Self {
            input: input.chars().collect(),
            ..Default::default()
        };
        l.read_char();
        l
    }

    fn read_char(&mut self) {
        self.ch = if self.read_position >= self.input.len() {
            ZERO_CHAR
        } else {
            self.input[self.read_position]
        };
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn skip_whitespace(&mut self) {
        while self.ch == ' ' || self.ch == '\t' || self.ch == '\n' || self.ch == '\r' {
            self.read_char();
        }
    }

    fn skip_comment(&mut self) {
        while self.ch != '\n' && self.ch != '\r' {
            self.read_char();
        }
        self.skip_whitespace();
    }

    fn peek_char(&mut self) -> char {
        if self.read_position >= self.input.len() {
            ZERO_CHAR
        } else {
            self.input[self.read_position]
        }
    }

    fn read(&mut self, check_fn: fn(char) -> bool) -> String {
        let position = self.position;
        while check_fn(self.ch) {
            self.read_char();
        }
        self.input[position..self.position].iter().collect()
    }

    fn read_ident(&mut self) -> String {
        self.read(is_letter)
    }

    fn read_number(&mut self) -> String {
        self.read(is_digit)
    }

    fn read_number_token(&mut self) -> Token {
        let int_part = self.read_number();
        if self.ch != '.' {
            return Token {
                type_: Type::INT,
                literal: int_part,
            };
        }
        self.read_char();
        let frac_part = self.read_number();
        Token {
            type_: Type::FLOAT,
            literal: vec![int_part, '.'.into(), frac_part].concat(),
        }
    }

    fn read_string(&mut self) -> String {
        let position = self.position + 1;
        loop {
            self.read_char();
            if self.ch == '"' || self.ch == ZERO_CHAR {
                break;
            }
        }
        self.input[position..self.position].iter().collect()
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        if self.ch == '/' && self.peek_char() == '/' {
            self.skip_comment();
        }

        let token = match self.ch {
            '=' => {
                if self.peek_char() == '=' {
                    let ch = self.ch;
                    self.read_char();
                    Token {
                        type_: Type::EQ,
                        literal: vec![ch, self.ch].iter().collect(),
                    }
                } else {
                    Token {
                        type_: Type::ASSIGN,
                        literal: self.ch.into(),
                    }
                }
            }
            '!' => {
                if self.peek_char() == '=' {
                    let ch = self.ch;
                    self.read_char();
                    Token {
                        type_: Type::NEQ,
                        literal: vec![ch, self.ch].iter().collect(),
                    }
                } else {
                    Token {
                        type_: Type::BANG,
                        literal: self.ch.into(),
                    }
                }
            }
            ';' => Token {
                type_: Type::SEMICOLON,
                literal: self.ch.into(),
            },
            ':' => Token {
                type_: Type::COLON,
                literal: self.ch.into(),
            },
            '(' => Token {
                type_: Type::LPAREN,
                literal: self.ch.into(),
            },
            ')' => Token {
                type_: Type::RPAREN,
                literal: self.ch.into(),
            },
            ',' => Token {
                type_: Type::COMMA,
                literal: self.ch.into(),
            },
            '+' => Token {
                type_: Type::PLUS,
                literal: self.ch.into(),
            },
            '-' => Token {
                type_: Type::MINUS,
                literal: self.ch.into(),
            },
            '*' => Token {
                type_: Type::ASTARISK,
                literal: self.ch.into(),
            },
            '/' => Token {
                type_: Type::SLASH,
                literal: self.ch.into(),
            },
            '<' => Token {
                type_: Type::LT,
                literal: self.ch.into(),
            },
            '>' => Token {
                type_: Type::GT,
                literal: self.ch.into(),
            },
            '{' => Token {
                type_: Type::LBRACE,
                literal: self.ch.into(),
            },
            '}' => Token {
                type_: Type::RBRACE,
                literal: self.ch.into(),
            },
            '[' => Token {
                type_: Type::LBRACKET,
                literal: self.ch.into(),
            },
            ']' => Token {
                type_: Type::RBRACKET,
                literal: self.ch.into(),
            },
            '"' => Token {
                type_: Type::STRING,
                literal: self.read_string(),
            },
            ZERO_CHAR => Token {
                type_: Type::EOF,
                literal: String::new(),
            },
            _ => {
                if is_digit(self.ch) {
                    return self.read_number_token();
                }
                if is_letter(self.ch) {
                    let literal = self.read_ident();
                    let type_ = lookup_ident(&literal);
                    return Token { literal, type_ };
                }
                Token {
                    type_: Type::ILLEGAL,
                    literal: self.ch.into(),
                }
            }
        };
        self.read_char();
        token
    }
}

fn is_letter(ch: char) -> bool {
    ch.is_ascii_alphabetic() || ch == '_'
}

fn is_digit(ch: char) -> bool {
    ch.is_ascii_digit()
}

#[cfg(test)]
mod test {
    use super::Lexer;
    use crate::token::{lookup_ident, Token, Type};

    #[test]
    fn test_next_token() {
        let input = r#"
        let five = 5;
        let ten = 10;
        let add = fn(x, y) {
            x + y;
        };
        let result = add(five, ten);
        !-/*0;
        2 < 10 > 7;

        if (5 < 10) {
            return true;
        } else {
            return false;
        }

        10 == 10;
        10 != 9;

        "foobar";
        "foo bar";

        [1, 2];

        {"foo": "bar"};

        // comment
        let a = 1; // inline comment

        let b = 123.45;
        let c = 0.678;
        let d = 9.0;

        macro(x, y) { x + y; };
        "#;
        let mut lexer = Lexer::new(input.to_owned());
        let tests = vec![
            (Type::LET, "let"),
            (Type::IDENT, "five"),
            (Type::ASSIGN, "="),
            (Type::INT, "5"),
            (Type::SEMICOLON, ";"),
            (Type::LET, "let"),
            (Type::IDENT, "ten"),
            (Type::ASSIGN, "="),
            (Type::INT, "10"),
            (Type::SEMICOLON, ";"),
            (Type::LET, "let"),
            (Type::IDENT, "add"),
            (Type::ASSIGN, "="),
            (Type::FUNCTION, "fn"),
            (Type::LPAREN, "("),
            (Type::IDENT, "x"),
            (Type::COMMA, ","),
            (Type::IDENT, "y"),
            (Type::RPAREN, ")"),
            (Type::LBRACE, "{"),
            (Type::IDENT, "x"),
            (Type::PLUS, "+"),
            (Type::IDENT, "y"),
            (Type::SEMICOLON, ";"),
            (Type::RBRACE, "}"),
            (Type::SEMICOLON, ";"),
            (Type::LET, "let"),
            (Type::IDENT, "result"),
            (Type::ASSIGN, "="),
            (Type::IDENT, "add"),
            (Type::LPAREN, "("),
            (Type::IDENT, "five"),
            (Type::COMMA, ","),
            (Type::IDENT, "ten"),
            (Type::RPAREN, ")"),
            (Type::SEMICOLON, ";"),
            (Type::BANG, "!"),
            (Type::MINUS, "-"),
            (Type::SLASH, "/"),
            (Type::ASTARISK, "*"),
            (Type::INT, "0"),
            (Type::SEMICOLON, ";"),
            (Type::INT, "2"),
            (Type::LT, "<"),
            (Type::INT, "10"),
            (Type::GT, ">"),
            (Type::INT, "7"),
            (Type::SEMICOLON, ";"),
            (Type::IF, "if"),
            (Type::LPAREN, "("),
            (Type::INT, "5"),
            (Type::LT, "<"),
            (Type::INT, "10"),
            (Type::RPAREN, ")"),
            (Type::LBRACE, "{"),
            (Type::RETURN, "return"),
            (Type::TRUE, "true"),
            (Type::SEMICOLON, ";"),
            (Type::RBRACE, "}"),
            (Type::ELSE, "else"),
            (Type::LBRACE, "{"),
            (Type::RETURN, "return"),
            (Type::FALSE, "false"),
            (Type::SEMICOLON, ";"),
            (Type::RBRACE, "}"),
            (Type::INT, "10"),
            (Type::EQ, "=="),
            (Type::INT, "10"),
            (Type::SEMICOLON, ";"),
            (Type::INT, "10"),
            (Type::NEQ, "!="),
            (Type::INT, "9"),
            (Type::SEMICOLON, ";"),
            (Type::STRING, "foobar"),
            (Type::SEMICOLON, ";"),
            (Type::STRING, "foo bar"),
            (Type::SEMICOLON, ";"),
            (Type::LBRACKET, "["),
            (Type::INT, "1"),
            (Type::COMMA, ","),
            (Type::INT, "2"),
            (Type::RBRACKET, "]"),
            (Type::SEMICOLON, ";"),
            (Type::LBRACE, "{"),
            (Type::STRING, "foo"),
            (Type::COLON, ":"),
            (Type::STRING, "bar"),
            (Type::RBRACE, "}"),
            (Type::SEMICOLON, ";"),
            (Type::LET, "let"),
            (Type::IDENT, "a"),
            (Type::ASSIGN, "="),
            (Type::INT, "1"),
            (Type::SEMICOLON, ";"),
            (Type::LET, "let"),
            (Type::IDENT, "b"),
            (Type::ASSIGN, "="),
            (Type::FLOAT, "123.45"),
            (Type::SEMICOLON, ";"),
            (Type::LET, "let"),
            (Type::IDENT, "c"),
            (Type::ASSIGN, "="),
            (Type::FLOAT, "0.678"),
            (Type::SEMICOLON, ";"),
            (Type::LET, "let"),
            (Type::IDENT, "d"),
            (Type::ASSIGN, "="),
            (Type::FLOAT, "9.0"),
            (Type::SEMICOLON, ";"),
            (Type::MACRO, "macro"),
            (Type::LPAREN, "("),
            (Type::IDENT, "x"),
            (Type::COMMA, ","),
            (Type::IDENT, "y"),
            (Type::RPAREN, ")"),
            (Type::LBRACE, "{"),
            (Type::IDENT, "x"),
            (Type::PLUS, "+"),
            (Type::IDENT, "y"),
            (Type::SEMICOLON, ";"),
            (Type::RBRACE, "}"),
            (Type::SEMICOLON, ";"),
            (Type::EOF, ""),
        ];
        for (t, l) in tests.into_iter() {
            let token = lexer.next_token();
            println!("token: {:?}, t: {:?}, l: {:?}", &token, &t, &l);
            assert_eq!(token.literal, l);
            assert_eq!(token.type_, t);
        }
    }
}
