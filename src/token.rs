#[derive(Debug, PartialEq, Eq)]
pub enum Type {
    ILLEGAL,
    EOF,
    IDENT,
    INT,
    FLOAT,
    STRING,
    BANG,
    ASSIGN,
    PLUS,
    MINUS,
    ASTARISK,
    SLASH,
    LT,
    GT,
    EQ,
    NEQ,
    COMMA,
    SEMICOLON,
    COLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    LBRACKET,
    RBRACKET,
    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,
    MACRO,
}

#[derive(Debug)]
pub struct Token {
    pub type_: Type,
    pub literal: Vec<u8>,
}

pub fn lookup_ident(s: &[u8]) -> Type {
    match s {
        b"fn" => Type::FUNCTION,
        b"let" => Type::LET,
        b"true" => Type::TRUE,
        b"false" => Type::FALSE,
        b"if" => Type::IF,
        b"else" => Type::ELSE,
        b"return" => Type::RETURN,
        b"macro" => Type::MACRO,
        _ => Type::IDENT,
    }
}
