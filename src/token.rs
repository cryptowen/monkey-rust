#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub type_: Type,
    pub literal: String,
}

pub fn lookup_ident(s: &str) -> Type {
    match s {
        "fn" => Type::FUNCTION,
        "let" => Type::LET,
        "true" => Type::TRUE,
        "false" => Type::FALSE,
        "if" => Type::IF,
        "else" => Type::ELSE,
        "return" => Type::RETURN,
        "macro" => Type::MACRO,
        _ => Type::IDENT,
    }
}
