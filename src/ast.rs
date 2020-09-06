use crate::token::Token;

pub struct Program {
    statements: Vec<Statement>,
}

impl ToString for Program {
    fn to_string(&self) -> String {
        self.statements.iter().map(|s| s.to_string()).collect()
    }
}

pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
}

impl ToString for Statement {
    fn to_string(&self) -> String {
        match self {
            Self::Let(let_statement) => let_statement.to_string(),
            Self::Return(return_statement) => return_statement.to_string(),
            Self::Expression(expression_statement) => expression_statement.to_string(),
        }
    }
}

pub struct Ident {
    token: Token,
    value: String,
}

impl ToString for Ident {
    fn to_string(&self) -> String {
        self.value.clone()
    }
}

pub struct LetStatement {
    token: Token,
    name: Ident,
    value: Expression,
}

impl ToString for LetStatement {
    fn to_string(&self) -> String {
        format!(
            "let {} = {};",
            self.name.to_string(),
            self.value.to_string()
        )
    }
}

pub struct ReturnStatement {
    token: Token,
    return_value: Expression,
}

impl ToString for ReturnStatement {
    fn to_string(&self) -> String {
        todo!()
    }
}

pub struct ExpressionStatement {
    token: Token,
    expression: Expression,
}

impl ToString for ExpressionStatement {
    fn to_string(&self) -> String {
        todo!()
    }
}

pub enum Expression {
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    Bool(bool),
    If(IfExpression),
    Function(FunctionExpression),
    Call(CallExpression),
    String(String),
    Ident(String),
}

impl ToString for Expression {
    fn to_string(&self) -> String {
        match self {
            Self::Ident(ident) => ident.to_string(),
            _ => todo!(),
        }
    }
}

pub struct CallExpression {
    token: Token,
    function: FunctionExpression,
    arguments: Vec<Expression>,
}

pub struct FunctionExpression {
    token: Token,
    parameters: Vec<Ident>,
    body: BlockStatement,
}

pub struct BlockStatement {
    token: Token,
    statements: Vec<Statement>,
}

pub struct IfExpression {
    token: Token,
    condition: Box<Expression>,
    consequence: BlockStatement,
    alternative: BlockStatement,
}

pub struct PrefixExpression {
    token: Token,
    operator: Vec<u8>,
    right: Box<Expression>,
}

pub struct InfixExpression {
    token: Token,
    left: Box<Expression>,
    operator: Vec<u8>,
    right: Box<Expression>,
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::token::{Token, Type};

    #[test]
    fn test_string() {
        let program = Program {
            statements: vec![Statement::Let(LetStatement {
                token: Token {
                    type_: Type::LET,
                    literal: "let".to_owned(),
                },
                name: Ident {
                    token: Token {
                        type_: Type::IDENT,
                        literal: "myVar".to_owned(),
                    },
                    value: "myVar".to_owned(),
                },
                value: Expression::Ident("anotherVar".to_owned()),
            })],
        };
        println!("program: {}", program.to_string());
    }
}
