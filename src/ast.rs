use crate::token::Token;

pub type Ident = String;

#[derive(Default, Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl ToString for Program {
    fn to_string(&self) -> String {
        self.statements.iter().map(|s| s.to_string()).collect()
    }
}

#[derive(Debug, PartialEq, Clone)]
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

// pub struct Ident {
//     token: Token,
//     value: String,
// }

// impl ToString for Ident {
//     fn to_string(&self) -> String {
//         self.value.clone()
//     }
// }

#[derive(Debug, PartialEq, Clone)]
pub struct LetStatement {
    // token: Token,
    // name: Ident,
    pub name: Ident,
    pub value: Expression,
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

#[derive(Debug, PartialEq, Clone)]
pub struct ReturnStatement {
    token: Token,
    return_value: Expression,
}

impl ToString for ReturnStatement {
    fn to_string(&self) -> String {
        todo!()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Expression,
}

impl ToString for ExpressionStatement {
    fn to_string(&self) -> String {
        todo!()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    Bool(bool),
    If(IfExpression),
    Function(FunctionExpression),
    Call(CallExpression),
    String(String),
    Ident(String),
    IntegerLiteral(i64),
    FloatLiteral(f64),
}

impl ToString for Expression {
    fn to_string(&self) -> String {
        match self {
            Self::Ident(ident) => ident.to_string(),
            _ => todo!(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct CallExpression {
    token: Token,
    function: FunctionExpression,
    arguments: Vec<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionExpression {
    token: Token,
    parameters: Vec<Ident>,
    body: BlockStatement,
}

#[derive(Debug, PartialEq, Clone)]
pub struct BlockStatement {
    token: Token,
    statements: Vec<Statement>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfExpression {
    token: Token,
    condition: Box<Expression>,
    consequence: BlockStatement,
    alternative: BlockStatement,
}

#[derive(Debug, PartialEq, Clone)]
pub struct PrefixExpression {
    token: Token,
    operator: String,
    right: Box<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct InfixExpression {
    // token: Token,
    left: Box<Expression>,
    operator: String,
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
                // token: Token {
                //     type_: Type::LET,
                //     literal: "let".to_owned(),
                // },
                // name: Ident {
                //     token: Token {
                //         type_: Type::IDENT,
                //         literal: "myVar".to_owned(),
                //     },
                //     value: "myVar".to_owned(),
                // },
                name: "myVar".to_owned(),
                value: Expression::Ident("anotherVar".to_owned()),
            })],
        };
        println!("program: {}", program.to_string());
    }
}
