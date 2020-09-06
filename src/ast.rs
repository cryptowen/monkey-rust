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
    pub return_value: Expression,
}

impl ToString for ReturnStatement {
    fn to_string(&self) -> String {
        format!("return {};", self.return_value.to_string())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ExpressionStatement {
    // pub token: Token,
    pub expression: Expression,
}

impl ToString for ExpressionStatement {
    fn to_string(&self) -> String {
        self.expression.to_string()
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
    Index(IndexExpression),
    Array(ArrayLiteral),
}

impl ToString for Expression {
    fn to_string(&self) -> String {
        match self {
            Self::Ident(ident) => ident.to_string(),
            Self::Prefix(p) => format!("({}{})", p.operator, p.right.to_string()),
            Self::Infix(p) => format!(
                "({} {} {})",
                p.left.to_string(),
                p.operator,
                p.right.to_string()
            ),
            Self::Bool(p) => format!("{}", p),
            Self::IntegerLiteral(p) => format!("{}", p),
            Self::FloatLiteral(p) => format!("{}", p),
            Self::Call(p) => format!(
                "{}({})",
                p.function.to_string(),
                p.arguments
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Self::String(p) => p.to_string(),
            Self::Function(p) => p.to_string(),
            Self::If(p) => todo!(),
            Self::Index(p) => p.to_string(),
            Self::Array(p) => p.to_string(),
        }
    }
}

impl From<bool> for Expression {
    fn from(b: bool) -> Self {
        Expression::Bool(b)
    }
}

impl From<i64> for Expression {
    fn from(i: i64) -> Self {
        Expression::IntegerLiteral(i)
    }
}

impl From<f64> for Expression {
    fn from(i: f64) -> Self {
        Expression::FloatLiteral(i)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct CallExpression {
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionExpression {
    pub parameters: Vec<Ident>,
    pub body: BlockStatement,
}

impl ToString for FunctionExpression {
    fn to_string(&self) -> String {
        todo!()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ArrayLiteral(pub Vec<Box<Expression>>);

impl ToString for ArrayLiteral {
    fn to_string(&self) -> String {
        format!(
            "[{}]",
            self.0
                .iter()
                .map(|e| e.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct IndexExpression {
    pub left: Box<Expression>,
    pub index: Box<Expression>,
}

impl ToString for IndexExpression {
    fn to_string(&self) -> String {
        format!("({}[{}])", self.left.to_string(), self.index.to_string())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfExpression {
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    pub alternative: BlockStatement,
}

#[derive(Debug, PartialEq, Clone)]
pub struct PrefixExpression {
    pub operator: String,
    pub right: Box<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct InfixExpression {
    // token: Token,
    pub left: Box<Expression>,
    pub operator: String,
    pub right: Box<Expression>,
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::token::{Token, Type};

    #[test]
    fn test_string() {
        let program = Program {
            statements: vec![Statement::Let(LetStatement {
                name: "myVar".to_owned(),
                value: Expression::Ident("anotherVar".to_owned()),
            })],
        };
        println!("program: {}", program.to_string());
    }
}
