use crate::ast::{
    Expression, ExpressionStatement, IfExpression, InfixExpression, LetStatement, Program,
    ReturnStatement, Statement,
};
use crate::lexer::Lexer;
use crate::token::{Token, Type};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("unexpected token type, expected: {expected:?}, actual: {actual:?}")]
    UnexpectedToken { expected: Type, actual: Type },
    #[error("no prefix parse function for {0:?} found")]
    NoPrefixParseFunc(Type),
    #[error("parse int error")]
    ParseInt(#[from] std::num::ParseIntError),
}

pub type ParseResult<T> = Result<T, ParseError>;

#[repr(u8)]
#[derive(PartialOrd, PartialEq, Debug)]
pub enum Precedence {
    LOWEST = 1,
    EQUALS,
    LESSGREATER,
    SUM,
    PRODUCT,
    PREFIX,
    CALL,
    INDEX,
}

pub struct Parser {
    l: Lexer,
    errors: Vec<String>,
    current_token: Token,
    peek_token: Token,
}

impl Parser {
    pub fn new(mut l: Lexer) -> Self {
        let current_token = l.next_token();
        let peek_token = l.next_token();
        Self {
            l,
            errors: Vec::new(),
            current_token,
            peek_token,
        }
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.l.next_token();
    }

    pub fn parse_program(&mut self) -> ParseResult<Program> {
        let mut p = Program::default();

        while self.current_token.type_ != Type::EOF {
            let stmt = self.parse_statement()?;
            p.statements.push(stmt);
            self.next_token();
        }
        Ok(p)
    }

    fn parse_statement(&mut self) -> ParseResult<Statement> {
        println!("parse_statement. current: {:?}", &self.current_token);
        let s = match self.current_token.type_ {
            Type::LET => Statement::Let(self.parse_let_statement()?),
            Type::RETURN => Statement::Return(self.parse_return_statement()?),
            _ => Statement::Expression(self.parse_expression_statement()?),
        };
        Ok(s)
    }

    fn parse_return_statement(&mut self) -> ParseResult<ReturnStatement> {
        self.next_token();
        let return_value = self.parse_expression(Precedence::LOWEST)?;
        while self.peek_token.type_ == Type::SEMICOLON {
            self.next_token();
        }
        Ok(ReturnStatement { return_value })
    }

    fn peek_error(&mut self, t: Type) {
        self.errors.push(format!(
            "expected next token to be {:?}, got {:?} instead",
            t, self.peek_token
        ));
    }

    fn expect_peek(&mut self, t: Type) -> ParseResult<()> {
        if self.peek_token.type_ == t {
            self.next_token();
            Ok(())
        } else {
            Err(ParseError::UnexpectedToken {
                expected: t,
                actual: self.peek_token.type_.clone(),
            })
        }
    }

    fn parse_let_statement(&mut self) -> ParseResult<LetStatement> {
        self.expect_peek(Type::IDENT)?;
        let name = self.current_token.literal.clone();
        self.expect_peek(Type::ASSIGN)?;
        self.next_token();
        let value = self.parse_expression(Precedence::LOWEST)?;

        while self.peek_token.type_ == Type::SEMICOLON {
            self.next_token();
        }

        Ok(LetStatement { name, value })
    }

    fn parse_expression(&mut self, precedence: Precedence) -> ParseResult<Expression> {
        let mut left_expr = self.prefix_parse_fns()?;
        println!(
            "parse_expression. current token: {:?}, left_expr: {:?}",
            &self.current_token, &left_expr
        );
        while self.current_token.type_ != Type::SEMICOLON
            && precedence < type_to_predence(&self.peek_token.type_)
        {
            let infix = self.infix_parse_fns(left_expr.clone())?;
            if infix.is_none() {
                return Ok(left_expr);
            }
            self.next_token();
            left_expr = infix.unwrap();
        }
        Ok(left_expr)
    }

    fn prefix_parse_fns(&mut self) -> ParseResult<Expression> {
        let t = self.current_token.clone();
        match t.type_ {
            Type::IDENT => Ok(Expression::Ident(t.literal)),
            Type::INT => {
                let v = t.literal.parse::<i64>()?;
                Ok(Expression::IntegerLiteral(v))
            }
            Type::TRUE => Ok(Expression::Bool(true)),
            Type::FALSE => Ok(Expression::Bool(false)),

            _ => Err(ParseError::NoPrefixParseFunc(t.type_)),
        }
    }

    fn parse_ident(&mut self) -> ParseResult<Expression> {
        todo!()
    }

    fn infix_parse_fns(&mut self, expr: Expression) -> ParseResult<Option<Expression>> {
        let t = self.current_token.clone();
        match t.type_ {
            Type::EQ => Ok(Some(Expression::Ident(t.literal))),
            _ => Ok(None),
        }
    }

    fn parse_infix_expression(&mut self, left: Expression) -> ParseResult<Expression> {
        let current_token = self.current_token.clone();
        // let expr = Expression::Infix(InfixExpression{
        //     operator:
        // })
        // let prec = self.current_
        todo!()
    }

    fn parse_expression_statement(&mut self) -> ParseResult<ExpressionStatement> {
        let stmt = ExpressionStatement {
            token: self.current_token.clone(),
            expression: self.parse_expression(Precedence::LOWEST)?,
        };
        if self.peek_token.type_ == Type::SEMICOLON {
            self.next_token();
        }
        Ok(stmt)
    }
}

fn type_to_predence(t: &Type) -> Precedence {
    match t {
        Type::EQ | Type::NEQ => Precedence::EQUALS,
        Type::LT | Type::GT => Precedence::LESSGREATER,
        Type::PLUS | Type::MINUS => Precedence::SUM,
        Type::SLASH | Type::ASTARISK => Precedence::PRODUCT,
        Type::LPAREN => Precedence::CALL,
        Type::LBRACKET => Precedence::INDEX,
        _ => Precedence::LOWEST,
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::{Expression, Ident, LetStatement, ReturnStatement, Statement};

    #[test]
    fn test_let_statements() {
        let tests = vec![
            ("let x = 5;", "x", Expression::IntegerLiteral(5)),
            ("let y = true;", "y", Expression::Bool(true)),
            (
                "let foobar = y;",
                "foobar",
                Expression::Ident("y".to_owned()),
            ),
        ];
        for t in tests.into_iter() {
            let mut p = Parser::new(Lexer::new(t.0.to_owned()));
            let program = p.parse_program().unwrap();
            assert_eq!(program.statements.len(), 1);
            assert_eq!(
                program.statements[0],
                Statement::Let(LetStatement {
                    name: t.1.to_owned(),
                    value: t.2,
                })
            );
        }
    }

    #[test]
    fn test_let_statement_errors() {
        let tests = vec!["let = 5;", "let x =;", "let x 1;"];
        for t in tests.into_iter() {
            let mut p = Parser::new(Lexer::new(t.to_owned()));
            let program = p.parse_program();
            assert!(program.is_err());
        }
    }

    #[test]
    fn test_return_statements() {
        let tests = vec![
            ("return 5;", Expression::IntegerLiteral(5)),
            ("return true;", Expression::Bool(true)),
            ("return x;", Expression::Ident("x".to_owned())),
        ];
        for t in tests.into_iter() {
            let mut p = Parser::new(Lexer::new(t.0.to_owned()));
            let program = p.parse_program().unwrap();
            assert_eq!(program.statements.len(), 1);
            assert_eq!(
                program.statements[0],
                Statement::Return(ReturnStatement { return_value: t.1 })
            );
        }
    }
}
