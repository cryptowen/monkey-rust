use crate::ast::{Expression, ExpressionStatement, IfExpression, LetStatement, Program, Statement};
use crate::lexer::Lexer;
use crate::token::{Token, Type};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("unexpected token type, expected: {expected:?}, actual: {actual:?}")]
    UnexpectedToken { expected: Type, actual: Type },
    #[error("no prefix parse function for {0:?} found")]
    NoPrefixParseFunc(Type),
}

pub type ParseResult<T> = Result<T, ParseError>;

#[repr(u8)]
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
        let s = match self.current_token.type_ {
            Type::LET => Statement::Let(self.parse_let_statement()?),
            _ => Statement::Expression(self.parse_expression_statement()?),
        };
        Ok(s)
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

        while self.current_token.type_ == Type::SEMICOLON {
            self.next_token();
        }

        Ok(LetStatement { name, value })
    }

    fn parse_expression(&mut self, precedence: Precedence) -> ParseResult<Expression> {
        let prefix = self.prefix_parse_fns(&self.current_token.type_);
        if prefix.is_none() {
            return Err(ParseError::NoPrefixParseFunc(
                self.current_token.type_.clone(),
            ));
        }
        let mut left_expr = prefix.unwrap()();
        // while self.current_token.type_ != Type::SEMICOLON && precedence < self.peek_precedence() {
        //     let infix =
        // }
        todo!()
    }

    fn peek_precedence(&mut self) -> Precedence {
        match self.peek_token.type_ {
            Type::EQ | Type::NEQ => Precedence::EQUALS,
            Type::LT | Type::GT => Precedence::LESSGREATER,
            Type::PLUS | Type::MINUS => Precedence::SUM,
            Type::SLASH | Type::ASTARISK => Precedence::PRODUCT,
            Type::LPAREN => Precedence::CALL,
            Type::LBRACKET => Precedence::INDEX,
            _ => Precedence::LOWEST,
        }
    }

    fn prefix_parse_fns(&self, type_: &Type) -> Option<Box<dyn Fn() -> ParseResult<Expression>>> {
        todo!()
    }

    fn infix_parse_fns(&self, type_: &Type) -> Option<Box<dyn Fn() -> ParseResult<Expression>>> {
        todo!()
    }

    fn parse_expression_statement(&mut self) -> ParseResult<ExpressionStatement> {
        todo!()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::{Expression, Ident, LetStatement, Statement};

    #[test]
    fn test_let_statements() {
        let tests = vec![(
            "let x = 5;",
            Statement::Let(LetStatement {
                name: "x".to_owned(),
                value: Expression::IntegerLiteral(5),
            }),
        )];
        for t in tests.into_iter() {
            let mut p = Parser::new(Lexer::new(t.0.to_owned()));
            let program = p.parse_program().unwrap();
            assert_eq!(program.statements.len(), 1);
            assert_eq!(program.statements[0], t.1);
        }
    }
}
