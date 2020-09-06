use crate::ast::{
    ArrayLiteral, CallExpression, Expression, ExpressionStatement, IfExpression, IndexExpression,
    InfixExpression, LetStatement, PrefixExpression, Program, ReturnStatement, Statement,
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
    #[error("parse float error")]
    ParseFloat(#[from] std::num::ParseFloatError),
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
        // println!(
        //     "next token. current: {:?}, peek: {:?}",
        //     self.current_token, self.peek_token
        // );
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
        // println!("parse_statement. current: {:?}", &self.current_token);
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

    fn cur_token_is(&self, t: Type) -> bool {
        self.current_token.type_ == t
    }

    fn peek_precedence(&self) -> Precedence {
        type_to_predence(&self.peek_token.type_)
    }

    fn parse_expression(&mut self, precedence: Precedence) -> ParseResult<Expression> {
        println!(
            "parse_expression. precedence: {:?}, current: {:?}",
            &precedence, &self.current_token
        );
        let prefix = self.prefix_parse_fns(self.current_token.type_.clone());
        if prefix.is_none() {
            return Err(ParseError::NoPrefixParseFunc(
                self.current_token.type_.clone(),
            ));
        }
        let mut left_expr = prefix.unwrap()(self)?;
        while !self.cur_token_is(Type::SEMICOLON) && precedence < self.peek_precedence() {
            let infix = self.infix_parse_fns(self.peek_token.type_.clone());
            if infix.is_none() {
                return Ok(left_expr);
            }
            let infix = infix.unwrap();
            self.next_token();
            println!(
                "left_expr before: {:?}, current: {:?}, peek: {:?}",
                &left_expr, &self.current_token, &self.peek_token
            );
            left_expr = infix(self, left_expr)?;
            println!(
                "left_expr after: {:?}, current: {:?}, peek: {:?}",
                &left_expr, &self.current_token, &self.peek_token
            );
        }
        Ok(left_expr)
    }

    fn parse_ident(&mut self) -> ParseResult<Expression> {
        Ok(Expression::Ident(self.current_token.literal.clone()))
    }

    fn parse_int(&mut self) -> ParseResult<Expression> {
        let v = self.current_token.literal.parse::<i64>()?;
        Ok(Expression::IntegerLiteral(v))
    }

    fn parse_boolean(&mut self) -> ParseResult<Expression> {
        Ok(Expression::Bool(self.current_token.literal == "true"))
    }

    fn parse_float(&mut self) -> ParseResult<Expression> {
        Ok(Expression::FloatLiteral(
            self.current_token.literal.parse()?,
        ))
    }

    fn prefix_parse_fns(
        &mut self,
        t: Type,
    ) -> Option<Box<Fn(&mut Self) -> ParseResult<Expression>>> {
        let f = match t {
            Type::IDENT => Self::parse_ident,
            Type::INT => Self::parse_int,
            Type::TRUE | Type::FALSE => Self::parse_boolean,
            Type::FLOAT => Self::parse_float,
            Type::BANG | Type::MINUS => Self::parse_prefix_expression,
            Type::LPAREN => Self::parse_grouped_expression,
            Type::LBRACKET => Self::parse_array_literal,
            _ => {
                return None;
            }
        };
        Some(Box::new(f))
    }

    fn parse_array_literal(&mut self) -> ParseResult<Expression> {
        Ok(Expression::Array(ArrayLiteral(
            self.parse_expression_list(Type::RBRACKET)?
                .into_iter()
                .map(|x| Box::new(x))
                .collect(),
        )))
    }

    fn parse_grouped_expression(&mut self) -> ParseResult<Expression> {
        println!("start parse_grouped_expression");
        self.next_token();
        let expr = self.parse_expression(Precedence::LOWEST)?;
        self.expect_peek(Type::RPAREN)?;
        println!("finish parse_grouped_expression");
        Ok(expr)
    }

    fn parse_prefix_expression(&mut self) -> ParseResult<Expression> {
        let operator = self.current_token.literal.clone();
        self.next_token();
        let right = Box::new(self.parse_expression(Precedence::PREFIX)?);
        Ok(Expression::Prefix(PrefixExpression { operator, right }))
    }

    fn infix_parse_fns(
        &mut self,
        t: Type,
    ) -> Option<Box<Fn(&mut Self, Expression) -> ParseResult<Expression>>> {
        let f = match t {
            Type::PLUS
            | Type::MINUS
            | Type::ASTARISK
            | Type::SLASH
            | Type::EQ
            | Type::NEQ
            | Type::LT
            | Type::GT => Self::parse_infix_expression,
            Type::LPAREN => Self::parse_call_expression,
            Type::LBRACKET => Self::parse_index_expression,
            _ => return None,
        };
        Some(Box::new(f))
    }

    fn parse_index_expression(&mut self, left: Expression) -> ParseResult<Expression> {
        self.next_token();
        let expr = Expression::Index(IndexExpression {
            left: Box::new(left),
            index: Box::new(self.parse_expression(Precedence::LOWEST)?),
        });
        self.expect_peek(Type::RBRACKET)?;
        Ok(expr)
    }

    fn parse_expression_list(&mut self, end: Type) -> ParseResult<Vec<Expression>> {
        let mut list = Vec::new();
        if self.peek_token.type_ == end {
            self.next_token();
            return Ok(list);
        }

        self.next_token();
        list.push(self.parse_expression(Precedence::LOWEST)?);

        while self.peek_token.type_ == Type::COMMA {
            self.next_token();
            self.next_token();
            list.push(self.parse_expression(Precedence::LOWEST)?);
        }

        self.expect_peek(end)?;
        Ok(list)
    }

    fn parse_call_expression(&mut self, function: Expression) -> ParseResult<Expression> {
        Ok(Expression::Call(CallExpression {
            function: Box::new(function),
            arguments: self.parse_expression_list(Type::RPAREN)?,
        }))
    }

    fn parse_infix_expression(&mut self, left: Expression) -> ParseResult<Expression> {
        let current_token = self.current_token.clone();
        println!(
            "parse_infix_expression. left: {:?}, current: {:?}, peek: {:?}",
            &left, &self.current_token, &self.peek_token
        );
        let operator = current_token.literal;
        let prec = type_to_predence(&self.current_token.type_);
        self.next_token();
        let right = Box::new(self.parse_expression(prec)?);
        Ok(Expression::Infix(InfixExpression {
            left: Box::new(left),
            operator,
            right,
        }))
    }

    fn parse_expression_statement(&mut self) -> ParseResult<ExpressionStatement> {
        let stmt = ExpressionStatement {
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
    use crate::ast::{
        Expression, Ident, LetStatement, PrefixExpression, ReturnStatement, Statement,
    };

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

    #[test]
    fn test_identifier_expression() {
        let input = "foobar";
        let mut p = Parser::new(Lexer::new(input.to_owned()));
        let program = p.parse_program().unwrap();
        assert_eq!(program.statements.len(), 1);
        assert_eq!(
            program.statements[0],
            Statement::Expression(ExpressionStatement {
                expression: Expression::Ident(input.to_owned())
            })
        );
    }

    #[test]
    fn test_integer_expression() {
        let input = "5;";
        let mut p = Parser::new(Lexer::new(input.to_owned()));
        let program = p.parse_program().unwrap();
        assert_eq!(program.statements.len(), 1);
        assert_eq!(
            program.statements[0],
            Statement::Expression(ExpressionStatement {
                expression: Expression::IntegerLiteral(5)
            })
        );
    }

    #[test]
    fn test_float_expression() {
        let tests = vec![("12.34", 12.34), ("0.56", 0.56), ("78.00", 78.00)];
        for t in tests.into_iter() {
            let mut p = Parser::new(Lexer::new(t.0.to_owned()));
            let program = p.parse_program().unwrap();
            assert_eq!(program.statements.len(), 1);
            assert_eq!(
                program.statements[0],
                Statement::Expression(ExpressionStatement {
                    expression: Expression::FloatLiteral(t.1)
                })
            );
        }
    }

    #[test]
    fn test_parseing_prefix_expression() {
        let tests = vec![
            ("!5", "!", Expression::IntegerLiteral(5)),
            ("-15", "-", Expression::IntegerLiteral(15)),
            ("!true", "!", Expression::Bool(true)),
            ("!false", "!", Expression::Bool(false)),
        ];
        for t in tests.into_iter() {
            let mut p = Parser::new(Lexer::new(t.0.to_owned()));
            let program = p.parse_program().unwrap();
            assert_eq!(program.statements.len(), 1);
            assert_eq!(
                program.statements[0],
                Statement::Expression(ExpressionStatement {
                    expression: Expression::Prefix(PrefixExpression {
                        operator: t.1.to_owned(),
                        right: Box::new(t.2),
                    })
                })
            );
        }
    }

    #[test]
    fn test_parseing_infix_expressions() {
        let tests = vec![
            ("5 + 5;", 5.into(), "+", 5.into()),
            ("5 - 5;", 5.into(), "-", 5.into()),
            ("5 * 5;", 5.into(), "*", 5.into()),
            ("5 / 5;", 5.into(), "/", 5.into()),
            ("5 > 5;", 5.into(), ">", 5.into()),
            ("5 < 5;", 5.into(), "<", 5.into()),
            ("5 == 5;", 5.into(), "==", 5.into()),
            ("5 != 5;", 5.into(), "!=", 5.into()),
            ("true == true", true.into(), "==", true.into()),
            ("true != false", true.into(), "!=", false.into()),
            ("false == false", false.into(), "==", false.into()),
            ("5 + 5.1;", 5.into(), "+", 5.1.into()),
            ("5.0 - 5.2;", 5.0.into(), "-", 5.2.into()),
            ("5.3 * 5.4;", 5.3.into(), "*", 5.4.into()),
            ("5.5 / 5.6;", 5.5.into(), "/", 5.6.into()),
            ("5.7 > 5.8;", 5.7.into(), ">", 5.8.into()),
            ("5.9 < 5;", 5.9.into(), "<", 5.into()),
            ("5 == 5.0;", 5.into(), "==", 5.0.into()),
            ("5.1 != 5.1;", 5.1.into(), "!=", 5.1.into()),
        ];
        for t in tests.into_iter() {
            let mut p = Parser::new(Lexer::new(t.0.to_owned()));
            let program = p.parse_program().unwrap();
            assert_eq!(program.statements.len(), 1);
            assert_eq!(
                program.statements[0],
                Statement::Expression(ExpressionStatement {
                    expression: Expression::Infix(InfixExpression {
                        left: Box::new(t.1),
                        operator: t.2.to_owned(),
                        right: Box::new(t.3),
                    })
                })
            );
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let tests = vec![
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
            (
                "a * [1, 2, 3, 4][b * c] * d",
                "((a * ([1, 2, 3, 4][(b * c)])) * d)",
            ),
            (
                "add(a * b[2], b[1], 2 * [1, 2][1])",
                "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
            ),
        ];
        for t in tests.into_iter() {
            println!("test: {}", t.0);
            let mut p = Parser::new(Lexer::new(t.0.to_owned()));
            let program = p.parse_program().unwrap();
            let s = program.to_string();
            assert_eq!(s, t.1.to_string());
        }
    }
}
