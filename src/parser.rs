use crate::ast::Expression::Null;
use crate::ast::{
    BlockStatement, BooleanLiteral, Expression, ExpressionStatement, Identifier, IfExpression,
    InfixExpression, IntegerLiteral, LetStatement, PrefixExpression, Program, ReturnStatement,
    Statement,
};
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};
use crate::trace::TraceGuard;
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::fmt;

lazy_static! {
    static ref PRECEDENCES: HashMap<&'static TokenType, Precedence> = {
        let mut m = HashMap::new();
        m.insert(&TokenType::Eq, Precedence::Equals);
        m.insert(&TokenType::NotEq, Precedence::Equals);
        m.insert(&TokenType::LT, Precedence::LessGreater);
        m.insert(&TokenType::GT, Precedence::LessGreater);
        m.insert(&TokenType::Plus, Precedence::Sum);
        m.insert(&TokenType::Minus, Precedence::Sum);
        m.insert(&TokenType::Slash, Precedence::Product);
        m.insert(&TokenType::Asterisk, Precedence::Product);
        m
    };
}

type PrefixParseFn = fn(&mut Parser) -> Result<Expression, ParseError>;
type InfixParseFn = fn(&mut Parser, Expression) -> Result<Expression, ParseError>;

#[derive(Debug, PartialEq, PartialOrd, Ord, Eq, Copy, Clone)]
pub enum Precedence {
    Lowest,
    Equals,      // ==
    LessGreater, // > or <
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // myFunction(X)
}

#[derive(Debug, PartialEq)]
pub enum ParseError {
    UnexpectedToken {
        expected: TokenType,
        actual: TokenType,
    },
    PrefixParseFnNotFound {
        token_type: TokenType,
    },
    IntegerConv {
        value: String,
    },
    EndOfFile,
    ExpectedIdentifier,
    ExpectedExpression,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::UnexpectedToken { expected, actual } => {
                write!(
                    f,
                    "expected next token to be {:?}, got {:?} instead",
                    expected.to_string(),
                    actual.to_string()
                )
            }
            ParseError::PrefixParseFnNotFound { token_type } => {
                write!(
                    f,
                    "no prefix parse found function for {:?}",
                    token_type.to_string()
                )
            }
            ParseError::IntegerConv { value } => {
                write!(f, "could not parse '{}' as integer", value)
            }
            ParseError::EndOfFile => {
                write!(f, "unexpected end of file")
            }
            ParseError::ExpectedIdentifier => {
                write!(f, "expected identifier")
            }
            ParseError::ExpectedExpression => {
                write!(f, "expected expression")
            }
        }
    }
}

pub struct Parser {
    lexer: Lexer,
    errors: Vec<ParseError>,
    cur_token: Token,
    peek_token: Token,

    prefix_parse_fns: HashMap<TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<TokenType, InfixParseFn>,
}

impl Parser {
    pub fn new(mut l: Lexer) -> Result<Parser, ParseError> {
        // Read tokens to populate on init
        let cur_token = l.next_token();
        let peek_token = l.next_token();

        let mut p = Parser {
            lexer: l,
            errors: Vec::new(),
            cur_token,
            peek_token,
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };

        p.register_prefix(TokenType::Ident, Parser::parse_identifier);
        p.register_prefix(TokenType::Int, Parser::parse_integer_literal);

        p.register_prefix(TokenType::Bang, Parser::parse_prefix_expression);
        p.register_prefix(TokenType::Minus, Parser::parse_prefix_expression);

        p.register_infix(TokenType::Plus, Parser::parse_infix_expression);
        p.register_infix(TokenType::Minus, Parser::parse_infix_expression);
        p.register_infix(TokenType::Slash, Parser::parse_infix_expression);
        p.register_infix(TokenType::Asterisk, Parser::parse_infix_expression);
        p.register_infix(TokenType::Eq, Parser::parse_infix_expression);
        p.register_infix(TokenType::NotEq, Parser::parse_infix_expression);
        p.register_infix(TokenType::LT, Parser::parse_infix_expression);
        p.register_infix(TokenType::GT, Parser::parse_infix_expression);

        p.register_prefix(TokenType::True, Parser::parse_boolean_literal);
        p.register_prefix(TokenType::False, Parser::parse_boolean_literal);

        p.register_prefix(TokenType::Lparen, Parser::parse_grouped_expression);

        p.register_prefix(TokenType::If, Parser::parse_if_expression);

        Ok(p)
    }

    pub fn next_token(&mut self) {
        self.cur_token = std::mem::replace(&mut self.peek_token, self.lexer.next_token());
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program::new(Vec::new());
        while self.cur_token.token_type != TokenType::Eof {
            if let Some(stmt) = self.parse_statement() {
                program.statements.push(stmt)
            }
            self.next_token();
        }

        program
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.cur_token.token_type {
            TokenType::Let => self.parse_let_statement(),
            TokenType::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ParseError> {
        let _trace = TraceGuard::new("parse_expression");
        let mut left = match self.prefix_parse_fns.get(&self.cur_token.token_type) {
            Some(prefix_fn) => prefix_fn(self)?,
            None => {
                self.no_prefix_parse_fn_error(&self.cur_token.clone().token_type);
                // Removed this so we would fail more inline with the book,
                // although it is better to fail explicitly
                // Err(ParseError::ExpectedExpression)
                return Ok(Expression::Null);
            }
        };

        while !self.peek_token_is(&TokenType::Semicolon) && precedence < self.peek_precedence() {
            let has_infix = self
                .infix_parse_fns
                .contains_key(&self.peek_token.token_type);
            if !has_infix {
                break;
            }

            self.next_token();

            if let Some(infix_fn) = self.infix_parse_fns.get(&self.cur_token.token_type) {
                left = infix_fn(self, left)?;
            }
        }

        Ok(left)
    }

    fn parse_identifier(&mut self) -> Result<Expression, ParseError> {
        Ok(Expression::IdentifierExpr(Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        }))
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        let let_token = self.cur_token.clone();

        if !self.expect_peek(&TokenType::Ident) {
            return None;
        }

        let name = Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.clone().literal,
        };

        // Consume the assign token
        if !self.expect_peek(&TokenType::Assign) {
            return None;
        }

        // TODO: Parse the expression
        // For now, skip until the semicolon
        while !self.cur_token_is(&TokenType::Semicolon) {
            self.next_token();
        }

        Some(Statement::Let(LetStatement {
            token: let_token,
            name,
            value: Expression::IdentifierExpr(Identifier {
                token: Token::new(TokenType::Ident, "dummy".to_string()),
                value: "dummy".to_string(),
            }), // Placeholder
        }))
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        let let_token = self.cur_token.clone();

        self.next_token();

        // TODO: Parse the expression
        // For now, skip until the semicolon
        while !self.cur_token_is(&TokenType::Semicolon) {
            self.next_token();
        }

        Some(Statement::Return(ReturnStatement {
            token: let_token,
            return_value: Expression::IdentifierExpr(Identifier {
                token: Token::new(TokenType::Ident, "dummy".to_string()),
                value: "dummy".to_string(),
            }), // Placeholder
        }))
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let _trace = TraceGuard::new("parse_expression_statement");
        let stmt = Statement::Expression(ExpressionStatement {
            token: self.cur_token.clone(),
            expression: match self.parse_expression(Precedence::Lowest) {
                Ok(expr) => expr,
                Err(err) => {
                    self.errors.push(err);
                    // Return a Null expression in case of error
                    Null
                }
            },
        });

        if self.peek_token_is(&TokenType::Semicolon) {
            self.next_token();
        }

        Some(stmt)
    }

    fn parse_boolean_literal(&mut self) -> Result<Expression, ParseError> {
        let _trace = TraceGuard::new("parse_boolean_literal");
        match self.cur_token_is(&TokenType::True) {
            true => Ok(Expression::BooleanLiteralExpr(BooleanLiteral {
                token: self.cur_token.clone(),
                value: true,
            })),
            false => Ok(Expression::BooleanLiteralExpr(BooleanLiteral {
                token: self.cur_token.clone(),
                value: false,
            })),
        }
    }

    fn parse_integer_literal(&mut self) -> Result<Expression, ParseError> {
        let _trace = TraceGuard::new("parse_integration_literal");
        let cur_token_lit = self.cur_token.literal.clone();
        match cur_token_lit.parse::<i64>() {
            Ok(value) => Ok(Expression::IntegerLiteralExpr(IntegerLiteral {
                token: self.cur_token.clone(),
                value,
            })),
            Err(_) => Err(ParseError::IntegerConv {
                value: cur_token_lit,
            }),
        }
    }

    fn no_prefix_parse_fn_error(&mut self, t: &TokenType) {
        self.errors.push(ParseError::PrefixParseFnNotFound {
            token_type: t.clone(),
        });
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression, ParseError> {
        let _trace = TraceGuard::new("parse_prefix_expression");
        let token = self.cur_token.clone();
        let operator = self.cur_token.literal.clone();

        self.next_token();

        let right = self.parse_expression(Precedence::Prefix)?;

        Ok(Expression::PrefixExpr(PrefixExpression {
            token,
            operator,
            right: Box::new(right),
        }))
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression, ParseError> {
        let _trace = TraceGuard::new("parse_infix_expression");
        let token = self.cur_token.clone();
        let operator = self.cur_token.literal.clone();
        let precedence = self.cur_precedence();

        self.next_token();

        let right = self.parse_expression(precedence)?;

        Ok(Expression::InfixExpr(InfixExpression {
            token,
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }))
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression, ParseError> {
        self.next_token();

        let exp = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_peek(&TokenType::Rparen) {
            return Ok(Expression::Null);
        }

        Ok(exp)
    }

    fn parse_if_expression(&mut self) -> Result<Expression, ParseError> {
        if !self.expect_peek(&TokenType::Lparen) {
            return Ok(Expression::Null);
        }

        self.next_token();

        let condition = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_peek(&TokenType::Rparen) {
            return Ok(Expression::Null);
        }

        if !self.expect_peek(&TokenType::Lbrace) {
            return Ok(Expression::Null);
        }

        let consequence = self.parse_block_statement()?;

        match self.peek_token_is(&TokenType::Else) {
            true => {
                self.next_token();
                if !self.expect_peek(&TokenType::Lbrace) {
                    return Ok(Expression::Null);
                }
                let alternative = self.parse_block_statement()?;

                Ok(Expression::IfExpr(IfExpression {
                    token: self.cur_token.clone(),
                    condition: Box::new(condition),
                    consequence: Some(consequence),
                    alternative: Some(alternative),
                }))
            }
            false => Ok(Expression::IfExpr(IfExpression {
                token: self.cur_token.clone(),
                condition: Box::new(condition),
                consequence: Some(consequence),
                alternative: None,
            })),
        }
    }

    fn parse_block_statement(&mut self) -> Result<BlockStatement, ParseError> {
        let mut statements = Vec::new();

        self.next_token();

        while !self.cur_token_is(&TokenType::Rbrace) && !self.cur_token_is(&TokenType::Eof) {
            if let Some(stmt) = self.parse_statement() {
                statements.push(stmt);
            }
            self.next_token();
        }

        Ok(BlockStatement {
            token: self.cur_token.clone(),
            statements,
        })
    }

    fn cur_token_is(&self, t: &TokenType) -> bool {
        &self.cur_token.token_type == t
    }

    fn peek_token_is(&self, t: &TokenType) -> bool {
        &self.peek_token.token_type == t
    }

    fn expect_peek(&mut self, t: &TokenType) -> bool {
        if self.peek_token_is(t) {
            self.next_token();
            true
        } else {
            self.peek_error(t);
            false
        }
    }

    pub fn errors(&self) -> &Vec<ParseError> {
        &self.errors
    }

    fn peek_error(&mut self, t: &TokenType) {
        self.errors.push(ParseError::UnexpectedToken {
            expected: t.clone(),
            actual: self.peek_token.token_type.clone(),
        });
    }

    fn peek_precedence(&self) -> Precedence {
        match PRECEDENCES.get(&self.peek_token.token_type) {
            Some(p) => p.clone(),
            None => Precedence::Lowest,
        }
    }

    fn cur_precedence(&self) -> Precedence {
        match PRECEDENCES.get(&self.cur_token.token_type) {
            Some(p) => *p,
            None => Precedence::Lowest,
        }
    }

    fn register_prefix(
        &mut self,
        t: TokenType,
        f: fn(&mut Parser) -> Result<Expression, ParseError>,
    ) {
        self.prefix_parse_fns.insert(t, f);
    }

    fn register_infix(&mut self, t: TokenType, f: InfixParseFn) {
        self.infix_parse_fns.insert(t, f);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Statement;

    #[derive(Debug)]
    pub enum ExpectedValue {
        Int(i64),
        Boolean(bool),
        Identifier(String),
    }

    #[test]
    fn test_let_statements() {
        let input = "\n\
             let x = 5;\n\
             let y = 10;\n\
             let foobar = 838383;\n\
             ";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer).unwrap();

        let program = parser.parse_program();

        check_parser_errors(&parser);

        if program.statements.len() != 3 {
            panic!(
                "program.statements does not contain 3 statements. got='{:?}'",
                program.statements.len()
            );
        }

        let tests = ["x", "y", "foobar"];

        for (i, tt) in tests.iter().enumerate() {
            let stmt = &program.statements[i];
            if !test_let_statement(stmt, tt) {
                panic!("test_let_statement failed for statement '{}'", i)
            }
        }
    }

    #[test]
    fn test_return_statements() {
        let input = "\n\
             return 5;\n\
             return 10;\n\
             return 993322;\n\
             ";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer).unwrap();

        let program = parser.parse_program();
        check_parser_errors(&parser);

        if program.statements.len() != 3 {
            panic!(
                "program.statements does not contain 3 statements. got='{:?}'",
                program.statements.len()
            );
        }

        program.statements.iter().for_each(|stmt| match stmt {
            Statement::Return(return_stmt) => {
                if return_stmt.token_literal() != "return" {
                    panic!(
                        "return_stmt.token_literal() not 'return'. got='{}'",
                        return_stmt.token_literal()
                    );
                }
            }
            _ => panic!("stmt not ReturnStatement. got='{:?}'", stmt),
        })
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer).unwrap();

        let program = parser.parse_program();
        check_parser_errors(&parser);

        if program.statements.len() != 1 {
            panic!(
                "program.statements does not contain 1 statement. got='{:?}'",
                program.statements.len()
            );
        }

        let stmt = &program.statements[0];
        let expr = match stmt {
            Statement::Expression(expr_stmt) => expr_stmt,
            _ => panic!("stmt not ExpressionStatement. got='{:?}'", stmt),
        };

        let ident = match &expr.expression {
            Expression::IdentifierExpr(ident) => ident,
            _ => panic!("expr not Identifier. got='{:?}'", expr.expression),
        };

        if ident.value != "foobar" {
            panic!("ident.value not 'foobar'. got='{}'", ident.value);
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let tests = vec![
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("true", "true"),
            ("false", "false"),
            ("-a * b", "((-a) * b)"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
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
        ];

        let mut errs = 0;
        for (input, expected) in tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer).unwrap();
            let program = parser.parse_program();
            check_parser_errors(&parser);

            let actual = program.string();
            if actual != expected {
                eprintln!("expected='{}', got='{}'", expected, actual);
                errs += 1;
            }
        }

        if errs > 0 {
            panic!("Test failed. we have errors")
        }
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer).unwrap();
        let program = parser.parse_program();
        check_parser_errors(&parser);

        if program.statements.len() != 1 {
            panic!(
                "program.statements does not contain 1 statement. got='{:?}'",
                program.statements.len()
            );
        }

        let stmt = &program.statements[0];

        let expr = match stmt {
            Statement::Expression(expr_stmt) => expr_stmt,
            _ => panic!("stmt not ExpressionStatement. got='{:?}'", stmt),
        };

        if !test_integer_literal(&expr.expression, 5) {
            panic!("test_integer_literal failed for expression");
        }
    }

    #[test]
    fn test_boolean_literal_expression() {
        let input = "true;";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer).unwrap();
        let program = parser.parse_program();
        check_parser_errors(&parser);

        if program.statements.len() != 1 {
            panic!(
                "program.statements does not contain 1 statement. got='{:?}'",
                program.statements.len()
            );
        }

        let stmt = &program.statements[0];
        let expr = match stmt {
            Statement::Expression(expr_stmt) => expr_stmt,
            _ => panic!("stmt not ExpressionStatement. got='{:?}'", stmt),
        };

        let literal = match &expr.expression {
            Expression::BooleanLiteralExpr(lit) => lit,
            _ => panic!("expr not BooleanLiteral. got='{:?}'", expr.expression),
        };

        if !literal.value {
            panic!("literal.value not 'true'. got='{}'", literal.value);
        }
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        let tests = vec![
            ("!5;", "!", ExpectedValue::Int(5)),
            ("-15;", "-", ExpectedValue::Int(15)),
            ("!true;", "!", ExpectedValue::Boolean(true)),
            ("!false;", "!", ExpectedValue::Boolean(false)),
        ];

        for (input, operator, value) in tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer).unwrap();
            let program = parser.parse_program();
            check_parser_errors(&parser);

            if program.statements.len() != 1 {
                panic!(
                    "program.statements does not contain 1 statement. got='{:?}'",
                    program.statements.len()
                );
            }

            let stmt = &program.statements[0];
            let expr = match stmt {
                Statement::Expression(expr_stmt) => expr_stmt,
                _ => panic!("stmt not ExpressionStatement. got='{:?}'", stmt),
            };

            let prefix = match &expr.expression {
                Expression::PrefixExpr(prefix) => prefix,
                _ => panic!("expr not PrefixExpression. got='{:?}'", expr.expression),
            };

            if prefix.operator != operator {
                panic!(
                    "prefix.operator is not '{}'. got='{}'",
                    operator, prefix.operator
                );
            }

            if !test_literal_expression(&prefix.right, &value) {
                panic!("test_literal_expression failed for prefix.right");
            }
        }
    }

    #[test]
    fn test_parsing_infix_expressions() {
        let infix_tests = vec![
            ("5 + 5;", ExpectedValue::Int(5), "+", ExpectedValue::Int(5)),
            ("5 - 5;", ExpectedValue::Int(5), "-", ExpectedValue::Int(5)),
            ("5 * 5;", ExpectedValue::Int(5), "*", ExpectedValue::Int(5)),
            ("5 / 5;", ExpectedValue::Int(5), "/", ExpectedValue::Int(5)),
            ("5 > 5;", ExpectedValue::Int(5), ">", ExpectedValue::Int(5)),
            ("5 < 5;", ExpectedValue::Int(5), "<", ExpectedValue::Int(5)),
            (
                "5 == 5;",
                ExpectedValue::Int(5),
                "==",
                ExpectedValue::Int(5),
            ),
            (
                "5 != 5;",
                ExpectedValue::Int(5),
                "!=",
                ExpectedValue::Int(5),
            ),
            (
                "true == true",
                ExpectedValue::Boolean(true),
                "==",
                ExpectedValue::Boolean(true),
            ),
        ];

        for (input, left_value, operator, right_value) in infix_tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer).unwrap();
            let program = parser.parse_program();
            check_parser_errors(&parser);

            if program.statements.len() != 1 {
                panic!(
                    "program.statements does not contain 1 statement. got='{:?}'",
                    program.statements.len()
                );
            }

            let stmt = &program.statements[0];
            let expr = match stmt {
                Statement::Expression(expr_stmt) => expr_stmt,
                _ => panic!("stmt not ExpressionStatement. got='{:?}'", stmt),
            };

            if !test_infix_expression(&expr.expression, &left_value, operator, &right_value) {
                panic!("test_infix_expression failed for expression");
            }
        }
    }

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer).unwrap();
        let program = parser.parse_program();
        check_parser_errors(&parser);

        if program.statements.len() != 1 {
            panic!(
                "program.statements does not contain 1 statement. got='{:?}'",
                program.statements.len()
            );
        }

        let stmt = &program.statements[0];
        let expr = match stmt {
            Statement::Expression(expr_stmt) => expr_stmt,
            _ => panic!("stmt not ExpressionStatement. got='{:?}'", stmt),
        };

        let if_expr = match &expr.expression {
            Expression::IfExpr(if_expr) => if_expr,
            _ => panic!("expr not IfExpression. got='{:?}'", expr.expression),
        };

        if !test_infix_expression(
            &if_expr.condition,
            &ExpectedValue::Identifier("x".to_string()),
            "<",
            &ExpectedValue::Identifier("y".to_string()),
        ) {
            panic!("test_infix_expression failed for if_expr.condition");
        }

        if if_expr.consequence.as_ref().unwrap().statements.len() != 1 {
            panic!(
                "consequence is not 1 statement. got='{:?}'",
                if_expr.consequence.as_ref().unwrap().statements.len()
            );
        }

        let consequence = &if_expr.consequence.as_ref().unwrap().statements[0];
        let consequence_expr = match consequence {
            Statement::Expression(expr_stmt) => expr_stmt,
            _ => panic!("stmt not ExpressionStatement. got='{:?}'", consequence),
        };

        if !test_identifier(&consequence_expr.expression, "x".to_string()) {
            panic!("test_identifier failed for consequence");
        }

        if let Some(_) = if_expr.alternative {
            panic!(
                "if_expr.alternative was not None. got='{:?}'",
                if_expr.alternative
            )
        }
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x } else { y }";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer).unwrap();
        let program = parser.parse_program();
        check_parser_errors(&parser);

        if program.statements.len() != 1 {
            panic!(
                "program.statements does not contain 1 statement. got='{:?}'",
                program.statements.len()
            );
        }

        let stmt = &program.statements[0];
        let expr = match stmt {
            Statement::Expression(expr_stmt) => expr_stmt,
            _ => panic!("stmt not ExpressionStatement. got='{:?}'", stmt),
        };

        let if_expr = match &expr.expression {
            Expression::IfExpr(if_expr) => if_expr,
            _ => panic!("expr not IfExpression. got='{:?}'", expr.expression),
        };

        if !test_infix_expression(
            &if_expr.condition,
            &ExpectedValue::Identifier("x".to_string()),
            "<",
            &ExpectedValue::Identifier("y".to_string()),
        ) {
            panic!("test_infix_expression failed for if_expr.condition");
        }

        if if_expr.consequence.as_ref().unwrap().statements.len() != 1 {
            panic!(
                "consequence is not 1 statement. got='{:?}'",
                if_expr.consequence.as_ref().unwrap().statements.len()
            );
        }

        let consequence = &if_expr.consequence.as_ref().unwrap().statements[0];
        let consequence_expr = match consequence {
            Statement::Expression(expr_stmt) => expr_stmt,
            _ => panic!("stmt not ExpressionStatement. got='{:?}'", consequence),
        };

        if !test_identifier(&consequence_expr.expression, "x".to_string()) {
            panic!("test_identifier failed for consequence");
        }

        if if_expr.alternative.is_none() {
            panic!(
                "if_expr.alternative was None. got='{:?}'",
                if_expr.alternative
            )
        }

        let alternative = if_expr.alternative.as_ref().unwrap();
        if alternative.statements.len() != 1 {
            panic!(
                "alternative is not 1 statement. got='{:?}'",
                alternative.statements.len()
            );
        }

        let alt_stmt = &alternative.statements[0];
        let alt_expr = match alt_stmt {
            Statement::Expression(expr_stmt) => expr_stmt,
            _ => panic!("stmt not ExpressionStatement. got='{:?}'", alt_stmt),
        };

        if !test_identifier(&alt_expr.expression, "y".to_string()) {
            panic!("test_identifier failed for alternative");
        }
    }

    // **************************************************** //
    // ***************** Helper functions ***************** //
    // **************************************************** //

    fn test_integer_literal(expr: &Expression, value: i64) -> bool {
        match expr {
            Expression::IntegerLiteralExpr(lit) => {
                if lit.value != value {
                    eprintln!(
                        "IntegerLiteralExpr has value '{}' expected '{}'",
                        lit.value, value
                    );
                    return false;
                }
                if lit.token_literal() != value.to_string() {
                    eprintln!(
                        "IntegerLiteralExpr has token_literal '{}' expected '{}'",
                        lit.token_literal(),
                        value
                    );
                    return false;
                }
                true
            }
            _ => {
                eprintln!("Expression is not IntegerLiteralExpr. got '{:?}'", expr);
                false
            }
        }
    }

    fn test_identifier(expr: &Expression, value: String) -> bool {
        match expr {
            Expression::IdentifierExpr(ident) => {
                if ident.value != value {
                    eprintln!(
                        "IdentifierExpr has value '{}' expected '{}'",
                        ident.value, value
                    );
                    return false;
                }
                if ident.token_literal() != value {
                    eprintln!(
                        "IdentifierExpr has token_literal '{}' expected '{}'",
                        ident.token_literal(),
                        value
                    );
                    return false;
                }
                true
            }
            _ => {
                eprintln!("Expression is not IdentifierExpr. got '{:?}'", expr);
                false
            }
        }
    }

    fn test_infix_expression(
        exp: &Expression,
        left: &ExpectedValue,
        operator: &str,
        right: &ExpectedValue,
    ) -> bool {
        match exp {
            Expression::InfixExpr(infix) => {
                if !test_literal_expression(&infix.left, left) {
                    return false;
                }
                if infix.operator != operator {
                    eprintln!(
                        "infix.operator is not '{}'. got='{}'",
                        operator, infix.operator
                    );
                    return false;
                }
                if !test_literal_expression(&infix.right, right) {
                    return false;
                }
                true
            }
            _ => {
                eprintln!("Expression is not InfixExpr. got '{:?}'", exp);
                false
            }
        }
    }

    fn test_literal_expression(exp: &Expression, expected: &ExpectedValue) -> bool {
        match expected {
            ExpectedValue::Int(expected_int) => {
                if let Expression::IntegerLiteralExpr(lit) = exp {
                    if lit.value != *expected_int {
                        eprintln!(
                            "IntegerLiteralExpr value mismatch: got {}, expected {}",
                            lit.value, expected_int
                        );
                        return false;
                    }
                    if lit.token_literal() != expected_int.to_string() {
                        eprintln!(
                            "IntegerLiteralExpr token mismatch: got {}, expected {}",
                            lit.token_literal(),
                            expected_int
                        );
                        return false;
                    }
                } else {
                    eprintln!("Expected IntegerLiteralExpr, but got: {:?}", exp);
                    return false;
                }
            }
            ExpectedValue::Identifier(expected_ident) => {
                if let Expression::IdentifierExpr(ident) = exp {
                    if ident.value != *expected_ident {
                        eprintln!(
                            "IdentifierExpr value mismatch: got {}, expected {}",
                            ident.value, expected_ident
                        );
                        return false;
                    }
                    if ident.token_literal() != *expected_ident {
                        eprintln!(
                            "IdentifierExpr token literal mismatch: got {}, expected {}",
                            ident.token_literal(),
                            expected_ident
                        );
                        return false;
                    }
                } else {
                    eprintln!("Expected IdentifierExpr, but got: {:?}", exp);
                    return false;
                }
            }
            ExpectedValue::Boolean(expected_bool) => {
                if let Expression::BooleanLiteralExpr(bool_expr) = exp {
                    if bool_expr.value != *expected_bool {
                        eprintln!(
                            "BooleanLiteralExpr value mismatch: got {}, expected {}",
                            bool_expr.value, expected_bool
                        );
                        return false;
                    }
                    if bool_expr.token_literal() != expected_bool.to_string() {
                        eprintln!(
                            "BooleanLiteralExpr token literal mismatch: got {}, expected {}",
                            bool_expr.token_literal(),
                            expected_bool
                        );
                        return false;
                    }
                } else {
                    eprintln!("Expected BooleanLiteralExpr, but got: {:?}", exp);
                    return false;
                }
            }
        }
        true
    }

    fn test_boolean_literal(expr: &Expression, value: bool) -> bool {
        match expr {
            Expression::BooleanLiteralExpr(lit) => {
                if lit.value != value {
                    eprintln!(
                        "BooleanLiteralExpr has value '{}' expected '{}'",
                        lit.value, value
                    );
                    return false;
                }
                if lit.token_literal() != value.to_string() {
                    eprintln!(
                        "BooleanLiteralExpr has token_literal '{}' expected '{}'",
                        lit.token_literal(),
                        value
                    );
                    return false;
                }
                true
            }
            _ => {
                eprintln!("Expression is not BooleanLiteralExpr. got '{:?}'", expr);
                false
            }
        }
    }

    fn test_let_statement(stmt: &Statement, name: &str) -> bool {
        match stmt {
            Statement::Let(let_stmt) => {
                if let_stmt.name.value != name {
                    eprintln!(
                        "let_stmt.name.value not '{}'. got='{}'",
                        name, let_stmt.name.value
                    );
                    return false;
                }
                if let_stmt.token_literal() != "let" {
                    eprintln!(
                        "let_stmt.token_literal() not 'let' got='{}'",
                        let_stmt.token_literal()
                    );
                    return false;
                }

                println!(
                    "let_stmt.name.value is '{}' & let_stmt.token_literal() is '{}'",
                    name,
                    let_stmt.token_literal()
                );
                true
            }
            _ => {
                eprintln!("s not letStatement. got '{:?}'", stmt);
                false
            }
        }
    }

    fn check_parser_errors(p: &Parser) {
        if !p.errors.is_empty() {
            eprintln!("parser has {} errors", p.errors.len());
            p.errors
                .iter()
                .for_each(|err| eprintln!("parser error: {}", err));
            panic!("Test failed. we have errors")
        }
    }
}
