use crate::ast::{Expression, Identifier, LetStatement, Program, ReturnStatement, Statement};
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};
use std::fmt;

#[derive(Debug, PartialEq)]
pub enum ParseError {
    UnexpectedToken {
        expected: TokenType,
        actual: TokenType,
    },
    EndOfFile,
    ExpectedIdentifier,
    ExpectedExpression,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::UnexpectedToken { expected, actual } => {
                write!(f, "expected next token to be {:?}, got {:?} instead", expected.to_string(), actual.to_string())
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
}

impl Parser {
    pub fn new(mut l: Lexer) -> Result<Parser, ParseError> {
        // Read tokens to populate on init
        let cur_token = l.next_token();
        let peek_token = l.next_token();

        Ok(Parser {
            lexer: l,
            errors: Vec::new(),
            cur_token,
            peek_token,
        })
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
            _ => {
                None
            }
        }
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Statement;

    #[test]
    fn test_let_statements() {
        let input = "\n\
             let x = 5;\n\
             let y = 10;\n\
             let foobar = 838383;\n\
             ";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer).unwrap();

        let program = parser
            .parse_program();

        check_parser_errors(&parser);

        if program.statements.len() != 3 {
            panic!(
                "program.statements does not contain 3 statements. got='{:?}'",
                program.statements.len()
            );
        }

        let tests = [
            "x",
            "y",
            "foobar"
        ];

        for (i, tt) in tests.iter().enumerate() {
            let stmt = &program.statements[i];
            if !test_let_statement(stmt, tt) {
                panic!("test_let_statement failed for statement '{}'", i)
            }
        }
    }

    #[test]
    fn test_let_statements_errors() {
        let input = "\n\
             let x 5;\n\
             let  = 10;\n\
             let 838383;\n\
             ";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer).unwrap();

        let program = parser
            .parse_program();

        if parser.errors.len() != 3 {
            panic!("expected 3 errors got {}", parser.errors.len())
        }

        eprintln!("parser has {} errors", parser.errors.len());
        parser.errors
            .iter()
            .for_each(|err| eprintln!("parser error: {}", err));
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

                println!("let_stmt.name.value is '{}' & let_stmt.token_literal() is '{}'", name, let_stmt.token_literal());
                true
            }
            _ => {
                eprintln!("s not letStatement. got '{:?}'", stmt);
                false
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
            panic!("program.statements does not contain 3 statements. got='{:?}'", program.statements.len());
        }

        program.statements.iter().for_each(
            |stmt| {
                match stmt {
                    Statement::Return(return_stmt) => {
                        if return_stmt.token_literal() != "return" {
                            panic!("return_stmt.token_literal() not 'return'. got='{}'", return_stmt.token_literal());
                        }
                    }
                    _ => panic!("stmt not ReturnStatement. got='{:?}'", stmt)
                }
            }
        )
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
