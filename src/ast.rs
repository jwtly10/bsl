/**/
use crate::token::Token;
use std::fmt;

pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new(statements: Vec<Statement>) -> Self {
        Program { statements }
    }
    pub fn string(&self) -> String {
        let mut out = String::new();
        for s in &self.statements {
            out.push_str(&s.string());
        }
        out
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
}

impl Statement {
    pub fn token_literal(&self) -> String {
        match self {
            Statement::Let(ls) => ls.token_literal(),
            Statement::Return(rs) => rs.token_literal(),
            Statement::Expression(es) => es.token_literal()
        }
    }

    pub fn string(&self) -> String {
        match self {
            Statement::Let(ls) => ls.string(),
            Statement::Return(rs) => rs.string(),
            Statement::Expression(es) => es.string()
        }
    }
}

#[derive(Debug, Clone)]
pub struct LetStatement {
    pub token: Token, // token.Let
    pub name: Identifier,
    pub value: Expression,
}

impl LetStatement {
    pub fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    pub fn string(&self) -> String {
        format!("{} {} = {};", self.token_literal(), self.name.value, self.value)
    }
}

#[derive(Debug, Clone)]
pub struct ReturnStatement {
    pub token: Token, // token.Return
    pub return_value: Expression,
}


impl ReturnStatement {
    pub fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    pub fn string(&self) -> String {
        format!("{} {};", self.token_literal(), self.return_value)
    }
}

#[derive(Debug, Clone)]
pub struct ExpressionStatement {
    pub token: Token, // fist token of exp
    pub expression: Expression,
}


impl ExpressionStatement {
    pub fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    pub fn string(&self) -> String {
        format!("{}", self.expression)
    }
}


#[derive(Debug, Clone)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Identifier {
    pub fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    pub fn string(&self) -> String {
        self.value.clone()
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    IdentifierExpr(Identifier),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::IdentifierExpr(i) => write!(f, "{}", i.value),
        }
    }
}

impl Expression {
    pub fn token_literal(&self) -> String {
        match self {
            Expression::IdentifierExpr(i) => i.token_literal(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string() {
        let program = Program::new(vec![
            Statement::Let(LetStatement {
                token: Token {
                    token_type: crate::token::TokenType::Let,
                    literal: "let".to_string(),
                },
                name: Identifier {
                    token: Token {
                        token_type: crate::token::TokenType::Ident,
                        literal: "myVar".to_string(),
                    },
                    value: "myVar".to_string(),
                },
                value: Expression::IdentifierExpr(Identifier {
                    token: Token {
                        token_type: crate::token::TokenType::Ident,
                        literal: "anotherVar".to_string(),
                    },
                    value: "anotherVar".to_string(),
                }),
            }),
        ]);

        let expected = "let myVar = anotherVar;";
        if program.string() != expected {
            panic!("Expected '{}', got '{}'", expected, program.string());
        }
    }
}

