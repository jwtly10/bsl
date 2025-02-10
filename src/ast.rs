/**/use crate::token::Token;

pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new(statements: Vec<Statement>) -> Self {
        Program { statements }
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    // Expression(ExpressionStatement)
}

impl Statement {
    pub fn token_literal(&self) -> String {
        match self {
            Statement::Let(ls) => ls.token_literal(),
            Statement::Return(rs) => rs.token_literal(),
            // Statement::Expression(es) => es.token_literal()
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
}

#[derive(Debug, Clone)]
pub enum Expression {
    IdentifierExpr(Identifier),
}

impl Expression {
    pub fn token_literal(&self) -> String {
        match self {
            Expression::IdentifierExpr(i) => i.token_literal(),
        }
    }
}

