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
    Block(BlockStatement),
    Expression(ExpressionStatement),
    Null,
}

impl Statement {
    pub fn token_literal(&self) -> String {
        match self {
            Statement::Let(ls) => ls.token_literal(),
            Statement::Return(rs) => rs.token_literal(),
            Statement::Block(b) => b.token_literal(),
            Statement::Expression(es) => es.token_literal(),
            Statement::Null => "".to_string(),
        }
    }

    pub fn string(&self) -> String {
        match self {
            Statement::Let(ls) => ls.string(),
            Statement::Return(rs) => rs.string(),
            Statement::Block(bs) => bs.string(),
            Statement::Expression(es) => es.string(),
            Statement::Null => "".to_string(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>,
}

impl BlockStatement {
    pub fn token_literal(&self) -> String {
        self.token.literal.clone()
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
        format!(
            "{} {} = {};",
            self.token_literal(),
            self.name.value,
            self.value
        )
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
    IntegerLiteralExpr(IntegerLiteral),
    BooleanLiteralExpr(BooleanLiteral),
    StringLiteralExpr(StringLiteral),

    FunctionExpr(FunctionLiteral),
    CallExpr(CallExpression),

    IfExpr(IfExpression),
    PrefixExpr(PrefixExpression),
    InfixExpr(InfixExpression),
    Null,
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::IdentifierExpr(i) => write!(f, "{}", i.value),
            Expression::IntegerLiteralExpr(il) => write!(f, "{}", il.value),
            Expression::BooleanLiteralExpr(b) => write!(f, "{}", b.value),
            Expression::StringLiteralExpr(s) => write!(f, "{}", s.value),

            Expression::FunctionExpr(fl) => write!(f, "{}", fl.token.literal),
            Expression::CallExpr(ce) => write!(f, "{}", ce.string()),

            Expression::IfExpr(ie) => write!(f, "{}", ie.string()),
            Expression::PrefixExpr(pe) => write!(f, "{}", pe.string()),
            Expression::InfixExpr(ie) => write!(f, "{}", ie.string()),
            Expression::Null => write!(f, "null"),
        }
    }
}

impl Expression {
    pub fn token_literal(&self) -> String {
        match self {
            Expression::IdentifierExpr(i) => i.token_literal(),
            Expression::IntegerLiteralExpr(il) => il.token_literal(),
            Expression::BooleanLiteralExpr(b) => b.token.literal.clone(),
            Expression::StringLiteralExpr(s) => s.token.literal.clone(),

            Expression::FunctionExpr(fl) => fl.token.literal.clone(),
            Expression::CallExpr(ce) => ce.token_literal(),

            Expression::IfExpr(ie) => ie.token_literal(),
            Expression::PrefixExpr(pe) => pe.token_literal(),
            Expression::InfixExpr(ie) => ie.token_literal(),
            Expression::Null => "".to_string(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct CallExpression {
    pub token: Token,
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
}

impl CallExpression {
    pub fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    pub fn string(&self) -> String {
        let mut out = String::new();
        let mut args = Vec::new();
        for a in &self.arguments {
            args.push(a.to_string());
        }
        out.push_str(&format!("{}({})", self.function, args.join(", ")));
        out
    }
}

#[derive(Debug, Clone)]
pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
}

impl FunctionLiteral {
    pub fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    pub fn string(&self) -> String {
        let mut out = String::new();
        let mut params = Vec::new();
        for p in &self.parameters {
            params.push(p.value.clone());
        }
        out.push_str(&format!(
            "{}({}) {}",
            self.token_literal(),
            params.join(", "),
            self.body.string()
        ));
        out
    }
}

#[derive(Debug, Clone)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Box<Expression>,
    pub consequence: Option<BlockStatement>,
    pub alternative: Option<BlockStatement>,
}

impl IfExpression {
    pub fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    pub fn string(&self) -> String {
        let mut out = String::new();
        let consequence = match &self.consequence {
            Some(c) => c.string(),
            None => "".to_string(),
        };
        let alt = match &self.alternative {
            Some(a) => a.string(),
            None => "".to_string(),
        };
        out.push_str(&format!("if {} {} {}", self.condition, consequence, alt));
        out
    }
}

#[derive(Debug, Clone)]
pub struct BooleanLiteral {
    pub token: Token,
    pub value: bool,
}

impl BooleanLiteral {
    pub fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    pub fn string(&self) -> String {
        self.token.literal.clone()
    }
}

#[derive(Debug, Clone)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl IntegerLiteral {
    pub fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    pub fn string(&self) -> String {
        self.token.literal.clone()
    }
}

#[derive(Debug, Clone)]
pub struct StringLiteral {
    pub token: Token,
    pub value: String,
}

impl StringLiteral {
    pub fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    pub fn string(&self) -> String {
        self.token.literal.clone()
    }
}

#[derive(Debug, Clone)]
pub struct PrefixExpression {
    pub token: Token,           // The prefix token, e.g. !
    pub operator: String,       // The operator, e.g. !
    pub right: Box<Expression>, // The right expression
}

impl PrefixExpression {
    pub fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    pub fn string(&self) -> String {
        format!("({}{})", self.operator, self.right)
    }
}

#[derive(Debug, Clone)]
pub struct InfixExpression {
    pub token: Token,           // The infix token, e.g. +
    pub left: Box<Expression>,  // The left expression
    pub operator: String,       // The operator, e.g. +
    pub right: Box<Expression>, // The right expression
}

impl InfixExpression {
    pub fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    pub fn string(&self) -> String {
        format!("({} {} {})", self.left, self.operator, self.right)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string() {
        let program = Program::new(vec![Statement::Let(LetStatement {
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
        })]);

        let expected = "let myVar = anotherVar;";
        if program.string() != expected {
            panic!("Expected '{}', got '{}'", expected, program.string());
        }
    }
}
