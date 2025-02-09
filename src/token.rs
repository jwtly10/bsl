use lazy_static::lazy_static;
use std::collections::HashMap;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenType {
    Illegal,
    Eof,

    // Identifiers + literals
    Ident,
    Int,

    // Operators
    Assign,
    Plus,

    // Delimiters
    Comma,
    Semicolon,

    Lparen,
    Rparen,
    Lbrace,
    Rbrace,

    // Keywords
    Function,
    Let,
}

impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TokenType::Illegal => write!(f, "ILLEGAL"),
            TokenType::Eof => write!(f, "EOF"),

            TokenType::Ident => write!(f, "IDENT"),
            TokenType::Int => write!(f, "INT"),

            TokenType::Assign => write!(f, "="),
            TokenType::Plus => write!(f, "+"),

            TokenType::Comma => write!(f, ","),
            TokenType::Semicolon => write!(f, ";"),
            TokenType::Lparen => write!(f, "("),
            TokenType::Rparen => write!(f, ")"),
            TokenType::Lbrace => write!(f, "{{"),
            TokenType::Rbrace => write!(f, "}}"),

            TokenType::Function => write!(f, "FUNCTION"),
            TokenType::Let => write!(f, "LET"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

impl Token {
    pub fn new(token_type: TokenType, literal: String) -> Token {
        Token {
            token_type,
            literal,
        }
    }
}

pub fn new_token(token_type: TokenType, ch: char) -> Token {
    Token::new(token_type, ch.to_string())
}

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, TokenType> = {
        let mut m = HashMap::new();
        m.insert("fn", TokenType::Function);
        m.insert("let", TokenType::Let );
        m
    };
}

pub fn lookup_identifier(identifier: &str) -> TokenType {
    match KEYWORDS.get(identifier) {
        Some(token_type) => token_type.clone(),
        None => TokenType::Ident,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lookup_identifier() {
        assert_eq!(TokenType::Function, lookup_identifier("fn"));
        assert_eq!(TokenType::Let, lookup_identifier("let"))
    }
}

