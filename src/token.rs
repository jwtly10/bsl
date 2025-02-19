use lazy_static::lazy_static;
use std::collections::HashMap;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum TokenType {
    Illegal,
    Eof,

    // Identifiers + literals
    Ident,
    Int,
    String,

    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,

    LT,
    GT,

    Eq,
    NotEq,

    // Delimiters
    Comma,
    Semicolon,

    Lparen,
    Rparen,
    Lbrace,
    Rbrace,

    Lbracket,
    Rbracket,

    // Keywords
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TokenType::Illegal => write!(f, "ILLEGAL"),
            TokenType::Eof => write!(f, "EOF"),

            // Identifiers + literals
            TokenType::Ident => write!(f, "IDENT"),
            TokenType::Int => write!(f, "INT"),
            TokenType::String => write!(f, "STRING"),

            // Operators
            TokenType::Assign => write!(f, "="),
            TokenType::Plus => write!(f, "+"),
            TokenType::Minus => write!(f, "-"),
            TokenType::Bang => write!(f, "!"),
            TokenType::Asterisk => write!(f, "*"),
            TokenType::Slash => write!(f, "/"),

            TokenType::LT => write!(f, "<"),
            TokenType::GT => write!(f, ">"),

            TokenType::Eq => write!(f, "=="),
            TokenType::NotEq => write!(f, "!="),

            // Delimiters
            TokenType::Comma => write!(f, ","),
            TokenType::Semicolon => write!(f, ";"),
            TokenType::Lparen => write!(f, "("),
            TokenType::Rparen => write!(f, ")"),
            TokenType::Lbrace => write!(f, "{{"),
            TokenType::Rbrace => write!(f, "}}"),
            TokenType::Lbracket => write!(f, "["),
            TokenType::Rbracket => write!(f, "["),

            // Keywords
            TokenType::Function => write!(f, "FUNCTION"),
            TokenType::Let => write!(f, "LET"),
            TokenType::True => write!(f, "TRUE"),
            TokenType::False => write!(f, "FALSE"),
            TokenType::If => write!(f, "IF"),
            TokenType::Else => write!(f, "ELSE"),
            TokenType::Return => write!(f, "RETURN"),
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
        m.insert("let", TokenType::Let);
        m.insert("true", TokenType::True);
        m.insert("false", TokenType::False);
        m.insert("if", TokenType::If);
        m.insert("else", TokenType::Else);
        m.insert("return", TokenType::Return);
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
