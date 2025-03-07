use crate::token::{lookup_identifier, new_token, Token, TokenType};

pub struct Lexer {
    pub input: String,
    pub position: usize,
    pub read_position: usize,
    pub ch: Option<char>, // Optional to handle EOF
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let mut l = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: None,
        };
        // Here we initialise the lexer, to read the first char
        l.read_char();
        l
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = None
        } else {
            self.ch = self.input.chars().nth(self.read_position);
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(&mut self) -> Option<char> {
        if self.read_position >= self.input.len() {
            None
        } else {
            Some(self.input.chars().nth(self.read_position).unwrap())
        }
    }

    fn read_number(&mut self) -> String {
        let start_pos = self.position;

        while let Some(ch) = self.ch {
            if !is_digit(ch) {
                break;
            }
            self.read_char()
        }

        let end_pos = self.position; // the last position before non-letter match
        self.input[start_pos..end_pos].to_string()
    }

    fn read_identifier(&mut self) -> String {
        let start_pos = self.position;

        // Keep reading until non-letter match
        while let Some(ch) = self.ch {
            if !is_letter(ch) {
                break;
            }
            self.read_char()
        }

        let end_pos = self.position; // the last position before non-letter match
        self.input[start_pos..end_pos].to_string()
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.ch {
            match ch {
                ' ' | '\t' | '\n' | '\r' => self.read_char(),
                _ => break,
            }
        }
    }

    pub fn next_token(&mut self) -> Token {
        match self.ch {
            // Operators
            Some('=') => match self.peek_char() {
                Some('=') => {
                    self.read_char();
                    let tok = Token::new(TokenType::Eq, "==".to_string());
                    self.read_char();
                    tok
                }
                _ => {
                    let tok = new_token(TokenType::Assign, self.ch.unwrap());
                    self.read_char();
                    tok
                }
            },
            Some('-') => {
                let tok = new_token(TokenType::Minus, self.ch.unwrap());
                self.read_char();
                tok
            }
            Some('!') => match self.peek_char() {
                Some('=') => {
                    self.read_char();
                    let tok = Token::new(TokenType::NotEq, "!=".to_string());
                    self.read_char();
                    tok
                }
                _ => {
                    let tok = new_token(TokenType::Bang, self.ch.unwrap());
                    self.read_char();
                    tok
                }
            },
            Some('/') => {
                let tok = new_token(TokenType::Slash, self.ch.unwrap());
                self.read_char();
                tok
            }
            Some('*') => {
                let tok = new_token(TokenType::Asterisk, self.ch.unwrap());
                self.read_char();
                tok
            }
            Some('<') => {
                let tok = new_token(TokenType::LT, self.ch.unwrap());
                self.read_char();
                tok
            }
            Some('>') => {
                let tok = new_token(TokenType::GT, self.ch.unwrap());
                self.read_char();
                tok
            }
            // Delimiters
            Some('(') => {
                let tok = new_token(TokenType::Lparen, self.ch.unwrap());
                self.read_char();
                tok
            }
            Some(')') => {
                let tok = new_token(TokenType::Rparen, self.ch.unwrap());
                self.read_char();
                tok
            }
            Some(',') => {
                let tok = new_token(TokenType::Comma, self.ch.unwrap());
                self.read_char();
                tok
            }
            Some('+') => {
                let tok = new_token(TokenType::Plus, self.ch.unwrap());
                self.read_char();
                tok
            }
            Some('{') => {
                let tok = new_token(TokenType::Lbrace, self.ch.unwrap());
                self.read_char();
                tok
            }
            Some('}') => {
                let tok = new_token(TokenType::Rbrace, self.ch.unwrap());
                self.read_char();
                tok
            }
            Some('[') => {
                let tok = new_token(TokenType::Lbracket, self.ch.unwrap());
                self.read_char();
                tok
            }
            Some(']') => {
                let tok = new_token(TokenType::Rbracket, self.ch.unwrap());
                self.read_char();
                tok
            }
            Some(';') => {
                let tok = new_token(TokenType::Semicolon, self.ch.unwrap());
                self.read_char();
                tok
            }
            Some(':') => {
                let tok = new_token(TokenType::Colon, self.ch.unwrap());
                self.read_char();
                tok
            }
            // String literals
            Some('"') => Token::new(TokenType::String, self.read_string()),
            // EOF
            None => Token::new(TokenType::Eof, String::new()),
            Some(ch) => {
                if ch.is_whitespace() {
                    self.skip_whitespace();
                    self.next_token() // Recursively callback after skipping whitespace
                } else if is_letter(ch) {
                    let identifier = self.read_identifier();
                    return Token::new(lookup_identifier(identifier.as_str()), identifier);
                } else if is_digit(ch) {
                    return Token::new(TokenType::Int, self.read_number());
                } else {
                    let tok = new_token(TokenType::Illegal, ch);
                    self.read_char();
                    tok
                }
            }
        }
    }

    fn read_string(&mut self) -> String {
        let start_pos = self.position + 1; // Skip the opening quote
        self.read_char(); // Skip the opening quote

        while let Some(ch) = self.ch {
            if ch == '"' || ch == '\0' {
                break;
            }
            self.read_char()
        }

        let end_pos = self.position; // the last position before non-letter match
        self.read_char(); // Skip the closing quote
        self.input[start_pos..end_pos].to_string()
    }
}

fn is_letter(ch: char) -> bool {
    ch.is_alphabetic() || ch == '_'
}

fn is_digit(ch: char) -> bool {
    ch.is_ascii_digit()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_read_identifier() {
        let input = String::from("testing");
        let mut lexer = Lexer::new(input.clone());

        let output = lexer.read_identifier();

        assert_eq!(output, input)
    }

    #[test]
    fn test_is_letter() {
        assert!(is_letter('a'));
        assert!(is_letter('z'));
        assert!(is_letter('_'));

        assert!(!is_letter('-'));
        assert!(!is_letter('1'));
    }

    #[test]
    fn test_is_digit() {
        assert!(is_digit('0'));
        assert!(is_digit('1'));
        assert!(is_digit('9'));

        assert!(!is_digit('d'));
        assert!(!is_digit('!'));
    }

    #[test]
    fn test_new_token_delim() {
        let input = "=+{}()";

        let tests = vec![
            (TokenType::Assign, "="),
            (TokenType::Plus, "+"),
            (TokenType::Lbrace, "{"),
            (TokenType::Rbrace, "}"),
            (TokenType::Lparen, "("),
            (TokenType::Rparen, ")"),
        ];

        let lexer = Lexer::new(input.to_string());

        test_case_assertions(lexer, tests)
    }

    #[test]
    fn test_new_token_ident() {
        let input = "let var = 5;";

        let tests = vec![
            (TokenType::Let, "let"),
            (TokenType::Ident, "var"),
            (TokenType::Assign, "="),
            (TokenType::Int, "5"),
            (TokenType::Semicolon, ";"),
        ];

        let lexer = Lexer::new(input.to_string());
        test_case_assertions(lexer, tests)
    }

    #[test]
    fn test_new_token_multi_char_symbols() {
        let input = "10 == 10;\n\
                10!= 9;\n\
             ";

        let tests = vec![
            (TokenType::Int, "10"),
            (TokenType::Eq, "=="),
            (TokenType::Int, "10"),
            (TokenType::Semicolon, ";"),
            (TokenType::Int, "10"),
            (TokenType::NotEq, "!="),
            (TokenType::Int, "9"),
            (TokenType::Semicolon, ";"),
            (TokenType::Eof, ""),
        ];

        let lexer = Lexer::new(input.to_string());
        test_case_assertions(lexer, tests)
    }

    #[test]
    fn test_next_token() {
        let input = "let five = 5;\n\
             let ten = 10;\n\
             let add = fn(x, y) {\n\
             x + y;\n\
             };\n\
             let result = add(five, ten);\n\
             !-/*5;
             5 < 10 > 5;\n\
             if (5<10){\n\
                 return true;\n\
             } else { \n\
                 return false;\n\
             }\n\
             10 == 10;\n\
             10 != 9;\n\
             \"foobar\";\n\
             \"foo bar\";\n\
             [1, 2];\n\
             {\"foo\": \"bar\"}\n\
             ";

        let tests = vec![
            (TokenType::Let, "let"),
            (TokenType::Ident, "five"),
            (TokenType::Assign, "="),
            (TokenType::Int, "5"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "let"),
            (TokenType::Ident, "ten"),
            (TokenType::Assign, "="),
            (TokenType::Int, "10"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "let"),
            (TokenType::Ident, "add"),
            (TokenType::Assign, "="),
            (TokenType::Function, "fn"),
            (TokenType::Lparen, "("),
            (TokenType::Ident, "x"),
            (TokenType::Comma, ","),
            (TokenType::Ident, "y"),
            (TokenType::Rparen, ")"),
            (TokenType::Lbrace, "{"),
            (TokenType::Ident, "x"),
            (TokenType::Plus, "+"),
            (TokenType::Ident, "y"),
            (TokenType::Semicolon, ";"),
            (TokenType::Rbrace, "}"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "let"),
            (TokenType::Ident, "result"),
            (TokenType::Assign, "="),
            (TokenType::Ident, "add"),
            (TokenType::Lparen, "("),
            (TokenType::Ident, "five"),
            (TokenType::Comma, ","),
            (TokenType::Ident, "ten"),
            (TokenType::Rparen, ")"),
            (TokenType::Semicolon, ";"),
            (TokenType::Bang, "!"),
            (TokenType::Minus, "-"),
            (TokenType::Slash, "/"),
            (TokenType::Asterisk, "*"),
            (TokenType::Int, "5"),
            (TokenType::Semicolon, ";"),
            (TokenType::Int, "5"),
            (TokenType::LT, "<"),
            (TokenType::Int, "10"),
            (TokenType::GT, ">"),
            (TokenType::Int, "5"),
            (TokenType::Semicolon, ";"),
            (TokenType::If, "if"),
            (TokenType::Lparen, "("),
            (TokenType::Int, "5"),
            (TokenType::LT, "<"),
            (TokenType::Int, "10"),
            (TokenType::Rparen, ")"),
            (TokenType::Lbrace, "{"),
            (TokenType::Return, "return"),
            (TokenType::True, "true"),
            (TokenType::Semicolon, ";"),
            (TokenType::Rbrace, "}"),
            (TokenType::Else, "else"),
            (TokenType::Lbrace, "{"),
            (TokenType::Return, "return"),
            (TokenType::False, "false"),
            (TokenType::Semicolon, ";"),
            (TokenType::Rbrace, "}"),
            (TokenType::Int, "10"),
            (TokenType::Eq, "=="),
            (TokenType::Int, "10"),
            (TokenType::Semicolon, ";"),
            (TokenType::Int, "10"),
            (TokenType::NotEq, "!="),
            (TokenType::Int, "9"),
            (TokenType::Semicolon, ";"),
            (TokenType::String, "foobar"),
            (TokenType::Semicolon, ";"),
            (TokenType::String, "foo bar"),
            (TokenType::Semicolon, ";"),
            (TokenType::Lbracket, "["),
            (TokenType::Int, "1"),
            (TokenType::Comma, ","),
            (TokenType::Int, "2"),
            (TokenType::Rbracket, "]"),
            (TokenType::Semicolon, ";"),
            (TokenType::Lbrace, "{"),
            (TokenType::String, "foo"),
            (TokenType::Colon, ":"),
            (TokenType::String, "bar"),
            (TokenType::Rbrace, "}"),
            (TokenType::Eof, ""),
        ];

        let lexer = Lexer::new(input.to_string());

        test_case_assertions(lexer, tests)
    }

    fn test_case_assertions(mut lexer: Lexer, tests: Vec<(TokenType, &str)>) {
        for (i, tt) in tests.iter().enumerate() {
            let tok = lexer.next_token();
            assert_eq!(
                tok.token_type, tt.0,
                "tests[{}] - tokenType wrong. Expected={:?}, got={:?}(char: {:?})",
                i, tt.0, tok.token_type, lexer.ch
            );
            assert_eq!(
                tok.literal, tt.1,
                "tests[{}] - literal wrong. expected={:?}, got={:?}",
                i, tt.1, tok.literal
            );
        }
    }
}
