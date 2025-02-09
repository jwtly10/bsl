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
            Some(';') => {
                let tok = new_token(TokenType::Semicolon, self.ch.unwrap());
                self.read_char();
                tok
            }
            Some('=') => {
                let tok = new_token(TokenType::Assign, self.ch.unwrap());
                self.read_char();
                tok
            }
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
            None => Token::new(TokenType::Eof, String::new()),
            Some(ch) => {
                if ch.is_whitespace() {
                    self.skip_whitespace();
                    self.next_token() // Recursively callback after skipping whitespace
                } else if is_letter(ch) {
                    let identifier = self.read_identifier();
                    return Token::new(lookup_identifier(identifier.as_str()), identifier);
                } else if is_digit(ch) {
                    return Token::new(TokenType::Int, self.read_number())
                } else {
                    let tok = new_token(TokenType::Illegal, ch);
                    self.read_char();
                    tok
                }
            }
        }
    }
}

fn is_letter(ch: char) -> bool {
    // TODO: Could use is_alphabetic()
    ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || ch == '_'
}

fn is_digit(ch: char) -> bool {
    '0' <= ch && ch <= '9'
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_read_identifier() {
        let input = String::from("testing");
        let mut lexer = Lexer::new(String::from(input.clone()));

        let output = lexer.read_identifier();

        assert_eq!(output, input)
    }

    #[test]
    fn test_is_letter() {
        assert_eq!(true, is_letter('a'));
        assert_eq!(true, is_letter('z'));
        assert_eq!(true, is_letter('_'));

        assert_eq!(false, is_letter('-'));
        assert_eq!(false, is_letter('1'));
    }

    #[test]
    fn test_is_digit() {
        assert_eq!(true, is_digit('0'));
        assert_eq!(true, is_digit('1'));
        assert_eq!(true, is_digit('9'));

        assert_eq!(false, is_digit('d'));
        assert_eq!(false, is_digit('!'));
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
    fn test_next_token_snippet() {
        let input = "let five = 5;\n\
             let ten = 10;\n\
             let add = fn(x, y) {\n\
             x + y;\n\
             };\n\
             let result = add(five, ten);\n\
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
