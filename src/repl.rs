use crate::lexer::Lexer;
use crate::token::TokenType;
use std::io;
use std::io::{BufRead, Write};

static PROMPT: &str = ">> ";


pub fn start_repl(in_stream: &mut dyn BufRead, out_stream: &mut dyn Write) -> io::Result<()> {
    loop {
        write!(out_stream, "{}", PROMPT)?;
        out_stream.flush()?;

        let mut line = String::new();
        let bytes_read = in_stream.read_line(&mut line)?;

        if bytes_read == 0 {
            break; // EOF
        }

        process_line(line.trim(), out_stream)?;
    }

    Ok(())
}

fn process_line(line: &str, out_stream: &mut dyn Write) -> io::Result<()> {
    let mut lexer = Lexer::new(line.to_string());
    loop {
        let tok = lexer.next_token();

        if tok.token_type == TokenType::Eof {
            break;
        }

        writeln!(out_stream, "{:?}", tok)?;
    }

    Ok(())
}

