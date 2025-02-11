use crate::lexer::Lexer;
use crate::parser::{ParseError, Parser};
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
    let lexer = Lexer::new(line.to_string());
    // TODO: remove .expect()
    let mut parser = Parser::new(lexer).expect("Failed to create parser... should not happen");

    let program = parser.parse_program();
    if !parser.errors().is_empty() {
        print_parser_errors(out_stream, parser.errors());
        return Ok(());
    }

    writeln!(out_stream, "{}", program.string()).expect("Failed to write program to output");
    Ok(())
}

fn print_parser_errors(out_stream: &mut dyn Write, errors: &Vec<ParseError>) {
    for error in errors {
        writeln!(out_stream, "parser errors:").expect("Failed to write error sub text to output");
        writeln!(out_stream, "\t{}", error).expect("Failed to write error to output");
    }
}
