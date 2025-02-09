use bsl::repl;
use std::io::{self, BufReader, Write};

fn main() -> io::Result<()> {
    let stdin = io::stdin();
    let mut reader = BufReader::new(stdin);
    let mut stdout = io::stdout();

    writeln!(stdout, "Welcome to BSL (Better Scripting Language) REPL")?;

    repl::start_repl(&mut reader, &mut stdout)?;

    Ok(())
}
