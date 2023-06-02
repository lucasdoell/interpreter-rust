use crate::{lexer::Lexer, token};
use std::io::{self, BufRead, Write};

pub fn start<T: BufRead, U: Write>(input: T, mut output: U) {
    let mut scanner = io::BufReader::new(input);
    let mut buf = String::new();

    loop {
        print!("{}", ">> ");
        io::stdout().flush().unwrap();
        let scanned = scanner.read_line(&mut buf).unwrap();
        if scanned == 0 {
            return;
        }
        let line = buf.trim_end().to_string();
        let mut l = Lexer::new(line);
        loop {
            let tok = l.next_token();
            if tok.token_type == token::EOF {
                break;
            }
            write!(output, "{:?}\n", tok).unwrap();
        }
        buf.clear();
    }
}
