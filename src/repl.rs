use crate::{
    ast::Node,
    lexer::Lexer,
    parser::{self, Parser},
};
use std::io::{self, BufRead, Write};

pub fn start<T: BufRead, U: Write>(input: T, mut output: U) {
    let mut scanner = io::BufReader::new(input);
    let mut buf = String::new();

    loop {
        print!("{}", "\n>> ");
        io::stdout().flush().unwrap();
        let scanned = scanner.read_line(&mut buf).unwrap();
        if scanned == 0 {
            return;
        }

        let line = buf.trim_end().to_string();
        let l = Lexer::new(line);
        let mut p = parser::Parser::new(l);

        let program = Parser::parse_program(&mut p);
        if p.errors().len() != 0 {
            print_parser_errors(&mut output, p.errors());
            p.clear_errors();
        }

        print!("{}", program.unwrap().string());

        io::stdout().flush().unwrap();
        buf.clear();
    }

    fn print_parser_errors(output: &mut dyn Write, errors: Vec<String>) {
        println!("Whoops! We ran into some monkey business here!");
        println!(" parser errors:");
        for msg in errors {
            writeln!(output, "\t{}", msg).unwrap();
        }
    }
}
