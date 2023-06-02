mod lexer;
mod repl;
mod token;

use std::env;
use std::io;

fn main() {
    let user = env::var("USER").expect("Failed to get username");

    println!("Hello {}! This is the Monkey programming language!", user);
    println!("Feel free to type in commands");

    repl::start(io::stdin().lock(), io::stdout().lock());
}
