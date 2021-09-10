use std::io::Write;
use std::io::stdin;
use std::io::BufRead;
use std::io::BufReader;
use std::io::stdout;

use lexer::Lexer;
use plumbing::Object;
use syntax::SyntaxKind;

mod lexer;
mod plumbing;
mod syntax;

fn main() {
    let mut reader = BufReader::new(stdin());
    let mut line = String::new();
    loop {
        print!("> ");
        stdout().flush().unwrap();
        if reader.read_line(&mut line).unwrap() == 0 {
            println!();
            break;
        }
        line = line.chars().take(line.len() - 1).collect();

        let mut lexer = Lexer::new(&line);
        let mut tokens = Vec::new();
        loop {
            tokens.push(lexer.next_token());
            if tokens.last().unwrap().kind == SyntaxKind::EndOfFileToken {
                break;
            }
        }

        for token in tokens {
            print!("{:?} '{}'", token.kind, token.text);
            if token.value != Object::Null {
                print!(" {}", token.value);
            }
            println!();
        }
    }
}
