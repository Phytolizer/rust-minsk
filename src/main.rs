use std::io::stdin;
use std::io::stdout;
use std::io::BufRead;
use std::io::BufReader;
use std::io::Write;

use lexer::Lexer;
use plumbing::Object;
use syntax::SyntaxKind;

use crate::parser::Parser;
use crate::syntax::SyntaxNode;

mod lexer;
mod parser;
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

        let mut parser = Parser::new(&line);
        let expression = parser.parse();
        SyntaxNode::Expression(*expression).pretty_print(&mut stdout());
    }
}
