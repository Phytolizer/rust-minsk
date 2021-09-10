use std::io::stdin;
use std::io::stdout;
use std::io::BufRead;
use std::io::BufReader;
use std::io::Write;

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
        line.clear();
        if reader.read_line(&mut line).unwrap() == 0 {
            println!();
            break;
        }
        line = line.chars().take(line.len() - 1).collect();

        let mut parser = Parser::new(&line);
        let syntax_tree = parser.parse();
        SyntaxNode::Expression(*syntax_tree.root).pretty_print(&mut stdout());
        if !syntax_tree.diagnostics.is_empty() {
            print!("\x1b[0;31m");
            for diagnostic in &syntax_tree.diagnostics {
                println!("{}", diagnostic);
            }
            print!("\x1b[0m");
        }
    }
}
