use std::io::stdin;
use std::io::stdout;
use std::io::BufRead;
use std::io::BufReader;
use std::io::Write;

use crate::evaluator::Evaluator;
use crate::parser::Parser;
use crate::syntax::SyntaxNode;
use crate::syntax::SyntaxNodeRef;
use crate::syntax::SyntaxTree;

mod evaluator;
mod lexer;
mod parser;
mod plumbing;
mod syntax;

fn main() {
    let mut reader = BufReader::new(stdin());
    let mut line = String::new();
    let mut show_tree = false;
    loop {
        print!("\x1b[0;32m");
        print!("👉 ");
        print!("\x1b[0m");
        stdout().flush().unwrap();
        line.clear();
        if reader.read_line(&mut line).unwrap() == 0 {
            println!();
            break;
        }
        line = line.chars().take(line.len() - 1).collect();

        if line == "#showTree" {
            show_tree = !show_tree;
            println!(
                "{}",
                if show_tree {
                    "🌳\x1b[0;32m✔\x1b[0m Showing parse trees."
                } else {
                    "🌳\x1b[0;31m🗙\x1b[0m Not showing parse trees."
                }
            );
            continue;
        }

        let syntax_tree = SyntaxTree::parse(&line);
        if show_tree {
            print!("\x1b[2;37m");
            SyntaxNodeRef::Expression(syntax_tree.root.create_ref()).pretty_print(&mut stdout());
        }
        if !syntax_tree.diagnostics.is_empty() {
            print!("\x1b[0;31m");
            for diagnostic in &syntax_tree.diagnostics {
                println!("{}", diagnostic);
            }
        } else {
            let evaluator = Evaluator::new(syntax_tree.root);
            print!("\x1b[35m");
            println!("{}", evaluator.evaluate());
        }
        print!("\x1b[0m");
    }
}
