use minsk::compilation::Compilation;
use minsk::syntax::SyntaxNodeRef;
use minsk::syntax::SyntaxTree;
use std::io::stdin;
use std::io::stdout;
use std::io::BufRead;
use std::io::BufReader;
use std::io::Write;

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
            let tree_node = SyntaxNodeRef::Expression(syntax_tree.root.create_ref());
            tree_node.pretty_print(&mut stdout());
        }

        let compilation = Compilation::new(syntax_tree);
        let result = compilation.evaluate();

        match result {
            Err(diagnostics) => {
                print!("\x1b[0;31m");
                for diagnostic in diagnostics {
                    println!("{}", diagnostic);
                }
            }
            Ok(value) => {
                print!("\x1b[35m");
                println!("{}", value);
            }
        }
        print!("\x1b[0m");
    }
}
