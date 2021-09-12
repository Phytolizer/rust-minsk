use minsk::compilation::Compilation;
use minsk::plumbing::Object;
use minsk::syntax::SyntaxNodeRef;
use minsk::syntax::SyntaxTree;
use minsk::text::VariableSymbol;
use std::collections::HashMap;
use std::io::stdin;
use std::io::stdout;
use std::io::BufRead;
use std::io::BufReader;
use std::io::Write;

fn main() {
    let mut reader = BufReader::new(stdin());
    let mut line = String::new();
    let mut show_tree = false;
    let mut variables = HashMap::<VariableSymbol, Object>::new();
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
            tree_node.pretty_print();
        }

        let source_text = syntax_tree.source_text.clone();
        let compilation = Compilation::new(syntax_tree);
        let result = compilation.evaluate(&mut variables);

        match result {
            Err(diagnostics) => {
                println!();
                for diagnostic in diagnostics {
                    let chars = line.chars().collect::<Vec<_>>();
                    let line_index = source_text.get_line_index(diagnostic.span.start);
                    let line_number = line_index + 1;
                    let char_offset = diagnostic.span.start - source_text.lines[line_index].start + 1;
                    let prefix = chars[source_text.lines[line_index].start..diagnostic.span.start]
                        .iter()
                        .collect::<String>();
                    let error = chars[diagnostic.span.start..diagnostic.span.end()]
                        .iter()
                        .collect::<String>();
                    let suffix = chars[diagnostic.span.end()..source_text.lines[line_index].end()]
                        .iter()
                        .collect::<String>();

                    print!("\x1b[0;31m");
                    print!("({}, {}): ", line_number, char_offset);
                    println!("{}", diagnostic);

                    print!("\x1b[0m");
                    print!("\t{}", prefix);
                    print!("\x1b[0;31m");
                    print!("{}", error);
                    print!("\x1b[0m");
                    print!("{}", suffix);
                    println!();
                }
                println!();
            }
            Ok(value) => {
                print!("\x1b[35m");
                println!("{}", value);
            }
        }
        print!("\x1b[0m");
    }
}
