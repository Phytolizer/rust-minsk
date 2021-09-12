use crossterm::cursor::MoveTo;
use crossterm::terminal::Clear;
use crossterm::terminal::ClearType;
use crossterm::ExecutableCommand;
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
    let mut input = String::new();
    let mut show_tree = false;
    let mut variables = HashMap::<VariableSymbol, Object>::new();
    let mut text_builder = String::new();
    loop {
        print!("\x1b[0;32m");
        if text_builder.is_empty() {
            print!("👉 ");
        } else {
            print!("👆 ");
        }
        print!("\x1b[0m");
        stdout().flush().unwrap();
        input.clear();
        if reader.read_line(&mut input).unwrap() == 0 {
            println!();
            break;
        }
        input = input.trim_end().to_string();

        if text_builder.is_empty() {
            match input.as_str() {
                "" => break,
                "#showTree" => {
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
                "#cls" => {
                    stdout()
                        .execute(Clear(ClearType::All))
                        .unwrap()
                        .execute(MoveTo(0, 0))
                        .unwrap();
                    continue;
                }
                _ => {}
            }
        }

        text_builder.push_str(&input);
        text_builder.push('\n');
        let syntax_tree = SyntaxTree::parse(&text_builder);

        if !input.is_empty() && !syntax_tree.diagnostics.is_empty() {
            continue;
        }

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
                    let chars = source_text.as_chars();
                    let line_index = source_text.get_line_index(diagnostic.span.start);
                    let line = &source_text.lines[line_index];
                    let line_number = line_index + 1;
                    let char_offset =
                        diagnostic.span.start - line.start + 1;
                    let prefix = chars[line.start..diagnostic.span.start]
                        .iter()
                        .collect::<String>();
                    let error = chars[diagnostic.span.start..diagnostic.span.end()]
                        .iter()
                        .collect::<String>();
                    let suffix = chars[diagnostic.span.end()..line.end()]
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
        text_builder.clear();
    }
}
