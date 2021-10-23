use crossterm::cursor::MoveTo;
use crossterm::style::Attribute;
use crossterm::style::Color;
use crossterm::style::Print;
use crossterm::style::ResetColor;
use crossterm::style::SetAttribute;
use crossterm::style::SetForegroundColor;
use crossterm::terminal::Clear;
use crossterm::terminal::ClearType;
use crossterm::ExecutableCommand;
use insults::INSULTS;
use minsk::compilation::Compilation;
use minsk::plumbing::Object;
use minsk::syntax::SyntaxNodeRef;
use minsk::syntax::SyntaxTree;
use minsk::text::VariableSymbol;
use rand::seq::SliceRandom;
use rand::thread_rng;
use std::collections::HashMap;
use std::io::stdin;
use std::io::stdout;
use std::io::BufRead;
use std::io::BufReader;
use std::io::Write;

mod insults;

fn main() {
    let mut reader = BufReader::new(stdin());
    let mut input = String::new();
    let mut show_tree = false;
    let mut variables = HashMap::<VariableSymbol, Object>::new();
    let mut text_builder = String::new();
    let mut previous: Option<Compilation> = None;
    let mut rng = thread_rng();
    let mut insults = INSULTS.iter().collect::<Vec<_>>();
    insults.shuffle(&mut rng);
    let mut insult_index = 0;
    loop {
        stdout().execute(SetForegroundColor(Color::Green)).unwrap();
        if text_builder.is_empty() {
            print!("» ");
        } else {
            print!("· ");
        }
        stdout().execute(ResetColor).unwrap();
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
                    print!("🌳");
                    if show_tree {
                        stdout()
                            .execute(SetForegroundColor(Color::Green))
                            .unwrap()
                            .execute(Print("✔"))
                            .unwrap()
                            .execute(ResetColor)
                            .unwrap()
                            .execute(Print(" Showing parse trees."))
                            .unwrap();
                    } else {
                        stdout()
                            .execute(SetForegroundColor(Color::Red))
                            .unwrap()
                            .execute(Print("🗙"))
                            .unwrap()
                            .execute(ResetColor)
                            .unwrap()
                            .execute(Print(" Not showing parse trees."))
                            .unwrap();
                    }
                    println!();
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
                "#reset" => {
                    previous = None;
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
            let tree_node = SyntaxNodeRef::CompilationUnit(syntax_tree.root.create_ref());
            tree_node.pretty_print();
        }

        let source_text = syntax_tree.source_text.clone();
        let mut compilation = if let Some(previous) = previous.clone() {
            previous.continue_with(syntax_tree)
        } else {
            Compilation::new(syntax_tree)
        };
        let result = compilation.evaluate(&mut variables);

        match result {
            Err(diagnostics) => {
                let insult = insults[insult_index];
                stdout()
                    .execute(SetAttribute(Attribute::Bold))
                    .unwrap()
                    .execute(SetForegroundColor(Color::Green))
                    .unwrap();
                println!("{}", insult);
                println!("~~~");
                insult_index += 1;
                if insult_index == insults.len() {
                    insults.shuffle(&mut rng);
                    insult_index = 0;
                }
                println!();
                for diagnostic in diagnostics {
                    let chars = source_text.as_chars();
                    let line_index = source_text.get_line_index(diagnostic.span.start);
                    let line = &source_text.lines[line_index];
                    let line_number = line_index + 1;
                    let char_offset = diagnostic.span.start - line.start + 1;
                    let prefix = chars[line.start..diagnostic.span.start]
                        .iter()
                        .collect::<String>();
                    let error = chars[diagnostic.span.start..diagnostic.span.end()]
                        .iter()
                        .collect::<String>();
                    let suffix = chars[diagnostic.span.end()..line.end()]
                        .iter()
                        .collect::<String>();

                    stdout()
                        .execute(SetAttribute(Attribute::NormalIntensity))
                        .unwrap()
                        .execute(SetForegroundColor(Color::Red))
                        .unwrap();
                    print!("({}, {}): ", line_number, char_offset);
                    println!("{}", diagnostic);

                    stdout().execute(ResetColor).unwrap();
                    print!("\t{}", prefix);
                    stdout().execute(SetForegroundColor(Color::Red)).unwrap();
                    print!("{}", error);
                    stdout().execute(ResetColor).unwrap();
                    print!("{}", suffix);
                    println!();
                }
                println!();
            }
            Ok(value) => {
                stdout()
                    .execute(SetForegroundColor(Color::Magenta))
                    .unwrap();
                println!("{}", value);
                previous = Some(compilation);
            }
        }
        stdout().execute(ResetColor).unwrap();
        text_builder.clear();
    }
}
