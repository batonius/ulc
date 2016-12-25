#![cfg_attr(feature="nightly", feature(slice_patterns, test))]

#[cfg(feature="nightly")]
extern crate test;
extern crate rustyline;

mod terms;
mod builtin;
mod parse;
mod folder;
mod reduct;
mod types;
mod type_checker;

use rustyline::Editor;

fn main() {
    let mut rustyline = Editor::<()>::new();
    loop {
        match rustyline.readline("> ") {
            Ok(line) => {
                if line.is_empty() {
                    break;
                }
                rustyline.add_history_entry(&line);
                let term = parse::parse_term(&line).expect("Parsing error.");
                println!("{:#}", reduct::beta_reduction_strict(&term));
                println!("\t: {:#}",
                         type_checker::check_term_type(&term)
                             .unwrap_or(types::TermType::new_named("Type check failed.")));
            }
            _ => {
                break;
            }
        }
    }
}
