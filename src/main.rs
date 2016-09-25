#![feature(slice_patterns, test)]

extern crate test;

mod terms;
mod builtin;
mod parse;
mod folder;
mod reduct;
mod types;
mod type_checker;

fn main() {
    let mut input = String::new();
    loop {
        input.clear();
        std::io::stdin().read_line(&mut input).unwrap();
        input = input.as_str().trim().to_owned();
        if input.is_empty() {
            break;
        }
        let term = parse::parse_term(&input).expect("Parsing error");
        println!("{:#}", term);
        println!("{:#}",
                 type_checker::check_term_type(&term)
                     .unwrap_or(types::TermType::new_named("Fail!")));
        println!("{:#}", reduct::beta_reduction_strict(&term));
        // println!("{:#}", reduct::beta_reduction_lazy(&term));
    }
}
