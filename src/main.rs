extern crate combine;

mod types;
mod parse;
mod visitor;
mod reduct;

fn main() {
    let mut input = String::new();
    loop {
        input.clear();
        std::io::stdin().read_line(&mut input).unwrap();
        input = input.as_str().trim().to_owned();
        if input.is_empty() {
            break;
        }
        println!("{:#}", reduct::beta_reduction::<visitor::IterativeVisitorStrategy>(
            &parse::parse_term(&input).expect("Parsing error")));
    }
}
