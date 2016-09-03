extern crate combine;

mod types;
mod parse;
mod visitor;
mod reduct;

fn main() {
    let a: String = std::env::args()
        .skip(1)
        .fold(" ".to_owned(), |acc, t| acc + &t);
    println!{"{}", a};
    for term in parse::parse_term(&a) {
        println!("{:#}", term);
        println!("{:?}", reduct::free_vars::<visitor::IterativeVisitorStrategy>(&term));
    }
}
