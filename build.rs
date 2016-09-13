#[cfg(feature="default")]
extern crate lalrpop;

#[cfg(feature="default")]
fn do_it() {
    lalrpop::process_root().unwrap();
}

#[cfg(feature="with-combine")]
fn do_it() {
}

fn main() {
    do_it();
}
