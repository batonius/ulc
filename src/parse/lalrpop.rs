use super::TermParser;
use super::term_parser::parse_Term;
use terms::RcTerm;

pub struct LalrpopParser;

impl TermParser for LalrpopParser {
    fn parse(s: &str) -> Option<RcTerm> {
        match parse_Term(s) {
            Ok(e) => Some(e),
            _ => None,
        }
    }
}
