pub mod term_parser;

use terms::RcTerm;
use types::RcTermType;
use self::term_parser::{parse_Type, parse_Term};

pub trait TermParser {
    fn parse(s: &str) -> Option<RcTerm>;
}

pub struct LalrpopParser;

impl TermParser for LalrpopParser {
    fn parse(s: &str) -> Option<RcTerm> {
        match parse_Term(s) {
            Ok(e) => Some(e),
            _ => None,
        }
    }
}

pub fn parse_term(s: &str) -> Option<RcTerm> {
    LalrpopParser::parse(s)
}

#[cfg_attr(not(test), allow(dead_code))]
pub fn parse_type(s: &str) -> Option<RcTermType> {
    match parse_Type(s) {
        Ok(e) => Some(e),
        _ => None,
    }
}

#[cfg(test)]
mod test {
    use terms::{Term, Variable};
    use builtin::BuiltinType;

    #[test]
    fn parser_test() {
        let tests =
            vec![(" a", Some(Term::var_rc(Variable::new("a")))),
                 ("+", Some(Term::builtin_rc(BuiltinType::Add, vec![]))),
                 ("123", Some(Term::num_lit_rc(123))),
                 (" (\\ a . a) ",
                  Some(Term::abs_rc(Variable::new("a"), Term::var_rc(Variable::new("a"))))),
                 (" (\\ a . 321) ", Some(Term::abs_rc(Variable::new("a"), Term::num_lit_rc(321)))),
                 ("+ a 12",
                  Some(Term::appl_rc(Term::appl_rc(Term::builtin_rc(BuiltinType::Add, vec![]),
                                                   Term::var_rc(Variable::new("a"))),
                                     Term::num_lit_rc(12)))),
                 ("(a) false",
                  Some(Term::appl_rc(Term::var_rc(Variable::new("a")), Term::bool_lit_rc(false)))),
                 ("if true then 1 else (f a)",
                  Some(Term::if_rc(Term::bool_lit_rc(true),
                                   Term::num_lit_rc(1),
                                   Term::appl_rc(Term::var_rc(Variable::new("f")),
                                                 Term::var_rc(Variable::new("a"))))))];
        for (s, t) in tests.into_iter() {
            assert_eq!(super::parse_term(s), t);
        }
    }
}
