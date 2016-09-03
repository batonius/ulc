use types::RcTerm;

mod combine;

pub trait TermParser {
    fn parse(s: &str) -> Option<RcTerm>;
}

pub fn parse_term(s: &str) -> Option<RcTerm> {
    combine::CombineParser::parse(s)
}

#[cfg(test)]
mod test {
    use types::{Term, Variable};

    #[test]
    fn parser_test() {
        let tests = vec![(" a", Some(Term::var_rc(Variable::new("a")))),
                         ("123", Some(Term::num_lit_rc(123))),
                         (" (\\ a . a) ",
                          Some(Term::abs_rc(Variable::new("a"), Term::var_rc(Variable::new("a"))))),
                         (" (\\ a . 321) ",
                          Some(Term::abs_rc(Variable::new("a"), Term::num_lit_rc(321)))),
                         ("(a) b",
                          Some(Term::appl_rc(Term::var_rc(Variable::new("a")),
                                             Term::var_rc(Variable::new("b")))))];
        for (s, t) in tests.into_iter() {
            assert_eq!(super::parse_term(s), t);
        }
    }
}
