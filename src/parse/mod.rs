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
    use std::rc::Rc;

    #[test]
    fn var_term() {
        assert_eq!(super::parse_term(" a"),
                   Some(Rc::new(Term::Var(Variable::new_rc("a")))));
    }

    #[test]
    fn abs_term() {
        assert_eq!(super::parse_term(" (\\ a . a) "),
                   Some(Rc::new(Term::Abs(Variable::new_rc("a"),
                                          Rc::new(Term::Var(Variable::new_rc("a")))))));
    }

    #[test]
    fn appl_term() {
        assert_eq!(super::parse_term("(a) b"),
                   Some(Rc::new(Term::Appl(Rc::new(Term::Var(Variable::new_rc("a"))),
                                           Rc::new(Term::Var(Variable::new_rc("b")))))));
    }
}
