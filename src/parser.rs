use types::{Term, Variable, RcVar, RcTerm};
use combine::{many, letter, char, between, spaces, parser, Parser, ParserExt};
use combine::primitives::{State, Stream, ParseResult};
use std::rc::Rc;

pub fn parse_term(s: &str) -> Option<RcTerm> {
    match parser(term_parser).parse(s) {
        Ok((e, _)) => Some(e),
        _ => None,
    }
}

fn term_parser<I>(input: State<I>) -> ParseResult<RcTerm, I>
    where I: Stream<Item = char>
{
    let lex_char = |c| char(c).skip(spaces());
    let sub_term = || between(lex_char('('), lex_char(')'), parser(term_parser));
    let var_term = || {letter()
                       .skip(spaces()) .map(|t| Rc::new(Term::Var(Variable::new_rc(vec![t]))))};
    let abs_parser = || {
        (lex_char('\\'), letter(), spaces(), lex_char('.'), parser(term_parser))
            .map(|t| Rc::new(Term::Abs(Variable::new_rc(vec![t.1]), t.4)))};
    let without_appl = || spaces().with(abs_parser().or(var_term()).or(sub_term()));
    let mut appl_parser = (without_appl(), many(without_appl()))
        .map(|v: (RcTerm, Vec<RcTerm>)| {
            if v.1.is_empty() {
                v.0
            } else {
                v.1.into_iter().fold(v.0, |acc, t| Rc::new(Term::Appl(acc, t)))
            }});
    appl_parser.parse_state(input)
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
