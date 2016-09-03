use types::{Term, Variable, RcTerm};
use combine::{many, many1, digit, letter, char, between, spaces, parser, Parser, ParserExt};
use combine::primitives::{State, Stream, ParseResult};
use super::TermParser;
use std::iter::Iterator;

pub struct CombineParser;

impl TermParser for CombineParser {
    fn parse(s: &str) -> Option<RcTerm> {
        match parser(term_parser).parse(s) {
            Ok((e, _)) => Some(e),
            _ => None,
        }
    }
}

fn term_parser<I>(input: State<I>) -> ParseResult<RcTerm, I>
    where I: Stream<Item = char>
{
    let lex_char = |c| char(c).skip(spaces());
    let sub_term = || between(lex_char('('), lex_char(')'), parser(term_parser));
    let num_lit = || {
        spaces().with(many1(digit()).map(|v: Vec<char>| {
            Term::num_lit_rc(v.into_iter()
                .fold(0, |acc, c| {
                    acc * 10 + c.to_digit(10).unwrap_or(0)
                }) as isize)
        }))
    };
    let var_term = || {
        letter()
            .skip(spaces())
            .map(|t| Term::var_rc(Variable::new(vec![t])))
    };
    let abs_parser = || {
        (lex_char('\\'), letter(), spaces(), lex_char('.'), parser(term_parser))
            .map(|t| Term::abs_rc(Variable::new(vec![t.1]), t.4))
    };
    let without_appl = || spaces().with(abs_parser().or(var_term()).or(num_lit()).or(sub_term()));
    let mut appl_parser = (without_appl(), many(without_appl()))
        .map(|v: (RcTerm, Vec<RcTerm>)| {
            if v.1.is_empty() {
                v.0
            } else {
                v.1.into_iter().fold(v.0, |acc, t| Term::appl_rc(acc, t))
            }
        });
    appl_parser.parse_state(input)
}
