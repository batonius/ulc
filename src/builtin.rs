use types::{RcTerm, Term, Literal};
use std::fmt;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BuiltinType {
    Add,
    Sub,
    If,
    Mul,
    Div,
    Eq,
}

impl fmt::Display for BuiltinType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            BuiltinType::Add => write!(f, "+"),
            BuiltinType::Sub => write!(f, "-"),
            BuiltinType::If => write!(f, "?"),
            BuiltinType::Mul => write!(f, "*"),
            BuiltinType::Div => write!(f, "/"),
            BuiltinType::Eq => write!(f, "=")
        }
    }
}

impl BuiltinType {
    fn arity(&self) -> usize {
        match *self {
            BuiltinType::Add |
            BuiltinType::Sub |
            BuiltinType::Mul |
            BuiltinType::Div |
            BuiltinType::Eq => 2,
            BuiltinType::If => 3,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BuiltinClosure {
    builtin_type: BuiltinType,
    args: Vec<RcTerm>,
}

impl fmt::Display for BuiltinClosure {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let _ = write!(f, "({:#} ", self.builtin_type);
        for arg in &self.args {
            let _ = write!(f, "{:#} ", arg);
        }
        write!(f, ") ")
    }
}

impl BuiltinClosure {
    pub fn new(builtin_type: BuiltinType, args: Vec<RcTerm>) -> BuiltinClosure {
        BuiltinClosure {
            builtin_type: builtin_type,
            args: args,
        }
    }

    pub fn apply_term(&self, term: &RcTerm) -> Option<RcTerm> {
        if self.args.len() >= self.builtin_type.arity() {
            None
        } else {
            let mut args = self.args.clone();
            args.push(term.clone());
            Some(Term::builtin_rc(self.builtin_type, args))
        }
    }

    pub fn args(&self) -> &Vec<RcTerm> {
        &self.args
    }

    pub fn builtin_type(&self) -> BuiltinType {
        self.builtin_type
    }

    pub fn try_compute(&self) -> Option<RcTerm> {
        if self.builtin_type.arity() != self.args.len() {
            return None;
        }

        match self.builtin_type {
            BuiltinType::Add => BuiltinClosure::try_compute_num(&self.args, |a, b| a+b),
            BuiltinType::Sub => BuiltinClosure::try_compute_num(&self.args, |a, b| a-b),
            BuiltinType::Mul => BuiltinClosure::try_compute_num(&self.args, |a, b| a*b),
            BuiltinType::Div => BuiltinClosure::try_compute_num(&self.args, |a, b| a/b),
            BuiltinType::Eq => BuiltinClosure::try_compute_bool(&self.args, |a, b| a==b),
            BuiltinType::If => BuiltinClosure::try_compute_if(&self.args),
        }
    }

    fn try_compute_num<F>(args: &[RcTerm], f: F) -> Option<RcTerm>
    where F: FnOnce(isize, isize) -> isize {
        if let [ref a, ref b] = *args {
            if let (&Term::Lit(Literal::Num(ref x)),
                    &Term::Lit(Literal::Num(ref y))) = (a.as_ref(), b.as_ref()) {
                return Some(Term::num_lit_rc(f(*x, *y)));
            }
        }
        None
    }

    fn try_compute_bool<F>(args: &[RcTerm], f: F) -> Option<RcTerm>
        where F: FnOnce(isize, isize) -> bool {
        if let [ref a, ref b] = *args {
            if let (&Term::Lit(Literal::Num(ref x)),
                    &Term::Lit(Literal::Num(ref y))) = (a.as_ref(), b.as_ref()) {
                return Some(Term::bool_lit_rc(f(*x, *y)));
            }
        }
        None
    }

    fn try_compute_if(args: &[RcTerm]) -> Option<RcTerm> {
        if let [ref a, ref b, ref c] = *args {
            if let Term::Lit(Literal::Bool(ref t)) = *a.as_ref() {
                return Some(if *t { b.clone() } else { c.clone() });
            }
        }
        None
    }
}
