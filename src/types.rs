use std::fmt;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct Variable {
    var_name: String,
}

pub type RcVar = Rc<Variable>;

impl Variable {
    pub fn new_rc<S>(name: S) -> RcVar
        where S: Into<String>
    {
        Rc::new(Variable { var_name: name.into() })
    }
}

impl fmt::Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.var_name)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Term {
    Var(RcVar),
    Abs(RcVar, RcTerm),
    Appl(RcTerm, RcTerm),
}

impl Term {
    pub fn var_rc(var: RcVar) -> RcTerm {
        Rc::new(Term::Var(var))
    }

    pub fn abs_rc(var: RcVar, term: RcTerm) -> RcTerm {
        Rc::new(Term::Abs(var, term))
    }

    pub fn appl_rc(l: RcTerm, r: RcTerm) -> RcTerm {
        Rc::new(Term::Appl(l, r))
    }
}

pub type RcTerm = Rc<Term>;

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Term::Var(ref var) => var.fmt(f),
            Term::Abs(ref var, ref term) => write!(f, "\\{:#}.{:#}", var, term),
            Term::Appl(ref left, ref right) => write!(f, "({:#} {:#})", left, right),
        }
    }
}
