use std::fmt;
use std::rc::Rc;
use builtin::{BuiltinType, BuiltinClosure};

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct Variable {
    var_name: String,
}

impl Variable {
    pub fn new<S>(name: S) -> Variable
        where S: Into<String>
    {
        Variable { var_name: name.into() }
    }
}

impl fmt::Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.var_name)
    }
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Literal {
    Num(isize),
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Literal::Num(val) => write!(f, "{}", val),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Term {
    Var(Variable),
    Abs(Variable, RcTerm),
    Appl(RcTerm, RcTerm),
    Lit(Literal),
    Builtin(BuiltinClosure),
}

impl Term {
    pub fn var_rc(var: Variable) -> RcTerm {
        Rc::new(Term::Var(var))
    }

    pub fn abs_rc(var: Variable, term: RcTerm) -> RcTerm {
        Rc::new(Term::Abs(var, term))
    }

    pub fn appl_rc(l: RcTerm, r: RcTerm) -> RcTerm {
        Rc::new(Term::Appl(l, r))
    }

    pub fn num_lit_rc(val: isize) -> RcTerm {
        Rc::new(Term::Lit(Literal::Num(val)))
    }

    pub fn builtin_rc(builtin_type: BuiltinType, args: Vec<RcTerm>) -> RcTerm {
        Rc::new(Term::Builtin(BuiltinClosure::new(builtin_type, args)))
    }
}

pub type RcTerm = Rc<Term>;

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Term::Var(ref var) => var.fmt(f),
            Term::Abs(ref var, ref term) => write!(f, "\\{:#}.{:#}", var, term),
            Term::Appl(ref left, ref right) => write!(f, "({:#} {:#})", left, right),
            Term::Lit(ref lit) => lit.fmt(f),
            Term::Builtin(ref builtin) => builtin.fmt(f),
        }
    }
}
