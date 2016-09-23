use std::fmt;
use std::rc::Rc;
use builtin::{BuiltinType, BuiltinClosure};
use types::{TermType, RcTermType};

#[derive(Debug, Clone)]
pub struct Variable {
    var_name: String,
    var_type: RcTermType,
}

impl PartialEq for Variable {
    fn eq(&self, other: &Self) -> bool {
        self.var_name == other.var_name
    }
}

impl Variable {
    pub fn new<S>(name: S) -> Variable
        where S: Into<String>
    {
        Variable {
            var_name: name.into(),
            var_type: TermType::new_none(),
        }
    }

    pub fn with_type<S>(name: S, ty: RcTermType) -> Variable
        where S: Into<String>
    {
        Variable {
            var_name: name.into(),
            var_type: ty,
        }
    }

    pub fn name(&self) -> &str {
        &self.var_name
    }

    pub fn var_type(&self) -> &RcTermType {
        &self.var_type
    }
}

impl fmt::Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{:#}", self.var_name, self.var_type.as_ref())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Num(isize),
    Bool(bool),
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Literal::Num(val) => write!(f, "{}", val),
            Literal::Bool(val) => write!(f, "{}", val),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Term {
    Var(Variable),
    Abs(Variable, RcTerm),
    Appl(RcTerm, RcTerm),
    Lit(Literal),
    Builtin(BuiltinClosure),
    If(RcTerm, RcTerm, RcTerm),
}

pub type RcTerm = Rc<Term>;

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

    pub fn bool_lit_rc(val: bool) -> RcTerm {
        Rc::new(Term::Lit(Literal::Bool(val)))
    }
    pub fn builtin_rc(builtin_type: BuiltinType, args: Vec<RcTerm>) -> RcTerm {
        Rc::new(Term::Builtin(BuiltinClosure::new(builtin_type, args)))
    }

    pub fn if_rc(i: RcTerm, t: RcTerm, e: RcTerm) -> RcTerm {
        Rc::new(Term::If(i, t, e))
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Term::Var(ref var) => var.fmt(f),
            Term::Abs(ref var, ref term) => write!(f, "\\{:#}.{:#}", var, term),
            Term::Appl(ref left, ref right) => write!(f, "({:#} {:#})", left, right),
            Term::Lit(ref lit) => lit.fmt(f),
            Term::Builtin(ref builtin) => builtin.fmt(f),
            Term::If(ref i, ref t, ref e) => write!(f, "if {:#} then {:#} else {:#}", i, t, e),
        }
    }
}
