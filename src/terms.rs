use std::fmt;
use std::rc::Rc;
use std::cell::RefCell;
use builtin::{BuiltinType, BuiltinClosure};

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq)]
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

pub type RcTerm = Rc<RefCell<Term>>;

#[derive(Debug, PartialEq)]
pub enum Term {
    Var(Variable),
    Abs(Variable, RcTerm),
    Appl(RcTerm, RcTerm),
    Lit(Literal),
    Builtin(BuiltinClosure),
    If(RcTerm, RcTerm, RcTerm),
}

impl Term {
    pub fn var_rc(var: Variable) -> RcTerm {
        Rc::new(RefCell::new(Term::Var(var)))
    }

    pub fn abs_rc(var: Variable, term: RcTerm) -> RcTerm {
        Rc::new(RefCell::new(Term::Abs(var, term)))
    }

    pub fn appl_rc(l: RcTerm, r: RcTerm) -> RcTerm {
        Rc::new(RefCell::new(Term::Appl(l, r)))
    }

    pub fn num_lit_rc(val: isize) -> RcTerm {
        Rc::new(RefCell::new(Term::Lit(Literal::Num(val))))
    }

    pub fn bool_lit_rc(val: bool) -> RcTerm {
        Rc::new(RefCell::new(Term::Lit(Literal::Bool(val))))
    }

    pub fn builtin_rc(builtin_type: BuiltinType, args: Vec<RcTerm>) -> RcTerm {
        Rc::new(RefCell::new(Term::Builtin(BuiltinClosure::new(builtin_type, args))))
    }

    pub fn if_rc(i: RcTerm, t: RcTerm, e: RcTerm) -> RcTerm {
        Rc::new(RefCell::new(Term::If(i, t, e)))
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Term::Var(ref var) => var.fmt(f),
            Term::Abs(ref var, ref term) => write!(f, "\\{:#}.{:#}", var, &*term.borrow()),
            Term::Appl(ref left, ref right) => {
                write!(f, "({:#} {:#})", &*left.borrow(), &*right.borrow())
            }
            Term::Lit(ref lit) => lit.fmt(f),
            Term::Builtin(ref builtin) => builtin.fmt(f),
            Term::If(ref i, ref t, ref e) => {
                write!(f,
                       "if {:#} then {:#} else {:#}",
                       &*i.borrow(),
                       &*t.borrow(),
                       &*e.borrow())
            }
        }
    }
}
