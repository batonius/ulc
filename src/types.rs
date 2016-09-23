use std::fmt;
use std::rc::Rc;

pub type RcTermType = Rc<TermType>;

#[derive(Debug, PartialEq, Clone)]
pub enum TermType {
    None,
    Var(String),
    Int,
    Bool,
    Arrow(RcTermType, RcTermType),
}

impl fmt::Display for TermType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            TermType::None => write!(f, "?"),
            TermType::Int => write!(f, "Int"),
            TermType::Bool => write!(f, "Bool"),
            TermType::Var(ref s) => write!(f, "{}", s),
            TermType::Arrow(ref from, ref to) => {
                write!(f, "({:#} -> {:#})", from.as_ref(), to.as_ref())
            }
        }
    }
}

impl TermType {
    pub fn new_none() -> RcTermType {
        Rc::new(TermType::None)
    }

    pub fn new_var<S: Into<String>>(s: S) -> RcTermType {
        Rc::new(TermType::Var(s.into()))
    }

    pub fn new_int() -> RcTermType {
        Rc::new(TermType::Int)
    }

    pub fn new_bool() -> RcTermType {
        Rc::new(TermType::Bool)
    }

    pub fn new_arrow(from: RcTermType, to: RcTermType) -> RcTermType {
        Rc::new(TermType::Arrow(from, to))
    }
}
