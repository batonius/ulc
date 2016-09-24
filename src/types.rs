use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub struct TypeVariable {
    var_name: String,
}

impl TypeVariable {
    pub fn new<S>(name: S) -> TypeVariable
        where S: Into<String>
    {
        TypeVariable { var_name: name.into() }
    }

    pub fn name(&self) -> &str {
        &self.var_name
    }
}

impl fmt::Display for TypeVariable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.var_name)
    }
}

pub type RcTermType = Rc<TermType>;

#[derive(Debug, PartialEq, Clone)]
pub enum TermType {
    None,
    Var(TypeVariable),
    Named(String),
    Int,
    Bool,
    Arrow(RcTermType, RcTermType),
    Pi(TypeVariable, RcTermType),
    Kind,
}

impl fmt::Display for TermType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            TermType::None => write!(f, "?"),
            TermType::Int => write!(f, "Int"),
            TermType::Bool => write!(f, "Bool"),
            TermType::Kind => write!(f, "*"),
            TermType::Var(ref v) => write!(f, "{}", v),
            TermType::Named(ref s) => write!(f, "{}", s),
            TermType::Arrow(ref from, ref to) => {
                write!(f, "({:#} -> {:#})", from.as_ref(), to.as_ref())
            }
            TermType::Pi(ref ty_var, ref ty) => write!(f, "/\\{:#}.{:#}", ty_var, ty),
        }
    }
}

impl TermType {
    pub fn new_none() -> RcTermType {
        Rc::new(TermType::None)
    }

    pub fn new_named<S: Into<String>>(s: S) -> RcTermType {
        Rc::new(TermType::Named(s.into()))
    }

    pub fn new_var(v: TypeVariable) -> RcTermType {
        Rc::new(TermType::Var(v))
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

    pub fn new_pi(v: TypeVariable, b: RcTermType) -> RcTermType {
        Rc::new(TermType::Pi(v, b))
    }

    pub fn new_kind() -> RcTermType {
        Rc::new(TermType::Kind)
    }
}
