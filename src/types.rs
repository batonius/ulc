use std::fmt;
use std::rc::Rc;
use terms::Variable;

#[derive(Debug, Clone, PartialEq)]
pub enum Sort {
    None,
    Kind(RcKind),
    Type(RcTermType),
}

pub type RcSort = Rc<Sort>;

impl Sort {
    pub fn new_none() -> RcSort {
        Rc::new(Sort::None)
    }

    pub fn new_kind(kind: RcKind) -> RcSort {
        Rc::new(Sort::Kind(kind))
    }

    pub fn new_type(ty: RcTermType) -> RcSort {
        Rc::new(Sort::Type(ty))
    }
}

impl fmt::Display for Sort {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Sort::None => write!(f, "?"),
            Sort::Type(ref ty) => write!(f, "{:#}", ty),
            Sort::Kind(ref kind) => write!(f, "{:#}", kind),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Kind {
    Star,
    Arrow(RcKind, RcKind),
}

pub type RcKind = Rc<Kind>;

impl Kind {
    pub fn new_star() -> RcKind {
        Rc::new(Kind::Star)
    }

    pub fn new_arrow(from: RcKind, to: RcKind) -> RcKind {
        Rc::new(Kind::Arrow(from, to))
    }
}

impl fmt::Display for Kind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Kind::Star => write!(f, "*"),
            Kind::Arrow(ref from, ref to) => write!(f, "{:#}=>{:#}", from.as_ref(), to.as_ref()),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeVariable {
    var_name: String,
    var_kind: Option<RcKind>,
}

impl TypeVariable {
    pub fn new<S>(name: S, kind: Option<RcKind>) -> TypeVariable
        where S: Into<String>
    {
        TypeVariable {
            var_name: name.into(),
            var_kind: kind,
        }
    }

    pub fn from_var(var: &Variable) -> Option<TypeVariable> {
        match *var.sort() {
            Sort::Kind(ref kind) => Some(Self::new(var.name(), Some(kind.clone()))),
            _ => None,
        }
    }

    pub fn name(&self) -> &str {
        &self.var_name
    }

    pub fn kind(&self) -> &Option<RcKind> {
        &self.var_kind
    }
}

impl fmt::Display for TypeVariable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.var_kind {
            None => write!(f, "{}", self.var_name),
            Some(ref k) => write!(f, "{}:{:#}", self.var_name, &k),
        }
    }
}

pub type RcTermType = Rc<TermType>;

#[derive(Debug, PartialEq, Clone)]
pub enum TermType {
    Int,
    Bool,
    Type,
    Var(TypeVariable),
    Named(String),
    Arrow(RcTermType, RcTermType),
    Pi(TypeVariable, RcTermType),
    Appl(RcTermType, RcTermType),
}

impl fmt::Display for TermType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            TermType::Int => write!(f, "Int"),
            TermType::Bool => write!(f, "Bool"),
            TermType::Type => write!(f, "Type"),
            TermType::Var(ref v) => write!(f, "{}", v),
            TermType::Named(ref s) => write!(f, "{}", s),
            TermType::Arrow(ref from, ref to) => {
                write!(f, "({:#} -> {:#})", from.as_ref(), to.as_ref())
            }
            TermType::Appl(ref l, ref r) => write!(f, "{{{:#} {:#}}}", l.as_ref(), r.as_ref()),
            TermType::Pi(ref ty_var, ref ty) => write!(f, "\\{:#}.{:#}", ty_var, ty),
        }
    }
}

impl TermType {
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

    pub fn new_type() -> RcTermType {
        Rc::new(TermType::Type)
    }

    pub fn new_appl(l: RcTermType, r: RcTermType) -> RcTermType {
        Rc::new(TermType::Appl(l, r))
    }
}
