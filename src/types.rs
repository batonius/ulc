use std::fmt;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct Variable {
    var_name: String,
}

pub type RcVar = Rc<Variable>;
pub type BoundVars<'a> = Vec<&'a RcVar>;

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
        match self {
            &Term::Var(ref var) => var.fmt(f),
            &Term::Abs(ref var, ref term) => write!(f, "\\{:#}.{:#}", var, term),
            &Term::Appl(ref left, ref right) => write!(f, "({:#} {:#})", left, right),
        }
    }
}

pub trait ResultTermVisitor {
    type Result;
    fn visit_var(&mut self, _: &BoundVars, _: &RcVar) {}
    fn visit_abs(&mut self, _: &BoundVars, _: &RcVar, _: &RcTerm) {}
    fn visit_appl(&mut self, _: &BoundVars, _: &RcTerm, _: &RcTerm) {}

    fn leave_var(&mut self, _: &BoundVars, _: &RcVar) -> Self::Result;
    fn leave_abs(&mut self, _: &BoundVars, _: &RcVar, _: &RcTerm, _: Self::Result) -> Self::Result;
    fn leave_appl(&mut self,
                  _: &BoundVars,
                  _: &RcTerm,
                  _: &RcTerm,
                  _: Self::Result,
                  _: Self::Result)
                  -> Self::Result;
}

impl Term {
    pub fn visit_result<V: ResultTermVisitor>(&self, visitor: &mut V) -> V::Result {
        enum StackAction<'a> {
            ProcessTerm(&'a Term),
            BacktrackTerm(&'a Term),
            UnboundVar,
        };

        let mut stack: Vec<StackAction> = Vec::new();
        let mut result_stack: Vec<V::Result> = Vec::new();
        let mut bound_vars: BoundVars = Vec::new();

        stack.push(StackAction::ProcessTerm(self));

        while let Some(stack_action) = stack.pop() {
            match stack_action {
                StackAction::ProcessTerm(term) => {
                    match *term {
                        Term::Var(ref v) => {
                            visitor.visit_var(&bound_vars, v);
                            result_stack.push(visitor.leave_var(&bound_vars, v));
                        }
                        Term::Abs(ref v, ref b) => {
                            visitor.visit_abs(&bound_vars, v, b);
                            stack.push(StackAction::BacktrackTerm(term));
                            stack.push(StackAction::UnboundVar);
                            stack.push(StackAction::ProcessTerm(b));
                            bound_vars.push(v);
                        }
                        Term::Appl(ref l, ref r) => {
                            visitor.visit_appl(&bound_vars, l, r);
                            stack.push(StackAction::BacktrackTerm(term));
                            stack.push(StackAction::ProcessTerm(l));
                            stack.push(StackAction::ProcessTerm(r));
                        }
                    }
                }
                StackAction::BacktrackTerm(term) => {
                    match *term {
                        Term::Var(..) => {
                            panic!();
                        }
                        Term::Abs(ref v, ref b) => {
                            let b_result = result_stack.pop().unwrap();
                            result_stack.push(visitor.leave_abs(&bound_vars, v, b, b_result));
                        }
                        Term::Appl(ref l, ref r) => {
                            let l_result = result_stack.pop().unwrap();
                            let r_result = result_stack.pop().unwrap();
                            result_stack.push(visitor.leave_appl(&bound_vars, l, r,
                                                                 l_result, r_result));
                        }
                    }
                }
                StackAction::UnboundVar => {
                    bound_vars.pop();
                }
            }
        }
        result_stack.pop().unwrap()
    }
}
