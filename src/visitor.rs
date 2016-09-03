use types::{RcVar, BoundVars, RcTerm, Term};

pub trait TermVisitor {
    type Result;

    fn enter_var(&mut self, _: &BoundVars, _: &RcVar) {}
    fn enter_abs(&mut self, _: &BoundVars, _: &RcVar, _: &RcTerm) {}
    fn enter_appl(&mut self, _: &BoundVars, _: &RcTerm, _: &RcTerm) {}

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

pub trait SimpleTermVisitor {
    fn visit_var(&mut self, _: &BoundVars, _: &RcVar) {}
    fn visit_abs(&mut self, _: &BoundVars, _: &RcVar, _: &RcTerm) {}
    fn visit_appl(&mut self, _: &BoundVars, _: &RcTerm, _: &RcTerm) {}
}

impl<V> TermVisitor for V
    where V: SimpleTermVisitor
{
    type Result = ();

    fn enter_var(&mut self, bound_vars: &BoundVars, var: &RcVar) {
        self.visit_var(bound_vars, var);
    }

    fn enter_abs(&mut self, bound_vars: &BoundVars, var: &RcVar, body: &RcTerm) {
        self.visit_abs(bound_vars, var, body);
    }
    fn enter_appl(&mut self, bound_vars: &BoundVars, left: &RcTerm, right: &RcTerm) {
        self.visit_appl(bound_vars, left, right);
    }

    fn leave_var(&mut self, _: &BoundVars, _: &RcVar) -> Self::Result {
        ()
    }

    fn leave_abs(&mut self, _: &BoundVars, _: &RcVar, _: &RcTerm, _: Self::Result) -> Self::Result {
        ()
    }

    fn leave_appl(&mut self,
                  _: &BoundVars,
                  _: &RcTerm,
                  _: &RcTerm,
                  _: Self::Result,
                  _: Self::Result)
                  -> Self::Result {
        ()
    }
}

pub trait TermVisitorStrategy {
    fn visit<V: TermVisitor>(term: &Term, visitor: &mut V) -> V::Result;
}

pub struct IterativeVisitorStrategy;

impl TermVisitorStrategy for IterativeVisitorStrategy {
    fn visit<V: TermVisitor>(term: &Term, visitor: &mut V) -> V::Result {
        enum StackAction<'a> {
            ProcessTerm(&'a Term),
            BacktrackTerm(&'a Term),
            UnboundVar,
        };

        let mut stack: Vec<StackAction> = Vec::new();
        let mut result_stack: Vec<V::Result> = Vec::new();
        let mut bound_vars: BoundVars = Vec::new();

        stack.push(StackAction::ProcessTerm(term));

        while let Some(stack_action) = stack.pop() {
            match stack_action {
                StackAction::ProcessTerm(term) => {
                    match *term {
                        Term::Var(ref v) => {
                            visitor.enter_var(&bound_vars, v);
                            result_stack.push(visitor.leave_var(&bound_vars, v));
                        }
                        Term::Abs(ref v, ref b) => {
                            visitor.enter_abs(&bound_vars, v, b);
                            stack.push(StackAction::BacktrackTerm(term));
                            stack.push(StackAction::UnboundVar);
                            stack.push(StackAction::ProcessTerm(b));
                            bound_vars.push(v);
                        }
                        Term::Appl(ref l, ref r) => {
                            visitor.enter_appl(&bound_vars, l, r);
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

pub struct RecursiveVisitorStrategy;

impl TermVisitorStrategy for RecursiveVisitorStrategy {
    fn visit<V: TermVisitor>(term: &Term, visitor: &mut V) -> V::Result {
        fn do_rec<'a, 'b, V: TermVisitor>(term: &'a Term,
                                          visitor: &mut V,
                                          bound_vars: &'b mut BoundVars<'a>)
                                          -> V::Result {
            match *term {
                Term::Var(ref v) => {
                    visitor.enter_var(bound_vars, v);
                    visitor.leave_var(bound_vars, v)
                }
                Term::Abs(ref v, ref b) => {
                    visitor.enter_abs(bound_vars, v, b);
                    bound_vars.push(v);
                    let rec_result = do_rec(b, visitor, bound_vars);
                    let res = visitor.leave_abs(bound_vars, v, b, rec_result);
                    bound_vars.pop();
                    res
                }
                Term::Appl(ref l, ref r) => {
                    visitor.enter_appl(bound_vars, l, r);
                    let l_res = do_rec(l, visitor, bound_vars);
                    let r_res = do_rec(r, visitor, bound_vars);
                    visitor.leave_appl(bound_vars, l, r, l_res, r_res)
                }
            }
        }

        let mut bound_vars: BoundVars = Vec::new();

        do_rec(term, visitor, &mut bound_vars)
    }
}

impl Term {
    pub fn visit<V: TermVisitor, S: TermVisitorStrategy>(&self, visitor: &mut V) -> V::Result {
        S::visit(self, visitor)
    }

    pub fn visit_iteratively<V: TermVisitor>(&self, visitor: &mut V) -> V::Result {
        self.visit::<V, IterativeVisitorStrategy>(visitor)
    }

    pub fn visit_recursevely<V: TermVisitor>(&self, visitor: &mut V) -> V::Result {
        self.visit::<V, RecursiveVisitorStrategy>(visitor)
    }
}
