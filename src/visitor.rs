use types::{Variable, RcTerm, Term, Literal};
use builtin::BuiltinClosure;

pub enum IfBranchesPolicy {
    ProccessBranches,
    DontProcessBranches,
}

pub trait TermVisitor {
    type Result;

    fn enter_var(&mut self, _: &Variable) -> Option<Self::Result> {
        None
    }
    fn enter_abs(&mut self, _: &Variable, _: &RcTerm) -> Option<Self::Result> {
        None
    }
    fn enter_appl(&mut self, _: &RcTerm, _: &RcTerm) -> Option<Self::Result> {
        None
    }
    fn enter_lit(&mut self, _: &Literal) -> Option<Self::Result> {
        None
    }
    fn enter_builtin(&mut self, _: &BuiltinClosure) -> Option<Self::Result> {
        None
    }
    fn enter_if(&mut self, _: &RcTerm, _: &RcTerm, _: &RcTerm) -> IfBranchesPolicy {
        IfBranchesPolicy::ProccessBranches
    }

    fn leave_var(&mut self, _: &Variable) -> Self::Result;
    fn leave_abs(&mut self, _: &Variable, _: &RcTerm, _: Self::Result) -> Self::Result;
    fn leave_appl(&mut self,
                  _: &RcTerm,
                  _: &RcTerm,
                  _: Self::Result,
                  _: Self::Result)
                  -> Self::Result;
    fn leave_lit(&mut self, _: &Literal) -> Self::Result;
    fn leave_builtin(&mut self, _: &BuiltinClosure, _: Vec<Self::Result>) -> Self::Result;
    fn leave_if(&mut self,
                _: &RcTerm,
                _: &RcTerm,
                _: &RcTerm,
                _: Self::Result,
                _: Option<(Self::Result, Self::Result)>)
                -> Self::Result;
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
            IfBranchesProcessed(bool),
        };

        let mut stack: Vec<StackAction> = Vec::new();
        let mut result_stack: Vec<V::Result> = Vec::new();

        stack.push(StackAction::ProcessTerm(term));

        while let Some(stack_action) = stack.pop() {
            match stack_action {
                StackAction::ProcessTerm(term) => {
                    match *term {
                        Term::Var(ref v) => {
                            if let Some(r) = visitor.enter_var(v) {
                                result_stack.push(r);
                            } else {
                                result_stack.push(visitor.leave_var(v));
                            }
                        }
                        Term::Abs(ref v, ref b) => {
                            if let Some(r) = visitor.enter_abs(v, b) {
                                result_stack.push(r);
                            } else {
                                stack.push(StackAction::BacktrackTerm(term));
                                stack.push(StackAction::ProcessTerm(b));
                            }
                        }
                        Term::Appl(ref l, ref r) => {
                            if let Some(r) = visitor.enter_appl(l, r) {
                                result_stack.push(r);
                            } else {
                                stack.push(StackAction::BacktrackTerm(term));
                                stack.push(StackAction::ProcessTerm(l));
                                stack.push(StackAction::ProcessTerm(r));
                            }
                        }
                        Term::Lit(ref lit) => {
                            if let Some(r) = visitor.enter_lit(lit) {
                                result_stack.push(r);
                            } else {
                                result_stack.push(visitor.leave_lit(lit));
                            }
                        }
                        Term::Builtin(ref builtin) => {
                            if let Some(r) = visitor.enter_builtin(builtin) {
                                result_stack.push(r);
                            } else {
                                stack.push(StackAction::BacktrackTerm(term));
                                for arg in builtin.args() {
                                    stack.push(StackAction::ProcessTerm(arg));
                                }
                            }
                        }
                        Term::If(ref i, ref t, ref e) => {
                            match visitor.enter_if(i, t, e) {
                                IfBranchesPolicy::DontProcessBranches => {
                                    stack.push(StackAction::IfBranchesProcessed(false));
                                    stack.push(StackAction::BacktrackTerm(term));
                                    stack.push(StackAction::ProcessTerm(i));
                                }
                                IfBranchesPolicy::ProccessBranches => {
                                    stack.push(StackAction::IfBranchesProcessed(true));
                                    stack.push(StackAction::BacktrackTerm(term));
                                    stack.push(StackAction::ProcessTerm(i));
                                    stack.push(StackAction::ProcessTerm(t));
                                    stack.push(StackAction::ProcessTerm(e));
                                }
                            }
                        }
                    }
                }
                StackAction::BacktrackTerm(term) => {
                    match *term {
                        Term::Var(..) | Term::Lit(..) => {
                            panic!();
                        }
                        Term::Abs(ref v, ref b) => {
                            let b_result = result_stack.pop().unwrap();
                            result_stack.push(visitor.leave_abs(v, b, b_result));
                        }
                        Term::Appl(ref l, ref r) => {
                            let l_result = result_stack.pop().unwrap();
                            let r_result = result_stack.pop().unwrap();
                            result_stack.push(visitor.leave_appl(l, r, l_result, r_result));
                        }
                        Term::Builtin(ref builtin) => {
                            let mut results = Vec::new();
                            for _ in builtin.args() {
                                results.push(result_stack.pop().unwrap());
                            }
                            result_stack.push(visitor.leave_builtin(builtin, results));
                        }
                        Term::If(ref i, ref t, ref e) => {
                            let i_result = result_stack.pop().unwrap();
                            let t_e_result = if let StackAction::IfBranchesProcessed(true) =
                                                    stack.pop().unwrap() {
                                Some((result_stack.pop().unwrap(), result_stack.pop().unwrap()))
                            } else {
                                None
                            };
                            result_stack.push(visitor.leave_if(i, t, e, i_result, t_e_result));
                        }
                    }
                }
                StackAction::IfBranchesProcessed(_) => {
                    panic!();
                }
            }
        }
        result_stack.pop().unwrap()
    }
}

pub struct RecursiveVisitorStrategy;

impl TermVisitorStrategy for RecursiveVisitorStrategy {
    fn visit<V: TermVisitor>(term: &Term, visitor: &mut V) -> V::Result {
        fn do_rec<V: TermVisitor>(term: &Term, visitor: &mut V) -> V::Result {
            match *term {
                Term::Var(ref v) => {
                    if let Some(r) = visitor.enter_var(v) {
                        r
                    } else {
                        visitor.leave_var(v)
                    }
                }
                Term::Abs(ref v, ref b) => {
                    if let Some(r) = visitor.enter_abs(v, b) {
                        r
                    } else {
                        let rec_result = do_rec(b, visitor);
                        visitor.leave_abs(v, b, rec_result)
                    }
                }
                Term::Appl(ref l, ref r) => {
                    if let Some(r) = visitor.enter_appl(l, r) {
                        r
                    } else {
                        let l_res = do_rec(l, visitor);
                        let r_res = do_rec(r, visitor);
                        visitor.leave_appl(l, r, l_res, r_res)
                    }
                }
                Term::Lit(ref l) => {
                    if let Some(r) = visitor.enter_lit(l) {
                        r
                    } else {
                        visitor.leave_lit(l)
                    }
                }
                Term::Builtin(ref builtin) => {
                    if let Some(r) = visitor.enter_builtin(builtin) {
                        r
                    } else {
                        let mut results = Vec::new();
                        for arg in builtin.args() {
                            results.push(do_rec(arg, visitor));
                        }
                        visitor.leave_builtin(builtin, results)
                    }
                }
                Term::If(ref i, ref t, ref e) => {
                    match visitor.enter_if(i, t, e) {
                        IfBranchesPolicy::DontProcessBranches => {
                            let i_res = do_rec(i, visitor);
                            visitor.leave_if(i, t, e, i_res, None)
                        }
                        IfBranchesPolicy::ProccessBranches => {
                            let i_res = do_rec(i, visitor);
                            let t_res = do_rec(t, visitor);
                            let e_res = do_rec(e, visitor);
                            visitor.leave_if(i, t, e, i_res, Some((t_res, e_res)))
                        }
                    }
                }
            }
        }

        do_rec(term, visitor)
    }
}

impl Term {
    pub fn visit<V: TermVisitor, S: TermVisitorStrategy>(&self, visitor: &mut V) -> V::Result {
        S::visit(self, visitor)
    }
}
