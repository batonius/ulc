use types::{Term, RcTerm, RcVar};
use visitor::{SimpleTermVisitor, TermVisitor, TermVisitorStrategy, BoundVars};
use std::collections::HashSet;
use std::marker::PhantomData;

pub fn free_vars<VS: TermVisitorStrategy>(term: &Term) -> HashSet<RcVar> {
    struct FreeVarCollector(HashSet<RcVar>);
    impl SimpleTermVisitor for FreeVarCollector {
        fn visit_var(&mut self, bound_vars: &BoundVars, var: &RcVar) {
            if !bound_vars.iter().any(|x| *x == var) {
                self.0.insert(var.clone());
            }
        }
    }
    let mut free_var_collector = FreeVarCollector(HashSet::new());
    term.visit::<FreeVarCollector, VS>(&mut free_var_collector);
    free_var_collector.0
}

pub fn subs_var<VS: TermVisitorStrategy>(term: &RcTerm,
                                         var: &RcVar,
                                         val: &RcTerm)
                                         -> Option<RcTerm> {
    struct VarSubstitutor<'a> {
        var: &'a RcVar,
        val: &'a RcTerm,
    };

    impl<'a> TermVisitor for VarSubstitutor<'a> {
        type Result = Option<RcTerm>;
        fn leave_var(&mut self, bound_vars: &BoundVars, v: &RcVar) -> Self::Result {
            if bound_vars.iter().any(|x| *x == self.var) || v != self.var {
                None
            } else {
                Some(self.val.clone())
            }
        }
        fn leave_abs(&mut self,
                     _: &BoundVars,
                     v: &RcVar,
                     _: &RcTerm,
                     body_result: Self::Result)
                     -> Self::Result {
            body_result.map(|new_body| Term::abs_rc(v.clone(), new_body))
        }
        fn leave_appl(&mut self,
                      _: &BoundVars,
                      l: &RcTerm,
                      r: &RcTerm,
                      l_res: Self::Result,
                      r_res: Self::Result)
                      -> Self::Result {
            if l_res.is_none() && r_res.is_none() {
                None
            } else {
                Some(Term::appl_rc(l_res.unwrap_or_else(|| l.clone()),
                                   r_res.unwrap_or_else(|| r.clone())))
            }
        }
    }

    let mut var_substitutor = VarSubstitutor {
        var: var,
        val: val,
    };
    term.visit::<VarSubstitutor, VS>(&mut var_substitutor)
}

pub fn beta_step<VS: TermVisitorStrategy>(term: &RcTerm) -> Option<RcTerm> {
    struct BetaStep<VS> {
        _field: PhantomData<VS>,
    }

    impl<VS: TermVisitorStrategy> TermVisitor for BetaStep<VS> {
        type Result = Option<RcTerm>;
        fn leave_var(&mut self, _: &BoundVars, _: &RcVar) -> Self::Result {
            None
        }
        fn leave_abs(&mut self,
                     _: &BoundVars,
                     v: &RcVar,
                     _: &RcTerm,
                     body_result: Self::Result)
                     -> Self::Result {
            body_result.map(|new_body| Term::abs_rc(v.clone(), new_body))
        }
        fn leave_appl(&mut self,
                      _: &BoundVars,
                      l: &RcTerm,
                      r: &RcTerm,
                      l_res: Self::Result,
                      r_res: Self::Result)
                      -> Self::Result {
            if l_res.is_none() && r_res.is_none() {
                match **l {
                    Term::Abs(ref v, ref b) => subs_var::<VS>(b, v, r),
                    _ => None,
                }
            } else {
                Some(Term::appl_rc(l_res.unwrap_or_else(|| l.clone()),
                                   r_res.unwrap_or_else(|| r.clone())))
            }
        }
    }

    let mut beta_stepper: BetaStep<VS> = BetaStep { _field: PhantomData };
    term.visit::<BetaStep<VS>, VS>(&mut beta_stepper)
}

pub fn beta_reduction<VS: TermVisitorStrategy>(term: &RcTerm) -> RcTerm {
    let mut result = term.clone();
    while let Some(new_term) = beta_step::<VS>(&result) {
        result = new_term;
    }
    result
}

#[cfg(test)]
mod test {
    use parse::parse_term;
    use types::Variable;
    use visitor::{IterativeVisitorStrategy, RecursiveVisitorStrategy};
    use std::collections::HashSet;
    use std::iter::FromIterator;

    #[test]
    fn free_vars() {
        let b = Variable::new_rc("b");
        let x = Variable::new_rc("x");
        let tests = vec![("\\a.a", vec![]),
                         ("b", vec![b.clone()]),
                         ("\\a. (a b)", vec![b.clone()]),
                         ("(\\a. (\\x.b x) x)", vec![b.clone(), x.clone()])];
        for (s, t) in tests.into_iter() {
            let expected = HashSet::from_iter(t.into_iter());
            assert_eq!(super::free_vars::<IterativeVisitorStrategy>(&parse_term(s).unwrap()),
                       expected);
            assert_eq!(super::free_vars::<RecursiveVisitorStrategy>(&parse_term(s).unwrap()),
                       expected);
        }
    }

    #[test]
    fn subs_vars() {
        let a = Variable::new_rc("a");
        let b = Variable::new_rc("b");
        let c = Variable::new_rc("c");
        let tests = vec![("a", &a, "b", "b"),
                         ("b x", &b, "\\n.n", "(\\n.n) x"),
                         ("\\z. z c (\\c. c)", &c, "d", "\\z. z d (\\c. c)"),
                         ("x", &a, "b", "x")];
        for (term, var, val, res) in tests.into_iter() {
            let src_term = parse_term(term).unwrap();
            let val_term = parse_term(val).unwrap();
            let result_term = parse_term(res).unwrap();
            let rec_subs_result =
                super::subs_var::<RecursiveVisitorStrategy>(&src_term, &var, &val_term)
                    .unwrap_or(src_term.clone());
            let iter_subs_result =
                super::subs_var::<IterativeVisitorStrategy>(&src_term, &var, &val_term)
                    .unwrap_or(src_term.clone());

            assert_eq!(rec_subs_result, result_term);
            assert_eq!(iter_subs_result, result_term);
        }
    }

    #[test]
    fn beta_step() {
        let tests =
            vec![("a", "a"), ("(\\x.x) z", "z"), ("(\\z. x ((\\y. z y) a)) b", "(\\z. x (z a)) b")];

        for (from, to) in tests {
            let from_term = parse_term(from).unwrap();
            let to_term = parse_term(to).unwrap();

            assert_eq!(super::beta_step::<RecursiveVisitorStrategy>(&from_term)
                           .unwrap_or(from_term.clone()),
                       to_term);
            assert_eq!(super::beta_step::<IterativeVisitorStrategy>(&from_term)
                           .unwrap_or(from_term.clone()),
                       to_term);
        }
    }

    #[test]
    fn beta_recution() {
        let tests = vec![("a", "a"), ("(\\x.x) z", "z"), ("(\\z. x ((\\y. z y) a)) b", "x (b a)")];

        for (from, to) in tests {
            let from_term = parse_term(from).unwrap();
            let to_term = parse_term(to).unwrap();

            assert_eq!(super::beta_reduction::<RecursiveVisitorStrategy>(&from_term),
                       to_term);
            assert_eq!(super::beta_reduction::<IterativeVisitorStrategy>(&from_term),
                       to_term);
        }
    }
}
