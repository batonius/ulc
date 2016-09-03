use types::{Term, RcTerm, RcVar, BoundVars};
use visitor::{SimpleTermVisitor, TermVisitor, TermVisitorStrategy};
use std::collections::HashSet;

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

pub fn subs_var<VS: TermVisitorStrategy>(term: &RcTerm, var: &RcVar, val: &RcTerm) -> RcTerm {
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
    term.visit::<VarSubstitutor, VS>(&mut var_substitutor).unwrap_or_else(|| term.clone())
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
                super::subs_var::<RecursiveVisitorStrategy>(&src_term, &var, &val_term);
            let iter_subs_result =
                super::subs_var::<IterativeVisitorStrategy>(&src_term, &var, &val_term);

            assert_eq!(rec_subs_result, result_term);
            assert_eq!(iter_subs_result, result_term);
        }
    }
}
