use types::{Term, RcTerm, RcVar, TermVisitor, ResultTermVisitor, BoundVars};
use std::collections::{VecDeque, HashSet};

pub fn free_vars(term: &Term) -> HashSet<RcVar> {
    struct FreeVarCollector(HashSet<RcVar>);
    impl ResultTermVisitor for FreeVarCollector {
        type Result = ();
        fn visit_var(&mut self, bound_vars: &BoundVars, var: &RcVar) {
            if !bound_vars.iter().any(|x| *x == var) {
                self.0.insert(var.clone());
            }
        }
        fn leave_var(&mut self, _: &BoundVars, _: &RcVar) -> Self::Result { () }
        fn leave_abs(&mut self, _: &BoundVars, _: &RcVar, _: &RcTerm, _: Self::Result) -> Self::Result { () }
        fn leave_appl(&mut self,
                      _: &BoundVars,
                      _: &RcTerm,
                      _: &RcTerm,
                      _: Self::Result,
                      _: Self::Result)
                      -> Self::Result { () }
    }
    let mut free_var_collector = FreeVarCollector(HashSet::new());
    term.visit_result(&mut free_var_collector);
    free_var_collector.0
}

pub fn subs_var_rec(term: &RcTerm, var: &RcVar, val: &RcTerm) -> RcTerm {
    fn do_subs(term: &Term,
               var: &RcVar,
               val: &RcTerm,
               bound_vars: &mut Vec<RcVar>)
               -> Option<RcTerm> {
        match *term {
            Term::Var(ref v) => {
                if bound_vars.iter().any(|x| x == v) || v != var {
                    None
                } else {
                    Some(val.clone())
                }
            }
            Term::Abs(ref v, ref b) => {
                bound_vars.push(v.clone());
                let res = do_subs(b, var, val, bound_vars)
                    .map(|new_body| Term::abs_rc(v.clone(), new_body));
                bound_vars.pop();
                res
            }
            Term::Appl(ref l, ref r) => {
                let l_res = do_subs(l, var, val, bound_vars);
                let r_res = do_subs(r, var, val, bound_vars);
                if l_res.is_none() && r_res.is_none() {
                    None
                } else {
                    Some(Term::appl_rc(l_res.unwrap_or_else(|| l.clone()),
                                       r_res.unwrap_or_else(|| r.clone())))
                }
            }
        }
    }

    let mut bound_vars: Vec<RcVar> = Vec::new();

    do_subs(term, var, val, &mut bound_vars).unwrap_or_else(|| term.clone())
}

pub fn subs_var_iter(term: &RcTerm, var: &RcVar, val: &RcTerm) -> RcTerm {
    struct VarSubstitutor<'a> {
        var: &'a RcVar,
        val: &'a RcTerm,
    };

    impl<'a> ResultTermVisitor for VarSubstitutor<'a> {
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
    term.visit_result(&mut var_substitutor).unwrap_or_else(|| term.clone())
}

#[cfg(test)]
mod test {
    use parser::parse_term;
    use types::Variable;
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
            assert_eq!(super::free_vars(&parse_term(s).unwrap()),
                       HashSet::from_iter(t.into_iter()));
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
            let res_subs_result = super::subs_var_rec(&src_term, &var, &val_term);
            let iter_subs_result = super::subs_var_iter(&src_term, &var, &val_term);

            assert_eq!(res_subs_result, result_term);
            assert_eq!(iter_subs_result, result_term);
        }
    }
}
