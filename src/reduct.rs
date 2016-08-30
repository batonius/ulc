use types::{Term, RcTerm, RcVar, TermVisitor, BoundVars, Variable};
use std::collections::HashSet;

pub fn free_vars(term: &Term) -> HashSet<RcVar> {
    struct FreeVarCollector(HashSet<RcVar>);
    impl TermVisitor for FreeVarCollector {
        fn visit_var(&mut self, bound_vars: &BoundVars, var: &RcVar) {
            if !bound_vars.iter().any(|x| *x == var) {
                self.0.insert(var.clone());
            }
        }
    }
    let mut free_var_collector = FreeVarCollector(HashSet::new());
    term.visit(&mut free_var_collector);
    free_var_collector.0
}

pub fn subs_var(term: &RcTerm, var: &RcVar, val: &RcTerm) -> RcTerm {
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
            assert_eq!(&super::subs_var(&parse_term(term).unwrap(),
                                        var,
                                        &parse_term(val).unwrap()),
                       &parse_term(res).unwrap());
        }
    }
}
