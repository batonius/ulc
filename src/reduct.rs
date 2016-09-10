use types::{Term, RcTerm, Variable, Literal};
use visitor::{TermVisitor, TermVisitorStrategy, IfBranchesPolicy};
use std::marker::PhantomData;
use builtin::BuiltinClosure;

pub fn subs_var<VS: TermVisitorStrategy>(term: &RcTerm, var: &Variable, val: &RcTerm) -> RcTerm {
    struct VarSubstitutor<'a> {
        var: &'a Variable,
        val: &'a RcTerm,
        bound_vars: Vec<Variable>,
    };

    impl<'a> TermVisitor for VarSubstitutor<'a> {
        type Result = Option<RcTerm>;

        fn leave_lit(&mut self, _: &Literal) -> Self::Result {
            None
        }

        fn leave_var(&mut self, v: &Variable) -> Self::Result {
            if self.bound_vars.iter().any(|x| x == self.var) || v != self.var {
                None
            } else {
                Some(self.val.clone())
            }
        }

        fn enter_abs(&mut self, var: &Variable, _: &RcTerm) -> Option<Self::Result> {
            self.bound_vars.push(var.clone());
            None
        }

        fn leave_abs(&mut self,
                     v: &Variable,
                     _: &RcTerm,
                     body_result: Self::Result)
                     -> Self::Result {
            self.bound_vars.pop();
            body_result.map(|new_body| Term::abs_rc(v.clone(), new_body))
        }

        fn leave_appl(&mut self,
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

        fn leave_builtin(&mut self,
                         builtin: &BuiltinClosure,
                         results: Vec<Self::Result>)
                         -> Self::Result {
            if results.iter().all(Option::is_none) {
                None
            } else {
                let results = results.into_iter()
                    .zip(builtin.args().iter())
                    .map(|(r, t)| r.unwrap_or_else(|| t.clone()))
                    .collect();
                Some(Term::builtin_rc(builtin.builtin_type(), results))
            }
        }
        fn leave_if(&mut self,
                    i: &RcTerm,
                    t: &RcTerm,
                    e: &RcTerm,
                    i_res: Self::Result,
                    t_e_result: Option<(Self::Result, Self::Result)>)
                    -> Self::Result {
            let (t_res, e_res) = t_e_result.unwrap();
            if i_res.is_none() && t_res.is_none() && e_res.is_none() {
                None
            } else {
                Some(Term::if_rc(i_res.unwrap_or_else(|| i.clone()),
                                 t_res.unwrap_or_else(|| t.clone()),
                                 e_res.unwrap_or_else(|| e.clone())))
            }
        }
    }

    let mut var_substitutor = VarSubstitutor {
        var: var,
        val: val,
        bound_vars: Vec::new(),
    };
    term.visit::<VarSubstitutor, VS>(&mut var_substitutor).unwrap_or_else(|| term.clone())
}

pub trait BetaRecutionStrategy: TermVisitor + Default {}

pub struct StrictBetaStep<VS> {
    _field: PhantomData<VS>,
}

impl<VS: TermVisitorStrategy> TermVisitor for StrictBetaStep<VS> {
    type Result = Option<RcTerm>;
    fn leave_lit(&mut self, _: &Literal) -> Self::Result {
        None
    }
    fn leave_var(&mut self, _: &Variable) -> Self::Result {
        None
    }

    fn enter_abs(&mut self, _: &Variable, _: &RcTerm) -> Option<Self::Result> {
        Some(None)
    }

    fn leave_abs(&mut self, _: &Variable, _: &RcTerm, _: Self::Result) -> Self::Result {
        None
    }

    fn enter_if(&mut self, _: &RcTerm, _: &RcTerm, _: &RcTerm) -> IfBranchesPolicy {
        IfBranchesPolicy::DontProcessBranches
    }

    fn leave_appl(&mut self,
                  l: &RcTerm,
                  r: &RcTerm,
                  l_res: Self::Result,
                  r_res: Self::Result)
                  -> Self::Result {
        if l_res.is_none() && r_res.is_none() {
            match **l {
                Term::Abs(ref v, ref b) => Some(subs_var::<VS>(b, v, r)),
                Term::Builtin(ref builtin) => builtin.apply_term(r),
                _ => None,
            }
        } else {
            Some(Term::appl_rc(l_res.unwrap_or_else(|| l.clone()),
                               r_res.unwrap_or_else(|| r.clone())))
        }
    }

    fn leave_builtin(&mut self,
                     builtin: &BuiltinClosure,
                     results: Vec<Self::Result>)
                     -> Self::Result {
        if results.iter().all(Option::is_none) {
            builtin.try_compute()
        } else {
            let results = results.into_iter()
                .zip(builtin.args().iter())
                .map(|(r, t)| r.unwrap_or_else(|| t.clone()))
                .collect();
            Some(Term::builtin_rc(builtin.builtin_type(), results))
        }
    }

    fn leave_if(&mut self,
                i: &RcTerm,
                t: &RcTerm,
                e: &RcTerm,
                i_res: Self::Result,
                _: Option<(Self::Result, Self::Result)>)
                -> Self::Result {
        match i_res {
            None => {
                if let Term::Lit(Literal::Bool(b)) = *i.as_ref() {
                    Some(if b { t.clone() } else { e.clone() })
                } else {
                    None
                }
            }
            Some(i) => Some(Term::if_rc(i, t.clone(), e.clone())),
        }
    }
}

pub struct LazyBetaStep<VS> {
    _field: PhantomData<VS>,
}

impl<VS: TermVisitorStrategy> TermVisitor for LazyBetaStep<VS> {
    type Result = Option<RcTerm>;
    fn leave_lit(&mut self, _: &Literal) -> Self::Result {
        None
    }
    fn leave_var(&mut self, _: &Variable) -> Self::Result {
        None
    }
    fn leave_abs(&mut self, v: &Variable, _: &RcTerm, body_result: Self::Result) -> Self::Result {
        body_result.map(|new_body| Term::abs_rc(v.clone(), new_body))
    }

    fn enter_if(&mut self, _: &RcTerm, _: &RcTerm, _: &RcTerm) -> IfBranchesPolicy {
        IfBranchesPolicy::DontProcessBranches
    }

    fn enter_appl(&mut self, l: &RcTerm, r: &RcTerm) -> Option<Self::Result> {
        match **l {
            Term::Abs(ref v, ref b) => Some(Some(subs_var::<VS>(b, v, r))),
            Term::Builtin(ref builtin) => Some(builtin.apply_term(r)),
            _ => None,
        }
    }

    fn enter_builtin(&mut self, builtin: &BuiltinClosure) -> Option<Self::Result> {
        builtin.try_compute().map(Some)
    }

    fn leave_appl(&mut self,
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

    fn leave_builtin(&mut self,
                     builtin: &BuiltinClosure,
                     results: Vec<Self::Result>)
                     -> Self::Result {
        if results.iter().all(Option::is_none) {
            None
        } else {
            let results = results.into_iter()
                .zip(builtin.args().iter())
                .map(|(r, t)| r.unwrap_or_else(|| t.clone()))
                .collect();
            Some(Term::builtin_rc(builtin.builtin_type(), results))
        }
    }

    fn leave_if(&mut self,
                i: &RcTerm,
                t: &RcTerm,
                e: &RcTerm,
                i_res: Self::Result,
                _: Option<(Self::Result, Self::Result)>)
                -> Self::Result {
        let cond = i_res.unwrap_or_else(|| i.clone());
        if let Term::Lit(Literal::Bool(b)) = *(cond.as_ref()) {
            Some(if b { t.clone() } else { e.clone() })
        } else {
            None
        }
    }
}

impl<VS> Default for StrictBetaStep<VS> {
    fn default() -> Self {
        StrictBetaStep { _field: PhantomData }
    }
}

impl<VS> Default for LazyBetaStep<VS> {
    fn default() -> Self {
        LazyBetaStep { _field: PhantomData }
    }
}

impl<VS> BetaRecutionStrategy for StrictBetaStep<VS> where StrictBetaStep<VS>: TermVisitor {}
impl<VS> BetaRecutionStrategy for LazyBetaStep<VS> where LazyBetaStep<VS>: TermVisitor {}

pub fn beta_step<VS, BRS>(term: &RcTerm) -> Option<RcTerm>
    where VS: TermVisitorStrategy,
          BRS: BetaRecutionStrategy<Result = Option<RcTerm>>
{
    let mut beta_stepper = BRS::default();
    term.visit::<BRS, VS>(&mut beta_stepper)
}

fn beta_reduction<VS, BRS>(term: &RcTerm) -> RcTerm
    where VS: TermVisitorStrategy,
          BRS: BetaRecutionStrategy<Result = Option<RcTerm>>
{
    let mut result = term.clone();
    while let Some(new_term) = beta_step::<VS, BRS>(&result) {
        result = new_term;
    }
    result
}

pub fn beta_reduction_strict<VS>(term: &RcTerm) -> RcTerm
    where VS: TermVisitorStrategy
{
    beta_reduction::<VS, StrictBetaStep<VS>>(term)
}

pub fn beta_reduction_lazy<VS>(term: &RcTerm) -> RcTerm
    where VS: TermVisitorStrategy
{
    beta_reduction::<VS, LazyBetaStep<VS>>(term)
}

#[cfg(test)]
mod test {
    use parse::parse_term;
    use types::{Term, Variable};
    use visitor::{IterativeVisitorStrategy, RecursiveVisitorStrategy};
    use super::{StrictBetaStep, LazyBetaStep};
    use test::Bencher;

    static FACTORIAL: &'static str = "(\\f.(\\x.f (x x)) (\\x.f (x x))) \
                                      (\\f.\\n.(? (= n 1) 1 (* n (f (- n 1))))) 20";
    static STRICT_FACTORIAL: &'static str =
        "(\\f.(\\x.f (\\v.(x x v))) (\\x.f (\\v.(x x v)))) (\\f.\\n.(if (= n 1) then 1 else (* n \
         (f (- n 1))))) 20";
    static FAC_20: isize = 2_432_902_008_176_640_000;

    #[test]
    fn subs_vars() {
        let a = Variable::new("a");
        let b = Variable::new("b");
        let c = Variable::new("c");
        let tests = vec![("a", &a, "b", "b"),
                         ("b x", &b, "\\n.n", "(\\n.n) x"),
                         ("\\z. z c (\\c. c)", &c, "10", "\\z. z 10 (\\c. c)"),
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

    #[test]
    fn beta_step() {
        let tests = vec![("a", "a"), ("(\\x.x) z", "z"), ("(\\a.10) x", "10")];

        for (from, to) in tests {
            let from_term = parse_term(from).unwrap();
            let to_term = parse_term(to).unwrap();

            assert_eq!(super::beta_step::<RecursiveVisitorStrategy,
                                          LazyBetaStep<RecursiveVisitorStrategy>>(&from_term)
                           .unwrap_or(from_term.clone()),
                       to_term);
            assert_eq!(super::beta_step::<IterativeVisitorStrategy,
                                          LazyBetaStep<IterativeVisitorStrategy>>(&from_term)
                           .unwrap_or(from_term.clone()),
                       to_term);
            assert_eq!(super::beta_step::<RecursiveVisitorStrategy,
                                          StrictBetaStep<RecursiveVisitorStrategy>>(&from_term)
                           .unwrap_or(from_term.clone()),
                       to_term);
            assert_eq!(super::beta_step::<IterativeVisitorStrategy,
                                          StrictBetaStep<IterativeVisitorStrategy>>(&from_term)
                           .unwrap_or(from_term.clone()),
                       to_term);
        }
    }

    #[test]
    fn beta_step_lazy() {
        let tests = vec![("(\\z. x ((\\y. z y) 9)) b", "x ((\\y. b y) 9)")];

        for (from, to) in tests {
            let from_term = parse_term(from).unwrap();
            let to_term = parse_term(to).unwrap();

            assert_eq!(super::beta_step::<RecursiveVisitorStrategy,
                                          LazyBetaStep<RecursiveVisitorStrategy>>(&from_term)
                           .unwrap_or(from_term.clone()),
                       to_term);
            assert_eq!(super::beta_step::<IterativeVisitorStrategy,
                                          LazyBetaStep<IterativeVisitorStrategy>>(&from_term)
                           .unwrap_or(from_term.clone()),
                       to_term);
        }
    }

    #[test]
    fn beta_recution() {
        let tests = vec![("a", "a"),
                         ("(\\x.x) z", "z"),
                         ("(\\z. x ((\\y. z y) 1)) b", "x (b 1)"),
                         ("(\\f.\\x.f x) (\\n.0) 100", "0"),
                         ("(\\f.\\a. f (f a)) (\\x.+ x 10) 0", "20"),
                         ("(\\x.? x a b) false", "b"),
                         ("(\\x.if x then 1 else ((\\x.x x)(\\x.x x))) true", "1")];

        for (from, to) in tests {
            let from_term = parse_term(from).unwrap();
            let to_term = parse_term(to).unwrap();

            assert_eq!(super::beta_reduction_strict::<RecursiveVisitorStrategy>(&from_term),
                       to_term);
            assert_eq!(super::beta_reduction_strict::<IterativeVisitorStrategy>(&from_term),
                       to_term);
            assert_eq!(super::beta_reduction_lazy::<RecursiveVisitorStrategy>(&from_term),
                       to_term);
            assert_eq!(super::beta_reduction_lazy::<IterativeVisitorStrategy>(&from_term),
                       to_term);
        }
    }

    #[test]
    fn beta_recution_lazy() {
        let tests = vec![("(\\a.\\b.b) ((\\x.x x)(\\x.x x)) 10", "10"),
                         ("(\\x.? x ((\\x.x x)(\\x.x x)) 42) false", "42")];

        for (from, to) in tests {
            let from_term = parse_term(from).unwrap();
            let to_term = parse_term(to).unwrap();

            assert_eq!(super::beta_reduction_lazy::<RecursiveVisitorStrategy>(&from_term),
                       to_term);
            assert_eq!(super::beta_reduction_lazy::<IterativeVisitorStrategy>(&from_term),
                       to_term);
        }
    }

    #[bench]
    fn lazy_iter_reduction(b: &mut Bencher) {
        let term = parse_term(FACTORIAL).unwrap();
        b.iter(|| {
            assert_eq!(super::beta_reduction_lazy::<IterativeVisitorStrategy>(&term),
                       Term::num_lit_rc(FAC_20))
        });
    }

    #[bench]
    fn lazy_rec_reduction(b: &mut Bencher) {
        let term = parse_term(FACTORIAL).unwrap();
        b.iter(|| {
            assert_eq!(super::beta_reduction_lazy::<RecursiveVisitorStrategy>(&term),
                       Term::num_lit_rc(FAC_20))
        });
    }

    #[bench]
    fn strict_iter_reduction(b: &mut Bencher) {
        let term = parse_term(STRICT_FACTORIAL).unwrap();
        b.iter(|| {
            assert_eq!(super::beta_reduction_strict::<IterativeVisitorStrategy>(&term),
                       Term::num_lit_rc(FAC_20))
        });
    }

    #[bench]
    fn strict_rec_reduction(b: &mut Bencher) {
        let term = parse_term(STRICT_FACTORIAL).unwrap();
        b.iter(|| {
            assert_eq!(super::beta_reduction_strict::<RecursiveVisitorStrategy>(&term),
                       Term::num_lit_rc(FAC_20))
        });
    }
}
