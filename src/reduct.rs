use terms::{Term, RcTerm, Variable, Literal};
use builtin::BuiltinClosure;
use folder::*;

pub fn subs_var(term: &RcTerm, var: &Variable, val: &RcTerm) -> RcTerm {
    struct VarSubstitutor<'a> {
        var: &'a Variable,
        val: &'a RcTerm,
        bound_vars: Vec<Variable>,
    };

    impl<'a> TermFolder for VarSubstitutor<'a> {
        fn fold_var(&mut self, v: &Variable) -> Option<RcTerm> {
            if self.bound_vars.iter().any(|x| x == self.var) || v != self.var {
                None
            } else {
                Some(self.val.clone())
            }
        }

        fn fold_abs(&mut self, v: &Variable, b: &RcTerm) -> Option<RcTerm> {
            self.bound_vars.push(v.clone());
            let res = default_fold_abs(self, v, b);
            self.bound_vars.pop();
            res
        }
    }

    let mut substitutor = VarSubstitutor {
        var: var,
        val: val,
        bound_vars: Vec::new(),
    };
    term.fold(&mut substitutor).unwrap_or_else(|| term.clone())
}

struct BetaReductor<T> {
    _field: T,
}

impl<T: BetaReductionStrategy> BetaReductor<T> {
    pub fn new() -> BetaReductor<T> {
        BetaReductor { _field: T::default() }
    }
}

trait BetaReductionStrategy: Sized + Default {
    fn fold_beta_appl(&mut BetaReductor<Self>, l: &RcTerm, r: &RcTerm) -> Option<RcTerm>;
}

impl<T: BetaReductionStrategy> TermFolder for BetaReductor<T> {
    fn fold_appl(&mut self, l: &RcTerm, r: &RcTerm) -> Option<RcTerm> {
        T::fold_beta_appl(self, l, r)
    }

    fn fold_abs(&mut self, _: &Variable, _: &RcTerm) -> Option<RcTerm> {
        None
    }

    fn fold_builtin(&mut self, builtin: &BuiltinClosure) -> Option<RcTerm> {
        default_fold_builtin(self, builtin)
            .and_then(|t| {
                if let Term::Builtin(ref bi) = *t.as_ref() {
                    bi.try_compute().or_else(|| Some(t.clone()))
                } else {
                    Some(t.clone())
                }
            })
            .or_else(|| builtin.try_compute())
    }

    fn fold_if(&mut self, i: &RcTerm, t: &RcTerm, e: &RcTerm) -> Option<RcTerm> {
        let i_res = i.fold(self);
        let i_is_none = i_res.is_none();
        let i = i_res.as_ref().unwrap_or(i);
        if let Term::Lit(Literal::Bool(b)) = *i.as_ref() {
            let res = if b { t } else { e };
            res.fold(self).or_else(|| Some(res.clone()))
        } else if i_is_none {
            None
        } else {
            Some(Term::if_rc(i.clone(), t.clone(), e.clone()))
        }
    }
}

struct StrictBetaReductionStrategy {}

impl Default for StrictBetaReductionStrategy {
    fn default() -> StrictBetaReductionStrategy {
        StrictBetaReductionStrategy {}
    }
}

impl BetaReductionStrategy for StrictBetaReductionStrategy {
    fn fold_beta_appl(_self: &mut BetaReductor<StrictBetaReductionStrategy>,
                      l: &RcTerm,
                      r: &RcTerm)
                      -> Option<RcTerm> {
        let l_res = l.fold(_self);
        let r_res = r.fold(_self);
        let both_none = l_res.is_none() && r_res.is_none();
        let l = l_res.as_ref().unwrap_or(l);
        let r = r_res.as_ref().unwrap_or(r);
        match *l.as_ref() {
            Term::Abs(ref v, ref b) => {
                let subs = subs_var(b, v, r);
                subs.fold(_self).or_else(|| Some(subs))
            }
            Term::Builtin(ref builtin) => builtin.apply_term(r),
            _ => {
                if both_none {
                    None
                } else {
                    Some(Term::appl_rc(l.clone(), r.clone()))
                }
            }
        }
    }
}

struct LazyBetaReductionStrategy {}

impl Default for LazyBetaReductionStrategy {
    fn default() -> LazyBetaReductionStrategy {
        LazyBetaReductionStrategy {}
    }
}

impl BetaReductionStrategy for LazyBetaReductionStrategy {
    fn fold_beta_appl(_self: &mut BetaReductor<LazyBetaReductionStrategy>,
                      l: &RcTerm,
                      r: &RcTerm)
                      -> Option<RcTerm> {
        let l_res = l.fold(_self);
        let l_res_is_none = l_res.is_none();
        let l = l_res.as_ref().unwrap_or(l);
        match *l.as_ref() {
            Term::Abs(ref v, ref b) => Some(subs_var(b, v, r)),
            Term::Builtin(ref builtin) => builtin.apply_term(r),
            _ => {
                if l_res_is_none {
                    None
                } else {
                    Some(Term::appl_rc(l.clone(), r.clone()))
                }
            }
        }
    }
}

fn beta_step<BRS>(term: &RcTerm) -> Option<RcTerm>
    where BRS: BetaReductionStrategy
{
    let mut beta_reductor = BetaReductor::<BRS>::new();
    term.fold(&mut beta_reductor)
}

fn beta_reduction<BRS>(term: &RcTerm) -> RcTerm
    where BRS: BetaReductionStrategy
{
    let mut result = term.clone();
    while let Some(new_term) = beta_step::<BRS>(&result) {
        result = new_term;
    }
    result
}

pub fn beta_reduction_strict(term: &RcTerm) -> RcTerm {
    beta_reduction::<StrictBetaReductionStrategy>(term)
}

pub fn beta_reduction_lazy(term: &RcTerm) -> RcTerm {
    beta_reduction::<LazyBetaReductionStrategy>(term)
}

#[cfg(test)]
mod test {
    use parse::parse_term;
    use terms::{Term, Variable};
    use super::{StrictBetaReductionStrategy, LazyBetaReductionStrategy};
    use test::Bencher;

    static FACTORIAL: &'static str = "(\\f.(\\x.f (x x)) (\\x.f (x x))) \
                                      (\\f.\\n.(if (= n 1) then 1 else (* n (f (- n 1))))) 20";
    static STRICT_FACTORIAL: &'static str = "(\\f.(\\x.f (\\v.(x x v))) (\\x.f (\\v.(x x v)))) \
                                             (\\f.\\n.(if (= n 1) then 1 else (* n (f (- n 1))))) \
                                             20";
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
            let src_term = parse_term(term).expect(term);
            let val_term = parse_term(val).expect(val);
            let result_term = parse_term(res).expect(res);
            let rec_subs_result = super::subs_var(&src_term, &var, &val_term);
            let iter_subs_result = super::subs_var(&src_term, &var, &val_term);

            assert_eq!(rec_subs_result, result_term);
            assert_eq!(iter_subs_result, result_term);
        }
    }

    #[test]
    fn beta_step() {
        let tests = vec![("a", "a"), ("(\\x.x) z", "z"), ("(\\a.10) x", "10")];

        for (from, to) in tests {
            let from_term = parse_term(from).expect(from);
            let to_term = parse_term(to).expect(to);

            assert_eq!(super::beta_step::<StrictBetaReductionStrategy>(&from_term)
                           .unwrap_or(from_term.clone()),
                       to_term);
            assert_eq!(super::beta_step::<LazyBetaReductionStrategy>(&from_term)
                           .unwrap_or(from_term.clone()),
                       to_term);
        }
    }

    #[test]
    fn beta_step_lazy() {
        let tests = vec![("(\\z. x ((\\y. z y) 9)) b", "x ((\\y. b y) 9)")];

        for (from, to) in tests {
            let from_term = parse_term(from).expect(from);
            let to_term = parse_term(to).expect(to);

            assert_eq!(super::beta_step::<LazyBetaReductionStrategy>(&from_term)
                           .unwrap_or(from_term.clone()),
                       to_term);
        }
    }

    #[test]
    fn beta_recution() {
        let tests = vec![("a", "a"),
                         ("(\\x.x) z", "z"),
                         ("(\\f.\\x.f x) (\\n.0) 100", "0"),
                         ("(\\f.\\a. f (f a)) (\\x.+ x 10) 0", "20"),
                         ("(\\x.if x then a else b) false", "b"),
                         ("(\\x.if x then 1 else ((\\x.x x)(\\x.x x))) true", "1")];

        for (from, to) in tests {
            let from_term = parse_term(from).expect(from);
            let to_term = parse_term(to).expect(to);

            assert_eq!(super::beta_reduction_strict(&from_term), to_term);
            assert_eq!(super::beta_reduction_lazy(&from_term), to_term);
        }
    }

    #[test]
    fn beta_recution_lazy() {
        let tests = vec![("(\\a.\\b.b) ((\\x.x x)(\\x.x x)) 10", "10"),
                         ("(\\z. x ((\\y. z y) 1)) b", "x ((\\y. b y) 1)"),
                         ("(\\x.if x then ((\\x.x x)(\\x.x x)) else 42) false", "42")];

        for (from, to) in tests {
            let from_term = parse_term(from).expect(from);
            let to_term = parse_term(to).expect(to);

            assert_eq!(super::beta_reduction_lazy(&from_term), to_term);
        }
    }

    #[bench]
    fn lazy_rec_reduction(b: &mut Bencher) {
        let term = parse_term(FACTORIAL).expect(FACTORIAL);
        b.iter(|| assert_eq!(super::beta_reduction_lazy(&term), Term::num_lit_rc(FAC_20)));
    }

    #[bench]
    fn strict_rec_reduction(b: &mut Bencher) {
        let term = parse_term(STRICT_FACTORIAL).expect(STRICT_FACTORIAL);
        b.iter(|| {
            assert_eq!(super::beta_reduction_strict(&term),
                       Term::num_lit_rc(FAC_20))
        });
    }
}
