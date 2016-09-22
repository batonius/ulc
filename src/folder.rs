use terms::{Term, RcTerm, Variable, Literal};
use builtin::BuiltinClosure;

pub trait TermFolder: Sized {
    fn fold_var(&mut self, v: &Variable) -> Option<RcTerm> {
        default_fold_var(self, v)
    }

    fn fold_abs(&mut self, v: &Variable, b: &RcTerm) -> Option<RcTerm> {
        default_fold_abs(self, v, b)
    }

    fn fold_appl(&mut self, r: &RcTerm, l: &RcTerm) -> Option<RcTerm> {
        default_fold_appl(self, r, l)
    }

    fn fold_builtin(&mut self, bc: &BuiltinClosure) -> Option<RcTerm> {
        default_fold_builtin(self, bc)
    }

    fn fold_if(&mut self, i: &RcTerm, t: &RcTerm, e: &RcTerm) -> Option<RcTerm> {
        default_fold_if(self, i, t, e)
    }

    fn fold_lit(&mut self, l: &Literal) -> Option<RcTerm> {
        default_fold_lit(self, l)
    }
}

pub fn fold<F: TermFolder>(f: &mut F, t: &Term) -> Option<RcTerm> {
    match *t {
        Term::Var(ref v) => f.fold_var(v),
        Term::Abs(ref v, ref b) => f.fold_abs(v, b),
        Term::Appl(ref l, ref r) => f.fold_appl(l, r),
        Term::Lit(ref l) => f.fold_lit(l),
        Term::Builtin(ref bc) => f.fold_builtin(bc),
        Term::If(ref i, ref t, ref e) => f.fold_if(i, t, e),
    }
}

impl Term {
    pub fn fold<F: TermFolder>(&self, f: &mut F) -> Option<RcTerm> {
        fold(f, self)
    }
}

pub fn default_fold_var<F: TermFolder>(_: &mut F, _: &Variable) -> Option<RcTerm> {
    None
}

pub fn default_fold_abs<F: TermFolder>(f: &mut F, v: &Variable, b: &RcTerm) -> Option<RcTerm> {
    fold(f, b).map(|b| Term::abs_rc(v.clone(), b))
}

pub fn default_fold_appl<F: TermFolder>(f: &mut F, l: &RcTerm, r: &RcTerm) -> Option<RcTerm> {
    let l_res = fold(f, l);
    let r_res = fold(f, r);
    if l_res.is_none() && r_res.is_none() {
        None
    } else {
        Some(Term::appl_rc(l_res.unwrap_or_else(|| l.clone()),
                           r_res.unwrap_or_else(|| r.clone())))
    }
}

pub fn default_fold_builtin<F: TermFolder>(f: &mut F, bc: &BuiltinClosure) -> Option<RcTerm> {
    let results: Vec<_> = bc.args().iter().map(|t| fold(f, t)).collect();
    if results.iter().all(Option::is_none) {
        None
    } else {
        let results = results.into_iter()
            .zip(bc.args().iter())
            .map(|(r, t)| r.unwrap_or_else(|| t.clone()))
            .collect();
        Some(Term::builtin_rc(bc.builtin_type(), results))
    }
}

pub fn default_fold_if<F: TermFolder>(f: &mut F,
                                      i: &RcTerm,
                                      t: &RcTerm,
                                      e: &RcTerm)
                                      -> Option<RcTerm> {
    let i_res = fold(f, i);
    let t_res = fold(f, t);
    let e_res = fold(f, e);
    if i_res.is_none() && t_res.is_none() && e_res.is_none() {
        None
    } else {
        Some(Term::if_rc(i_res.unwrap_or_else(|| i.clone()),
                         t_res.unwrap_or_else(|| t.clone()),
                         e_res.unwrap_or_else(|| e.clone())))
    }
}

pub fn default_fold_lit<F: TermFolder>(_: &mut F, _: &Literal) -> Option<RcTerm> {
    None
}
