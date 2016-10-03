use types::{TermType, RcTermType, TypeVariable, Sort, Kind, RcKind};
use terms::{Term, Literal, Variable};
use builtin::BuiltinType;

pub fn subst_type(ty: &RcTermType, ty_var: &TypeVariable, res: &RcTermType) -> RcTermType {
    match *ty.as_ref() {
            TermType::Int |
            TermType::Bool |
            TermType::Named(..) |
            TermType::Type => None,
            TermType::Var(ref v) => {
                if ty_var.name() == v.name() {
                    Some(res.clone())
                } else {
                    None
                }
            }
            TermType::Arrow(ref from, ref to) => {
                Some(TermType::new_arrow(subst_type(from, ty_var, res),
                                         subst_type(to, ty_var, res)))
            }
            TermType::Pi(ref v, ref ty) => {
                if v.name() == ty_var.name() {
                    None
                } else {
                    Some(TermType::new_pi(v.clone(), subst_type(ty, ty_var, res)))
                }
            }
            TermType::Appl(ref l, ref r) => {
                Some(TermType::new_appl(subst_type(l, ty_var, res), subst_type(r, ty_var, res)))
            }
        }
        .unwrap_or_else(|| ty.clone())
}

fn get_vars_type(var: &Variable) -> Option<RcTermType> {
    match *var.sort() {
        Sort::None => None,
        Sort::Kind(..) => Some(TermType::new_type()),
        Sort::Type(ref ty) => Some(ty.clone()),
    }
}

fn get_type_kind(ty: &TermType, bound_term_vars: &Vec<Variable>) -> Option<RcKind> {
    fn get_type_kind_rec(ty: &TermType,
                         bound_type_vars: &mut Vec<TypeVariable>,
                         bound_term_vars: &Vec<Variable>)
                         -> Option<RcKind> {
        match *ty {
            TermType::Int |
            TermType::Bool |
            TermType::Named(..) |
            TermType::Arrow(..) |
            TermType::Type => Some(Kind::new_star()),
            TermType::Var(ref tv) => {
                if let Some(ref k) = *tv.kind() {
                    return Some(k.clone());
                }

                if let Some(v) = bound_type_vars.iter()
                    .rev()
                    .find(|u| u.name() == tv.name()) {
                    return v.kind().clone();
                }

                if let Some(v) = bound_term_vars.iter()
                    .rev()
                    .find(|u| u.name() == tv.name()) {
                    if let Sort::Kind(ref k) = *v.sort() {
                        return Some(k.clone());
                    }
                }
                tv.kind().clone()
            }
            TermType::Pi(ref tv, ref body) => {
                tv.kind().as_ref().and_then(|vk| {
                    bound_type_vars.push(tv.clone());
                    let res = get_type_kind_rec(body, bound_type_vars, bound_term_vars)
                        .map(|k| Kind::new_arrow(vk.clone(), k));
                    bound_type_vars.pop();
                    res
                })
            }
            TermType::Appl(ref l, ref r) => {
                if let (Some(ref l_kind), Some(ref r_kind)) = (get_type_kind_rec(l,
                                                                                 bound_type_vars,
                                                                                 bound_term_vars),
                                                               get_type_kind_rec(r,
                                                                                 bound_type_vars,
                                                                                 bound_term_vars)) {
                    if let Kind::Arrow(ref from, ref to) = *l_kind.as_ref() {
                        if from == r_kind {
                            return Some(to.clone());
                        }
                    }
                }
                None
            }
        }
    }

    let mut bound_type_vars = Vec::new();
    get_type_kind_rec(ty, &mut bound_type_vars, bound_term_vars)
}

fn reduce_type_step(ty: &TermType, bound_term_vars: &Vec<Variable>) -> Option<RcTermType> {
    match *ty {
        TermType::Int |
        TermType::Bool |
        TermType::Named(..) |
        TermType::Var(..) |
        TermType::Pi(..) |
        TermType::Type => None,
        TermType::Arrow(ref from, ref to) => {
            let r_from = reduce_type_step(from, bound_term_vars);
            let r_to = reduce_type_step(to, bound_term_vars);
            if r_from.is_none() && r_to.is_none() {
                None
            } else {
                Some(TermType::new_arrow(r_from.unwrap_or_else(|| from.clone()),
                                         r_to.unwrap_or_else(|| to.clone())))
            }
        }
        TermType::Appl(ref l, ref r) => {
            let l = reduce_type_step(l, bound_term_vars).unwrap_or_else(|| l.clone());
            let r = reduce_type_step(r, bound_term_vars).unwrap_or_else(|| r.clone());
            if let (&TermType::Pi(ref var, ref body), Some(ref l_kind), Some(ref r_kind)) =
                   (l.as_ref(),
                    get_type_kind(&l, bound_term_vars),
                    get_type_kind(&r, bound_term_vars)) {
                if let Kind::Arrow(ref from, _) = *l_kind.as_ref() {
                    if from == r_kind {
                        return Some(subst_type(body, var, &r));
                    }
                }
            }
            None
        }
    }
}

fn reduce_type(ty: RcTermType, bound_term_vars: &Vec<Variable>) -> RcTermType {
    let mut result = ty;
    while let Some(ty) = reduce_type_step(&result, bound_term_vars) {
        result = ty;
    }
    result
}

pub fn check_term_type(term: &Term) -> Option<RcTermType> {
    let mut var_bindings = Vec::new();

    fn check_type_rec(term: &Term, var_bindings: &mut Vec<Variable>) -> Option<RcTermType> {
        match *term {
            Term::Var(ref v) => {
                match var_bindings.iter().rev().find(|u| u.name() == v.name()) {
                    Some(u) => {
                        if u.sort() == v.sort() || *v.sort() == Sort::None {
                            return get_vars_type(u);
                        }
                    }
                    None => {
                        if *v.sort() != Sort::None {
                            return get_vars_type(v);
                        }
                    }
                }
                None
            }
            Term::Abs(ref v, ref b) => {
                var_bindings.push(v.clone());
                let body_type = check_type_rec(b, var_bindings);
                var_bindings.pop();
                match *v.sort() {
                    Sort::None => None,
                    Sort::Kind(..) => {
                        match (TypeVariable::from_var(v), body_type) {
                            (Some(tv), Some(t)) => Some(TermType::new_pi(tv, t)),
                            _ => None,
                        }
                    }
                    Sort::Type(ref ty) => body_type.map(|t| TermType::new_arrow(ty.clone(), t)),
                }
            }
            Term::Appl(ref l_term, ref r_term) => {
                let left_type = check_type_rec(l_term, var_bindings)
                    .map(|ty| reduce_type(ty, var_bindings));
                let right_type = check_type_rec(r_term, var_bindings)
                    .map(|ty| reduce_type(ty, var_bindings));
                if let (Some(ref l), Some(ref r)) = (left_type, right_type) {
                    match (l.as_ref(), r_term.as_ref()) {
                        (&TermType::Arrow(ref from, ref to), _) => {
                            if from.is_isomorphic_to(r) {
                                return Some(to.clone());
                            }
                        }
                        (&TermType::Pi(..), &Term::Type(ref v_ty)) => {
                            return Some(TermType::new_appl(l.clone(), v_ty.clone()));
                        }
                        _ => (),
                    }
                }
                None
            }
            Term::Lit(ref l) => {
                match *l {
                    Literal::Num(..) => Some(TermType::new_int()),
                    Literal::Bool(..) => Some(TermType::new_bool()),
                }
            }
            Term::If(ref i, ref t, ref e) => {
                let if_type = check_type_rec(i, var_bindings)
                    .map(|ty| reduce_type(ty, var_bindings));
                let then_type = check_type_rec(t, var_bindings)
                    .map(|ty| reduce_type(ty, var_bindings));
                let else_type = check_type_rec(e, var_bindings)
                    .map(|ty| reduce_type(ty, var_bindings));
                if let (Some(ref if_t), Some(ref then_t), Some(ref else_t)) = (if_type,
                                                                               then_type,
                                                                               else_type) {
                    if *if_t.as_ref() == TermType::Bool && then_t.is_isomorphic_to(else_t) {
                        return Some(then_t.clone());
                    }
                }
                None
            }
            Term::Builtin(ref bc) => {
                let results = bc.args()
                    .iter()
                    .map(|t| {
                        check_type_rec(t, var_bindings).map(|ty| reduce_type(ty, var_bindings))
                    })
                    .collect::<Vec<_>>();
                if results.iter().any(|t| t.is_none()) {
                    None
                } else {
                    let results: Vec<RcTermType> =
                        results.into_iter().map(|t| t.unwrap()).collect();
                    match bc.builtin_type() {
                        BuiltinType::Add | BuiltinType::Sub | BuiltinType::Mul |
                        BuiltinType::Div => {
                            match *results.as_slice() {
                                [] => Some(TermType::new_arrow
                                           (TermType::new_int(),
                                            TermType::new_arrow
                                            (TermType::new_int(),
                                             TermType::new_int()))),
                                [ref a] if *a.as_ref() == TermType::Int => {
                                    Some(TermType::new_arrow(TermType::new_int(),
                                                             TermType::new_int()))
                                }
                                [ref a, ref b] if *a.as_ref() == TermType::Int &&
                                                  *b.as_ref() == TermType::Int => {
                                    Some(TermType::new_int())
                                }
                                _ => None,
                            }
                        }
                        BuiltinType::Eq => {
                            match *results.as_slice() {
                                [] => Some(TermType::new_arrow
                                           (TermType::new_int(),
                                            TermType::new_arrow
                                            (TermType::new_int(),
                                             TermType::new_bool()))),
                                [ref a] if *a.as_ref() == TermType::Int => {
                                    Some(TermType::new_arrow(TermType::new_int(),
                                                             TermType::new_bool()))
                                }
                                [ref a, ref b] if *a.as_ref() == TermType::Int &&
                                                  *b.as_ref() == TermType::Int => {
                                    Some(TermType::new_bool())
                                }
                                _ => None,
                            }
                        }
                    }
                }
            }
            Term::Type(..) => Some(TermType::new_type()),
        }
    }

    check_type_rec(term, &mut var_bindings).map(|ty| reduce_type(ty, &var_bindings))
}

#[cfg(test)]
mod test {
    use parse::{parse_type, parse_term};

    #[test]
    fn type_check_test() {
        let tests = vec![("1", "Int"),
                         ("true", "Bool"),
                         ("+", "Int->Int->Int"),
                         ("(- 10)", "Int->Int"),
                         ("(= 20 30)", "Bool"),
                         ("\\x:A.x", "A->A"),
                         ("\\x:A->B.\\y:A.x y", "(A->B)->A->B"),
                         ("(\\f:Int->Int.\\x:Int.f (f x)) (* 30) 0", "Int"),
                         (r"(\a:*.\f:a.f)", r"\a:*.a->a"),
                         (r"(\a:*.\f:a.f) [A]", r"A->A"),
                         (r"(\a:*.\f:a.f) [A] x:A", r"A"),
                         (r"(\a:*.\f:a->a.\x:a.f (f x))", r"\a:*.(a->a)->a->a"),
                         (r"a:{(\a:*.a->a) Int}", r"Int->Int"),
                         (r"a:{{(\a:*=>*.\b:*.{a ({a b})}) (\x:*.x->x)} Int}",
                          r"(Int->Int)->(Int->Int)"),
                         ("let inc:Int->Int=\\x:Int.(+ x 1) in let \
                           dup:\\t:*.(t->t)->t->t=\\t:*.\\f:t->t.\\x:t.f (f x) in dup [Int] inc 10",
                          r"Int"),
                         (r"(\a:*.\f:\b:*.b->b.\x:a.f [a] x) [Int] (\c:*.\x:c.x) 10", r"Int"),
                         (r"(\a:*=>*.\b:*.\x:{a ({a b})}.x) [\x:*.x->x] [Int] ",
                          r"((Int->Int)->(Int->Int))->((Int->Int)->(Int->Int))"),
                         ("\\x:Int.\\y:A.\\z:A. if (= x 0) then z else y", "Int->A->A->A")];
        for (term, term_type) in tests.into_iter() {
            let src_term = parse_term(term).expect(term);
            let src_type = parse_type(term_type).expect(term_type);
            assert_eq!(super::check_term_type(&src_term), Some(src_type));
        }
    }
}
