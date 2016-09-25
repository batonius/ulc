use types::{TermType, RcTermType, TypeVariable, Sort};
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
        }
        .unwrap_or_else(|| ty.clone())
}

fn get_vars_type(var: &Variable) -> Option<RcTermType> {
    match *var.sort() {
        Sort::None => None,
        Sort::Kind(..) => Some(TermType::new_type()),
        Sort::Type(ref ty) => Some(ty.clone())
    }
}

pub fn check_term_type(term: &Term) -> Option<RcTermType> {
    let mut var_bindings = Vec::new();

    fn check_type_rec(term: &Term, var_bindings: &mut Vec<Variable>) -> Option<RcTermType> {
        match *term {
            Term::Var(ref v) => {
                match var_bindings.iter().rev().find(|u| u.name() == v.name()) {
                    Some(u) => {
                        if u.sort() == v.sort() ||
                           *v.sort() == Sort::None {
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
                            _ => None
                        }
                    }
                    Sort::Type(ref ty) => {
                        body_type.map(|t| TermType::new_arrow(ty.clone(), t))
                    }
                }
            }
            Term::Appl(ref l_term, ref r_term) => {
                let left_type = check_type_rec(l_term, var_bindings);
                let right_type = check_type_rec(r_term, var_bindings);
                if let (Some(ref l), Some(ref r)) = (left_type, right_type) {
                    match (l.as_ref(), r_term.as_ref()) {
                        (&TermType::Arrow(ref from, ref to), _) => {
                            if from == r {
                                return Some(to.clone());
                            }
                        }
                        (&TermType::Pi(ref v, ref ty), &Term::Type(ref v_ty)) => {
                            return Some(subst_type(ty, v, v_ty))
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
                let if_type = check_type_rec(i, var_bindings);
                let then_type = check_type_rec(t, var_bindings);
                let else_type = check_type_rec(e, var_bindings);
                if let (Some(ref if_t), Some(ref then_t), Some(ref else_t)) = (if_type,
                                                                               then_type,
                                                                               else_type) {
                    if *if_t.as_ref() == TermType::Bool && then_t == else_t {
                        return Some(then_t.clone());
                    }
                }
                None
            }
            Term::Builtin(ref bc) => {
                let results =
                    bc.args().iter().map(|t| check_type_rec(t, var_bindings)).collect::<Vec<_>>();
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

    check_type_rec(term, &mut var_bindings)
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
                         (r"let inc:Int->Int=\x:Int.(+ x 1) in 
let dup:\t:*.(t->t)->t->t=\t:*.\f:t->t.\x:t.f (f x) in dup [Int] inc 10",
                          r"Int"),
                         (r"(\a:*.\f:\b:*.b->b.\x:a.f [a] x) [Int] (\b:*.\x:b.x) 10", r"Int"),
                         ("\\x:Int.\\y:A.\\z:A. if (= x 0) then z else y", "Int->A->A->A")];
        for (term, term_type) in tests.into_iter() {
            let src_term = parse_term(term).expect(term);
            let src_type = parse_type(term_type).expect(term_type);
            assert_eq!(super::check_term_type(&src_term), Some(src_type));
        }
    }
}
