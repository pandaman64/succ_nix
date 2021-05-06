// 型は単相だと思って全ての名前に対して単相の型を与える感じ
mod ast;
mod domain;
pub mod hir;
mod typing;

pub use typing::{success_type, Environment, Solution, Type};

#[cfg(test)]
mod test {
    use super::*;
    use crate::{domain::AttrSetType, typing::TypeErrorSink};
    use std::collections::{BTreeMap, HashMap};

    fn env() -> (HashMap<String, hir::Id>, Environment) {
        let not_ty = Type::fun(Type::boolean(), Type::boolean());
        let plus_ty = Type::fun(Type::integer(), Type::fun(Type::integer(), Type::integer()));
        let builtins_id = hir::Id::new();
        let builtins_ty = Type::attr_set(AttrSetType {
            attrs: {
                let mut attrs = BTreeMap::new();
                attrs.insert("not".into(), not_ty);
                attrs.insert("plus".into(), plus_ty);
                attrs
            },
            rest: Type::none().into(),
        });
        let true_id = hir::Id::new();
        let false_id = hir::Id::new();
        (
            vec![
                (String::from("builtins"), builtins_id),
                (String::from("true"), true_id),
                (String::from("false"), false_id),
            ]
            .into_iter()
            .collect(),
            vec![
                (builtins_id, builtins_ty),
                (true_id, Type::tt()),
                (false_id, Type::ff()),
            ]
            .into_iter()
            .collect(),
        )
    }

    fn run(
        input: &str,
        (name_env, mut ty_env): (HashMap<String, hir::Id>, Environment),
    ) -> (Type, TypeErrorSink) {
        let ast = rnix::parse(input).node();
        let terms = typed_arena::Arena::new();
        let alpha_env = name_env
            .into_iter()
            .map(|(v, id)| {
                let t = terms.alloc(hir::TermData::Var(id));
                (v, &*t)
            })
            .collect();
        let hir = hir::from_rnix(ast, &terms, &alpha_env);
        eprintln!("hir = {}", hir);

        let (t, c) = success_type(&mut ty_env, &hir);
        eprintln!("type = {}\nconstraint = {}", t, c);
        eprintln!("env:");
        for (v, t) in alpha_env.iter() {
            eprintln!("{} --> {}", v, t);
        }
        for (v, t) in ty_env.iter() {
            eprintln!("{} --> {}", v, t);
        }
        let mut sol = Solution::default();
        let mut sink = TypeErrorSink::default();
        sol.refine(&c, &mut sink);
        eprintln!("sol:");
        for (v, t) in sol.map.iter() {
            eprintln!("{} --> {}", v, t);
        }

        (sol.map_ty(&t), sink)
    }

    fn success(input: &str, env: (HashMap<String, hir::Id>, Environment), expected: Type) {
        let (actual, sink) = run(input, env);

        assert!(!sink.is_error());
        assert_eq!(actual, expected, "\n{} ≠ {}", actual, expected);
    }

    fn fail(input: &str, env: (HashMap<String, hir::Id>, Environment)) {
        let (_acutual, sink) = run(input, env);

        assert!(sink.is_error());
    }

    #[test]
    fn test_not_first() {
        success(
            "x: y: builtins.not x",
            env(),
            Type::fun(Type::boolean(), Type::fun(Type::any(), Type::boolean())),
        );
    }

    #[test]
    fn test_id() {
        success("x: x", env(), Type::fun(Type::any(), Type::any()));
    }

    #[test]
    fn test_xx() {
        success(
            "x: x x",
            env(),
            Type::fun(Type::fun(Type::any(), Type::any()), Type::any()),
        );
    }

    #[test]
    fn test_not_id() {
        fail("builtins.not (x: x)", env())
    }

    #[test]
    fn test_branch() {
        success(
            "x: if x then builtins.not x else x",
            env(),
            Type::fun(Type::boolean(), Type::boolean()),
        );
    }

    #[test]
    fn test_branch_half_succ() {
        // let F be this lambda abstaction.
        // If (F x) terminates, x = ff and F x = ff, which means F is typable with ff -> ff.
        success(
            "x: if x then x true else x",
            env(),
            Type::fun(Type::ff(), Type::ff()),
        );
    }

    #[test]
    fn test_let() {
        success(
            "let x = true; y = z: z x; in y",
            env(),
            Type::fun(Type::fun(Type::any(), Type::any()), Type::any()),
        )
    }

    #[test]
    #[ignore]
    fn test_let_infinite_types() {
        // CR pandaman: 再帰型が体系に必要だと思います。
        // infinite loop. something is wrong
        success("let x = x x; in x", env(), Type::any());
        // from TAPL
        success(
            "let hungry = x: hungry; in hungry",
            env(),
            Type::fun(Type::any(), Type::any()),
        );
    }

    #[test]
    fn test_let_fail() {
        fail("let x = true; y = z: z x; in y x", env())
    }

    #[test]
    fn test_attr_set() {
        success("{ x = true; y = z: z; }.x", env(), Type::tt());
        success(
            "{ x = true; y = z: z; }.y",
            env(),
            Type::fun(Type::any(), Type::any()),
        );
    }

    #[test]
    fn test_rec() {
        success(
            "let f = x: if x then x else f (builtins.not x); in f",
            env(),
            Type::fun(Type::boolean(), Type::tt()),
        );
    }

    #[test]
    fn test_fix() {
        success(
            "let y = f: f (y f); in y",
            env(),
            Type::fun(Type::fun(Type::any(), Type::any()), Type::any()),
        );
    }

    #[test]
    fn test_attr_set_arg() {
        success(
            "x: x.y",
            env(),
            Type::fun(
                Type::attr_set(AttrSetType {
                    attrs: {
                        let mut attrs = BTreeMap::new();
                        attrs.insert("y".into(), Type::any());
                        attrs
                    },
                    rest: Type::any().into(),
                }),
                Type::any(),
            ),
        );

        // TODO: in practice, Nix functions check the argument does not have more
        // attributes than neccessary unless specified via `{ <attrs>, ... }`.
        // thefore, the following inference result is imprecise (rest must be none).
        success(
            "{ y }: y",
            env(),
            Type::fun(
                Type::attr_set(AttrSetType {
                    attrs: {
                        let mut attrs = BTreeMap::new();
                        attrs.insert("y".into(), Type::any());
                        attrs
                    },
                    rest: Type::any().into(),
                }),
                Type::any(),
            ),
        )
    }

    #[test]
    fn test_integer() {
        success(
            "x: y: builtins.plus x y",
            env(),
            Type::fun(Type::integer(), Type::fun(Type::integer(), Type::integer())),
        );
        success(
            "{ x, y }: builtins.plus x y",
            env(),
            Type::fun(
                Type::attr_set(AttrSetType {
                    attrs: {
                        let mut attrs = BTreeMap::new();
                        attrs.insert("x".into(), Type::integer());
                        attrs.insert("y".into(), Type::integer());
                        attrs
                    },
                    rest: Type::any().into(),
                }),
                Type::integer(),
            ),
        )
    }

    #[test]
    fn test_fail_nonexistent_path() {
        success("builtins.nonexistent", env(), Type::none());
    }
}
