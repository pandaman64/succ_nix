// 型は単相だと思って全ての名前に対して単相の型を与える感じ
mod ast;
mod domain;
pub mod hir;
mod parser;
mod typing;

pub use parser::parse_term;
pub use typing::{success_type, Environment, Solution, Type};

#[cfg(test)]
mod test {
    use super::*;
    use crate::domain::AttrSetType;
    use std::collections::{BTreeMap, HashMap};

    fn env() -> (HashMap<String, hir::Id>, Environment) {
        let not_id = hir::Id::new();
        (
            vec![(String::from("not"), not_id)].into_iter().collect(),
            vec![(not_id, Type::fun(Type::boolean(), Type::boolean()))]
                .into_iter()
                .collect(),
        )
    }

    fn run(
        input: &str,
        (name_env, mut ty_env): (HashMap<String, hir::Id>, Environment),
    ) -> (bool, Type) {
        let ast = parse_term(input).unwrap();
        let terms = typed_arena::Arena::new();
        let alpha_env = name_env
            .into_iter()
            .map(|(v, id)| {
                let t = terms.alloc(hir::TermData::Var(id));
                (v, &*t)
            })
            .collect();
        let hir = hir::from_ast(&ast, &terms, &alpha_env);
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
        let result = sol.refine(&c);
        eprintln!("sol[{}]:", result);
        for (v, t) in sol.map.iter() {
            eprintln!("{} --> {}", v, t);
        }

        (result, sol.map_ty(&t))
    }

    fn success(input: &str, env: (HashMap<String, hir::Id>, Environment), expected: Type) {
        let (result, actual) = run(input, env);

        assert!(result);
        assert_eq!(actual, expected, "\n{} ≠ {}", actual, expected);
    }

    fn fail(input: &str, env: (HashMap<String, hir::Id>, Environment)) {
        let (result, _actual) = run(input, env);

        assert!(!result);
    }

    #[test]
    fn test_not_first() {
        success(
            "x: y: not x",
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
        fail("not (x: x)", env())
    }

    #[test]
    fn test_branch() {
        success(
            "x: if x then not x else x",
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
            "let f = x: if x then x else f (not x); in f",
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
}
