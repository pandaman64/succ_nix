// 型は単相だと思って全ての名前に対して単相の型を与える感じ
pub mod builtins;
mod context;
mod domain;
pub mod hir;
mod typing;

use context::Context;
use typing::TypeErrorSink;
pub use typing::{success_type, Environment, Solution, Type};

pub fn run(input: &str) -> (Type, Solution, TypeErrorSink) {
    let ctx = Context::new();
    let (alpha_env, mut ty_env) = builtins::env(&ctx);

    let ast = rnix::parse(input).node();
    eprintln!("ast = {:#?}", ast);
    let hir = hir::from_rnix(ast, &ctx, &alpha_env);
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

    (t, sol, sink)
}

#[cfg(test)]
mod test {
    use super::*;
    use domain::AttrSetType;
    use std::collections::BTreeMap;

    fn success(input: &str, expected: Type) {
        let (ty, sol, sink) = run(input);
        let actual = sol.map_ty(&ty);

        assert!(!sink.is_error());
        assert_eq!(actual, expected, "\n{} ≠ {}", actual, expected);
    }

    fn fail(input: &str) {
        let (_ty, _sol, sink) = run(input);

        assert!(sink.is_error());
    }

    #[test]
    fn test_not_first() {
        success(
            "x: y: ! x",
            Type::fun(Type::boolean(), Type::fun(Type::any(), Type::boolean())),
        );
    }

    #[test]
    fn test_id() {
        success("x: x", Type::fun(Type::any(), Type::any()));
    }

    #[test]
    fn test_xx() {
        success(
            "x: x x",
            Type::fun(Type::fun(Type::any(), Type::any()), Type::any()),
        );
    }

    #[test]
    fn test_not_id() {
        fail("! (x: x)")
    }

    #[test]
    fn test_branch() {
        success(
            "x: if x then ! x else x",
            Type::fun(Type::boolean(), Type::boolean()),
        );
    }

    #[test]
    fn test_branch_half_succ() {
        // let F be this lambda abstaction.
        // If (F x) terminates, x = ff and F x = ff, which means F is typable with ff -> ff.
        success(
            "x: if x then x true else x",
            Type::fun(Type::ff(), Type::ff()),
        );
    }

    #[test]
    fn test_let() {
        success(
            "let x = true; y = z: z x; in y",
            Type::fun(Type::fun(Type::any(), Type::any()), Type::any()),
        )
    }

    #[test]
    #[ignore]
    fn test_let_infinite_types() {
        // CR pandaman: 再帰型が体系に必要だと思います。
        // infinite loop. something is wrong
        success("let x = x x; in x", Type::any());
        // from TAPL
        success(
            "let hungry = x: hungry; in hungry",
            Type::fun(Type::any(), Type::any()),
        );
    }

    #[test]
    fn test_let_fail() {
        fail("let x = true; y = z: z x; in y x")
    }

    #[test]
    fn test_attr_set() {
        success("{ x = true; y = z: z; }.x", Type::tt());
        success(
            "{ x = true; y = z: z; }.y",
            Type::fun(Type::any(), Type::any()),
        );
    }

    #[test]
    fn test_rec() {
        success(
            "let f = x: if x then x else f (! x); in f",
            Type::fun(Type::boolean(), Type::tt()),
        );
    }

    #[test]
    fn test_fix() {
        success(
            "let y = f: f (y f); in y",
            Type::fun(Type::fun(Type::any(), Type::any()), Type::any()),
        );
    }

    #[test]
    fn test_attr_set_arg() {
        success(
            "x: x.y",
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
            "x: y: builtins.add x y",
            Type::fun(Type::integer(), Type::fun(Type::integer(), Type::integer())),
        );
        success(
            "{ x, y }: builtins.add x y",
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
        success("builtins.nonexistent", Type::none());
    }

    #[test]
    fn test_attr_set_rec() {
        success(
            "rec { x = 100; y = x; }",
            Type::attr_set(AttrSetType {
                attrs: {
                    let mut attrs = BTreeMap::new();
                    attrs.insert("x".into(), Type::integer());
                    attrs.insert("y".into(), Type::integer());
                    attrs
                },
                rest: Type::none().into(),
            }),
        );
    }

    #[test]
    fn test_assert() {
        success("assert true; 100", Type::integer());
        success("x: assert x; x", Type::fun(Type::tt(), Type::tt()));
        success(
            "x: assert !x; x",
            Type::fun(Type::boolean(), Type::boolean()),
        );
    }
}
