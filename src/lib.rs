// 型は単相だと思って全ての名前に対して単相の型を与える感じ
pub mod builtins;
mod context;
mod domain;
pub mod hir;
mod typing;

use context::Context;
use typing::TypeErrorSink;
pub use typing::{success_type, Environment, Solution, Type};

pub fn run(input: &str, limit: usize) -> (Type, Solution, TypeErrorSink) {
    let ctx = Context::new();
    let (alpha_env, mut ty_env) = builtins::env(&ctx);

    let ast = rnix::parse(input).node();
    eprintln!("ast = {:#?}", ast);
    let hir = hir::from_rnix(ast, &ctx, &alpha_env, &[]);
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
    let mut sink = TypeErrorSink::default();
    let sol = Solution::solve(&c, &mut sink, limit);
    eprintln!("sol:{}", !sink.is_error());
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
        let _ = tracing_subscriber::fmt::try_init();

        let (ty, sol, sink) = run(input, 1000);
        let actual = sol.map_ty(&ty);

        assert!(!sink.is_error());
        assert_eq!(actual, expected, "\n{} ≠ {}", actual, expected);
    }

    fn fail(input: &str) {
        let _ = tracing_subscriber::fmt::try_init();

        let (_ty, _sol, sink) = run(input, 100);

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
    fn test_let_infinite_types() {
        // CR pandaman: 再帰型が体系に必要だと思います。
        // These tests fails because the current implementation cannot properly
        // infer recursive types and thus loop infinitely until the hard limit.

        // success("let x = x x; in x", Type::any());
        fail("let x = x x; in x");

        // from TAPL
        // success(
        //     "let hungry = x: hungry; in hungry",
        //     Type::fun(Type::any(), Type::any()),
        // );
        fail("let hungry = x: hungry; in hungry");
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
            Type::fun(Type::boolean(), Type::any()),
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

    #[test]
    fn test_with() {
        success(
            "with { a = 100; b.c = 200; }; b",
            Type::attr_set(AttrSetType {
                attrs: {
                    let mut attrs = BTreeMap::new();
                    attrs.insert("c".into(), Type::integer());
                    attrs
                },
                rest: Type::none().into(),
            }),
        );

        success(
            "let a = ''foo''; b = { a = 100; }; in with b; a",
            Type::string(),
        );

        // If you run this program, you'll get "foo" because `with` resolves from the nearest.
        // However, as our desugaring nondeterministically choose the namespace from which we
        // select, we get a union type.
        success(
            "with { a = 100; }; with { a = ''foo''; }; a",
            Type::string().sup(&Type::integer()),
        );
    }

    #[test]
    fn test_dynamic_attr() {
        // Currently, we do not constrain the result type enough,
        // so we get a looser success type.
        success("{ a = 100; }.${''x''}", Type::any());
    }

    #[test]
    fn test_assert_msg() {
        success(
            "pred: if pred then true else (x: x) pred",
            Type::fun(Type::boolean(), Type::any()),
        );
    }

    #[test]
    fn test_app_information_loss() {
        success("(x: x) true", Type::any());
    }
}
