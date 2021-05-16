// 型は単相だと思って全ての名前に対して単相の型を与える感じ
pub mod builtins;
mod context;
mod domain;
pub mod hir;
mod span;
mod typing;

use context::Context;
use typing::TypeErrorSink;
pub use typing::{success_type, Environment, Solution, Type};

fn report(filename: &str, input: &str, sink: &TypeErrorSink) {
    use codespan_reporting::diagnostic::{Diagnostic, Label};
    use codespan_reporting::files::SimpleFile;
    use codespan_reporting::term::{
        emit,
        termcolor::{ColorChoice, StandardStream},
        Config,
    };

    let file = SimpleFile::new(filename, input);
    let writer = StandardStream::stderr(ColorChoice::Auto);
    let mut writer = writer.lock();
    let config = Config::default();

    for error in sink.errors() {
        use typing::TypeErrorKind::*;

        let diagnostic = match &error.kind {
            Subset(smaller, larger) => Diagnostic::error()
                .with_message(format!(
                    "subset constraint `{} ⊂ {}` cannot be met",
                    smaller, larger
                ))
                .with_labels(vec![Label::primary((), error.span.start..error.span.end)
                    .with_message("the constraint comes from here")]),
            Disj => Diagnostic::error()
                .with_message("no clauses in disjunction cannot be satisfied")
                .with_labels(vec![Label::primary((), error.span.start..error.span.end)
                    .with_message("the constraint comes from here")]),
            Limit => Diagnostic::error()
                .with_message("limit reached while inferencing types")
                .with_labels(vec![Label::primary((), error.span.start..error.span.end)
                    .with_message("while inferencing the types for this term")]),
        };

        emit(&mut writer, &config, &file, &diagnostic).unwrap();
    }
}

pub fn run(filename: &str, input: &str, limit: usize) -> (Type, Solution, TypeErrorSink) {
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

    report(filename, input, &sink);

    (t, sol, sink)
}

#[cfg(test)]
mod test {
    use super::*;
    use domain::AttrSetType;
    use std::collections::BTreeMap;

    fn success(filename: &str, input: &str, expected: Type) {
        let _ = tracing_subscriber::fmt::try_init();

        let (ty, sol, sink) = run(filename, input, 1000);
        let actual = sol.map_ty(&ty);

        assert!(!sink.is_error());
        assert_eq!(actual, expected, "\n{} ≠ {}", actual, expected);
    }

    fn fail(filename: &str, input: &str) {
        let _ = tracing_subscriber::fmt::try_init();

        let (_ty, _sol, sink) = run(filename, input, 100);

        assert!(sink.is_error());
    }

    #[test]
    fn test_not_first() {
        success(
            "test_not_first.nix",
            "x: y: ! x",
            Type::fun(Type::boolean(), Type::fun(Type::any(), Type::boolean())),
        );
    }

    #[test]
    fn test_id() {
        success("test_id.nix", "x: x", Type::fun(Type::any(), Type::any()));
    }

    #[test]
    fn test_xx() {
        success(
            "test_xx.nix",
            "x: x x",
            Type::fun(Type::fun(Type::any(), Type::any()), Type::any()),
        );
    }

    #[test]
    fn test_not_id() {
        fail("test_not_id.nix", "! (x: x)")
    }

    #[test]
    fn test_branch() {
        success(
            "test_branch.nix",
            "x: if x then ! x else x",
            Type::fun(Type::boolean(), Type::boolean()),
        );
    }

    #[test]
    fn test_branch_half_succ() {
        // let F be this lambda abstaction.
        // If (F x) terminates, x = ff and F x = ff, which means F is typable with ff -> ff.
        success(
            "test_branch_half_succ.nix",
            "x: if x then x true else x",
            Type::fun(Type::ff(), Type::ff()),
        );
    }

    #[test]
    fn test_let() {
        success(
            "test_let.nix",
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
        fail("test_let_recursive_xx.nix", "let x = x x; in x");

        // from TAPL
        // success(
        //     "let hungry = x: hungry; in hungry",
        //     Type::fun(Type::any(), Type::any()),
        // );
        fail("test_hungry.nix", "let hungry = x: hungry; in hungry");
    }

    #[test]
    fn test_let_fail() {
        fail("test_let_fail.nix", "let x = true; y = z: z x; in y x")
    }

    #[test]
    fn test_attr_set() {
        success(
            "test_attr_set_x.nix",
            "{ x = true; y = z: z; }.x",
            Type::tt(),
        );
        success(
            "test_attr_set_y.nix",
            "{ x = true; y = z: z; }.y",
            Type::fun(Type::any(), Type::any()),
        );
    }

    #[test]
    fn test_rec() {
        success(
            "test_rec.nix",
            "let f = x: if x then x else f (! x); in f",
            Type::fun(Type::boolean(), Type::any()),
        );
    }

    #[test]
    fn test_fix() {
        success(
            "test_nix.nix",
            "let y = f: f (y f); in y",
            Type::fun(Type::fun(Type::any(), Type::any()), Type::any()),
        );
    }

    #[test]
    fn test_attr_set_arg() {
        success(
            "test_attr_set_arg.nix",
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
            "test_attr_set_arg_overapprox.nix",
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
            "test_integer.nix",
            "x: y: builtins.add x y",
            Type::fun(Type::integer(), Type::fun(Type::integer(), Type::integer())),
        );
        success(
            "test_integer_arg_pat.nix",
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
        success("test_nonexistent.nix", "builtins.nonexistent", Type::none());
    }

    #[test]
    fn test_attr_set_rec() {
        success(
            "test_attr_set_rec.nix",
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
        success("test_assert_tt.nix", "assert true; 100", Type::integer());
        success(
            "test_assert_tt_propaget.nix",
            "x: assert x; x",
            Type::fun(Type::tt(), Type::tt()),
        );
        success(
            "test_assert_not.nix",
            "x: assert !x; x",
            Type::fun(Type::boolean(), Type::boolean()),
        );
    }

    #[test]
    fn test_with() {
        success(
            "test_with.nix",
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
            "test_with_precedence.nix",
            "let a = ''foo''; b = { a = 100; }; in with b; a",
            Type::string(),
        );

        // If you run this program, you'll get "foo" because `with` resolves from the nearest.
        // However, as our desugaring nondeterministically choose the namespace from which we
        // select, we get a union type.
        success(
            "test_with_multiple.nix",
            "with { a = 100; }; with { a = ''foo''; }; a",
            Type::string().sup(&Type::integer()),
        );
    }

    #[test]
    fn test_dynamic_attr() {
        // Currently, we do not constrain the result type enough,
        // so we get a looser success type.
        success(
            "test_dynamic_attr.nix",
            "{ a = 100; }.${''x''}",
            Type::any(),
        );
    }

    #[test]
    fn test_assert_msg() {
        success(
            "test_assert_msg.nix",
            "pred: if pred then true else (x: x) pred",
            Type::fun(Type::boolean(), Type::any()),
        );
    }

    #[test]
    fn test_app_information_loss() {
        success("test_app_information_loss.nix", "(x: x) true", Type::any());
    }
}
