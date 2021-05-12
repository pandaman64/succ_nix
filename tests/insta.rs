use std::fs;

#[test]
fn test_insta() {
    tracing_subscriber::fmt::init();

    insta::glob!("corpus/*.nix", |path| {
        let input = fs::read_to_string(path).unwrap();
        let (ty, sol, sink) = succ_nix::run(&input, 65536);

        if !sink.is_error() {
            insta::assert_snapshot!(sol.map_ty(&ty).to_string());
        }
    });
}
