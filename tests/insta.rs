use std::fs;

#[test]
fn test_insta() {
    insta::glob!("corpus/*.nix", |path| {
        let input = fs::read_to_string(path).unwrap();
        let (ty, sol, sink) = succ_nix::run(&input);

        if !sink.is_error() {
            insta::assert_snapshot!(sol.map_ty(&ty).to_string());
        }
    });
}
