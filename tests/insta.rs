use std::fs;

#[test]
// tooooo slow :(
#[ignore]
fn test_insta() {
    insta::glob!("corpus/*.nix", |path| {
        let input = fs::read_to_string(path).unwrap();
        let (ty, sol, sink) = succ_nix::run(&input);

        assert!(!sink.is_error(), "failed to typeck: {}", path.display());
        insta::assert_snapshot!(sol.map_ty(&ty).to_string());
    });
}
