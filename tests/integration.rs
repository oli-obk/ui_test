#[test]
fn integrations() {
    run("basic");
}

fn run(name: &str) {
    let path = std::path::Path::new(file!()).parent().unwrap();

    let mut cmd = std::process::Command::new(std::env::var_os("CARGO").unwrap());
    cmd.arg("test");

    cmd.arg("--target-dir");
    cmd.arg(path.parent().unwrap().join("target"));

    cmd.arg("--manifest-path");
    cmd.arg(format!("{}/integrations/{name}/Cargo.toml", path.display()));

    assert!(cmd.status().unwrap().success());
}
