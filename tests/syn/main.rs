#![cfg(all(test, feature = "syn_tests"))]
#[macro_use]
extern crate lazy_static;

use std::{
    path::PathBuf,
    fs::write,
    process::Command,
};
use reqwest::get;
lazy_static! {
    static ref TARGET_DIR: PathBuf = PathBuf::from("tests/rust");
}
static CLONE_PATH: &str = "tests/clone.sh";
static SYN_SH: &str = "https://raw.githubusercontent.com/dtolnay/syn/master/tests/clone.sh";
fn setup() {
    if TARGET_DIR.exists() {
        return;
    }
    let clone_sh = get(SYN_SH)
                    .expect("Failed to get syn/tests/clone.sh")
                    .text()
                    .expect("Failed to get syn/tests/clone.sh request body");
    write(CLONE_PATH, clone_sh)
        .expect("Failed to write clone.sh");
    let chmod = Command::new("chmod").arg("+x").arg(CLONE_PATH).output().expect("failed to chmod");
    if !chmod.status.success() {
        panic!("chmod +x unsuccessful {:?}", String::from_utf8_lossy(&chmod.stderr));
    }
    let out = Command::new(CLONE_PATH)
                        .output()
                        .expect("failed to execute clone.sh");
    if !out.status.success() {
        panic!("clone.sh unsuccessful {:?}", 
            String::from_utf8_lossy(&out.stderr));
    }
}
#[test]
fn test_all_syn() {
    setup();
    
}