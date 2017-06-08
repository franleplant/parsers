#[macro_use]
extern crate parsers;

use parsers::cfg::CFG;
use parsers::lr0::LR0;

// Simple test to assert the lr0 impl can handle states with complex names
#[test]
fn lr0_test() {
    let vn = set!("START");
    let vt = set!("a");
    let s = "START";
    let p = vec![("START", vec!["START", "a"]), ("START", vec!["a"])];

    let g = CFG::new(vn, vt, p, s);

    let mut parser = LR0::new(g);

    assert!(parser.parse("a"));
}
