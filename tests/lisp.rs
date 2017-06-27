#[macro_use]
extern crate parsers;

use parsers::cfg::CFG;
use parsers::lr0::LR0;

#[test]
#[should_panic]
fn lisp() {
    // Lisps classical grammars are not parsable with lr0 :(
    let vn = set!("S", "L", "A", "M");
    let vt = set!("(", ")", "a");
    let s = "S";
    let p = vec![("S", vec!["A"]),
                 ("S", vec!["L"]),

                 ("L", vec!["(", ")"]),
                 ("L", vec!["(", "M", ")"]),

                 ("M", vec!["S"]),
                 ("M", vec!["S", "M"]),

                 ("A", vec!["a"])];

    let g = CFG::new(vn, vt, p, s);

    let mut parser = LR0::new(g);

    assert!(parser.parse("a"));
}
