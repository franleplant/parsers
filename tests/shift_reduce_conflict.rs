#[macro_use] extern crate parsers;

use parsers::cfg::CFG;
use parsers::lr0::{LR0};

#[test]
#[should_panic]
fn shift_reduce_conflict() {
    let vn = set!("S", "E");
    let vt = set!("1");
    let s = "S";
    let p = vec![
        ("S", vec!["E"]),
        ("E", vec!["1", "E"]),
        ("E", vec!["1"]),
    ];

    let g = CFG::new(vn, vt, p, s);

    let mut parser = LR0::new(g);

    assert!(parser.parse( "1" ));
}

