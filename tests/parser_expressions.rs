#[macro_use] extern crate parsers;

use parsers::cfg::CFG;
use parsers::lr0::{LR0};

// this grammar is slr0 after all :(
#[test]
#[should_panic]
fn expressions_grammar() {
    let vn = set!("E", "T", "F");
    let vt = set!("+", "*", "(", ")", "i");
    let s = "E";
    let p = vec![
        ("E", vec!["E", "+", "T"]),
        ("E", vec!["T"]),
        ("T", vec!["T", "*", "F"]),
        ("T", vec!["F"]),
        ("F", vec!["(", "E", ")"]),
        ("F", vec!["i"]),
    ];

    let g = CFG::new(vn, vt, p, s);

    let mut parser = LR0::new(g);

    assert!(parser.parse( "i*i" ));
    assert!(parser.parse( "(i*i)" ));

    assert!(parser.parse( "i+i" ));
    assert!(parser.parse( "(i+i)" ));

    assert!(parser.parse( "i*i+i" ));
    assert!(parser.parse( "(i*i)+i" ));
    assert!(parser.parse( "i*(i+i)" ));
    assert!(parser.parse( "(i*i+i)" ));


    assert!(!parser.parse( "i*" ));
    assert!(!parser.parse( "iiiii" ));
    assert!(!parser.parse( "i*****" ));
}

