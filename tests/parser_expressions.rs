#[macro_use] extern crate parsers;

use parsers::cfg::CFG;
use parsers::lr0::{LR0, Token};

#[test]
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

    assert!(parser.parse( Token::from_str("i*i")));
    assert!(parser.parse( Token::from_str("(i*i)")));

    assert!(parser.parse( Token::from_str("i+i")));
    assert!(parser.parse( Token::from_str("(i+i)")));

    assert!(parser.parse( Token::from_str("i*i+i")));
    assert!(parser.parse( Token::from_str("(i*i)+i")));
    assert!(parser.parse( Token::from_str("i*(i+i)")));
    assert!(parser.parse( Token::from_str("(i*i+i)")));


    assert!(!parser.parse( Token::from_str("i*")));
    assert!(!parser.parse( Token::from_str("iiiii")));
    assert!(!parser.parse( Token::from_str("i*****")));
}

