#[macro_use] extern crate parsers;

use parsers::cfg::CFG;
use parsers::lr0::LR0;

#[test]
fn expressions_grammar() {
    let vn = set!('E', 'T', 'F');
    let vt = set!('+', '*', '(', ')', 'i');
    let s = 'E';
    let p = vec![
        ('E', vec!['E', '+', 'T']),
        ('E', vec!['T']),
        ('T', vec!['T', '*', 'F']),
        ('T', vec!['F']),
        ('F', vec!['(', 'E', ')']),
        ('F', vec!['i']),
    ];

    let g = CFG::new(vn, vt, p, s);

    let mut parser = LR0::new(g);

    assert!(parser.parse("i*i".to_string()));
    assert!(parser.parse("(i*i)".to_string()));

    assert!(parser.parse("i+i".to_string()));
    assert!(parser.parse("(i+i)".to_string()));

    assert!(parser.parse("i*i+i".to_string()));
    assert!(parser.parse("(i*i)+i".to_string()));
    assert!(parser.parse("i*(i+i)".to_string()));
    assert!(parser.parse("(i*i+i)".to_string()));


    assert!(!parser.parse("i*".to_string()));
    assert!(!parser.parse("iiiii".to_string()));
    assert!(!parser.parse("i*****".to_string()));
}

