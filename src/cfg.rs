use std::collections::{BTreeSet, BTreeMap};
//use std::fmt;

pub type Terminal = char;
pub type TerminalSet = BTreeSet<Terminal>;
// TODO make nonterminal a string, this will require some changes
// with terminal and deriviation
// TODO: reconsider this since we can create a map from chars to proper names
pub type NonTerminal = char;
pub type NonTerminalSet = BTreeSet<NonTerminal>;

pub type Derivation = Vec<char>;
pub type Production = (NonTerminal, Derivation);
pub type Productions = Vec<Production>;

pub type ProductionIndex = usize;
pub type ProductionMap = BTreeMap<NonTerminal, Vec<ProductionIndex>>;

#[derive(Clone, Debug)]
pub struct CFG {
    pub vn: NonTerminalSet,
    pub vt: TerminalSet,
    pub prod_map: ProductionMap,
    pub prod_vec: Productions,
    pub s: NonTerminal,
}


impl CFG {
    pub fn new(vn: NonTerminalSet, vt: TerminalSet, prod_vec: Productions, s: NonTerminal) -> CFG {
        let mut prod_map: ProductionMap = BTreeMap::new();

        assert!(vn.is_disjoint(&vt), "VN and VT must be disjoint.\nVN: {:?} \nVT: {:?}", vn, vt);

        let len = prod_vec.len();

        for i in 0..len {
            let (nt, ref derivation) = prod_vec[i];
            assert!(vn.contains(&nt), "NonTerminal in production rule does not belong to VN {:?} -> {:?} \n {:?}", nt, derivation, vn);
            for c in derivation {
                assert!(vn.contains(&c) || vt.contains(&c), "Char in derivation {:?} -> {:?} does not belong to VN or VT {:?}", nt, derivation, c);
            }

            prod_map.entry(nt).or_insert(vec!()).push( i );
        }

        CFG {
            vn: vn,
            vt: vt,
            prod_map: prod_map,
            prod_vec: prod_vec,
            s: s,
        }
    }

    pub fn get_v(&self) -> BTreeSet<char> {
        self.vt.union(&self.vn).cloned().collect()
    }

    pub fn is_terminal(&self, x: &char) -> bool {
        self.vt.contains(x)
    }

    pub fn is_nonterminal(&self, x: &char) -> bool {
        self.vn.contains(x)
    }
}





// TODO
//impl fmt::Display for CFG {
    //fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        //write!(f, "\n\n").unwrap();
        //write!(f, "Context Free Grammar \n").unwrap();
        //write!(f, "==================== \n").unwrap();
        //write!(f, "VN: {:?} \n", self.vn).unwrap();
        //write!(f, "VT: {:?} \n", self.vt).unwrap();
        //write!(f, "S: {:?} \n", self.s).unwrap();
        //write!(f, "Productions: \n").unwrap();
        //for (nt, dervec) in &self.p {
            //for der in dervec {
                //let der_string = derivation_to_string(der);
                //write!(f, "{:?} -> {:?} \n", nt, der_string).unwrap();
            //}
        //}

        //write!(f, "\n")
    //}
//}





//fn derivation_to_string(der: &Derivation) -> String {
    //let mut der_string = String::new();
    //for e in der {
        //match *e {
            //V::T(t) => der_string.push(t),
            //V::N(nt) => der_string.push(nt),
            //_ => {},
        //}
    //}

    //der_string
//}


#[cfg(test)]
mod tests {
    #[test]
    fn cfg_new_test() {
        use super::{CFG, NonTerminalSet, TerminalSet, NonTerminal, Productions};

        let vn: NonTerminalSet = set!('S');
        let vt: TerminalSet = set!('a', '(', ')');
        let s: NonTerminal = 'S';
        let p: Productions = vec![
            ('S', vec!['(', 'S', ')'] ),
            ('S', vec!['a'] )
        ];

        let g = CFG::new(vn, vt, p, s);
        println!("Resulted grammar {:?}", g);
    }
}













        //let q0 = {
            //use std::collections::{BTreeSet};
            //let mut aux = BTreeSet::new();
            //aux.insert((0, 0));
            //aux.insert((1, 0));
            //aux.insert((2, 0));
            //aux
        //};


        //let q1 = {
            //use std::collections::{BTreeSet};
            //let mut aux = BTreeSet::new();
            //aux.insert((0, 1));
            //aux
        //};

        //let q2 = {
            //use std::collections::{BTreeSet};
            //let mut aux = BTreeSet::new();
            //aux.insert((1, 0));
            //aux.insert((1, 1));
            //aux.insert((2, 0));
            //aux
        //};

        //let q3 = {
            //use std::collections::{BTreeSet};
            //let mut aux = BTreeSet::new();
            //aux.insert((2, 1));
            //aux
        //};

        //let q4 = {
            //use std::collections::{BTreeSet};
            //let mut aux = BTreeSet::new();
            //aux.insert((1, 2));
            //aux
        //};

        //let q5 = {
            //use std::collections::{BTreeSet};
            //let mut aux = BTreeSet::new();
            //aux.insert((1, 3));
            //aux
        //};
