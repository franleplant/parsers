use std::collections::{BTreeSet, BTreeMap};
use std::fmt::{Debug};

pub type Symbol = String;
pub type Set<T> = BTreeSet<T>;

pub type Derivation = Vec<Symbol>;
pub type GenericDerivation<T> = Vec<T>;
pub type Production = (Symbol, Derivation);
pub type Productions = Vec<Production>;
pub type GenericProductions<T> = Vec<(T, GenericDerivation<T>)>;

pub type ProductionIndex = usize;
pub type ProductionMap = BTreeMap<Symbol, Vec<ProductionIndex>>;

#[derive(Clone, Debug)]
pub struct CFG {
    pub vn: Set<Symbol>,
    pub vt: Set<Symbol>,
    pub prod_map: ProductionMap,
    pub prod_vec: Productions,
    pub s: Symbol,
}


impl CFG {
    pub fn new<T: Into<String> + Debug + Ord + Clone>(vn: Set<T>, vt: Set<T>, prod_vec: GenericProductions<T>, s: T) -> CFG {
        let mut prod_map: ProductionMap = BTreeMap::new();
        let vn: Set<Symbol> = vn.iter().cloned().map(|el| el.into()).collect();
        let vt: Set<Symbol> = vt.iter().cloned().map(|el| el.into()).collect();
        let prod_vec: Productions = prod_vec.iter().cloned()
            .map(|(nt, der)| (nt.into(), der.iter().cloned().map(|el| el.into()).collect()) ).collect();


        assert!(vn.is_disjoint(&vt), "VN and VT must be disjoint.\nVN: {:?} \nVT: {:?}", vn, vt);

        let len = prod_vec.len();

        for i in 0..len {
            let (ref nt, ref derivation) = prod_vec[i];
            assert!(vn.contains(nt), "NonTerminal in production rule does not belong to VN {:?} -> {:?} \n {:?}", nt, derivation, vn);
            for c in derivation {
                assert!(vn.contains(c) || vt.contains(c), "Char in derivation {:?} -> {:?} does not belong to VN or VT {:?}", nt, derivation, c);
            }

            prod_map.entry(nt.clone()).or_insert(vec!()).push( i );
        }

        CFG {
            vn: vn,
            vt: vt,
            prod_map: prod_map,
            prod_vec: prod_vec,
            s: s.into(),
        }
    }

    pub fn get_v(&self) -> BTreeSet<Symbol> {
        self.vt.union(&self.vn).cloned().collect()
    }

    pub fn is_terminal(&self, x: &Symbol) -> bool {
        self.vt.contains(x)
    }

    pub fn is_nonterminal(&self, x: &Symbol) -> bool {
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


#[cfg(test)]
mod tests {
    #[test]
    fn cfg_new_test() {
        use super::{CFG};

        let vn = set!("S");
        let vt = set!("a", "(", ")");
        let s = "S";
        let p = vec![
            ("S", vec!["(", "S", ")"] ),
            ("S", vec!["a"] )
        ];

        let g = CFG::new(vn, vt, p, s);
        println!("Resulted grammar {:?}", g);
    }
}
