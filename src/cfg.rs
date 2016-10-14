use std::collections::{BTreeSet, BTreeMap};
use std::fmt;


type Terminal = char;
type TerminalSet = BTreeSet<Terminal>;
type NonTerminal = char;
type NonTerminalSet = BTreeSet<NonTerminal>;

#[derive(Clone, Debug)]
enum V {
    T(Terminal),
    N(NonTerminal),
    Lambda,
}

type Derivation = Vec<V>;
type DerivationVec = Vec<Derivation>;

type Production<T> = (NonTerminal, T);
type Productions<T> = Vec<Production<T>>;
type ProductionsMap = BTreeMap<NonTerminal, DerivationVec>;

type DotIndex = u32;
// TODO: use this knew, less naive, data structure instead
// TODO: create print traits for Item
type Item = (NonTerminal, Derivation, DotIndex);
type Items = BTreeSet<Item>;

#[derive(Clone, Debug)]
struct CFG {
    vn: NonTerminalSet,
    vt: TerminalSet,
    p: ProductionsMap,
    s: NonTerminal,
}


impl CFG {
    pub fn new<T: Into<String> + fmt::Debug>(vn: NonTerminalSet, vt: TerminalSet, p: Productions<T>, s: NonTerminal) -> CFG {
        let mut p_map: ProductionsMap = BTreeMap::new();

        if !vn.is_disjoint(&vt) {
            panic!("VN and VT must be disjoint.\nVN: {:?} \nVT: {:?}", vn, vt);
        }

        for (nt, der_str) in p {
            if !vn.contains(&nt) {
                panic!("NonTerminal in production rule does not belong to VN {:?} -> {:?} \n {:?}", nt, der_str, vn);
            }

            let dervec = p_map.entry(nt).or_insert(vec!());

            let mut der: Derivation = vec!();

            let der_string = der_str.into();

            if der_string.len() == 0 {
                der.push(V::Lambda);
            }

            for c in der_string.chars() {
                if vn.contains(&c) {
                    der.push( V::N(c.clone()) );
                    continue
                }

                if vt.contains(&c) {
                    der.push( V::T(c.clone()) );
                    continue
                }

                panic!("Char in derivation {:?} -> {:?} does not belong to VN or VT {:?}", nt, der_string, c);
            }

            dervec.push(der);
        }

        CFG {
            vn: vn,
            vt: vt,
            p: p_map,
            s: s,
        }
    }

    //pub fn get_nt_derivations(&self, nt: &NonTerminal) -> DerivationVec {
        ////TODO: optimize the case where the nt is not found
        //self.p.get(nt).unwrap().clone()
    //}
    
    pub fn get_items(&self) -> Items {
        let mut items = BTresSet::new();
        for (nt, derVec) in self.p.iter() {
            for der in derVec {
                let len = der.len();
                for index in 0..len {
                    items.insert( ( nt.clone(), item, index) );
                }
            }
        }

        items
    }

    // TODO a method to iterate over productions easily
    // maybe implement the Iter trait?
    //
    //
    //
    //
    pub fn closure(&self, items: &Items) -> Items {
        let mut closure = items.clone();
        let mut marked = BTreeSet::new();


        while (marked != non_terminal_items(closure)) {
            for item in clousure.cloned() {
                if (marked.contains(item)) {
                    continue;
                }

                marked.insert(item.clone());

                // In here we should have productions of the form
                // A -> a.Bb
                let (_, derivation, dot_index) = item;
                let nt = derivation[dot_index + 1];

                // TODO: use something like assert here
                if (!self.vn.contains(nt)) {
                    panic!("Ups something went wrong. Expected {} to be a NonTerminal", nt);
                }


                // add all productions of the form
                // B -> any
                let dervec = self.p.get(&nt);
                for der in dervec {
                    closure.insert(( nt.clone(), der, 0));
                }
            }
        }



        closure
    }

    pub fn goto(&self, items: &Items, x: V) -> Items {
        let ret_items = BTreeSet::new();

        for item in items {
            let (nt, derivation, dot_index) = item;
            let right_symbol = derivation[dot_index + 1];
            if (right_symbol != x) {
                continue;
            }

            ret_items.insert( (nt, derivation, dot_index + 1) );
        }

        self.closure( &ret_items )
    }

    pub fn get_automata_items(&self) -> Items {
    
    }

    pub fn to_lr0_matrix() {
    
    }
}



pub fn non_terminal_items(items: &Items) -> Items {
    items.iter().filter(| item | {
        let (_, derivation, dot_index) = item;

        match derivation[dot_index + 1] {
            V::N(_) => return true;
            _ => return false
        }
    })
}


// TODO this should probably go directly to the shitf-reduce table instead of ging to to the afd
//pub fn to_fsa() {}


impl fmt::Display for CFG {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\n\n").unwrap();
        write!(f, "Context Free Grammar \n").unwrap();
        write!(f, "==================== \n").unwrap();
        write!(f, "VN: {:?} \n", self.vn).unwrap();
        write!(f, "VT: {:?} \n", self.vt).unwrap();
        write!(f, "S: {:?} \n", self.s).unwrap();
        write!(f, "Productions: \n").unwrap();
        for (nt, dervec) in &self.p {
            for der in dervec {
                let der_string = derivation_to_string(der);
                write!(f, "{:?} -> {:?} \n", nt, der_string).unwrap();
            }
        }

        write!(f, "\n")
    }
}



impl V {
    fn to_char(&self) -> char {
        match *self {
            V::T(t) => t,
            V::N(nt) => nt,
            // TODO: proper lambda symbol
            V::Lambda =>  '&',
        }
    }

    fn to_string(&self) -> String {
        match *self {
            V::T(t) => format!("T('{}')", t),
            V::N(nt) => format!("N('{}')", nt),
            V::Lambda =>  "Lambda".to_string(),
        }
    }
}


fn derivation_to_string(der: &Derivation) -> String {
    let mut der_string = String::new();
    for e in der {
        match *e {
            V::T(t) => der_string.push(t),
            V::N(nt) => der_string.push(nt),
            _ => {},
        }
    }

    der_string
}


#[cfg(test)]
mod tests {
    #[test]
    fn cfg_new_test() {
        use super::{CFG, NonTerminalSet, TerminalSet, NonTerminal, Productions};

        let vn: NonTerminalSet = set!('S');
        let vt: TerminalSet = set!('a', '(', ')');
        let s: NonTerminal = 'S';
        let p: Productions<&'static str> = vec!(
            ('S', "(S)" ),
            ('S', "a" )
        );

        let g = CFG::new(vn, vt, p, s);
        println!("Resulted grammar {}", g);
    }
}
