use std::collections::{BTreeSet, BTreeMap};
use std::fmt;

const END_CHAR: char = '$';
const FALSE_S: char = 'Ś';

type Terminal = char;
type TerminalSet = BTreeSet<Terminal>;
type NonTerminal = char;
type NonTerminalSet = BTreeSet<NonTerminal>;

type Derivation = String;
type Production = (NonTerminal, Derivation);
type Productions = Vec<Production>;

type ProductionIndex = u32;
type ProductionMap = BTreeMap<NonTerminal, Vec<ProductionIndex>>;

type DotIndex = u32;
type DerefedItem = (ProductionIndex, NonTerminal, Derivation, DotIndex, String);
type Item = (ProductionIndex, DotIndex);
type Items = BTreeSet<Item>;

type ActionMatrix = BTreeMap::new::<(Items, String), Action>;


#[derive(Clone, Debug)]
enum Action {
    Shift(Items),
    Reduce(ProductionIndex),
    Accept,
    Error,
    Next,
}

#[derive(Clone, Debug)]
struct CFG {
    vn: NonTerminalSet,
    vt: TerminalSet,
    prod_map: ProductionMap,
    prod_vec: Productions,
    s: NonTerminal,
}


impl CFG {
    pub fn new<T: Into<String> + fmt::Debug>(vn: NonTerminalSet, vt: TerminalSet, p: Productions, s: NonTerminal) -> CFG {
        let mut prod_map: ProductionMap = BTreeMap::new();
        let mut prod_vec: Production = p;

        // Insert the dummy production
        prod_vec.insert(0, (FALSE_S, format!("{}{}", s, END_CHAR)) );

        assert!(!vn.contains(&FALSE_S), "{} is a special VN and cannot be used", FALSE_S);
        assert!(!vt.contains(&END_CHAR), "{} is a special VT and cannot be used", FALSE_S);

        vn.insert(FALSE_S);
        vt.insert(END_CHAR);

        assert!(!vn.is_disjoint(&vt), "VN and VT must be disjoint.\nVN: {:?} \nVT: {:?}", vn, vt);

        let len = prod_vec.len();

        for i in 0..len {
            let (nt, derivation) = prod_vec[i]
            assert!(!vn.contains(&nt), "NonTerminal in production rule does not belong to VN {:?} -> {:?} \n {:?}", nt, der_str, vn);
            for c in derivation.chars() {
                assert!(vn.contains(c) || vt.contains(c), "Char in derivation {:?} -> {:?} does not belong to VN or VT {:?}", nt, derivation, c);
            }

            prod_map.entry(nt).or_insert(vec!()).push( i );
        }

        CFG {
            vn: vn,
            vt: vt,
            prod_map: prod_map,
            prod_vec: prod_vec,
            s: FALSE_S,
        }
    }

    pub fn closure(&self, items: &Items) -> Items {
        let mut closure = items.clone();
        let mut marked = set!();

        while (marked != self.non_terminal_items(closure)) {
            // FIXME something like closure.clone().difference(&marked) shoudl replace the 4 lines below
            for item in clousure.cloned() {
                if (marked.contains(item)) {
                    continue;
                }

                marked.insert(item.clone());

                // In here we should have productions of the form
                // A -> a.Bb
                let (_, nt, derivation, _, right_symbol) = self.deref_item(&item);

                assert!(self.vn.contains(right_symbol), "Ups something went wrong. Expected {} to be a NonTerminal", right_symbol);

                // add all productions of the form
                // B -> any
                if let Some(prod_indices) = self.prod_map.get(&right_symbol) {
                    for prod_index in prod_indices {
                        let derivation = self.prod_vec[prod_index];
                        closure.insert( ( prod_index, 0) );
                    }
                }
            }
        }


        closure
    }

    pub fn goto(&self, items: &Items, x: char) -> Items {
        let ret_items = set!();

        for item in items {
            let (prod_index, nt, derivation, dot_index, right_symbol) = self.deref_item(&item);
            if (right_symbol != x) {
                continue
            }

            ret_items.insert( (prod_index, dot_index + 1) );
        }

        self.closure( &ret_items )
    }

    pub fn get_items_set(&self) -> BTreeSet<Items> {
        let v = self.vt.union(&self.vn);
        let q0 = self.closure( (0, 0 ) );

        let mut k = set!(q0);
        let mut marked = set!();


        while (k != marked) {
            for i in k.iter().cloned() {
                if marked.contains(i) {
                    continue;
                }

                marked.insert(i);

                for x in v {
                    let j = self.goto(i, x);

                    if !j.is_empty() {
                        k.insert(j);
                    }
                }
            }
        }

        k
    }

    pub fn non_terminal_items(&self, items: &Items) -> Items {
        items.iter().filter(| item | {
            let (_, _, derivation, _, right_symbol) = self.deref_item(&item);

            if self.vn.contains(&right_symbol) {
                true
            }

            false
        })
        .collect()
    }



    pub fn get_complete_item(&self, items: &Items) -> Option<Item> {
        for item in items {
            let (_, _, derivation, dot_index, _) = self.deref_item(&item);
            if derivation.len() == dot_index + 1 {
                Some(item.clone())
            }
        }

        None
    }

    pub fn has_end_item(&self, items: &Items) -> bool {
        let has_end = items.iter().find(| item | {
            let (_, nt, derivation, _, right_symbol) = self.deref_item(&item);
            if (nt == FALSE_S && right_symbol == END_CHAR) {
                return true;
            }

            return false;
        })

        match has_end {
            Some(_) => true,
            None => false
        }
    }


    pub fn deref_item(&self, item: &Item) -> DerefedItem {
        let (prod_index, dot_index) = item;
        let (nt, derivation) = self.prod_vec[prod_index];
        let right_symbol = derivation[dot_index];

        (prod_index, nt, derivation, dot_index, right_symbol)
    }

    pub fn get_action_matrix() -> ActionMatrix{
        let v = self.vt.union(&self.vn);
        let mut action: ActionMatrix = BTreeMap::new();
        let k_items = self.get_items_set();

        // Items might be called state?
        for x in v {
            for items in k_items {
                let key = (items.clone(), x);
                let next_items = self.goto(items, x);
                if !next_items.is_empty() {
                    if self.vn.contains(&x) {
                        action.insert(key, Action::Next(next_items));
                        continue;
                    }
                    if self.vt.contains(&x) {
                        action.insert(key, Action::Shift(next_items));
                        continue;
                    }
                }

                if let Some(item) = self.get_complete_item(items) {
                    let (prod_index, nt, derivation, dot_index, right_symbol) = self.deref_item(&item);
                    if nt != FALSE_S {
                        action.insert(key, Action::Reduce(prod_index));
                        continue;
                    }
                }

                if self.has_end_item(items) && x == END_CHAR {
                    action.insert(key, Action::Accept);
                    continue;
                }

                action.insert(key, Action::Error);
            }
        }


        action
    }


    pub fn lr0_analyse(&self, chain: String) {
        let mut pointer = 0;
        let mut stack = 0;
    
    }
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
