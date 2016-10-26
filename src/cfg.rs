use std::collections::{BTreeSet, BTreeMap};
use std::fmt;

//TODO
// - decouple CFG from the parser itself
// - improve parser data structures and helper methods such as stack
//      and items, we need a proper interface for derefing and printing
//      and manipulating them in general
// - improve performance by only calculating the matrix once
// - improve performance by only calculating goto once?
// - more tests with different grammars
//
//
//

const END_CHAR: char = '$';
const FALSE_S: char = 'Ś';

type Terminal = char;
type TerminalSet = BTreeSet<Terminal>;
// TODO make nonterminal a string, this will require some changes
// with terminal and deriviation
// TODO: reconsider this since we can create a map from chars to proper names
type NonTerminal = char;
type NonTerminalSet = BTreeSet<NonTerminal>;

type Derivation = Vec<char>;
type Production = (NonTerminal, Derivation);
type Productions = Vec<Production>;

type ProductionIndex = usize;
type ProductionMap = BTreeMap<NonTerminal, Vec<ProductionIndex>>;

type DotIndex = usize;
type DerefedItem = (ProductionIndex, NonTerminal, Derivation, DotIndex, Option<char>);
type Item = (ProductionIndex, DotIndex);
type Items = BTreeSet<Item>;

type ActionMatrix = BTreeMap<(Items, char), Action>;

#[derive(Clone, Debug, PartialEq, Eq)]
enum StackSymbol {
    State(Items),
    VN(NonTerminal),
    VT(Terminal),
}

type Stack = Vec<StackSymbol>;


#[derive(Clone, Debug, PartialEq, Eq)]
enum Action {
    Shift(Items),
    Reduce(ProductionIndex),
    Accept,
    Error,
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
    pub fn new(mut vn: NonTerminalSet, mut vt: TerminalSet, p: Productions, s: NonTerminal) -> CFG {
        let mut prod_map: ProductionMap = BTreeMap::new();
        let mut prod_vec: Productions = p;

        // Insert the dummy production
        prod_vec.insert(0, (FALSE_S, vec![s, END_CHAR]) );

        assert!(!vn.contains(&FALSE_S), "{} is a special VN and cannot be used", FALSE_S);
        assert!(!vt.contains(&END_CHAR), "{} is a special VT and cannot be used", FALSE_S);

        vn.insert(FALSE_S);
        vt.insert(END_CHAR);

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
            s: FALSE_S,
        }
    }

    pub fn closure(&self, items: &Items) -> Items {
        let mut closure = items.clone();
        let mut marked = set!();

        while marked != self.non_terminal_items(&closure) {
            // FIXME something like closure.clone().difference(&marked) shoudl replace the 4 lines below
            for item in closure.clone().iter() {
                if marked.contains(&item) {
                    continue;
                }

                marked.insert(item.clone());

                // In here we should have productions of the form
                // A -> a.Bb
                let (_, _, _, _, right_symbol) = self.deref_item(&item);

                if let Some(right_symbol) = right_symbol {
                    assert!(self.vn.contains(&right_symbol), "Ups something went wrong. Expected {} to be a NonTerminal", right_symbol);

                    // add all productions of the form
                    // B -> any
                    if let Some(prod_indices) = self.prod_map.get(&right_symbol) {
                        for prod_index in prod_indices {
                            closure.insert( ( *prod_index, 0) );
                        }
                    }
                }
            }
        }


        closure
    }

    pub fn goto(&self, items: &Items, x: char) -> Items {
        let mut ret_items = set!();

        for item in items {
            let (prod_index, _, derivation, dot_index, right_symbol) = self.deref_item(&item);
            if let Some(right_symbol) = right_symbol {
                if right_symbol != x {
                    continue
                }

                let next_dot_index = dot_index + 1;
                if next_dot_index <= derivation.len() {
                    ret_items.insert( (prod_index, next_dot_index) );
                }
            }
        }

        self.closure( &ret_items )
    }

    pub fn get_q0(&self) -> Items {
        let first_item = (0, 0);
        let mut q0 = BTreeSet::new();
        q0.insert(first_item);
        let q0 = self.closure(&q0);

        q0
    }

    pub fn get_items_set(&self) -> BTreeSet<Items> {
        let v: BTreeSet<char> = self.vt.union(&self.vn).cloned().collect();
        let first_item = (0, 0);
        let mut q0 = BTreeSet::new();
        q0.insert(first_item);
        let q0 = self.closure(&q0);

        let mut k = set!(q0);
        let mut marked = set!();


        while k != marked {
            for i in k.clone() {
                if marked.contains(&i) {
                    continue;
                }

                marked.insert(i.clone());

                for x in v.iter().cloned() {
                    let j = self.goto(&i, x);

                    if !j.is_empty() {
                        k.insert(j);
                    }
                }
            }
        }

        k
    }

    pub fn non_terminal_items(&self, items: &Items) -> Items {
        items.iter().cloned().filter(| item | {
            let (_, _, _, _, right_symbol) = self.deref_item(&item);

            if let Some(right_symbol) = right_symbol {
                if self.vn.contains(&right_symbol) {
                    return true
                }
            }

            false
        })
        .collect()
    }



    pub fn get_complete_item(&self, items: &Items) -> Option<Item> {
        for item in items {
            let (_, _, derivation, dot_index, _) = self.deref_item(&item);
            if derivation.len() == dot_index {
                return Some(item.clone())
            }
        }

        None
    }

    pub fn has_end_item(&self, items: &Items) -> bool {
        let has_end = items.iter().find(| item | {
            let (_, nt, _, _, right_symbol) = self.deref_item(&item);
            if let Some(right_symbol) = right_symbol {
                if nt == FALSE_S && right_symbol == END_CHAR {
                    return true;
                }
            }

            return false;
        });

        match has_end {
            Some(_) => true,
            None => false
        }
    }


    pub fn deref_item(&self, item: &Item) -> DerefedItem {
        let &(prod_index, dot_index) = item;
        assert!(prod_index < self.prod_vec.len(), "prod index {} out of bound", prod_index);
        let (nt, ref derivation) = self.prod_vec[prod_index];
        assert!(dot_index <= derivation.len(), "dot_index {} out of bound in {:?}", dot_index, derivation);

        // TODO better way of marking the end of the chain
        let right_symbol = if dot_index == derivation.len() {
            None
        } else {
          Some(derivation[dot_index])
        };

        (prod_index, nt, derivation.clone(), dot_index, right_symbol)
    }

    // TODO we should have in account reduction redution or shift reduction
    // conflicts in this matrix as in lr1 g
    pub fn get_action_matrix(&self) -> ActionMatrix{
        let v = self.vt.union(&self.vn);
        let mut action: ActionMatrix = BTreeMap::new();
        let k_items = self.get_items_set();


        // Items might be called state?
        for x in v {
            for items in &k_items {
                let key = (items.clone(), *x);
                let old_value = action.get(&key).cloned();
                let mut value = Action::Error;

                let next_items = self.goto(&items, *x);

                // TODO this whole conditional is ugly AF
                //println!("LOCO {:?}, {}, {:?}", items, x, next_items);
                if self.has_end_item(&items) && *x == END_CHAR {
                    value = Action::Accept;
                } else if !next_items.is_empty() {
                    value = Action::Shift(next_items);
                } else if let Some(item) = self.get_complete_item(&items) {
                    let (prod_index, nt, _, _, _) = self.deref_item(&item);
                    if nt != FALSE_S  && !self.vn.contains(x) {
                        value = Action::Reduce(prod_index);
                    }
                }



                // TODO do something else instead of printing error
                if let Some(old_value) = old_value {
                    if value != old_value {
                        // then we are trying to add a new value
                        println!("Trying to push a new value to the action matrix");
                        println!("action({:?}, {:?}), new value {:?} old value {:?}", items, x, value, old_value);
                    }
                }

                action.insert(key, value);
            }
        }


        action
    }


    pub fn lr0_analyse(&self, chain: String) -> bool {


        let mut state_names = BTreeMap::new();
        let k = self.get_items_set();
        let iter = k.iter().enumerate();
        for (i, state) in  iter {
            state_names.insert(state.clone(), format!("q{}", i));
        }

        println!("TEST {:?}", state_names);

        let q0 = self.get_q0();
        let mut stack: Stack = vec!(StackSymbol::State(q0));
        let action_matrix = self.get_action_matrix();

        let chain = {
            let mut vec: Vec<char> = chain.chars().collect();
            vec.push(END_CHAR);
            vec
        };

        let mut index = 0;
        while index < chain.len() {
            let stack_top = stack[stack.len() - 1].clone();

            // TODO better abstract the way we handle the stack
            let stack_top = match stack_top {
                StackSymbol::State(state) => state,
                _ => panic!("Ouch error"),
            };

            let tc = chain[index];
            println!("stack {:?}", stack);
            println!("tc {:?}", tc);
            let action = action_matrix.get( &(stack_top.clone(), tc) ).unwrap();
            println!("action {:?}", action);
            println!("====");

            match *action {
                Action::Shift(ref q) => {
                    if self.vn.contains(&tc) {
                        stack.push(StackSymbol::VN(tc));
                    } else if self.vt.contains(&tc) {
                        stack.push(StackSymbol::VT(tc));
                    }

                    stack.push(StackSymbol::State(q.clone()));
                    index = index + 1;
                },
                Action::Reduce(prod_index) => {
                    let (nt, ref derivation) = self.prod_vec[prod_index];
                    let len = 2 * derivation.len();

                    //println!("FUUU {:?}", stack);
                    for _ in 0..len {
                        match stack.pop() {
                            None => return false,
                            _ => {},
                        }
                    }
                    //println!("FUUU2222 {:?}", stack);


                    let stack_top = stack[stack.len() - 1].clone();
                    let stack_top = match stack_top {
                        StackSymbol::State(state) => state,
                        _ => panic!("Ouch error"),
                    };

                    stack.push(StackSymbol::VN(nt));
                    let q = self.goto(&stack_top, nt);
                    stack.push(StackSymbol::State(q.clone()));
                    println!("Reducing by {:?} -> {:?}", nt, derivation);
                },
                Action::Accept => {
                    // Later we shuld return something more meaninful
                    return true;
                },
                Action::Error => {
                    //TODO better error reporting
                    return false;
                }
            }
        }

        false
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
        use super::{CFG, NonTerminalSet, TerminalSet, NonTerminal, Productions, Action};

        let vn: NonTerminalSet = set!('S');
        let vt: TerminalSet = set!('a', '(', ')');
        let s: NonTerminal = 'S';
        let p: Productions = vec![
            ('S', vec!['(', 'S', ')'] ),
            ('S', vec!['a'] )
        ];

        let g = CFG::new(vn, vt, p, s);
        println!("Resulted grammar {:?}", g);

        let m = g.get_action_matrix();

        let q0 = set!((0, 0), (1, 0), (2, 0));
        let q1 = set!((0, 1));
        let q2 = set!((1, 0), (1, 1), (2, 0));
        let q3 = set!((2, 1));
        let q4 = set!((1, 2));
        let q5 = set!((1, 3));


        assert_eq!(*m.get( &(q0.clone(), '(')).unwrap(), Action::Shift(q2.clone()));
        assert_eq!(*m.get( &(q0.clone(), 'a')).unwrap(), Action::Shift(q3.clone()));
        assert_eq!(*m.get( &(q0.clone(), ')')).unwrap(), Action::Error);
        assert_eq!(*m.get( &(q0.clone(), '$')).unwrap(), Action::Error);
        assert_eq!(*m.get( &(q0.clone(), 'S')).unwrap(), Action::Shift(q1.clone()));

        assert_eq!(*m.get( &(q1.clone(), '(')).unwrap(), Action::Error);
        assert_eq!(*m.get( &(q1.clone(), 'a')).unwrap(), Action::Error);
        assert_eq!(*m.get( &(q1.clone(), ')')).unwrap(), Action::Error);
        assert_eq!(*m.get( &(q1.clone(), '$')).unwrap(), Action::Accept);
        assert_eq!(*m.get( &(q1.clone(), 'S')).unwrap(), Action::Error);

        assert_eq!(*m.get( &(q2.clone(), '(')).unwrap(), Action::Shift(q2.clone()));
        assert_eq!(*m.get( &(q2.clone(), 'a')).unwrap(), Action::Shift(q3.clone()));
        assert_eq!(*m.get( &(q2.clone(), ')')).unwrap(), Action::Error);
        assert_eq!(*m.get( &(q2.clone(), '$')).unwrap(), Action::Error);
        assert_eq!(*m.get( &(q2.clone(), 'S')).unwrap(), Action::Shift(q4.clone()));

        assert_eq!(*m.get( &(q3.clone(), '(')).unwrap(), Action::Reduce(2));
        assert_eq!(*m.get( &(q3.clone(), 'a')).unwrap(), Action::Reduce(2));
        assert_eq!(*m.get( &(q3.clone(), ')')).unwrap(), Action::Reduce(2));
        assert_eq!(*m.get( &(q3.clone(), '$')).unwrap(), Action::Reduce(2));
        assert_eq!(*m.get( &(q3.clone(), 'S')).unwrap(), Action::Error);

        assert_eq!(*m.get( &(q4.clone(), '(')).unwrap(), Action::Error);
        assert_eq!(*m.get( &(q4.clone(), 'a')).unwrap(), Action::Error);
        assert_eq!(*m.get( &(q4.clone(), ')')).unwrap(), Action::Shift(q5.clone()));
        assert_eq!(*m.get( &(q4.clone(), '$')).unwrap(), Action::Error);
        assert_eq!(*m.get( &(q4.clone(), 'S')).unwrap(), Action::Error);

        assert_eq!(*m.get( &(q5.clone(), '(')).unwrap(), Action::Reduce(1));
        assert_eq!(*m.get( &(q5.clone(), 'a')).unwrap(), Action::Reduce(1));
        assert_eq!(*m.get( &(q5.clone(), ')')).unwrap(), Action::Reduce(1));
        assert_eq!(*m.get( &(q5.clone(), '$')).unwrap(), Action::Reduce(1));
        assert_eq!(*m.get( &(q5.clone(), 'S')).unwrap(), Action::Error);

        //println!("{:?}", m);
        assert!(g.lr0_analyse("(a)".to_string()));
        assert!(g.lr0_analyse("((a))".to_string()));
        assert!(g.lr0_analyse("(((((a)))))".to_string()));

        assert!(!g.lr0_analyse("(a".to_string()));
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
