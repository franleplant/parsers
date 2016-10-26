use std::collections::{BTreeSet, BTreeMap};
//use std::fmt;
use super::cfg::CFG;

//TODO
// - more tests with different grammars
//

const END_CHAR: char = '$';
const FALSE_S: char = 'Ś';

pub type Terminal = char;
pub type TerminalSet = BTreeSet<Terminal>;
pub type NonTerminal = char;
pub type NonTerminalSet = BTreeSet<NonTerminal>;

pub type Derivation = Vec<char>;
pub type Production = (NonTerminal, Derivation);
pub type Productions = Vec<Production>;

pub type ProductionIndex = usize;
pub type ProductionMap = BTreeMap<NonTerminal, Vec<ProductionIndex>>;

pub type DotIndex = usize;
pub type DerefedItem = (ProductionIndex, NonTerminal, Derivation, DotIndex, Option<char>);
pub type Item = (ProductionIndex, DotIndex);
pub type Items = BTreeSet<Item>;
pub type State = Items;

pub type ActionMatrix = BTreeMap<(Items, char), Action>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum StackSymbol {
    State(Items),
    VN(NonTerminal),
    VT(Terminal),
}

pub type Stack = Vec<StackSymbol>;


#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Action {
    Shift(Items),
    Reduce(ProductionIndex),
    Accept,
    Error,
}

#[derive(Clone, Debug)]
pub struct LR0 {
    grammar: CFG,
    stack: Stack,
    pub action_matrix: ActionMatrix,
    q0: State,
    k: BTreeSet<State>,
    // TODO maybe store shortcut names for the states, et all

}


impl LR0 {
    pub fn new(grammar: CFG) -> LR0 {
        assert!(!grammar.is_nonterminal(&FALSE_S), "{} is a special VN and cannot be used", FALSE_S);
        assert!(!grammar.is_terminal(&END_CHAR), "{} is a special VT and cannot be used", END_CHAR);

        let vn = grammar.vn.union(&set!(FALSE_S)).cloned().collect();
        let vt = grammar.vt.union(&set!(END_CHAR)).cloned().collect();
        let s = FALSE_S;
        let mut prod = grammar.prod_vec.clone();
        prod.insert(0, (FALSE_S, vec![grammar.s, END_CHAR]) );

        let extended_grammar = CFG::new(vn, vt, prod, s);


        let mut parser = LR0 {
            grammar: extended_grammar,
            k: BTreeSet::new(),
            q0: BTreeSet::new(),
            action_matrix: BTreeMap::new(),
            stack: vec!(),
        };


        let (q0, k) = parser.calc_stateset();
        parser.q0 = q0;
        parser.k = k;

        parser.action_matrix = parser.calc_action_matrix();


        parser
    }

    pub fn closure(&self, items: &State) -> State {
        let mut closure = items.clone();
        let mut marked = set!();

        while marked != self.non_terminal_items(&closure) {
            for item in closure.clone().iter() {
                if marked.contains(&item) {
                    continue;
                }

                marked.insert(item.clone());

                // In here we should have productions of the form
                // A -> a.Bb
                let (_, _, _, _, right_symbol) = self.deref_item(&item);

                if let Some(right_symbol) = right_symbol {
                    assert!(self.grammar.is_nonterminal(&right_symbol), "Ups something went wrong. Expected {} to be a NonTerminal", right_symbol);

                    // add all productions of the form
                    // B -> any
                    if let Some(prod_indices) = self.grammar.prod_map.get(&right_symbol) {
                        for prod_index in prod_indices {
                            closure.insert( ( *prod_index, 0) );
                        }
                    }
                }
            }
        }


        closure
    }

    fn goto(&self, items: &State, x: char) -> State {
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

    pub fn calc_stateset(&self) -> (State, BTreeSet<State>) {
        let v = self.grammar.get_v();
        let q0 = self.closure( &set!((0, 0)) );

        let mut k = set!(q0.clone());
        let mut marked = set!();


        while k != marked {
            for state in k.clone() {
                if marked.contains(&state) {
                    continue;
                }

                marked.insert(state.clone());

                for x in v.iter().cloned() {
                    // Dont add S' -> S$. since it's not useful
                    if x == END_CHAR {
                        continue;
                    }

                    let next_state = self.goto(&state, x);
                    if !next_state.is_empty() {
                        k.insert(next_state);
                    }
                }
            }
        }

        (q0, k)
    }

    // TODO improve name
    fn non_terminal_items(&self, items: &Items) -> Items {
        items.iter().cloned().filter(| item | {
            let (_, _, _, _, right_symbol) = self.deref_item(&item);

            if let Some(right_symbol) = right_symbol {
                if self.grammar.is_nonterminal(&right_symbol) {
                    return true
                }
            }

            false
        })
        .collect()
    }



    fn get_complete_item(&self, items: &Items) -> Option<Item> {
        for item in items {
            let (_, _, derivation, dot_index, _) = self.deref_item(&item);
            if derivation.len() == dot_index {
                return Some(item.clone())
            }
        }

        None
    }

    fn is_accept_state(&self, items: &Items) -> bool {
        for item in items {
            let (_, nt, _, _, right_symbol) = self.deref_item(&item);
            if let Some(right_symbol) = right_symbol {
                if nt == FALSE_S && right_symbol == END_CHAR {
                    return true;
                }
            }
        }

        false
    }


    pub fn deref_item(&self, item: &Item) -> DerefedItem {
        let &(prod_index, dot_index) = item;
        assert!(prod_index < self.grammar.prod_vec.len(), "prod index {} out of bound", prod_index);
        let (nt, ref derivation) = self.grammar.prod_vec[prod_index];
        assert!(dot_index <= derivation.len(), "dot_index {} out of bound in {:?}", dot_index, derivation);

        let right_symbol = if dot_index == derivation.len() {
            None
        } else {
          Some(derivation[dot_index])
        };

        (prod_index, nt, derivation.clone(), dot_index, right_symbol)
    }

    fn stacktop(&self) -> StackSymbol {
        self.stack[self.stack.len() - 1].clone()
    }

    fn stacktop_state(&self) -> State {
        match self.stacktop() {
            StackSymbol::State(state) => state.clone(),
            _ => panic!("Ouch error"),
        }
    }


    fn next_action(&self, state: State, x: char, next_state: State) -> Action {
        if self.is_accept_state(&state) && x == END_CHAR {
            return Action::Accept;
        }

        if !next_state.is_empty() {
            return Action::Shift(next_state)
        }

        if let Some(item) = self.get_complete_item(&state) {
            let (prod_index, nt, _, _, _) = self.deref_item(&item);
            if nt != FALSE_S  && !self.grammar.is_nonterminal(&x) {
                return Action::Reduce(prod_index);
            }
        }

        Action::Error
    }

    // TODO we should have in account reduction redution or shift reduction
    // conflicts in this matrix as in lr1 g
    pub fn calc_action_matrix(&self) -> ActionMatrix{
        let v = self.grammar.get_v();
        let mut action: ActionMatrix = BTreeMap::new();
        let ref k = self.k;


        for x in v {
            for state in k {
                let key = (state.clone(), x);
                let old_value = action.get(&key).cloned();
                let next_state = self.goto(&state, x);

                let value = self.next_action(state.clone(), x, next_state);

                // TODO do something else instead of printing error
                if let Some(old_value) = old_value {
                    if value != old_value {
                        // then we are trying to add a new value
                        println!("Trying to push a new value to the action matrix");
                        println!("action({:?}, {:?}), new value {:?} old value {:?}", state, x, value, old_value);
                    }
                }

                action.insert(key, value);
            }
        }


        action
    }


    pub fn parse(&mut self, w: String) -> bool {
        let mut state_names = BTreeMap::new();
        let ref k = self.k;
        let iter = k.iter().enumerate();
        for (i, state) in iter {
            state_names.insert(state.clone(), format!("q{}", i));
        }

        self.stack = vec!(StackSymbol::State(self.q0.clone()));

        let chain: Vec<char> = format!("{}{}", w, END_CHAR).chars().collect();

        let mut index = 0;
        while index < chain.len() {
            let stack_top = self.stacktop_state();

            let tc = chain[index];
            println!("stack {:?}", self.stack);
            println!("tc {:?}", tc);
            let action = self.action_matrix.get( &(stack_top.clone(), tc) ).unwrap();
            println!("action {:?}", action);
            println!("====");

            match *action {
                Action::Shift(ref q) => {
                    if self.grammar.is_nonterminal(&tc) {
                        self.stack.push(StackSymbol::VN(tc));
                    } else if self.grammar.is_terminal(&tc) {
                        self.stack.push(StackSymbol::VT(tc));
                    }

                    self.stack.push(StackSymbol::State(q.clone()));
                    index = index + 1;
                },
                Action::Reduce(prod_index) => {
                    let (nt, ref derivation) = self.grammar.prod_vec[prod_index];
                    let len = 2 * derivation.len();

                    for _ in 0..len {
                        match self.stack.pop() {
                            None => return false,
                            _ => {},
                        }
                    }

                    let stack_top = self.stacktop_state();

                    self.stack.push(StackSymbol::VN(nt));

                    let next_action = self.action_matrix.get( &(stack_top, nt) ).expect("no null goto transitions");
                    let q = match *next_action {
                        Action::Shift(ref q) => q.clone(),
                        _ => panic!("Expected Action::Shift but found {:?}", next_action),
                    };

                    self.stack.push(StackSymbol::State(q.clone()));
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





#[cfg(test)]
mod tests {
    #[test]
    fn simple_grammar() {
        use cfg::CFG;
        use super::{Action, LR0};

        let vn = set!('S');
        let vt = set!('a', '(', ')');
        let s = 'S';
        let p = vec![
            ('S', vec!['(', 'S', ')'] ),
            ('S', vec!['a'] )
        ];

        let g = CFG::new(vn, vt, p, s);

        let mut parser = LR0::new(g);
        let m = parser.action_matrix.clone();
        let k = parser.k.clone();


        let q0 = set!((0, 0), (1, 0), (2, 0));
        let q1 = set!((0, 1));
        let q2 = set!((1, 0), (1, 1), (2, 0));
        let q3 = set!((2, 1));
        let q4 = set!((1, 2));
        let q5 = set!((1, 3));


        let k_expected = set!(q0.clone(), q1.clone(), q2.clone(), q3.clone(), q4.clone(), q5.clone());

        //println!("K {:?}", k);
        //println!("K {:?}", k_expected);
        assert_eq!(k, k_expected, "should assemble the right state set");

        println!("MOFO {:?}", m);
        println!("MOFO2 {:?}, {:?}", q0, m.get( &(q0.clone(), '(')) );
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
        assert!(parser.parse("(a)".to_string()));
        assert!(parser.parse("((a))".to_string()));
        assert!(parser.parse("(((((a)))))".to_string()));

        assert!(!parser.parse("(a".to_string()));
    }
}

