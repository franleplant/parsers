use std::collections::{BTreeSet, BTreeMap};
//use std::fmt;
use super::cfg::{CFG, Symbol, Derivation, ProductionIndex};
use std::convert::From;


const EOW: &'static str = "$";
const FALSE_S: &'static str = "Ś";

#[derive(Clone, Debug)]
pub struct Token {
    _type: Symbol,
}

impl Token {
    pub fn from_type(_type: Symbol) -> Token {
        Token {
            _type: _type,
        }
    }

    pub fn from_str(s: &str) -> Vec<Token> {
        s.clone().to_string().chars()
            .map(|c| c.to_string())
            .map(|s| Token::from_type(s))
            .collect()
    }

    pub fn get_type(&self) -> Symbol {
        self._type.clone()
    }
}


impl<'a> From<&'a Token> for String {
    fn from(t: &'a Token) -> String {
        t.get_type()
    }
}

pub type DotIndex = usize;
pub type DerefedItem = (ProductionIndex, Symbol, Derivation, DotIndex, Option<Symbol>);
pub type Item = (ProductionIndex, DotIndex);
pub type Items = BTreeSet<Item>;
pub type State = Items;

pub type ActionMatrix = BTreeMap<(Items, Symbol), Action>;
pub type Stack = Vec<State>;


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
    false_s: Symbol,
    eow: Symbol,
    // TODO maybe store shortcut names for the states, et all

}


impl LR0 {
    pub fn new(grammar: CFG) -> LR0 {
        let false_s = FALSE_S.to_string();
        let eow = EOW.to_string();

        assert!(!grammar.is_nonterminal(&false_s), "{} is a special VN and cannot be used", false_s);
        assert!(!grammar.is_terminal(&eow), "{} is a special VT and cannot be used", eow);


        let s = false_s.clone();

        let vn = {
            let mut vn = grammar.vn.clone();
            vn.insert(false_s.clone());
            vn
        };

        let vt = {
            let mut vt = grammar.vt.clone();
            vt.insert(eow.clone());
            vt
        };

        let prod = {
            let mut prod = grammar.prod_vec.clone();
            prod.insert(0, (false_s.clone(), vec![grammar.s, eow.clone()]) );
            prod
        };

        let extended_grammar = CFG::new(vn, vt, prod, s);


        let mut parser = LR0 {
            grammar: extended_grammar,
            k: BTreeSet::new(),
            q0: BTreeSet::new(),
            action_matrix: BTreeMap::new(),
            stack: vec!(),
            false_s: false_s,
            eow: eow,
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

    fn goto(&self, items: &State, x: Symbol) -> State {
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
                    if x == self.eow {
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
                if nt == self.false_s && right_symbol == self.eow {
                    return true;
                }
            }
        }

        false
    }


    pub fn deref_item(&self, item: &Item) -> DerefedItem {
        let &(prod_index, dot_index) = item;
        assert!(prod_index < self.grammar.prod_vec.len(), "prod index {} out of bound", prod_index);
        let (ref nt, ref derivation) = self.grammar.prod_vec[prod_index];
        assert!(dot_index <= derivation.len(), "dot_index {} out of bound in {:?}", dot_index, derivation);

        let right_symbol = if dot_index == derivation.len() {
            None
        } else {
          Some(derivation[dot_index].clone())
        };

        (prod_index, nt.clone(), derivation.clone(), dot_index, right_symbol)
    }

    fn stacktop(&self) -> State {
        self.stack[self.stack.len() - 1].clone()
    }

    fn next_action(&self, state: State, x: Symbol, next_state: State) -> Action {
        if self.is_accept_state(&state) && x == self.eow {
            return Action::Accept;
        }

        if !next_state.is_empty() {
            return Action::Shift(next_state)
        }

        if let Some(item) = self.get_complete_item(&state) {
            let (prod_index, nt, _, _, _) = self.deref_item(&item);
            if nt != self.false_s  && !self.grammar.is_nonterminal(&x) {
                return Action::Reduce(prod_index);
            }
        }

        Action::Error
    }

    // TODO to have even better error reporting I could calculate something like
    // primeros(NT) and display those as expected symbols
    // but I don't konw if it's going to be useful since in a real grammar those could be
    // pretty big, and compilers tend to tell you unexpected token, or expression expected
    // for what I don;t know how to do
    // Probably this should push a BTreeSet to avoid duplicates
    fn expected_symbols(&self, state: &State) -> BTreeSet<Symbol> {
        let v = self.grammar.get_v();

        v.iter().cloned()
            .map(| x | (x.clone(), self.action_matrix.get( &(state.clone(), x) ).unwrap()) )
            .filter(| &(_, action) | {
                match *action {
                    Action::Error => false,
                    _ => true
                }
            })
            .map(| (x, _) | x)
            .collect()
    }

    // TODO we should have in account reduction redution or shift reduction
    // conflicts in this matrix as in lr1 g
    pub fn calc_action_matrix(&self) -> ActionMatrix {
        let v = self.grammar.get_v();
        let mut action: ActionMatrix = BTreeMap::new();
        let ref k = self.k;


        for x in v {
            for state in k {
                let key = (state.clone(), x.clone());
                let old_value = action.get(&key).cloned();
                let next_state = self.goto(&state, x.clone());

                let value = self.next_action(state.clone(), x.clone(), next_state);

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


    // TODO this should return a Result with proper error reporting
    // - Ok((derivations, stack, et al?))
    // - Err((derivations, stack, position_in_chain, expected message?, et al))
    // TODO probably this function should be private and called something like validate_chain
    // and should return a bool
    // and then a top level public function called parse should do stuff with that boolean result
    // checking the stack and the position in the chain (LR0 should have a index field)
    // and asemble all the information to be returned to the user
    // this is likely necessary to simplify the out points of the existing parse,
    // perhaps we could accomplish the same with smart `break` and single out point for the whole
    // function
    pub fn parse(&mut self, mut chain: Vec<Token>) -> bool {
        let mut state_names = BTreeMap::new();
        let ref k = self.k;
        let iter = k.iter().enumerate();
        for (i, state) in iter {
            state_names.insert(state.clone(), format!("q{}", i));
        }

        let mut derivations = vec!();
        self.stack = vec!(self.q0.clone());

        let chain = {
            chain.push(Token::from_type(self.eow.clone()));
            chain
        };


        let mut index = 0;
        while index < chain.len() {
            let stack_top = self.stacktop();
            let ref token: Token = chain[index];
            let action = self.action_matrix.get( &(stack_top.clone(), token.into()) ).unwrap();

            match *action {
                Action::Shift(ref q) => {
                    self.stack.push(q.clone());
                    index = index + 1;
                },

                Action::Reduce(prod_index) => {
                    let (ref nt, ref derivation) = self.grammar.prod_vec[prod_index];

                    for _ in 0..derivation.len() {
                        match self.stack.pop() {
                            None => return false,
                            _ => {},
                        }
                    }

                    let stack_top = self.stacktop();
                    let next_action = self.action_matrix.get( &(stack_top, nt.clone()) ).expect("no null goto transitions");

                    let q = match *next_action {
                        Action::Shift(ref q) => q.clone(),
                        // TODO should this be a rejected scenario?
                        _ => panic!("Expected Action::Shift but found {:?}", next_action),
                    };

                    self.stack.push(q.clone());
                    derivations.push( (nt.clone(), derivation) );
                },
                Action::Accept => {
                    let w: Vec<String> = chain.iter().map(|t| t.into()).collect();
                    let w: String = w.join("");
                    println!("Accepted {:?}", w);
                    for (nt, derivation) in derivations {
                        println!("Reducing by {:?} -> {:?}", nt, derivation);
                    }
                    // Later we shuld return something more meaninful
                    return true;
                },
                Action::Error => {
                    let w: Vec<String> = chain.iter().map(|t| t.into()).collect();
                    let w: String = w.join("");
                    println!("Parse Error {:?}, {:?}", self.stack, w);

                    let state = self.stack.pop().unwrap();
                    let token_type: Symbol = token.into();
                    println!("expected {:?} found {:?} in {:?}", self.expected_symbols(&state), token_type, w);
                    // In here we can grab the for example ( S and search for right hand side
                    // derivations inside productions that start or contains perhaps that and
                    // basing on that we can do something like "expected ) found nothing"

                    for (nt, derivation) in derivations {
                        println!("Reducing by {:?} -> {:?}", nt, derivation);
                    }
                    //TODO better error reporting
                    return false;
                }
            }
        }

        println!("Parse Error {:?}, {:?}", self.stack, chain);
        for (nt, derivation) in derivations {
            println!("Reducing by {:?} -> {:?}", nt, derivation);
        }
        false
    }
}





#[cfg(test)]
mod tests {
    #[test]
    fn simple_grammar() {
        use cfg::CFG;
        use super::{Action, LR0, Token};

        let vn = set!("S");
        let vt = set!("a", "(", ")");
        let s = "S";
        let p = vec![
            ("S", vec!["(", "S", ")"] ),
            ("S", vec!["a"] )
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

        assert_eq!(k, k_expected, "should assemble the right state set");

        assert_eq!(*m.get( &(q0.clone(), "(".to_string())).unwrap(), Action::Shift(q2.clone()));
        assert_eq!(*m.get( &(q0.clone(), "a".to_string())).unwrap(), Action::Shift(q3.clone()));
        assert_eq!(*m.get( &(q0.clone(), ")".to_string())).unwrap(), Action::Error);
        assert_eq!(*m.get( &(q0.clone(), "$".to_string())).unwrap(), Action::Error);
        assert_eq!(*m.get( &(q0.clone(), "S".to_string())).unwrap(), Action::Shift(q1.clone()));

        assert_eq!(*m.get( &(q1.clone(), "(".to_string())).unwrap(), Action::Error);
        assert_eq!(*m.get( &(q1.clone(), "a".to_string())).unwrap(), Action::Error);
        assert_eq!(*m.get( &(q1.clone(), ")".to_string())).unwrap(), Action::Error);
        assert_eq!(*m.get( &(q1.clone(), "$".to_string())).unwrap(), Action::Accept);
        assert_eq!(*m.get( &(q1.clone(), "S".to_string())).unwrap(), Action::Error);

        assert_eq!(*m.get( &(q2.clone(), "(".to_string())).unwrap(), Action::Shift(q2.clone()));
        assert_eq!(*m.get( &(q2.clone(), "a".to_string())).unwrap(), Action::Shift(q3.clone()));
        assert_eq!(*m.get( &(q2.clone(), ")".to_string())).unwrap(), Action::Error);
        assert_eq!(*m.get( &(q2.clone(), "$".to_string())).unwrap(), Action::Error);
        assert_eq!(*m.get( &(q2.clone(), "S".to_string())).unwrap(), Action::Shift(q4.clone()));

        assert_eq!(*m.get( &(q3.clone(), "(".to_string())).unwrap(), Action::Reduce(2));
        assert_eq!(*m.get( &(q3.clone(), "a".to_string())).unwrap(), Action::Reduce(2));
        assert_eq!(*m.get( &(q3.clone(), ")".to_string())).unwrap(), Action::Reduce(2));
        assert_eq!(*m.get( &(q3.clone(), "$".to_string())).unwrap(), Action::Reduce(2));
        assert_eq!(*m.get( &(q3.clone(), "S".to_string())).unwrap(), Action::Error);

        assert_eq!(*m.get( &(q4.clone(), "(".to_string())).unwrap(), Action::Error);
        assert_eq!(*m.get( &(q4.clone(), "a".to_string())).unwrap(), Action::Error);
        assert_eq!(*m.get( &(q4.clone(), ")".to_string())).unwrap(), Action::Shift(q5.clone()));
        assert_eq!(*m.get( &(q4.clone(), "$".to_string())).unwrap(), Action::Error);
        assert_eq!(*m.get( &(q4.clone(), "S".to_string())).unwrap(), Action::Error);

        assert_eq!(*m.get( &(q5.clone(), "(".to_string())).unwrap(), Action::Reduce(1));
        assert_eq!(*m.get( &(q5.clone(), "a".to_string())).unwrap(), Action::Reduce(1));
        assert_eq!(*m.get( &(q5.clone(), ")".to_string())).unwrap(), Action::Reduce(1));
        assert_eq!(*m.get( &(q5.clone(), "$".to_string())).unwrap(), Action::Reduce(1));
        assert_eq!(*m.get( &(q5.clone(), "S".to_string())).unwrap(), Action::Error);

        assert!(parser.parse( Token::from_str("(a)") ));
        assert!(parser.parse( Token::from_str("((a))") ));
        assert!(parser.parse( Token::from_str("(((((a)))))") ));

        assert!(!parser.parse( Token::from_str("(a") ));
    }
}

