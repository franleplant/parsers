#[macro_export]
macro_rules! set {
    ( $( $c:expr ),* ) => {
        {
            use std::collections::{BTreeSet};
            let mut _aux = BTreeSet::new();
            $(
                _aux.insert($c);
            )*
            _aux
        }
    };
}
