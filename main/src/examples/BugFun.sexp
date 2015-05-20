(def-fun plus (e1 Int e2 Int) (+ e1 e2))

(def-type One (Set Int))
(def-type Two (Set Int))
(def-type Three (Set Int))
(def-type ThreeA (Set Int))
(def-type ThreeB (Set Int))
(def-type ThreeC (Set Int))

(fact (One {1}))
(fact (Two {2}))
(fact (Three {3}))
(rule (ThreeA {(plus one two)}) ((One {one}) (Two {two})))
(fact (ThreeB {(plus 1 2)})) // doesn't work

(def-fun plus3 (e1 Int e2 Int e3 Int) (plus (plus e1 e2) e3)) // doesn't work

(fact (ThreeC {(plus3 1 1 1)}))