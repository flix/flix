(def-type Pt (-> Int (Set Int)))
(def-type Store (Set <Int Int Int>))
(def-type KillEmpty (Set Int))

(rule (KillEmpty {l}) ((Store {<l p q>}) (Pt p {a b})) (!= a b))

(fact (Store {<1 2 3>}))
(fact (Pt 2 {4}))   // b does not get bound to anything, so evaluation of (!= a b) crashes
//(fact (Pt 2 {4 5})) // works fine
