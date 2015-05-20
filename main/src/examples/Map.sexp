(def-type A (Set Int))
(def-type R (Set Int))

(def-fun inc (x Int)
    (+ x 1))

(fact (A {1}))
(fact (A {2}))
(fact (A {3}))

(rule (R { (inc x) }) ((A {x})))
