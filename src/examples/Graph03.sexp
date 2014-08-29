(def-type Edge (Set (Str Str)))
(def-type Reachable (Set (Str Str)))
(def-type Cycle (Set Str))

(fact (Edge "z" {"x"}))
(fact (Edge "b" {"c"}))
(fact (Edge "y" {"z"}))
(fact (Edge "a" {"b"}))
(fact (Edge "c" {"a"}))
(fact (Edge "x" {"y"}))

(rule (Reachable x {y}) ((Edge x {y})))
(rule (Reachable x {z}) ((Reachable x {y}) (Reachable y {z})))
