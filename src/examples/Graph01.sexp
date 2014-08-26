(def-type Edge (String String))

(fact (Edge "c" "d"))
(fact (Edge "b" "c"))
(fact (Edge "a" "b"))
(fact (Edge "d" "a"))

(rule (Reachable x y) (Edge x y))
(rule (Reachable x z) (Reachable (x y) (Edge y z)))

(rule (Cycle x) (Reachable x x))
