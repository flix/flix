(rule (Edge "c" "d"))
(rule (Edge "b" "c"))
(rule (Edge "a" "b"))
(rule (Edge "d" "a"))

(rule (Reachable x y) (Edge x y))
(rule (Reachable x z) (Reachable (x y) (Edge y z)))

(rule (Cycle x) (Reachable x x))
