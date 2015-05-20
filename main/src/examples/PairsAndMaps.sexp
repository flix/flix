(def-type A (Set <Str Str>))
(def-type B (-> Str (Set Str)))

(fact (A {<"a" "b">}))
(fact (A {<"c" "d">}))
(fact (A {<"e" "f">}))
(fact (A {<"g" "h">}))

(rule (B x {y}) ((A {<x y>})))
