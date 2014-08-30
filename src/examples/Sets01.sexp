(def-type A (Set Str))
(def-type B (Set Str))
(def-type R (Set (Set Str)))

(fact (A {"a"}))
(fact (A {"b"}))

(fact (B {"x"}))
(fact (B {"y"}))

(rule (R {x}) ((A x)))
(rule (R {x}) ((B x)))
