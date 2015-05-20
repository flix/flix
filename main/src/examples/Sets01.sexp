(def-type A (Set Str))
(def-type B (Set Str))
(def-type S (Set Str))
(def-type R (Set (Set Str)))

(fact (A {"a"}))
(fact (A {"b"}))

(fact (B {"x"}))
(fact (B {"y"}))

(rule (S x) ((A x)))
(rule (S x) ((B x)))

(rule (R {x}) ((A x)))
(rule (R {x}) ((B x)))
