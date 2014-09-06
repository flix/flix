(def-type Number (variant ((:Top) (:Cst Int) (:Bot))))

(def-type A Number)
(def-type B Number)
(def-type R Number)



(fact (A (:Cst 3)))
(fact (B (:Cst 5)))

(rule (R x) ((A x)))
(rule (R x) ((B x)))