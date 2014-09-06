(def-type Number (variant ((Top) (Cst Int) (Bot))))

(def-type A Number)
(def-type B Number)
(def-type R Number)

(def-bot Number Bot)

(def-leq Number (e1 Number e2 Number)
    (match <e1 e2>
        (case <Bot _>                 true)
        (case <(Cst n1) (Cst n2)>     (== n1 n2))
        (case <_ Top>                 true)
        (case _                       false)))

(def-lub Number (e1 Number e2 Number)
    Top)

(fact (A (Cst 3)))
(fact (B (Cst 5)))

(rule (R x) ((A x)))
(rule (R x) ((B x)))