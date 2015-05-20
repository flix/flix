(def-type Number (variant ((Top) (Cst Int) (Bot))))

(def-type A Number)
(def-type B Number)

(def-bot Number Bot)

(def-leq Number (e1 Number e2 Number)
    (match <e1 e2>
        (case <Bot _>                 true)
        (case <(Cst n1) (Cst n2)>     (== n1 n2))
        (case <_ Top>                 true)
        (case _                       false)))

(def-lub Number (e1 Number e2 Number)
    (match <e1 e2>
        (case <Bot x>   x)
        (case <x Bot>   x)
        (case <(Cst n1) (Cst n2)>  (if (== n1 n2) (Cst n1) Top))
        (case _         Top)))

(fact (A (Cst 3)))
(fact (A (Cst 4)))

(rule (B l) ((A l)))
