(def-type Number (variant ((Top) (Cst Int) (Bot))))

(def-type A Number)
(def-type B Number)
(def-type R Number)
(def-type W Number)

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

(def-height Number (e Number)
    (match e
        (case Top      1)
        (case (Cst n)  2)
        (case Bot      3)))

(def-fun sum (e1 Number e2 Number)
    (match <e1 e2>
        (case <Bot _>   Bot)
        (case <_ Bot>   Bot)
        (case <(Cst n1) (Cst n2)>  (Cst (+ n1 n2)))
        (case _         Top)))

(fact (A (Cst 3)))
(fact (B (Cst 4)))

(rule (R (sum x y)) ((A x) (B y)))

(rule (W x) ((A x)))
(rule (W x) ((R x)))
