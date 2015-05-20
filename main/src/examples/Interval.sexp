(def-type Number (variant ((Top) (Interval <Int Int>) (Bot))))

(def-type A Number)
(def-type B Number)
(def-type R Number)

(def-bot Number Bot)

(def-leq Number (e1 Number e2 Number)
    (match <e1 e2>
        (case <Bot _>   true)
        (case <(Interval <b1 e1>) (Interval <b2 e2>)>
            (and (lte b1 b2) (gte e2 e1)))
        (case <_ Top>   true)
        (case _         false)))

(def-lub Number (e1 Number e2 Number)
    (match <e1 e2>
        (case <Bot x>       x)
        (case <x Bot>       x)
        (case <(Interval <b1 e1>) (Interval <b2 e2>)>
            (Interval <(min b1 b2) (max e1 e2)>))
        (case <Top _>       Top)
        (case <_ Top>       Top)))

(fact (A (Interval <2 2>)))
(fact (B (Interval <4 7>)))

(rule (R x) ((A x)))
(rule (R x) ((B x)))
