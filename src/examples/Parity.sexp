(def-type Parity (variant ((Top) (Odd) (Even) (Bot))))

(def-type A Parity)
(def-type B Parity)
(def-type R Parity)

(def-bot Parity Bot)

(def-leq Parity (e1 Parity e2 Parity)
    (match <e1 e2>
        (case <Bot _>       true)
        (case <Odd Odd>     true)
        (case <Even Even>   true)
        (case <_ Top>       true)
        (case _             false)))

(def-lub Parity (e1 Parity e2 Parity)
    (match <e1 e2>
        (case <Bot x>       x)
        (case <x Bot>       x)
        (case <Odd Odd>     Odd)
        (case <Even Even>   Even)
        (case <Odd Even>    Top)
        (case <Even Odd>    Top)
        (case <Top _>       Top)
        (case <_ Top>       Top)))

(def-height Parity (e Parity)
    (match e
        (case Top  1)
        (case Odd  2)
        (case Even 2)
        (case Bot  3)))

(fact (A Odd))
(fact (B Even))

(rule (R x) ((A x)))
(rule (R x) ((B x)))
