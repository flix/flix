(def-type Parity (
    PTop POdd PEven PBot))

(def-leq Parity (e1 Parity e2 Parity)
    (match (e1 e2)
        (case (PBot _)      true)
        (case (POdd POdd)   true)
        (case (PEven PEven) true)
        (case (_ PTop)      true)))

(def-lub Parity (e1 Parity e2 Parity)
    (match (e1 e2)
        (case (PBot x)      x)
        (case (x PBot)      x)
        (case (POdd POdd)   POdd)
        (case (PEven PEven) PEven)
        (case (PTop _)      PTop)
        (case (_ PTop)      PTop)))

(def-height Parity (e Parity)
    (match e
        (case PTop  1)
        (case POdd  2)
        (case PEven 2)
        (case PBot  3)))

(def-fun sum (e1 Parity e2 Parity)
    (match (e1 e2)
        (case (PBot _)          PBot)
        (case (_ PBot)          PBot)
        (case (POdd POdd)       PEven)
        (case (POdd PEven)      POdd)
        (case (PEven POdd)      POdd)
        (case (PEven PEven)     PEven)
        (case _                 PTop)))

(rule (A Odd))
(rule (B Even))

(rule (R x) ((A x)))
(rule (R x) ((B x)))
