(def-type Value (variant ((Top) (Cst Int) (Bot))))
(def-type Transformer Str)

(def-fun summ (e1 Value e2 Int) Top)
(def-fun apply (t Transformer l Value) (summ l 1))