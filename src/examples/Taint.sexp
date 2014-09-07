(def-type Taint (variant ((Secret) (NotSecret))))

(def-type Var (-> Str Taint))
(def-type Edge (-> Str (Set Str)))

(def-bot Taint Secret)

(def-leq Taint (e1 Taint e2 Taint)
    (match <e1 e2>
        (case <Secret _>    true)
        (case <_ NotSecret> true)
        (case _             false)))

(def-lub Taint (e1 Taint e2 Taint)
    (match <e1 e2>
        (case <Secret Secret>   Secret)
        (case _                 NotSecret)))

(fact (Var "b" NotSecret))

(fact (Edge "a" {"b"}))
(fact (Edge "b" {"c"}))
(fact (Edge "c" {"f" "g"}))

(fact (Edge "x" {"y"}))
(fact (Edge "y" {"z"}))
(fact (Edge "z" {"x"}))

(rule (Var y t) ((Edge x {y}) (Var x t)))