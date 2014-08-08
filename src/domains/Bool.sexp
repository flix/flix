(def-type Bool (
    (Bool.Top)
    (Bool.True) (Bool.False)
    (Bool.Bot)))

(def-leq (e1: Bool e2: Bool)
    (match (e1 e2)
        (case (Bool.Bot _) => true)
        (case (Bool.True True) => true)): Boolean)

(def-join (e1: Bool e2: Bool)
    (match (e1 e2)
        (case (Bool.Bot _) => Bool.Bot)
        (case (_ Bool.Bot) => Bool.Bot)
        (case (Bool.True Bool.True) => Bool.True)
        (case (Bool.False Bool.False) => Bool.False)
        (case (_, _) => Bool.Top)): Bool)

(def-fn and (e1: Bool e2: Bool)
    (match (e1 e2)
        (case (Bool.Bot _) => Bool.Bot)
        (case (Bool.True Bool.False) => Bool.False)


PointsTo(h1,f,h2) :- VarPointsTo(), Assign(), Assign().

(def-rule (VarPointsTo (v1, o))
    (Assign (v1 v2)
    (VarPointsTo (v2 o))))

(def-rule (VarPointsTo (v1, o)) (Assign (v1 v2) (VarPointsTo (v2 o))))

(def-rule (VarPointsTo v1 o)
    (Assign v1 v2)
    (VarPointsTo v2 o)))

(def-rule (VarPointsTo v1 o) <= (Assign v1 v2) (VarPointsTo v2 o)))

(VarPointsTo v1 o) <= (Assign v1 v2) (VarPointsTo v2 o)