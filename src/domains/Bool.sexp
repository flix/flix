(def-type Bool (Bool.Top Bool.True Bool.False Bool.Bot))

(def-bot Bool Bool.Bot)

(def-leq (e1 Bool e2 Bool)
    (match (e1 e2)
        (case (Bool.Bot _) true)
        (case (Bool.True True) true)))

(def-join (e1 Bool e2 Bool)
    (match (e1 e2)
        (case (Bool.Bot x) x)
        (case (x Bool.Bot) x)
        (case (Bool.True Bool.True) Bool.True)
        (case (Bool.False Bool.False) Bool.False)
        (case (_) Bool.Top)))

(def-fn and (e1 Bool e2 Bool)
    (match (e1 e2)
        (case (Bool.Bot _) Bool.Bot)
        (case (Bool.True Bool.False) Bool.False)

(rule (VarPointsTo (v1, o))
    (Assign (v1 v2)
    (VarPointsTo (v2 o))))

(def-rule (VarPointsTo (v1, o)) (Assign (v1 v2) (VarPointsTo (v2 o))))

(def-rule (VarPointsTo v1 o)
    (Assign v1 v2)
    (VarPointsTo v2 o)))

(rule (VarPointsTo v1 o) <= (Assign v1 v2) (VarPointsTo v2 o)))

(VarPointsTo v1 o) <= (Assign v1 v2) (VarPointsTo v2 o)