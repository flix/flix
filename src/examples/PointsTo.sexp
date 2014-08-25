(rule (VarPointsTo var obj) ((New var obj)))
(rule (VarPointsTo var1 obj) ((Assign var1 var2) (VarPointsTo var2 obj))
(rule (VarPointsTo var1 obj) (
    (Load var1 var2 field)
    (VarPointsTo var2 baseObj)
    (HeapPointsTo baseObj field obj)))

(rule (HeapPointsTo baseObj field obj) (
    (Store var1 field var2)
    (VarPointsTo var1 baseObj)
    (VarPointsTo var2 obj)))
