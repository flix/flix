(def-type FirstName Str)
(def-type LastName Str)
(def-type Person (-> FirstName (Set LastName)))
(def-type Sex (-> FirstName (-> LastName (Set Str))))
(def-type A (Set Str))

(fact (Person "Inger" {"Madsen"}))
(fact (Person "Magnus" {"Madsen"}))
(fact (Person "Frits" {"Thomsen"}))
(fact (Person "Bjarke" {"Thomsen"}))
(fact (Person "Caroline" {"Thomsen"}))

(fact (Sex "Inger" "Madsen" {"Female"}))
(fact (Sex "Magnus" "Madsen" {"Male"}))
(fact (Sex "Frits" "Thomsen" {"Male"}))
(fact (Sex "Bjarke" "Thomsen" {"Male"}))
(fact (Sex "Caroline" "Thomsen" {"Female"}))

// expected result: { "Male" }
(rule (A {sx}) ((Person first {last}) (Sex first last {sx})) (and (!= first "Inger") (!= "Caroline" first)))
