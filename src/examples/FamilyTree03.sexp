(def-type Parent (Set (Str Str)))
(def-type Age (Set (Str Int)))
(def-type NameAndAge (variant ((:NameAndAge (Str Int)))))
(def-type R (Set (Str (variant ((:NameAndAge (Str Int)))))))

(fact (Parent "Caroline" "IngerM"))
(fact (Parent "Caroline" "FritsT"))
(fact (Parent "Bjarke" "IngerM"))
(fact (Parent "Bjarke" "FritsT"))
(fact (Parent "Magnus" "IngerM"))
(fact (Parent "Magnus" "FritsT"))

(fact (Age "Caroline" 18))
(fact (Age "Bjarke" 24))
(fact (Age "Magnus" 28))

(rule (R (p (:NameAndAge c a))) (
    (Parent c p)
    (Age c a)))
