(def-type Parent (Set [Str Str]))
(def-type Person (Set [Str (variant ((:AgeAndSex [Str Int])))]))
(def-type MaleGrandParent (Set [Str Str]))

(fact (Parent "Caroline" "IngerM"))
(fact (Parent "Caroline" "FritsT"))
(fact (Parent "Bjarke" "IngerM"))
(fact (Parent "Bjarke" "FritsT"))
(fact (Parent "Magnus" "IngerM"))
(fact (Parent "Magnus" "FritsT"))
(fact (Parent "FritsT" "IngerT"))
(fact (Parent "FritsT" "OrlaT"))
(fact (Parent "IngerM" "GreteM"))

(fact (Person "Bjarke"   (:AgeAndSex ("Male" 1))))
(fact (Person "Magnus"   (:AgeAndSex ("Male" 2))))
(fact (Person "FritsT"   (:AgeAndSex ("Male" 3))))
(fact (Person "OrlaT"    (:AgeAndSex ("Male" 4))))

(fact (Person "Caroline" (:AgeAndSex ("Female" 5))))
(fact (Person "IngerM"   (:AgeAndSex ("Female" 6))))
(fact (Person "IngerT"   (:AgeAndSex ("Female" 7))))
(fact (Person "GreteM"   (:AgeAndSex ("Female" 8))))

(rule (MaleGrandParent x z) (
    (Parent x y)
    (Parent y z)
    (Person z (:AgeAndSex ("Male" _)))))
