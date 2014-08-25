(rule (Parent "Caroline" "IngerM"))
(rule (Parent "Caroline" "FritsT"))
(rule (Parent "Bjarke" "IngerM"))
(rule (Parent "Bjarke" "FritsT"))
(rule (Parent "Magnus" "IngerM"))
(rule (Parent "Magnus" "FritsT"))
(rule (Parent "FritsT" "IngerT"))
(rule (Parent "FritsT" "OrlaT"))
(rule (Parent "IngerM" "GreteM"))

(rule (AS "Bjarke" (AgeAndSex "Male" 1))
(rule (AS "Magnus" (AgeAndSex "Male" 2))
(rule (AS "FritsT" (AgeAndSex "Male" 3))
(rule (AS "OrlaT" (AgeAndSex "Male" 4))

(rule (AS "Caroline" (AgeAndSex "Female" 5))
(rule (AS "IngerM" (AgeAndSex "Female" 6))
(rule (AS "IngerT" (AgeAndSex "Female" 7))
(rule (AS "GreteM" (AgeAndSex "Female" 8))

(rule (MaleGrandParent x z) (
    (Parent x y)
    (Parent y z)
    (AS z (AgeAndSex "Male" _))))
