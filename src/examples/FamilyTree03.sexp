(rule (Parent "Caroline" "IngerM"))
(rule (Parent "Caroline" "FritsT"))
(rule (Parent "Bjarke" "IngerM"))
(rule (Parent "Bjarke" "FritsT"))
(rule (Parent "Magnus" "IngerM"))
(rule (Parent "Magnus" "FritsT"))

(rule (Age "Caroline" 18))
(rule (Age "Bjarke" 24))
(rule (Age "Magnus" 28))

(rule (R (p, (NameAndAge c a))) (
    (Parent c p)
    (Age c a)))
