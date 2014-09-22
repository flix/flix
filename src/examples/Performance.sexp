(def-type Map (-> <Str Str> (Set Str)))

(def-type Strings (Set Str))
(fact (Strings {"a" "b" "c" "d" "e" "f"}))

(rule (Map <x x> {x}) ((Strings {x})))