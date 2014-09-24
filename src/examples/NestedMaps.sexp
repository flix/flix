(def-type Nested (-> Int (-> Int (Set Int))))

(rule (Nested a (b cs)) ((Nested b (a cs))))
(fact (Nested 1 (2 {3 4})))
