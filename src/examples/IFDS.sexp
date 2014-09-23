// Basic data types
// ===========
(def-type Fact Str)
(def-type Node Str)
(def-type Proc Str)

// Inputs
// ===========
(def-type EshIntra (-> <Node Fact> (Set Fact))) // TODO: how to represent this input as an external native function?
(def-type EshCallStart (-> <<Node Fact> Proc> (Set Fact)))
(def-type EshEndReturn (-> <Proc Fact Node> (Set Fact)))
// Call-to-Return edges should be included above in EshIntra
(def-type CFG (Set <Node Node>))
(def-type CallGraph (Set <Node Proc>))
(def-type StartNode (Set <Proc Node>)) // TODO: should really be a function, but the result type is not a lattice
(def-type EndNode (Set <Proc Node>))

// Internal lattices
// ===========
(def-type PathEdge (Set <Fact <Node Fact>>))
(def-type SummaryEdge (-> <Node Fact> (Set Fact)))

// Output
// ===========
(def-type Result (-> Node (Set Fact)))


// Rules
// ===========
// intraproc
(rule (PathEdge {<d1 <m d3>>})
  ((PathEdge {<d1 <n d2>>}) (EshIntra <n d2> {d3}) (CFG {<n m>})))

// use summary
(rule (PathEdge {<d1 <m d3>>})
  ((PathEdge {<d1 <n d2>>}) (SummaryEdge <n d2> {d3}) (CFG {<n m>})))

// call-to-start
(rule (PathEdge {<d3 <start d3> >})
  ((PathEdge {<d1 <call d2>>})  (CallGraph {<call target>})
   (EshCallStart <<call d2> target> {d3}) (StartNode {<target start>})))

// compute summary
(rule (SummaryEdge <call d4> d5s)
  ((CallGraph {<call target>}) (StartNode {<target start>}) (EndNode {<target end>})
   (EshCallStart <<call d4> target> {d1}) (PathEdge {<d1 <end d2>>}) (EshEndReturn <target d2 call> d5s)))

// tabulate result
(rule (Result n {d2}) ((PathEdge {<_ <n d2> >})))

// Example (uninitialized variables) from IFDS paper
(fact (CFG {
  <"smain" "n1">
  <"n1" "n2">
  <"n2" "n3">
  <"n3" "emain">
}))

(fact (CFG {
  <"sp" "n4">
  <"n4" "n5"> <"n4" "ep">
  <"n5" "n6">
  <"n6" "n7">
  <"n7" "n8">
  <"n8" "n9">
  <"n9" "ep">

}))

(fact (StartNode {<"main" "smain"> <"p" "sp">}))
(fact (EndNode   {<"main" "emain"> <"p" "ep">}))

(fact (CallGraph {<"n2" "p"> <"n7" "p">}))

(def-type Facts (Set Fact))
(fact (Facts {"x" "g" "a"}))

(rule (EshIntra <n "zero"> {"zero"}) ((CFG {<n _>})))

(fact (EshIntra <"smain" "zero"> {"x" "g"}))
(fact (EshIntra <"n1" "g"> {"g"}))
(fact (EshIntra <"n2" "x"> {"x"}))
(def-type Identity (Set Node))
(fact (Identity {"n3" "sp" "n4" "n6" "n8" "n9"}))
(rule (EshIntra <idnode f> {f}) ((Facts {f}) (Identity {idnode})))
(fact (EshIntra <"n5" "a"> {"a"}))
(fact (EshIntra <"n6" "g"> {"a"}))
(fact (EshIntra <"n7" "a"> {"a"}))

(rule (EshCallStart <<call "zero"> target> {"zero"}) ((CallGraph {<call target>})))
(rule (EshEndReturn <target "zero" call> {"zero"})   ((CallGraph {<call target>})))

(fact (EshCallStart <<"n2" "x"> "p"> {"a"}))
(fact (EshCallStart <<"n2" "g"> "p"> {"g"}))
(fact (EshEndReturn <"p" "g" "n2"> {"g"}))

(fact (EshCallStart <<"n7" "a"> "p"> {"a"}))
(fact (EshCallStart <<"n7" "g"> "p"> {"g"}))
(fact (EshEndReturn <"p" "g" "n7"> {"g"}))

// Entrypoint
(fact (PathEdge {<"zero" <"smain" "zero">>}))
