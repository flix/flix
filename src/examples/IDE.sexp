// Basic data types
// ===========
(def-type Fact Str)
(def-type Node Str)
(def-type Proc Str)

// Example (constant propagation) from IDE paper
// ===========
(def-type Value (variant ((Top) (Cst Int) (Bot))))
(def-fun lub (e1 Value e2 Value)
    (match <e1 e2>
        (case <Bot x>   x)
        (case <x Bot>   x)
        (case <(Cst n1) (Cst n2)>  (if (== n1 n2) (Cst n1) Top))
        (case _         Top)))
(def-lub Value (e1 Value e2 Value) (lub e1 e2))
(def-bot Value Bot)
(def-leq Value (e1 Value e2 Value)
    (match <e1 e2>
        (case <Bot _>                 true)
        (case <(Cst n1) (Cst n2)>     (== n1 n2))
        (case <_ Top>                 true)
        (case _                       false)))
(def-fun sum (e1 Value e2 Int)
    (match e1
        (case (Cst n) (Cst (+ n e2)))
        (case _         e1)))
(def-fun prod (e1 Value e2 Int)
    (match <e1 e2>
        (case <(Cst n) _> (Cst (* n e2)))
        (case <Bot _>     Bot)
        (case <_ zero>    (if (== zero 0) (Cst 0) e1))
        (case _           e1)))

(def-type Transformer (variant ((BotTrans) (NonBotTrans <Int Int Value>))))
(def-fun compose (t1 Transformer t2 Transformer)
    (match <t1 t2>
        (case <_ BotTrans> BotTrans)
        (case <BotTrans (NonBotTrans <a b c>)>
            (match c
                (case Bot BotTrans)
                (case Top (NonBotTrans <0 0 Top>))
                (case (Cst cc) (NonBotTrans <0 cc c>))))
        (case <(NonBotTrans <a2 b2 c2>) (NonBotTrans <a1 b1 c1>)>
            (NonBotTrans <(* a1 a2) (+ (* a1 b2) b1) (lub (sum (prod c2 a1) b1) c1)>))
    )
)

// from paper: f = \l. a*l+b meet c; f(Top) = Top
// paper is upside-down, so: f = \l. a*l+b join c; f(Bot) = Bot
(def-fun apply (t Transformer l Value)
    (match t
        (case BotTrans Bot)
        (case (NonBotTrans <a b c>)
            (if (== l Bot) Bot (lub (sum (prod l a) b) c)))))

(def-fun translub (t1 Transformer t2 Transformer)
    (match <t1 t2>
        (case <BotTrans _> t2)
        (case <_ BotTrans> t1)
        (case <(NonBotTrans <a b c1>) (NonBotTrans <a b c2>)> (NonBotTrans <a b (lub c1 c2)>))
        (case <(NonBotTrans <a1 b1 c2>) (NonBotTrans <a2 b2 c2>)>
            (if (== 0 (% (- b1 b2) (- a2 a1)))
                // is divisible
                (NonBotTrans <a1 b2 (lub (Cst (+ (* a1 (/ (- b1 b2) (- a2 a1))) b1)) (lub c1 c2))>)
                // is not divisible
                (NonBotTrans <1 0 Top>)
            )
        )
    )
)

(def-lub Transformer (t1 Transformer t2 Transformer) (translub t1 t2))
(def-leq Transformer (t1 Transformer t2 Transformer) (== t2 (translub t1 t2)))
(def-bot Transformer BotTrans)

// Inputs
// ===========
(def-type EshIntra (-> <Node Fact> (-> Fact Transformer)))
(def-type EshCallStart (-> <<Node Fact> Proc> (-> Fact Transformer)))
(def-type EshEndReturn (-> <Proc Fact Node> (-> Fact Transformer)))
// Call-to-Return edges should be included above in EshIntra
(def-type CFG (Set <Node Node>))
(def-type CallGraph (Set <Node Proc>))
(def-type StartNode (Set <Proc Node>)) // TODO: should really be a function, but the result type is not a lattice
(def-type EndNode (Set <Proc Node>))

//(def-type Value ...) // lattice L
//(def-type Transformer ...) // lattice L -> L

(def-type Identity Transformer)

// Internal lattices
// ===========
(def-type JumpFn (-> <Fact <Node Fact>> Transformer))
(def-type SummaryFn (-> <Node Fact> (-> Fact Transformer)))
(def-type ResultProc (-> Proc (-> Fact Value)))
(def-type InProc (Set <Proc Node>))

// Output
// ===========
(def-type Result (-> Node (-> Fact Value)))


// Rules
// ===========
// nodes in a given procedure
(rule (InProc {<p start>}) ((StartNode {<p start>})))
(rule (InProc {<p m>}) ((InProc {<p n>}) (CFG {<n m>})))

// intraproc
(rule (JumpFn <d1 <m d3>> (compose long short))
  ((CFG {<n m>}) (JumpFn <d1 <n d2>> long) (EshIntra <n d2> d3 short)))

// use summary
(rule (JumpFn <d1 <m d3>> (compose caller summary))
  ((CFG {<n m>}) (JumpFn <d1 <n d2>> caller) (SummaryFn <n d2> d3 summary)))

// call-to-start
(rule (JumpFn <d3 <start d3>> id)
  ((Identity id)
   (JumpFn <d1 <call d2>> nonbottom1)  (CallGraph {<call target>})
   (EshCallStart <<call d2> target> d3 nonbottom2) (StartNode {<target start>}))
   (and (!= nonbottom1 {}) (!= nonbottom2 {})))

// compute summary
(rule (SummaryFn <call d4> d5 (compose (compose cs se) er))
  ((CallGraph {<call target>}) (StartNode {<target start>}) (EndNode {<target end>})
   (EshCallStart <<call d4> target> d1 cs) (JumpFn <d1 <end d2>> se) (EshEndReturn <target d2 call> d5 er)))

// tabulate result
(rule (Result n d (apply fn vp))
  ((ResultProc proc dp vp) (InProc {<proc n>}) (JumpFn <dp <n d>> fn)))
(rule (ResultProc proc dp (apply cs v))
  ((Result call d v) (EshCallStart <<call d> proc> dp cs)))

// Rules for Example (constant propagation) from IDE paper
// ===========
(fact (Identity (NonBotTrans <1 0 Bot>)))

(fact (CFG {
  <"smain" "n1">
  <"n1" "n2">
  <"n2" "n3">
  <"n3" "emain">
}))

(fact (CFG {
  <"sp" "n4">
  <"n4" "n5"> <"n4" "n9">
  <"n5" "n6">
  <"n6" "n7">
  <"n7" "n8">
  <"n8" "n9">
  <"n9" "ep">
}))

(fact (StartNode {<"main" "smain"> <"p" "sp">}))
(fact (EndNode   {<"main" "emain"> <"p" "ep">}))

(fact (CallGraph {<"n1" "p"> <"n6" "p">}))

(rule (EshIntra <n "zero"> "zero" id) ((CFG {<n _>}) (Identity id)))

(fact (EshIntra <"smain" "zero"> "x" BotTrans))
(rule (EshIntra <"n2" "x"> "x" id) ((Identity id)))
(rule (EshIntra <"n3" "x"> "x" id) ((Identity id)))

(rule (EshIntra <"sp" "a"> "a" id) ((Identity id)))
(rule (EshIntra <"sp" "x"> "x" id) ((Identity id)))
(rule (EshIntra <"n4" "a"> "a" id) ((Identity id)))
(rule (EshIntra <"n4" "x"> "x" id) ((Identity id)))
(fact (EshIntra <"n5" "a"> "a" (NonBotTrans <1 -2 Bot>)))
(rule (EshIntra <"n5" "x"> "x" id) ((Identity id)))
(rule (EshIntra <"n6" "a"> "a" id) ((Identity id)))
(rule (EshIntra <"n7" "a"> "a" id) ((Identity id)))
(rule (EshIntra <"n7" "x"> "x" id) ((Identity id)))
(fact (EshIntra <"n8" "a"> "a" (NonBotTrans <1 2 Bot>)))
(rule (EshIntra <"n8" "x"> "x" id) ((Identity id)))
(rule (EshIntra <"n9" "a"> "a" id) ((Identity id)))
(fact (EshIntra <"n9" "a"> "x" (NonBotTrans <-2 5 Bot>)))

(rule (EshCallStart <<call "zero"> target> "zero" id) ((CallGraph {<call target>}) (Identity id)))
(rule (EshEndReturn <target "zero" call> "zero" id) ((CallGraph {<call target>}) (Identity id)))

(fact (EshCallStart <<"n1" "zero"> "p"> "a" (NonBotTrans <0 7 Bot>)))
(rule (EshCallStart <<"n1" "x"> "p"> "x" id) ((Identity id)))
(rule (EshEndReturn <"p" "x" "n1"> "x" id) ((Identity id)))

(rule (EshCallStart <<"n6" "a"> "p"> "a" id) ((Identity id)))
(rule (EshCallStart <<"n6" "x"> "p"> "x" id) ((Identity id)))
(rule (EshEndReturn <"p" "x" "n6"> "x" id) ((Identity id)))

// Entrypoint
(rule (JumpFn <"zero" <"smain" "zero">> id) ((Identity id)))
(fact (ResultProc "main" "zero" Top))
