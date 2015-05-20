// Basic types
// =========
(def-type Object Str)
(def-type Variable Str)
(def-type Location Str)

// Inputs
// =========
(def-type AddrOf (Set <Variable Object>))
(def-type Copy (Set <Variable Variable>))
(def-type Store (Set <Location Variable Variable>))
(def-type Load (Set <Location Variable Variable>))
(def-type CFG (Set <Location Location>))

// Outputs
// =========
(def-type Pt (-> Variable (Set Object)))
(def-type PtSU (-> <Location Object> (Set Object)))

// Other types
// =========
(def-type SULattice (variant ((Top) (Single Object) (Bot))))
(def-bot SULattice Bot)
(def-leq SULattice (e1 SULattice e2 SULattice)
  (match <e1 e2>
    (case <Bot _> true)
    (case <_ Top> true)
    (case <(Single s1) (Single s2)> (== s1 s2))
    (case _ false)
))
(def-lub SULattice (e1 SULattice e2 SULattice)
  (match <e1 e2>
    (case <Bot e2> e2)
    (case <e1 Bot> e1)
    (case <(Single s1) (Single s2)> (if (== s1 s2) (Single s1) Top))
    (case _ Top)
))

// Other lattices
// =========
(def-type SUBefore (-> <Location Object> SULattice))
(def-type SUAfter (-> <Location Object> SULattice))
(def-type PtH (-> Object (Set Object)))
(def-type KillEmpty (Set Location))
(def-type KillNot (-> Location (Set Object)))
(def-type AllObjects (Set Object))
(def-type NonStore (Set Location)) // all locations that are not store instructions (since we don't have stratified negation)


// Rules
// =========

// AddrOf
// ---------
// pt(p,a) :- addrOf(p,a).
(rule (Pt p {a}) ((AddrOf {<p a>})))

// Copy
// ---------
// pt(p,a) :- copy(p,q), pt(q,a).
(rule (Pt p as) ((Copy {<p q>}) (Pt q as)))

// Store
// ---------
// su-after(l,a,t) :- store(l,p,q), pt(p,a), pt(q,b), t = SINGLETON(b).
(rule (SUAfter <l a> (Single b))
  ((Store {<l p q>}) (Pt p {a}) (Pt q {b})))

// pt-h(a,b) :- store(l,p,q), pt(p,a), pt(q,b).
(rule (PtH a bs)
  ((Store {<_ p q>}) (Pt p {a}) (Pt q bs)))

// Load
// ---------
// pt(p,b) :- load(l,p,q), pt(q,a), ptsu(l,a,b).
(rule (Pt p bs)
  ((Load {<l p q>}) (Pt q {a}) (PtSU <l a> bs)))

// CFlow
// ---------
//su-before(l2,a,t) :- cfg(l1,l2), su-after(l1,a,t).
(rule (SUBefore <l2 a> t) ((CFG {<l1 l2>}) (SUAfter <l1 a> t)))

// Preserve
// ---------
//su-after(l,a,t) :- su-before(l,a,t), NOT kill(l,a).
(rule (SUAfter <l a> t) ((SUBefore <l a> t) (KillEmpty {l})))
(rule (SUAfter <l a> t) ((SUBefore <l a> t) (KillNot l {a})))

// PtSu
// ---------
//ptsu(l,a,b) :- su-before(l,a,t), t = SINGLETON(b).
(rule (PtSU <l a> {b}) ((SUBefore <l a> (Single b))))

//ptsu(l,a,b) :- su-before(l,a,t), t >= TOP, pt-h(a,b).
(rule (PtSU <l a> bs) ((SUBefore <l a> Top) (PtH a bs)))

// Kill
// ---------
(rule (AllObjects {a}) ((AddrOf {<_ a>})))
// KillEmpty l is true if the kill set of l is the empty set
(rule (KillEmpty {l}) ((Store {<l p q>}) (Pt p {a b})))
(rule (KillEmpty {l}) ((NonStore {l})))
// the inverse of the kill set of l
(rule (KillNot l {a}) ((Store {<l p q>}) (Pt p {b}) (AllObjects {a})) (!= a b))

// NonStore
// ---------
(rule (NonStore {l}) ((Load {<l _ _>})))

// Example
// =========

// p = &a
// mb = &b
// *p = mb // l1
// q = *p  // l2
// mc = &c
// *p = mc // l3
// r = *p  // l4
// if(*) {
  // p2 = &d
  // mf = &f
  // *p2 = mf  // l5
// }
// p3 = phi(p, p2)
// s = *p3 // l6
// me = &e
// *p3 = me // l7
// t = *p3  // l8

// p = &a
(fact (AddrOf {<"p" "a">}))
// mb = &b
(fact (AddrOf {<"mb" "b">}))
// *p = mb // l1
(fact (Store {<"l1" "p" "mb">}))
// q = *p  // l2
(fact (Load {<"l2" "q" "p">}))
// mc = &c
(fact (AddrOf {<"mc" "c">}))
// *p = mc // l3
(fact (Store {<"l3" "p" "mc">}))
// r = *p  // l4
(fact (Load {<"l4" "r" "p">}))
// if(*) {
  // p2 = &d
(fact (AddrOf {<"p2" "d">}))
  // mf = &f
(fact (AddrOf {<"mf" "f">}))
  // *p2 = mf  // l5
(fact (Store {<"l5" "p2" "mf">}))
// }
// p3 = phi(p, p2)
(fact (Copy {<"p3" "p">}))
(fact (Copy {<"p3" "p2">}))
// s = *p3 // l6
(fact (Load {<"l6" "s" "p3">}))
// me = &e
(fact (AddrOf {<"me" "e">}))
// *p3 = me // l7
(fact (Store {<"l7" "p3" "me">}))
// t = *p3  // l8
(fact (Load {<"l8" "t" "p3">}))

(fact (CFG {
    <"l1" "l2">
    <"l2" "l3">
    <"l3" "l4">
    <"l4" "l5"> <"l4" "l6">
    <"l5" "l6">
    <"l6" "l7">
    <"l7" "l8">
}))

