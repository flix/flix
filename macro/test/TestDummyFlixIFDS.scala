import org.scalatest.FunSuite

import api.Flix
import impl.logic.Value
import Macros._

class TestDummyFlixIFDS extends FunSuite {
  // Dummy trait implementations
  trait MLabel
  trait Itm
  trait Method

  test("Dummy FlixIFDS") {
    val flix = new Flix

    flix += ("fneshintra" -> valueWrapperFunc(eshIntra _))
    flix += ("fneshcallstart" -> valueWrapperFunc(eshCallStart _))
    flix += ("fneshendreturn" -> valueWrapperFunc(eshEndReturn _))
    flix += ("fncfg" -> valueWrapperFunc(cfg _))
    flix += ("fncallgraph" -> valueWrapperFunc(callGraph _))
    flix += ("fnstartnode" -> valueWrapperFunc(startNode _))
    flix += ("fnendnode" -> valueWrapperFunc(endNode _))
    flix += ("fnseed" -> valueWrapperFunc(seed _))

    flix +=
      """
// Basic data types
// ===========
(def-type Fact Native)
(def-type Node Native)
(def-type Proc Native)

// Inputs
// ===========
(def-type EshIntra (-> <Node Fact> (Set Fact)))
(def-type EshCallStart (-> <<Node Fact> Proc> (Set Fact)))
(def-type EshEndReturn (-> <Fact Proc Fact Node> (Set Fact)))
// Call-to-Return edges should be included above in EshIntra
(def-type CFG (Set <Node Node>))
(def-type CallGraph (Set <Node Proc>))
(def-type StartNode (Set <Proc Node>))
(def-type EndNode (Set <Proc Node>))

// Internal lattices
// ===========
(def-type PathEdge (Set <Fact <Node Fact>>))
(def-type PathEdgeNodes (Set Node))
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
   (EshCallStart <<call d4> target> {d1}) (PathEdge {<d1 <end d2>>}) (EshEndReturn <d4 target d2 call> d5s)))

// tabulate result
(rule (Result n {d2}) ((PathEdge {<_ <n d2> >})))


// Interaction with Scala functions
// ===========
(rule (EshIntra <n d> (fneshintra <n d>)) ((PathEdge {<_ <n d>>})))
(rule (EshCallStart <<n d> target> (fneshcallstart <<n d> target>)) ((PathEdge {<_ <n d>>}) (CallGraph {<n target>})))
(rule (EshEndReturn <d1 target d2 n> (fneshendreturn <d1 target d2 n>))
  ((PathEdge {<_ <n d1>>}) (CallGraph {<n target>}) (EndNode {<target end>}) (PathEdge {<_ <end d2>>})))
(rule (PathEdgeNodes {node}) ((PathEdge {<_ <node _>>})))
(rule (CFG (fncfg src)) ((PathEdgeNodes {src})))
(rule (CallGraph (fncallgraph src)) ((PathEdgeNodes {src})))
(rule (StartNode {<proc (fnstartnode proc)>}) ((CallGraph {<_ proc>})))
(rule (EndNode {<proc (fnendnode proc)>}) ((CallGraph {<_ proc>})))
(fact (PathEdge (fnseed 0)))
      """

    def eshIntra(node: MLabel, fact: Itm) = {
//      val inst = scene.inst(node)
//      inst match {
//        case callSite: CallSite => transfer.callFlow(fact, node, callSite)
//        case _: End => Set.empty
//        case _: Start => Set(fact)
//        case _ => transfer.flow(fact, node, inst)
//      }
      Set(fact)
    }

    def eshCallStart(tpl: (MLabel, Itm), proc: Method) = {
//      val (node, fact) = tpl
//      scene.inst(node) match {
//        case call: CallSite => transfer.passArgs(fact, node, call, scene.startinstr(proc), proc)
//        case _ => Set.empty
//      }
      Set.empty
    }

    def eshEndReturn(callerFact: Itm, proc: Method, fact: Itm, node: MLabel) = {
//      scene.inst(node) match {
//        case call: CallSite => transfer.returnVal(callerFact, fact, node, call, scene.endinstr(proc))
//        case _ => Set.empty
//      }
      Set.empty
    }

    def cfg(src: MLabel) = {
//      scene.succs(src).map(succ => (src, succ))
      Set.empty
    }

    def callGraph(src: MLabel) = {
//      scene.cg.callees(src).map(callee => (src, callee))
      Set.empty
    }

    def startNode(proc: Method) = {
//      scene.start(proc)
      proc
    }

    def endNode(proc: Method) = {
//      scene.end(proc)
        proc
    }

    def seed() = {
//      seeds match {
//        case Some(ss) =>
//          ss.map({ case (node: MLabel, fact: Itm) => (fact, (node, fact)) })
//        case None =>
//          Set((transfer.zero, (scene.start(scene.cg.entrypoint), transfer.zero)))
//      }
      Set.empty
    }

    flix.solve()
    flix.print()
  }
}
