/*
 * Copyright 2017 Magnus Madsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.SimplifiedAst.Expression
import ca.uwaterloo.flix.language.ast.{SimplifiedAst, Symbol}
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}

import scala.collection.mutable

/**
  * The Tree Shaking phase removes all unused function definitions.
  *
  * A function is considered reachable if it:
  *
  * (a) The main function is always reachable.
  * (b) A function marked with @benchmark or @test is reachable.
  * (c) A function that appears in a lattice component.
  * (d) A function that appears in a property.
  * (e) A function that appears as a special operator.
  * (f) Appear in a function which itself is reachable.
  *
  */
object TreeShaker extends Phase[SimplifiedAst.Root, SimplifiedAst.Root] {
  /**
    * Performs tree shaking on the given AST `root`.
    */
  def run(root: SimplifiedAst.Root)(implicit flix: Flix): Validation[SimplifiedAst.Root, CompilationError] = flix.phase("TreeShaker") {
    /**
      * A set used to collect the definition symbols of reachable functions.
      */
    val reachableFunctions: mutable.Set[Symbol.DefnSym] = mutable.Set.empty ++ root.reachable

    /**
      * A queue of function definitions to be processed recursively.
      *
      * For example, if the queue contains the entry:
      *
      * -   f
      *
      * it means that the function definition f should be considered to determine new reachable functions.
      */
    val queue: mutable.Queue[SimplifiedAst.Def] = mutable.Queue.empty

    /**
      * Returns the function symbols reachable from the given Expression `e0`.
      */
    def visitExp(e0: Expression): Set[Symbol.DefnSym] = e0 match {
      case Expression.Unit =>
        Set.empty

      case Expression.True =>
        Set.empty

      case Expression.False =>
        Set.empty

      case Expression.Char(_) =>
        Set.empty

      case Expression.Float32(_) =>
        Set.empty

      case Expression.Float64(_) =>
        Set.empty

      case Expression.Int8(_) =>
        Set.empty

      case Expression.Int16(_) =>
        Set.empty

      case Expression.Int32(_) =>
        Set.empty

      case Expression.Int64(_) =>
        Set.empty

      case Expression.BigInt(_) =>
        Set.empty

      case Expression.Str(_) =>
        Set.empty

      case Expression.Var(_, _, _) =>
        Set.empty

      case Expression.Def(sym, _, _) =>
        Set(sym)

      case Expression.Lambda(_, exp, _, _) =>
        visitExp(exp)

      case Expression.Closure(sym, _, _, _) =>
        Set(sym)

      case Expression.ApplyClo(exp, args, _, _) =>
        visitExp(exp) ++ visitExps(args)

      case Expression.ApplyDef(sym, args, _, _) =>
        Set(sym) ++ visitExps(args)

      case Expression.ApplyCloTail(exp, args, _, _) =>
        visitExp(exp) ++ visitExps(args)

      case Expression.ApplyDefTail(sym, args, _, _) =>
        Set(sym) ++ visitExps(args)

      case Expression.ApplySelfTail(sym, _, args, _, _) =>
        Set(sym) ++ visitExps(args)

      case Expression.Unary(_, _, exp, _, _) =>
        visitExp(exp)

      case Expression.Binary(_, _, exp1, exp2, _, _) =>
        visitExp(exp1) ++ visitExp(exp2)

      case Expression.IfThenElse(exp1, exp2, exp3, _, _) =>
        visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

      case Expression.Branch(exp, branches, _, _) =>
        visitExp(exp) ++ visitExps(branches.values.toList)

      case Expression.JumpTo(_, _, _) =>
        Set.empty

      case Expression.Let(_, exp1, exp2, _, _) =>
        visitExp(exp1) ++ visitExp(exp2)

      case Expression.Is(_, _, exp, _) =>
        visitExp(exp)

      case Expression.Tag(_, _, exp, _, _) =>
        visitExp(exp)

      case Expression.Untag(_, _, exp, _, _) =>
        visitExp(exp)

      case Expression.Index(exp, _, _, _) =>
        visitExp(exp)

      case Expression.Tuple(elms, _, _) =>
        visitExps(elms)

      case Expression.RecordEmpty(_, _) =>
        Set.empty

      case Expression.RecordSelect(exp, _, _, _) =>
        visitExp(exp)

      case Expression.RecordExtend(_, value, rest, _, _) =>
        visitExp(value) ++ visitExp(rest)

      case Expression.RecordRestrict(_, rest, _, _) =>
        visitExp(rest)

      case Expression.ArrayLit(elms, _, _) =>
        visitExps(elms)

      case Expression.ArrayNew(elm, len, _, _) =>
        visitExp(elm) ++ visitExp(len)

      case Expression.ArrayLoad(base, index, _, _) =>
        visitExp(base) ++ visitExp(index)

      case Expression.ArrayStore(base, index, elm, _, _) =>
        visitExp(base) ++ visitExp(index) ++ visitExp(elm)

      case Expression.ArrayLength(base, _, _) =>
        visitExp(base)

      case Expression.ArraySlice(base, startIndex, endIndex, _, _) =>
        visitExp(base) ++ visitExp(startIndex) ++ visitExp(endIndex)

      case Expression.Ref(exp, _, _) =>
        visitExp(exp)

      case Expression.Deref(exp, _, _) =>
        visitExp(exp)

      case Expression.Assign(exp1, exp2, tpe, _) =>
        visitExp(exp1) ++ visitExp(exp2)

      case Expression.Existential(_, exp, _) =>
        visitExp(exp)

      case Expression.Universal(_, exp, _) =>
        visitExp(exp)

      case Expression.Cast(exp, _, _) =>
        visitExp(exp)

      case Expression.TryCatch(exp, rules, _, _) =>
        visitExp(exp) ++ visitExps(rules.map(_.exp))

      case Expression.InvokeConstructor(_, args, _, _) =>
        visitExps(args)

      case Expression.InvokeMethod(_, exp, args, _, _) =>
        visitExp(exp) ++ visitExps(args)

      case Expression.InvokeStaticMethod(_, args, _, _) =>
        visitExps(args)

      case Expression.GetField(_, exp, _, _) =>
        visitExp(exp)

      case Expression.PutField(_, exp1, exp2, _, _) =>
        visitExp(exp1) ++ visitExp(exp2)

      case Expression.GetStaticField(_, _, _) =>
        Set.empty

      case Expression.PutStaticField(_, exp, _, _) =>
        visitExp(exp)

      case Expression.NewChannel(exp, _, _) =>
        visitExp(exp)

      case Expression.GetChannel(exp, _, _) =>
        visitExp(exp)

      case Expression.PutChannel(exp1, exp2, _, _) =>
        visitExp(exp1) ++ visitExp(exp2)

      case Expression.SelectChannel(rules, default, _, _) =>
        val rs = visitExps(rules.map(_.chan)) ++ visitExps(rules.map(_.exp))
        val d = default.map(visitExp).getOrElse(Set.empty)
        rs ++ d

      case Expression.ProcessSpawn(exp, _, _) =>
        visitExp(exp)

      case Expression.FixpointConstraintSet(cs0, _, _) =>
        cs0.foldLeft(Set.empty[Symbol.DefnSym]) {
          case (fvs, c) => fvs ++ visitConstraint(c)
        }
      case Expression.FixpointCompose(exp1, exp2, _, _) =>
        visitExp(exp1) ++ visitExp(exp2)

      case Expression.FixpointSolve(exp, _, _, _) =>
        visitExp(exp)

      case Expression.FixpointProject(_, exp, _, _) =>
        visitExp(exp)

      case Expression.FixpointEntails(exp1, exp2, _, _) =>
        visitExp(exp1) ++ visitExp(exp2)

      case Expression.FixpointFold(name, exp1, exp2, exp3, _, _) =>
        visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

      case Expression.HoleError(_, _, _) =>
        Set.empty

      case Expression.MatchError(_, _) =>
        Set.empty

      case Expression.LambdaClosure(_, _, _, _, _) =>
        throw InternalCompilerException(s"Unexpected expression: '${e0.getClass}'.")

      case Expression.Apply(_, _, _, _) =>
        throw InternalCompilerException(s"Unexpected expression: '${e0.getClass}'.")
    }

    /**
      * Returns the function symbols reachable from `es`.
      */
    def visitExps(es: List[Expression]): Set[Symbol.DefnSym] = es.map(visitExp).fold(Set())(_ ++ _)

    /**
      * Returns the function symbols reachable from the given constraint `c0`.
      */
    def visitConstraint(c0: SimplifiedAst.Constraint): Set[Symbol.DefnSym] = {
      val headSymbols = c0.head match {
        case SimplifiedAst.Predicate.Head.Atom(_, _, terms, tpe, loc) =>
          terms.map(visitHeadTerm).fold(Set.empty)(_ ++ _)

        case SimplifiedAst.Predicate.Head.Union(exp, _, _) =>
          visitExp(exp)
      }

      val bodySymbols = c0.body.map {
        case SimplifiedAst.Predicate.Body.Atom(_, _, polarity, terms, tpe, loc) =>
          terms.map(visitBodyTerm).fold(Set.empty)(_ ++ _)

        case SimplifiedAst.Predicate.Body.Guard(exp, loc) =>
          visitExp(exp)
      }.fold(Set())(_ ++ _)

      headSymbols ++ bodySymbols
    }

    /**
      * Returns the function symbols reachable from the given SimplifiedAst.Term.Head `head`.
      */
    def visitHeadTerm(h0: SimplifiedAst.Term.Head): Set[Symbol.DefnSym] = {
      h0 match {
        case SimplifiedAst.Term.Head.QuantVar(sym, tpe, loc) => Set.empty
        case SimplifiedAst.Term.Head.CapturedVar(sym, tpe, loc) => Set.empty
        case SimplifiedAst.Term.Head.Lit(lit, tpe, loc) => visitExp(lit)
        case SimplifiedAst.Term.Head.App(exp, args, tpe, loc) => visitExp(exp)
      }
    }

    /**
      * Returns the function symbols reachable from the given SimplifiedAst.Term.Body `body`.
      */
    def visitBodyTerm(b0: SimplifiedAst.Term.Body): Set[Symbol.DefnSym] = {
      b0 match {
        case SimplifiedAst.Term.Body.Wild(tpe, loc) => Set.empty
        case SimplifiedAst.Term.Body.QuantVar(sym, tpe, loc) => Set.empty
        case SimplifiedAst.Term.Body.CapturedVar(sym, tpe, loc) => Set.empty
        case SimplifiedAst.Term.Body.Lit(exp, tpe, loc) => visitExp(exp)
      }
    }

    /**
      * Adds the function `sym` to the set of reachable functions.
      */
    def newReachableDefinitionSymbol(sym: Symbol.DefnSym): Unit = {
      // If `sym` has not already been determined reachable, look up its definition in `root`.
      if (!reachableFunctions.contains(sym)) {
        root.defs.get(sym) match {
          case Some(defn) =>
            reachableFunctions.add(sym)
            queue.enqueue(defn)
          // If `sym` is not defined in `root`, leave this for error checking later.
          case None =>
        }
      }
    }

    /*
     * (a) The main function is always reachable.
     */
    reachableFunctions.add(Symbol.mkDefnSym("main"))

    /*
     * (b) A function marked with @benchmark, @test or as an entry point is reachable.
     */
    for ((sym, defn) <- root.defs) {
      val isEntryPoint = defn.mod.isEntryPoint
      val isBenchmark = defn.ann.isBenchmark
      val isTest = defn.ann.isTest
      if (isEntryPoint || isBenchmark || isTest) {
        reachableFunctions.add(sym)
      }
    }

    /*
     * (c) A function that appears in a lattice component.
     */
    reachableFunctions ++= root.latticeOps.values.map {
      case SimplifiedAst.LatticeOps(tpe, bot, top, equ, leq, lub, glb, loc) =>
        Set(bot, top, equ, leq, lub, glb)
    }.fold(Set())(_ ++ _)

    /*
     * (d) A function that appears in a property.
     */
    reachableFunctions ++= root.properties.map {
      case SimplifiedAst.Property(law, defn, exp) => visitExp(exp) + law + defn
    }.fold(Set())(_ ++ _)

    /*
     * (e) A function that appears as a special operator.
     */
    reachableFunctions ++= root.specialOps.values.flatMap(_.values)

    /*
     * (f) Appear in a function which itself is reachable.
     */
    reachableFunctions.foreach {
      root.defs.get(_) match {
        case None => // nop
        case Some(defn) => queue.enqueue(defn)
      }
    }

    // Compute transitively reachable function.
    while (queue.nonEmpty) {
      // Extract a function body from the queue and search for other reachable functions.
      for (sym <- visitExp(queue.dequeue().exp)) {
        newReachableDefinitionSymbol(sym)
      }
    }

    // Compute the live defs.
    val liveDefs = root.defs.filter {
      case (sym, _) => reachableFunctions.contains(sym)
    }

    // Reassemble the AST.
    root.copy(defs = liveDefs).toSuccess
  }
}
