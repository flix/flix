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
import ca.uwaterloo.flix.language.ast.{SimplifiedAst, Symbol}
import ca.uwaterloo.flix.language.ast.SimplifiedAst.{Expression, Definition}
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

import scala.collection.mutable

/**
  * The Tree Shaking phase removes all unused function definitions.
  *
  * A function is considered reachable if it:
  *
  * (a) Appears in the global namespaces, takes zero arguments, and is not marked as synthetic.
  * (b) Appears in a fact or a rule as a filter/transfer function.
  * (c) Appears in a lattice declaration.
  * (d) Appears in a function which itself is reachable.
  */

object TreeShaker extends Phase[SimplifiedAst.Root, SimplifiedAst.Root] {

  /**
    * Performs tree shaking on the given AST `root`.
    */
  def run(root: SimplifiedAst.Root)(implicit flix: Flix): Validation[SimplifiedAst.Root, CompilationError] = {

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
    val queue: mutable.Queue[Definition.Constant] = mutable.Queue.empty

    /**
      * Returns true iff the function definition `defn` is initially reachable by (a).
      *
      * That is, returns true iff `defn` satisfies:
      *
      *   (a) Appears in the global namespaces, takes zero arguments, and is not marked as synthetic.
      */
    def isReachableRoot(defn: Definition.Constant): Boolean = {
      defn.sym.namespace.isEmpty && defn.formals.isEmpty && !defn.isSynthetic
    }

    /**
      * Returns the function symbols reachable from `hs`.
      */
    def visitHeadTerms(hs: List[SimplifiedAst.Term.Head]): Set[Symbol.DefnSym] = hs.map(visitHeadTerm).fold(Set())(_++_)

    /**
      * Returns the function symbols reachable from the given SimplifiedAst.Term.Head `head`.
      */
    def visitHeadTerm(h0: SimplifiedAst.Term.Head): Set[Symbol.DefnSym] = {
      h0 match {
        case SimplifiedAst.Term.Head.Lit(lit, tpe, loc) => visitExp(lit)
        case SimplifiedAst.Term.Head.App(sym, args, tpe, loc) => Set(sym)
        case _ => Set.empty
      }
    }

    /**
      * Returns the function symbols reachable from `bs`.
      */
    def visitBodyTerms(bs: List[SimplifiedAst.Term.Body]): Set[Symbol.DefnSym] = bs.map(visitBodyTerm).fold(Set())(_++_)

    /**
      * Returns the function symbols reachable from the given SimplifiedAst.Term.Body `body`.
      */
    def visitBodyTerm(b0: SimplifiedAst.Term.Body): Set[Symbol.DefnSym] = {
      b0 match {
        case SimplifiedAst.Term.Body.Lit(exp, tpe, loc) => visitExp(exp)
        case _ => Set.empty
      }
    }

    /**
      * Returns the function symbols reachable from `es`.
      */
    def visitExps(es: List[Expression]): Set[Symbol.DefnSym] =  es.map(visitExp).fold(Set())(_++_)

    /**
      * Returns the function symbols reachable from the given Expression `e0`.
      */
    def visitExp(e0: Expression): Set[Symbol.DefnSym] =  e0 match {
      case Expression.Ref(sym, tpe, loc) => Set(sym)
      case Expression.Lambda(args, body, tpe, loc) => visitExp(body)
      case Expression.Hook(hook, tpe, loc) => Set(hook.sym)
      case Expression.MkClosure(lambda, freeVars, tpe, loc) => visitExp(lambda)
      case Expression.MkClosureRef(ref, freeVars, tpe, loc) => visitExp(ref)
      case Expression.ApplyRef(sym, args, tpe, loc) => visitExps(args) + sym
      case Expression.ApplyTail(sym, formals, actuals, tpe, loc) => visitExps(actuals) + sym
      case Expression.Apply(exp, args, tpe, loc) => visitExps(args) ++ visitExp(exp)
      case Expression.Unary(op, exp, tpe, loc) => visitExp(exp)
      case Expression.Binary(op, exp1, exp2, tpe, loc) => visitExp(exp1) ++ visitExp(exp2)
      case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) => visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)
      case Expression.Let(sym, exp1, exp2, tpe, loc) => visitExp(exp1) ++ visitExp(exp2)
      case Expression.Is(exp, tag, loc) => visitExp(exp)
      case Expression.Tag(sym, tag, exp, tpe, loc) => visitExp(exp)
      case Expression.Untag(tag, exp, tpe, loc) => visitExp(exp)
      case Expression.Index(base, offset, tpe, loc) => visitExp(base)
      case Expression.Tuple(elms, tpe, loc) => visitExps(elms)
      case Expression.Existential(fparam, exp, loc) => visitExp(exp)
      case Expression.Universal(fparam, exp, loc) => visitExp(exp)
      case _ => Set.empty
    }

    /**
      * Adds the function `sym` to the set of reachable functions.
      */
    def newReachableDefinitionSymbol(sym: Symbol.DefnSym): Unit = {
      // If `sym` has not already been determined reachable, look up its definition in `root`.
      if (!reachableFunctions.contains(sym)) {
        root.definitions.get(sym) match {
          case Some(defn) =>
            reachableFunctions.add(sym)
            queue.enqueue(defn)
          // If `sym` is not defined in `root`, leave this for error checking later.
          case _ =>
        }
      }
    }

    /*
     * We can now use these helper functions to perform tree shaking.
     */

    // Start the timer.
    val t = System.nanoTime()

    /*
     * Find reachable functions that:
     *
     * (a) Appear in the global namespaces, take zero arguments, and are not marked as synthetic.
     */
    for ((sym, defn) <- root.definitions) {
      if (isReachableRoot(defn)) {
        reachableFunctions.add(sym)
      }
    }

    /*
     * Find reachable functions that:
     *
     * (b) Appear in a fact or a rule as a filter/transfer function.
     */
    for (stratum <- root.strata) {
      for (constraint <- stratum.constraints) {
        reachableFunctions ++= (constraint.head match {
          case SimplifiedAst.Predicate.Head.Positive(sym, terms, loc) => visitHeadTerms(terms)
          case SimplifiedAst.Predicate.Head.Negative(sym, terms, loc) => visitHeadTerms(terms)
          case _ => Set.empty
        })

        reachableFunctions ++= constraint.body.map {
          case SimplifiedAst.Predicate.Body.Positive(sym, terms, loc) => visitBodyTerms(terms)
          case SimplifiedAst.Predicate.Body.Negative(sym, terms, loc) => visitBodyTerms(terms)
          case SimplifiedAst.Predicate.Body.Filter(sym, terms, loc) => Set(sym)
          case SimplifiedAst.Predicate.Body.Loop(sym, term, loc) => visitHeadTerm(term)
        }.fold(Set())(_++_)
      }
    }

    /*
     * Find reachable functions that:
     *
     * (c) Appear in a lattice declaration.
     */
    reachableFunctions ++= root.lattices.values.map {
      case SimplifiedAst.Definition.Lattice(tpe, bot, top, leq, lub, glb, loc) =>
        visitExp(bot) ++ visitExp(top) ++ visitExp(leq) ++ visitExp(lub) ++ visitExp(glb)
    }.fold(Set())(_++_)

    /*
     * Find reachable functions that:
     *
     * (d) Appear in a function which itself is reachable.
     */
    reachableFunctions.foreach {
      root.definitions.get(_) match {
        case Some(defn) => queue.enqueue(defn)
        case _ =>
      }
    }

    while (queue.nonEmpty) {
      // Extract a function body from the queue and search for other reachable functions.
      visitExp(queue.dequeue().exp).foreach(newReachableDefinitionSymbol)
    }

    // Calculate the elapsed time.
    val e = System.nanoTime() - t

    // Reassemble the AST.
    root.copy(
      definitions = root.definitions.filterKeys(reachableFunctions.contains),
      time = root.time.copy(treeshaker = e)
    ).toSuccess
  }
}
