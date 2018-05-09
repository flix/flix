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
import ca.uwaterloo.flix.language.ast.SimplifiedAst.Expression
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}
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
  * (d) Appears in a property declaration.
  * (e) Appears as a special op.
  * (f) Appears in a function which itself is reachable.
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
    val queue: mutable.Queue[SimplifiedAst.Def] = mutable.Queue.empty

    /**
      * Returns true iff the function definition `defn` is initially reachable by (a).
      *
      * That is, returns true iff `defn` satisfies:
      *
      * (a) Appears in the global namespaces, takes zero arguments, and is not marked as synthetic.
      */
    def isReachableRoot(defn: SimplifiedAst.Def): Boolean = {
      (defn.sym.namespace.isEmpty && defn.fparams.isEmpty && !defn.mod.isSynthetic) || defn.ann.isBenchmark
    }

    /**
      * Returns the function symbols reachable from `hs`.
      */
    def visitHeadTerms(hs: List[SimplifiedAst.Term.Head]): Set[Symbol.DefnSym] = hs.map(visitHeadTerm).fold(Set())(_ ++ _)

    /**
      * Returns the function symbols reachable from the given SimplifiedAst.Term.Head `head`.
      */
    def visitHeadTerm(h0: SimplifiedAst.Term.Head): Set[Symbol.DefnSym] = {
      h0 match {
        case SimplifiedAst.Term.Head.Var(sym, tpe, loc) => Set.empty
        case SimplifiedAst.Term.Head.Lit(lit, tpe, loc) => visitExp(lit)
        case SimplifiedAst.Term.Head.App(sym, args, tpe, loc) => Set(sym)
      }
    }

    /**
      * Returns the function symbols reachable from `bs`.
      */
    def visitBodyTerms(bs: List[SimplifiedAst.Term.Body]): Set[Symbol.DefnSym] = bs.map(visitBodyTerm).fold(Set())(_ ++ _)

    /**
      * Returns the function symbols reachable from the given SimplifiedAst.Term.Body `body`.
      */
    def visitBodyTerm(b0: SimplifiedAst.Term.Body): Set[Symbol.DefnSym] = {
      b0 match {
        case SimplifiedAst.Term.Body.Wild(tpe, loc) => Set.empty
        case SimplifiedAst.Term.Body.Var(sym, tpe, loc) => Set.empty
        case SimplifiedAst.Term.Body.Lit(exp, tpe, loc) => visitExp(exp)
        case SimplifiedAst.Term.Body.Pat(pat, tpe, loc) => Set.empty
      }
    }

    /**
      * Returns the function symbols reachable from `es`.
      */
    def visitExps(es: List[Expression]): Set[Symbol.DefnSym] = es.map(visitExp).fold(Set())(_ ++ _)

    /**
      * Returns the function symbols reachable from the given Expression `e0`.
      */
    def visitExp(e0: Expression): Set[Symbol.DefnSym] = e0 match {
      case Expression.Unit => Set.empty
      case Expression.True => Set.empty
      case Expression.False => Set.empty
      case Expression.Char(lit) => Set.empty
      case Expression.Float32(lit) => Set.empty
      case Expression.Float64(lit) => Set.empty
      case Expression.Int8(lit) => Set.empty
      case Expression.Int16(lit) => Set.empty
      case Expression.Int32(lit) => Set.empty
      case Expression.Int64(lit) => Set.empty
      case Expression.BigInt(lit) => Set.empty
      case Expression.Str(lit) => Set.empty
      case Expression.Var(sym, tpe, loc) => Set.empty
      case Expression.Def(sym, tpe, loc) => Set(sym)
      case Expression.Eff(sym, tpe, loc) => Set.empty
      case Expression.Lambda(args, body, tpe, loc) => visitExp(body)
      case Expression.Closure(sym, freeVars, tpe, loc) => Set(sym)
      case Expression.ApplyClo(exp, args, tpe, loc) => visitExps(args) ++ visitExp(exp)
      case Expression.ApplyDef(sym, args, tpe, loc) => visitExps(args) + sym
      case Expression.ApplyEff(sym, args, tpe, loc) => visitExps(args)
      case Expression.ApplyCloTail(exp, args, tpe, loc) => visitExps(args) ++ visitExp(exp)
      case Expression.ApplyDefTail(sym, args, tpe, loc) => visitExps(args) + sym
      case Expression.ApplyEffTail(sym, args, tpe, loc) => visitExps(args)
      case Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) => visitExps(actuals) + sym
      case Expression.Unary(sop, op, exp, tpe, loc) => visitExp(exp)
      case Expression.Binary(sop, op, exp1, exp2, tpe, loc) => visitExp(exp1) ++ visitExp(exp2)
      case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) => visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)
      case Expression.Branch(exp, branches, tpe, loc) => visitExp(exp) ++ visitExps(branches.values.toList)
      case Expression.JumpTo(sym, tpe, loc) => Set.empty
      case Expression.Let(sym, exp1, exp2, tpe, loc) => visitExp(exp1) ++ visitExp(exp2)
      case Expression.LetRec(sym, exp1, exp2, tpe, loc) => visitExp(exp1) ++ visitExp(exp2)
      case Expression.Is(sym, tag, exp, loc) => visitExp(exp)
      case Expression.Tag(sym, tag, exp, tpe, loc) => visitExp(exp)
      case Expression.Untag(sym, tag, exp, tpe, loc) => visitExp(exp)
      case Expression.Index(base, offset, tpe, loc) => visitExp(base)
      case Expression.Tuple(elms, tpe, loc) => visitExps(elms)
      case Expression.ArrayLit(elms, tpe, loc) => visitExps(elms)
      case Expression.ArrayNew(elm, len, tpe, loc) => visitExp(elm) ++ visitExp(len)
      case Expression.ArrayLoad(base, index, tpe, lco) => visitExp(base) ++ visitExp(index)
      case Expression.ArrayStore(base, index, elm, tpe, loc) => visitExp(base) ++ visitExp(index) ++ visitExp(elm)
      case Expression.ArrayLength(base, tpe, loc) => visitExp(base)
      case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) => visitExp(base) ++ visitExp(beginIndex) ++ visitExp(endIndex)
      case Expression.Ref(exp, tpe, loc) => visitExp(exp)
      case Expression.Deref(exp, tpe, loc) => visitExp(exp)
      case Expression.Assign(exp1, exp2, tpe, loc) => visitExp(exp1) ++ visitExp(exp2)
      case Expression.HandleWith(exp, bindings, tpe, loc) => visitExp(exp) ++ visitExps(bindings.map(_.exp))
      case Expression.Existential(fparam, exp, loc) => visitExp(exp)
      case Expression.Universal(fparam, exp, loc) => visitExp(exp)
      case Expression.NativeConstructor(constructor, args, tpe, loc) => visitExps(args)
      case Expression.NativeField(field, tpe, loc) => Set.empty
      case Expression.NativeMethod(method, args, tpe, loc) => visitExps(args)
      case Expression.UserError(tpe, loc) => Set.empty
      case Expression.HoleError(sym, tpe, eff, loc) => Set.empty
      case Expression.MatchError(tpe, loc) => Set.empty
      case Expression.SwitchError(tpe, loc) => Set.empty

      case Expression.LambdaClosure(lambda, freeVars, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${e0.getClass.getSimpleName}'.")
      case Expression.Apply(exp, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${e0.getClass.getSimpleName}'.")
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
     * We can now use these helper functions to perform tree shaking.
     */

    // Start the timer.
    val t = System.nanoTime()

    /*
     * Find reachable functions that:
     *
     * (a) Appear in the global namespaces, take zero arguments, and are not marked as synthetic.
     */
    for ((sym, defn) <- root.defs) {
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
          case SimplifiedAst.Predicate.Head.True(loc) => Set.empty
          case SimplifiedAst.Predicate.Head.False(loc) => Set.empty
          case SimplifiedAst.Predicate.Head.Atom(sym, terms, loc) => visitHeadTerms(terms)
        })

        reachableFunctions ++= constraint.body.map {
          case SimplifiedAst.Predicate.Body.Atom(sym, polarity, terms, loc) => visitBodyTerms(terms)
          case SimplifiedAst.Predicate.Body.Filter(sym, terms, loc) => Set(sym)
          case SimplifiedAst.Predicate.Body.Loop(sym, term, loc) => visitHeadTerm(term)
        }.fold(Set())(_ ++ _)
      }
    }

    /*
     * Find reachable functions that:
     *
     * (c) Appear in a lattice declaration.
     */
    reachableFunctions ++= root.lattices.values.map {
      case SimplifiedAst.Lattice(tpe, bot, top, equ, leq, lub, glb, loc) =>
        visitExp(bot) ++ visitExp(top) ++ visitExp(equ) ++ visitExp(leq) ++ visitExp(lub) ++ visitExp(glb)
    }.fold(Set())(_ ++ _)

    /*
     * Find reachable functions that:
     *
     * (d) Appear in a property declaration.
     */
    reachableFunctions ++= root.properties.map {
      case SimplifiedAst.Property(law, defn, exp) => visitExp(exp) + law + defn
    }.fold(Set())(_ ++ _)

    /*
     * Find reachable functions that:
     *
     * (e) Appear as a special op.
     */
    reachableFunctions ++= root.specialOps.values.flatMap(_.values)

    /*
     * Find reachable functions that:
     *
     * (f) Appear in a function which itself is reachable.
     */
    reachableFunctions.foreach {
      root.defs.get(_) match {
        case Some(defn) => queue.enqueue(defn)
        case None =>
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
      defs = root.defs.filterKeys(reachableFunctions.contains),
      time = root.time.copy(treeshaker = e)
    ).toSuccess
  }
}
