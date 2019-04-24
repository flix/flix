/*
 *  Copyright 2017 Magnus Madsen and Jason Mittertreiner
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.{Ast, SourceLocation, Symbol}
import ca.uwaterloo.flix.language.ast.Ast.{DependencyEdge, DependencyGraph, Polarity}
import ca.uwaterloo.flix.language.ast.FinalAst.Predicate.Body
import ca.uwaterloo.flix.language.ast.FinalAst._
import ca.uwaterloo.flix.language.errors.StratificationError
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

import scala.collection.mutable

/**
  * The stratification phase breaks constraints into strata.
  *
  * "Formally, rules are stratified if whenever there is a rule with
  * head predicate p and a negated subgoal with predicate q, there is
  * no path in the dependency graph from p to q" -- Ullman 132
  *
  * Reports a [[StratificationError]] if the constraint cannot be
  * Stratified.
  *
  * This phase consists of two parts. The first computes the strata for the
  * program. If the first fails, then we continue to the second phase which
  * finds a cycle in the constraints and reports it.
  */
object Stratifier extends Phase[Root, Root] {

  /**
    * Returns a stratified version of the given AST `root`.
    */
  def run(root: Root)(implicit flix: Flix): Validation[Root, CompilationError] = flix.phase("Stratifier") {

    // Check if computation of stratification is disabled.
    if (flix.options.xnostratifier)
      return root.toSuccess

    // Run the control-flow analysis.
    implicit val analysis: ControlFlowAnalysis.Analysis = ControlFlowAnalysis.runAnalysis(root)

    // Stratify every definition.
    val defsVal = traverse(root.defs) {
      case (sym, defn) => visitDef(defn).map(d => sym -> d)
    }

    mapN(defsVal) {
      // Reassemble the AST.
      case ds => root.copy(defs = ds.toMap)
    }
  }

  /**
    * Performs stratification of the given definition `def0`.
    */
  private def visitDef(def0: Def)(implicit analysis: ControlFlowAnalysis.Analysis): Validation[Def, CompilationError] =
    visitExp(def0.exp) map {
      case e => def0.copy(exp = e)
    }

  /**
    * Performs stratification of the given expression `exp0`.
    *
    * Returns [[Success]] if the expression is stratified. Otherwise returns [[Failure]] with a [[StratificationError]].
    */
  private def visitExp(exp0: Expression)(implicit analysis: ControlFlowAnalysis.Analysis): Validation[Expression, StratificationError] = exp0 match {
    case Expression.Unit => Expression.Unit.toSuccess
    case Expression.True => Expression.True.toSuccess
    case Expression.False => Expression.False.toSuccess
    case Expression.Char(lit) => Expression.Char(lit).toSuccess
    case Expression.Float32(lit) => Expression.Float32(lit).toSuccess
    case Expression.Float64(lit) => Expression.Float64(lit).toSuccess
    case Expression.Int8(lit) => Expression.Int8(lit).toSuccess
    case Expression.Int16(lit) => Expression.Int16(lit).toSuccess
    case Expression.Int32(lit) => Expression.Int32(lit).toSuccess
    case Expression.Int64(lit) => Expression.Int64(lit).toSuccess
    case Expression.BigInt(lit) => Expression.BigInt(lit).toSuccess
    case Expression.Str(lit) => Expression.Str(lit).toSuccess

    case Expression.Var(sym, tpe, loc) => Expression.Var(sym, tpe, loc).toSuccess

    case Expression.Closure(sym, freeVars, fnType, tpe, loc) =>
      Expression.Closure(sym, freeVars, fnType, tpe, loc).toSuccess

    case Expression.ApplyClo(exp, args, tpe, loc) =>
      mapN(visitExp(exp), traverse(args)(visitExp)) {
        case (e, as) => Expression.ApplyClo(e, as, tpe, loc)
      }

    case Expression.ApplyDef(sym, args, tpe, loc) =>
      mapN(traverse(args)(visitExp)) {
        case as => Expression.ApplyDef(sym, as, tpe, loc)
      }

    case Expression.ApplyEff(sym, args, tpe, loc) =>
      mapN(traverse(args)(visitExp)) {
        case as => Expression.ApplyEff(sym, as, tpe, loc)
      }

    case Expression.ApplyCloTail(exp, args, tpe, loc) =>
      mapN(visitExp(exp), traverse(args)(visitExp)) {
        case (e, as) => Expression.ApplyCloTail(e, as, tpe, loc)
      }

    case Expression.ApplyDefTail(sym, args, tpe, loc) =>
      mapN(traverse(args)(visitExp)) {
        case as => Expression.ApplyDefTail(sym, as, tpe, loc)
      }

    case Expression.ApplyEffTail(sym, args, tpe, loc) =>
      mapN(traverse(args)(visitExp)) {
        case as => Expression.ApplyEffTail(sym, as, tpe, loc)
      }

    case Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) =>
      mapN(traverse(actuals)(visitExp)) {
        case as => Expression.ApplySelfTail(sym, formals, as, tpe, loc)
      }

    case Expression.Unary(sop, op, exp, tpe, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.Unary(sop, op, e, tpe, loc)
      }

    case Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
      mapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => Expression.Binary(sop, op, e1, e2, tpe, loc)
      }

    case Expression.Let(sym, exp1, exp2, tpe, loc) =>
      mapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => Expression.Let(sym, e1, e2, tpe, loc)
      }

    case Expression.LetRec(sym, exp1, exp2, tpe, loc) =>
      mapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => Expression.LetRec(sym, e1, e2, tpe, loc)
      }

    case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
      mapN(visitExp(exp1), visitExp(exp2), visitExp(exp3)) {
        case (e1, e2, e3) => Expression.IfThenElse(e1, e2, e3, tpe, loc)
      }

    case Expression.Branch(exp, branches, tpe, loc) =>
      val expVal = visitExp(exp)
      val branchesVal = traverse(branches) {
        case (label, body) => visitExp(body) map (b => label -> b)
      }
      mapN(expVal, branchesVal) {
        case (e, bs) => Expression.Branch(e, bs.toMap, tpe, loc)
      }

    case Expression.JumpTo(sym, tpe, loc) =>
      Expression.JumpTo(sym, tpe, loc).toSuccess

    case Expression.Is(sym, tag, exp, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.Is(sym, tag, e, loc)
      }

    case Expression.Tag(sym, tag, exp, tpe, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.Tag(sym, tag, e, tpe, loc)
      }

    case Expression.Untag(sym, tag, exp, tpe, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.Untag(sym, tag, e, tpe, loc)
      }

    case Expression.Index(base, offset, tpe, loc) =>
      mapN(visitExp(base)) {
        case b => Expression.Index(b, offset, tpe, loc)
      }

    case Expression.Tuple(elms, tpe, loc) =>
      mapN(traverse(elms)(visitExp)) {
        case es => Expression.Tuple(es, tpe, loc)
      }

    case Expression.RecordEmpty(tpe, loc) =>
      Expression.RecordEmpty(tpe, loc).toSuccess

    case Expression.RecordSelect(base, label, tpe, loc) =>
      mapN(visitExp(base)) {
        case b => Expression.RecordSelect(b, label, tpe, loc)
      }

    case Expression.RecordExtend(label, value, rest, tpe, loc) =>
      mapN(visitExp(value), visitExp(rest)) {
        case (v, r) => Expression.RecordExtend(label, v, r, tpe, loc)
      }

    case Expression.RecordRestrict(label, rest, tpe, loc) =>
      mapN(visitExp(rest)) {
        case r => Expression.RecordRestrict(label, r, tpe, loc)
      }

    case Expression.ArrayLit(elms, tpe, loc) =>
      mapN(traverse(elms)(visitExp)) {
        case es => Expression.ArrayLit(es, tpe, loc)
      }

    case Expression.ArrayNew(elm, len, tpe, loc) =>
      mapN(visitExp(elm), visitExp(len)) {
        case (e, l) => Expression.ArrayNew(e, l, tpe, loc)
      }

    case Expression.ArrayLoad(base, index, tpe, loc) =>
      mapN(visitExp(base), visitExp(index)) {
        case (b, i) => Expression.ArrayLoad(b, i, tpe, loc)
      }

    case Expression.ArrayLength(base, tpe, loc) =>
      mapN(visitExp(base)) {
        case b => Expression.ArrayLength(b, tpe, loc)
      }

    case Expression.ArrayStore(base, index, elm, tpe, loc) =>
      mapN(visitExp(base), visitExp(index), visitExp(elm)) {
        case (b, i, e) => Expression.ArrayStore(b, i, e, tpe, loc)
      }

    case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) =>
      mapN(visitExp(base), visitExp(beginIndex), visitExp(endIndex)) {
        case (b, i1, i2) => Expression.ArraySlice(b, i1, i2, tpe, loc)
      }

    case Expression.Ref(exp, tpe, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.Ref(e, tpe, loc)
      }

    case Expression.Deref(exp, tpe, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.Deref(e, tpe, loc)
      }

    case Expression.Assign(exp1, exp2, tpe, loc) =>
      mapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => Expression.Assign(e1, e2, tpe, loc)
      }

    case Expression.HandleWith(exp, bindings, tpe, loc) =>
      val bindingsVal = traverse(bindings) {
        case HandlerBinding(sym, b) => visitExp(exp).map(HandlerBinding(sym, _))
      }
      mapN(visitExp(exp), bindingsVal) {
        case (e, bs) => Expression.HandleWith(e, bs, tpe, loc)
      }

    case Expression.Existential(fparam, exp, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.Existential(fparam, e, loc)
      }

    case Expression.Universal(fparam, exp, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.Universal(fparam, e, loc)
      }

    case Expression.NativeConstructor(constructor, args, tpe, loc) =>
      mapN(traverse(args)(visitExp)) {
        case as => Expression.NativeConstructor(constructor, as, tpe, loc)
      }

    case Expression.TryCatch(exp, rules, tpe, loc) =>
      val rulesVal = traverse(rules) {
        case CatchRule(sym, clazz, e) => visitExp(e).map(CatchRule(sym, clazz, _))
      }
      mapN(visitExp(exp), rulesVal) {
        case (e, rs) => Expression.TryCatch(e, rs, tpe, loc)
      }

    case Expression.NativeField(field, tpe, loc) =>
      Expression.NativeField(field, tpe, loc).toSuccess

    case Expression.NativeMethod(method, args, tpe, loc) =>
      mapN(traverse(args)(visitExp)) {
        case as => Expression.NativeMethod(method, as, tpe, loc)
      }

    case Expression.NewChannel(exp, tpe, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.NewChannel(e, tpe, loc)
      }

    case Expression.GetChannel(exp, tpe, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.GetChannel(e, tpe, loc)
      }

    case Expression.PutChannel(exp1, exp2, tpe, loc) =>
      mapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => Expression.PutChannel(e1, e2, tpe, loc)
      }

    case Expression.SelectChannel(rules, default, tpe, loc) =>
      val rulesVal = traverse(rules) {
        case SelectChannelRule(sym, chan, exp) => mapN(visitExp(chan), visitExp(exp)) {
          case (c, e) => SelectChannelRule(sym, c, e)
        }
      }

      val defaultVal = default match {
        case Some(exp) => visitExp(exp) map {
          case e => Some(e)
        }
        case None => None.toSuccess
      }

      mapN(rulesVal, defaultVal) {
        case (rs, d) => Expression.SelectChannel(rs, d, tpe, loc)
      }

    case Expression.ProcessSpawn(exp, tpe, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.ProcessSpawn(e, tpe, loc)
      }

    case Expression.ProcessSleep(exp, tpe, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.ProcessSleep(e, tpe, loc)
      }

    case Expression.ProcessPanic(msg, tpe, loc) =>
      Expression.ProcessPanic(msg, tpe, loc).toSuccess

    case Expression.FixpointConstraint(con, tpe, loc) =>
      Expression.FixpointConstraint(con, tpe, loc).toSuccess

    case Expression.FixpointCompose(exp1, exp2, tpe, loc) =>
      mapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => Expression.FixpointCompose(e1, e2, tpe, loc)
      }

    case Expression.FixpointSolve(uid, exp, _, tpe, loc) =>
      val g = analysis.getDependencyGraph(uid)
      mapN(visitExp(exp), stratify(g, loc)) {
        case (e, s) => Expression.FixpointSolve(uid, e, s, tpe, loc)
      }

    case Expression.FixpointProject(pred, exp, tpe, loc) =>
      mapN(visitPredicateWithParam(pred), visitExp(exp)) {
        case (p, e) => Expression.FixpointProject(p, e, tpe, loc)
      }

    case Expression.FixpointEntails(exp1, exp2, tpe, loc) =>
      mapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => Expression.FixpointEntails(e1, e2, tpe, loc)
      }

    case Expression.HoleError(sym, tpe, loc) =>
      Expression.HoleError(sym, tpe, loc).toSuccess

    case Expression.MatchError(tpe, loc) =>
      Expression.MatchError(tpe, loc).toSuccess

    case Expression.SwitchError(tpe, loc) =>
      Expression.SwitchError(tpe, loc).toSuccess

  }

  /**
    * Performs stratification of the given predicate with parameter `p0`.
    */
  private def visitPredicateWithParam(p0: PredicateWithParam)(implicit analysis: ControlFlowAnalysis.Analysis): Validation[PredicateWithParam, StratificationError] = p0 match {
    case PredicateWithParam(sym, exp) => mapN(visitExp(exp)) {
      case e => PredicateWithParam(sym, e)
    }
  }

  /**
    * Computes the stratification of the given dependency graph `g` at the given source location `loc`.
    *
    * See Database and Knowledge - Base Systems Volume 1 Ullman, Algorithm 3.5 p 133
    */
  private def stratify(g: DependencyGraph, loc: SourceLocation): Validation[Ast.Stratification, StratificationError] = {
    //
    // Maintain a mutable map from predicate symbols to their (maximum) stratum number.
    //
    // Any predicate symbol not explicitly in the map has a default value of zero.
    //
    val stratumOf = mutable.Map.empty[Symbol.PredSym, Int]

    //
    // Compute the number of dependency edges.
    //
    // The number of strata is bounded by the number of predicate symbols which is bounded by the number of edges.
    //
    // Hence if we ever compute a stratum higher than this number then there is a negative cycle.
    //
    val maxStratum = g.xs.size

    //
    // Repeatedly examine the dependency edges.
    //
    // We always consider two cases:
    //   1. A positive body predicate requires its head predicate to be in its stratum or any higher stratum.
    //   2. A negative body predicate requires its head predicate to be in a strictly higher stratum.
    //
    // If we ever create more strata than there are dependency edges then there is a negative cycle and we abort.
    //
    var changed = true
    while (changed) {
      changed = false

      // Examine each dependency edge in turn.
      for (edge <- g.xs) {
        edge match {
          case DependencyEdge.Positive(headSym, bodySym) =>
            // Case 1: The stratum of the head must be in the same or a higher stratum as the body.
            val headStratum = stratumOf.getOrElseUpdate(headSym, 0)
            val bodyStratum = stratumOf.getOrElseUpdate(bodySym, 0)

            if (!(headStratum >= bodyStratum)) {
              // Put the head in the same stratum as the body.
              stratumOf.put(headSym, bodyStratum)
              changed = true
            }

          case DependencyEdge.Negative(headSym, bodySym) =>
            // Case 2: The stratum of the head must be in a strictly higher stratum than the body.
            val headStratum = stratumOf.getOrElseUpdate(headSym, 0)
            val bodyStratum = stratumOf.getOrElseUpdate(bodySym, 0)

            if (!(headStratum > bodyStratum)) {
              // Put the head in one stratum above the body stratum.
              val newHeadStratum = bodyStratum + 1
              stratumOf.put(headSym, newHeadStratum)
              changed = true

              // Check if we have found a negative cycle.
              if (newHeadStratum > maxStratum) {
                return StratificationError(findNegativeCycle(bodySym, headSym, g), loc).toFailure
              }
            }
        }
      }
    }

    // We are done. Successfully return the computed stratification.
    Ast.Stratification(stratumOf.toMap).toSuccess
  }

  /**
    * Returns a path that forms a cycle with the edge from `src` to `dst` in the given dependency graph `g`.
    */
  private def findNegativeCycle(src: Symbol.PredSym, dst: Symbol.PredSym, g: DependencyGraph): List[Symbol.PredSym] = {
    // Computes a map from symbols to their successors.
    val m = mutable.Map.empty[Symbol.PredSym, Set[Symbol.PredSym]]
    for (edge <- g.xs) {
      edge match {
        case DependencyEdge.Positive(head, body) =>
          val s = m.getOrElse(body, Set.empty)
          m.put(body, s + head)
        case DependencyEdge.Negative(head, body) =>
          val s = m.getOrElse(body, Set.empty)
          m.put(body, s + head)
      }
    }

    // TODO: Need some cycle finding algorithm.

    src :: dst :: Nil
  }

  /**
    * Reorders a constraint such that its negated atoms occur last.
    */
  def reorder(c0: Constraint): Constraint = {
    /**
      * Returns `true` if the body predicate is negated.
      */
    def isNegative(p: Predicate.Body): Boolean = p match {
      case Body.Atom(_, Polarity.Negative, _, _, _) => true
      case _ => false
    }

    // Collect all the negated and non-negated predicates.
    val negated = c0.body filter isNegative
    val nonNegated = c0.body filterNot isNegative

    // Reassemble the constraint.
    c0.copy(body = nonNegated ::: negated)
  }

}
