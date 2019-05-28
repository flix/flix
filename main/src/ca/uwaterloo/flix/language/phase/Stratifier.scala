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
import ca.uwaterloo.flix.language.ast.TypedAst._
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

// TODO: Carefully go through the documentation.

object Stratifier extends Phase[Root, Root] {

  /**
    * Returns a stratified version of the given AST `root`.
    */
  def run(root: Root)(implicit flix: Flix): Validation[Root, CompilationError] = flix.phase("Stratifier") {

    // Check if computation of stratification is disabled.
    if (flix.options.xnostratifier)
      return root.toSuccess

    // Compute an over-approximation of the dependency graph for all constraints in the program.
    val dg = root.defs.par.aggregate(DependencyGraph.Empty)({
      case (acc, (sym, decl)) => acc ++ constraintsOfDef(decl)
    }, _ ++ _)

    // Stratify every definition.
    val defsVal = traverse(root.defs) {
      case (sym, defn) => visitDef(defn)(dg).map(d => sym -> d)
    }

    mapN(defsVal) {
      // Reassemble the AST.
      case ds => root.copy(defs = ds.toMap)
    }
  }

  /**
    * Performs stratification of the given definition `def0`.
    */
  private def visitDef(def0: Def)(implicit dg: DependencyGraph): Validation[Def, CompilationError] =
    visitExp(def0.exp) map {
      case e => def0.copy(exp = e)
    }

  /**
    * Performs stratification of the given expression `exp0`.
    *
    * Returns [[Success]] if the expression is stratified. Otherwise returns [[Failure]] with a [[StratificationError]].
    */
  // TODO: ordering etc.

  private def visitExp(exp0: Expression)(implicit dg: DependencyGraph): Validation[Expression, StratificationError] = exp0 match {
    case Expression.Unit(_) => exp0.toSuccess

    case Expression.True(_) => exp0.toSuccess

    case Expression.False(_) => exp0.toSuccess

    case Expression.Char(_, _) => exp0.toSuccess

    case Expression.Float32(_, _) => exp0.toSuccess

    case Expression.Float64(_, _) => exp0.toSuccess

    case Expression.Int8(_, _) => exp0.toSuccess

    case Expression.Int16(_, _) => exp0.toSuccess

    case Expression.Int32(_, _) => exp0.toSuccess

    case Expression.Int64(_, _) => exp0.toSuccess

    case Expression.BigInt(_, _) => exp0.toSuccess

    case Expression.Str(_, _) => exp0.toSuccess

    case Expression.Wild(tpe, eff, loc) => ???

    case Expression.Var(sym, tpe, eff, loc) => ???

    case Expression.Def(sym, tpe, eff, loc) => ???

    case Expression.Eff(sym, tpe, eff, loc) => ???

    case Expression.Hole(sym, tpe, eff, loc) => ???

    case Expression.Lambda(fparam, exp, tpe, eff, loc) => ???

    case Expression.Apply(exp1, exp2, tpe, eff, loc) => ???

    case Expression.Unary(op, exp, tpe, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.Unary(op, exp, tpe, eff, loc)
      }

    case Expression.Binary(op, exp1, exp2, tpe, eff, loc) =>
      mapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => Expression.Binary(op, e1, e2, tpe, eff, loc)
      }

    case Expression.Let(sym, exp1, exp2, tpe, eff, loc) =>
      mapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => Expression.Let(sym, e1, e2, tpe, eff, loc)
      }

    case Expression.LetRec(sym, exp1, exp2, tpe, eff, loc) =>
      mapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => Expression.LetRec(sym, e1, e2, tpe, eff, loc)
      }

    case Expression.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      mapN(visitExp(exp1), visitExp(exp2), visitExp(exp3)) {
        case (e1, e2, e3) => Expression.IfThenElse(e1, e2, e3, tpe, eff, loc)
      }

    case Expression.Stm(exp1, exp2, tpe, eff, loc) => ???

    case Expression.Match(exp, rules, tpe, eff, loc) => ???

    case Expression.Switch(rules, tpe, eff, loc) => ???

    case Expression.Tag(sym, tag, exp, tpe, eff, loc) => ???

    case Expression.Tuple(elms, tpe, eff, loc) =>
      mapN(traverse(elms)(visitExp)) {
        case es => Expression.Tuple(es, tpe, eff, loc)
      }

    case Expression.RecordEmpty(tpe, eff, loc) =>
      Expression.RecordEmpty(tpe, eff, loc).toSuccess

    case Expression.RecordSelect(base, label, tpe, eff, loc) =>
      mapN(visitExp(base)) {
        case b => Expression.RecordSelect(b, label, tpe, eff, loc)
      }

    case Expression.RecordExtend(label, value, rest, tpe, eff, loc) =>
      mapN(visitExp(value), visitExp(rest)) {
        case (v, r) => Expression.RecordExtend(label, v, r, tpe, eff, loc)
      }

    case Expression.RecordRestrict(label, rest, tpe, eff, loc) =>
      mapN(visitExp(rest)) {
        case r => Expression.RecordRestrict(label, r, tpe, eff, loc)
      }

    case Expression.ArrayLit(elms, tpe, eff, loc) =>
      mapN(traverse(elms)(visitExp)) {
        case es => Expression.ArrayLit(es, tpe, eff, loc)
      }

    case Expression.ArrayNew(elm, len, tpe, eff, loc) =>
      mapN(visitExp(elm), visitExp(len)) {
        case (e, l) => Expression.ArrayNew(e, l, tpe, eff, loc)
      }

    case Expression.ArrayLoad(base, index, tpe, eff, loc) =>
      mapN(visitExp(base), visitExp(index)) {
        case (b, i) => Expression.ArrayLoad(b, i, tpe, eff, loc)
      }

    case Expression.ArrayLength(base, tpe, eff, loc) =>
      mapN(visitExp(base)) {
        case b => Expression.ArrayLength(b, tpe, eff, loc)
      }

    case Expression.ArrayStore(base, index, elm, tpe, eff, loc) =>
      mapN(visitExp(base), visitExp(index), visitExp(elm)) {
        case (b, i, e) => Expression.ArrayStore(b, i, e, tpe, eff, loc)
      }

    case Expression.ArraySlice(base, beginIndex, endIndex, tpe, eff, loc) =>
      mapN(visitExp(base), visitExp(beginIndex), visitExp(endIndex)) {
        case (b, i1, i2) => Expression.ArraySlice(b, i1, i2, tpe, eff, loc)
      }

    case Expression.VectorLit(elms, tpe, eff, loc) => ???

    case Expression.VectorNew(elm, len, tpe, eff, loc) => ???

    case Expression.VectorLoad(base, index, tpe, eff, loc) => ???

    case Expression.VectorLength(base, tpe, eff, loc) => ???

    case Expression.VectorStore(base, index, elm, tpe, eff, loc) => ???

    case Expression.VectorSlice(base, startIndex, endIndex, tpe, eff, loc) => ???

    case Expression.Ref(exp, tpe, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.Ref(e, tpe, eff, loc)
      }

    case Expression.Deref(exp, tpe, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.Deref(e, tpe, eff, loc)
      }

    case Expression.Assign(exp1, exp2, tpe, eff, loc) =>
      mapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => Expression.Assign(e1, e2, tpe, eff, loc)
      }

    case Expression.HandleWith(exp, bindings, tpe, eff, loc) =>
      val bindingsVal = traverse(bindings) {
        case HandlerBinding(sym, b) => visitExp(exp).map(HandlerBinding(sym, _))
      }
      mapN(visitExp(exp), bindingsVal) {
        case (e, bs) => Expression.HandleWith(e, bs, tpe, eff, loc)
      }

    case Expression.Existential(fparam, exp, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.Existential(fparam, e, eff, loc)
      }

    case Expression.Universal(fparam, exp, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.Universal(fparam, e, eff, loc)
      }

    case Expression.Ascribe(exp, tpe, eff, loc) => ???

    case Expression.Cast(exp, tpe, eff, loc) => ???

    case Expression.NativeConstructor(constructor, args, tpe, eff, loc) =>
      mapN(traverse(args)(visitExp)) {
        case as => Expression.NativeConstructor(constructor, as, tpe, eff, loc)
      }

    case Expression.TryCatch(exp, rules, tpe, eff, loc) =>
      val rulesVal = traverse(rules) {
        case CatchRule(sym, clazz, e) => visitExp(e).map(CatchRule(sym, clazz, _))
      }
      mapN(visitExp(exp), rulesVal) {
        case (e, rs) => Expression.TryCatch(e, rs, tpe, eff, loc)
      }

    case Expression.NativeField(field, tpe, eff, loc) =>
      Expression.NativeField(field, tpe, eff, loc).toSuccess

    case Expression.NativeMethod(method, args, tpe, eff, loc) =>
      mapN(traverse(args)(visitExp)) {
        case as => Expression.NativeMethod(method, as, tpe, eff, loc)
      }

    case Expression.NewChannel(exp, tpe, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.NewChannel(e, tpe, eff, loc)
      }

    case Expression.GetChannel(exp, tpe, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.GetChannel(e, tpe, eff, loc)
      }

    case Expression.PutChannel(exp1, exp2, tpe, eff, loc) =>
      mapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => Expression.PutChannel(e1, e2, tpe, eff, loc)
      }

    case Expression.SelectChannel(rules, default, tpe, eff, loc) =>
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
        case (rs, d) => Expression.SelectChannel(rs, d, tpe, eff, loc)
      }

    case Expression.ProcessSpawn(exp, tpe, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.ProcessSpawn(e, tpe, eff, loc)
      }

    case Expression.ProcessSleep(exp, tpe, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.ProcessSleep(e, tpe, eff, loc)
      }

    case Expression.ProcessPanic(msg, tpe, eff, loc) =>
      Expression.ProcessPanic(msg, tpe, eff, loc).toSuccess

    case Expression.FixpointConstraint(con, tpe, eff, loc) =>
      Expression.FixpointConstraint(con, tpe, eff, loc).toSuccess

    case Expression.FixpointCompose(exp1, exp2, tpe, eff, loc) =>
      mapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => Expression.FixpointCompose(e1, e2, tpe, eff, loc)
      }

    case Expression.FixpointSolve(exp, _, tpe, eff, loc) =>
      // TODO: Specialize according to the type.
      val stf = stratify(dg, loc)
      mapN(visitExp(exp), stf) {
        case (e, s) => Expression.FixpointSolve(e, s, tpe, eff, loc)
      }

    case Expression.FixpointProject(pred, exp, tpe, eff, loc) =>
      mapN(visitPredicateWithParam(pred), visitExp(exp)) {
        case (p, e) => Expression.FixpointProject(p, e, tpe, eff, loc)
      }

    case Expression.FixpointEntails(exp1, exp2, tpe, eff, loc) =>
      mapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => Expression.FixpointEntails(e1, e2, tpe, eff, loc)
      }
  }

  /**
    * Performs stratification of the given predicate with parameter `p0`.
    */
  private def visitPredicateWithParam(p0: PredicateWithParam)(implicit dg: DependencyGraph): Validation[PredicateWithParam, StratificationError] = p0 match {
    case PredicateWithParam(sym, exp) => mapN(visitExp(exp)) {
      case e => PredicateWithParam(sym, e)
    }
  }

  // TODO: Move this to work on typedast?

  // TODO: DOC
  private def constraintsOfDef(def0: Def): DependencyGraph = constraintsOfExp(def0.exp)

  // TODO: DOC
  private def constraintsOfExp(exp0: Expression): DependencyGraph = exp0 match {
    case Expression.Unit(_) => DependencyGraph.Empty

    case Expression.True(_) => DependencyGraph.Empty

    case Expression.False(_) => DependencyGraph.Empty

    case Expression.Char(_, _) => DependencyGraph.Empty

    case Expression.Float32(_, _) => DependencyGraph.Empty

    case Expression.Float64(_, _) => DependencyGraph.Empty

    case Expression.Int8(_, _) => DependencyGraph.Empty

    case Expression.Int16(_, _) => DependencyGraph.Empty

    case Expression.Int32(_, _) => DependencyGraph.Empty

    case Expression.Int64(_, _) => DependencyGraph.Empty

    case Expression.BigInt(_, _) => DependencyGraph.Empty

    case Expression.Str(_, _) => DependencyGraph.Empty

    case Expression.Var(_, _, _, _) => DependencyGraph.Empty

    case Expression.Unary(_, exp, _, _, _) =>
      constraintsOfExp(exp)

    case Expression.Binary(_, exp1, exp2, _, _, _) =>
      constraintsOfExp(exp1) ++ constraintsOfExp(exp2)

    case Expression.Let(_, exp1, exp2, _, _, _) =>
      constraintsOfExp(exp1) ++ constraintsOfExp(exp2)

    case Expression.LetRec(_, exp1, exp2, _, _, _) =>
      constraintsOfExp(exp1) ++ constraintsOfExp(exp2)

    case Expression.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      constraintsOfExp(exp1) ++ constraintsOfExp(exp2) ++ constraintsOfExp(exp3)

    case Expression.Tuple(elms, _, _, _) =>
      elms.foldLeft(DependencyGraph.Empty) {
        case (acc, e) => acc ++ constraintsOfExp(e)
      }

    case Expression.RecordEmpty(_, _, _) =>
      DependencyGraph.Empty

    case Expression.RecordSelect(base, _, _, _, _) =>
      constraintsOfExp(base)

    case Expression.RecordExtend(_, value, rest, _, _, _) =>
      constraintsOfExp(value) ++ constraintsOfExp(rest)

    case Expression.RecordRestrict(_, rest, _, _, _) =>
      constraintsOfExp(rest)

    case Expression.ArrayLit(elms, _, _, _) =>
      elms.foldLeft(DependencyGraph.Empty) {
        case (acc, e) => acc ++ constraintsOfExp(e)
      }

    case Expression.ArrayNew(elm, len, _, _, _) =>
      constraintsOfExp(elm) ++ constraintsOfExp(len)

    case Expression.ArrayLoad(base, index, _, _, _) =>
      constraintsOfExp(base) ++ constraintsOfExp(index)

    case Expression.ArrayLength(base, _, _, _) =>
      constraintsOfExp(base)

    case Expression.ArrayStore(base, index, elm, _, _, _) =>
      constraintsOfExp(base) ++ constraintsOfExp(index) ++ constraintsOfExp(elm)

    case Expression.ArraySlice(base, beginIndex, endIndex, _, _, _) =>
      constraintsOfExp(base) ++ constraintsOfExp(beginIndex) ++ constraintsOfExp(endIndex)

    case Expression.Ref(exp, _, _, _) =>
      constraintsOfExp(exp)

    case Expression.Deref(exp, _, _, _) =>
      constraintsOfExp(exp)

    case Expression.Assign(exp1, exp2, _, _, _) =>
      constraintsOfExp(exp1) ++ constraintsOfExp(exp2)

    case Expression.HandleWith(exp, bindings, _, _, _) =>
      bindings.foldLeft(constraintsOfExp(exp)) {
        case (acc, HandlerBinding(_, e)) => acc ++ constraintsOfExp(e)
      }

    case Expression.Existential(_, exp, _, _) =>
      constraintsOfExp(exp)

    case Expression.Universal(_, exp, _, _) =>
      constraintsOfExp(exp)

    case Expression.NativeConstructor(_, args, _, _, _) =>
      args.foldLeft(DependencyGraph.Empty) {
        case (acc, e) => acc ++ constraintsOfExp(e)
      }

    case Expression.TryCatch(exp, rules, _, _, _) =>
      rules.foldLeft(constraintsOfExp(exp)) {
        case (acc, CatchRule(_, _, e)) => acc ++ constraintsOfExp(e)
      }

    case Expression.NativeField(_, _, _, _) =>
      DependencyGraph.Empty

    case Expression.NativeMethod(_, args, _, _, _) =>
      args.foldLeft(DependencyGraph.Empty) {
        case (acc, e) => acc ++ constraintsOfExp(e)
      }

    case Expression.NewChannel(exp, _, _, _) =>
      constraintsOfExp(exp)

    case Expression.GetChannel(exp, _, _, _) =>
      constraintsOfExp(exp)

    case Expression.PutChannel(exp1, exp2, _, _, _) =>
      constraintsOfExp(exp1) ++ constraintsOfExp(exp2)

    case Expression.SelectChannel(rules, default, _, _, _) =>
      val dg = default match {
        case None => DependencyGraph.Empty
        case Some(d) => constraintsOfExp(d)
      }

      rules.foldLeft(dg) {
        case (acc, SelectChannelRule(_, exp1, exp2)) => acc ++ constraintsOfExp(exp1) ++ constraintsOfExp(exp2)
      }

    case Expression.ProcessSpawn(exp, _, _, _) =>
      constraintsOfExp(exp)

    case Expression.ProcessSleep(exp, _, _, _) =>
      constraintsOfExp(exp)

    case Expression.ProcessPanic(_, _, _, _) =>
      DependencyGraph.Empty

    case Expression.FixpointConstraint(con, _, _, _) =>
      visitConstraint(con)

    case Expression.FixpointCompose(exp1, exp2, _, _, _) =>
      constraintsOfExp(exp1) ++ constraintsOfExp(exp2)

    case Expression.FixpointSolve(exp, _, _, _, _) =>
      constraintsOfExp(exp)

    case Expression.FixpointProject(pred, exp, _, _, _) =>
      constraintsOfPredicateWithParam(pred) ++ constraintsOfExp(exp)

    case Expression.FixpointEntails(exp1, exp2, _, _, _) =>
      constraintsOfExp(exp1) ++ constraintsOfExp(exp2)

  }

  private def constraintsOfPredicateWithParam(p0: PredicateWithParam): DependencyGraph = p0 match {
    case PredicateWithParam(_, exp) => constraintsOfExp(exp)
  }

  // TODO: DOC
  private def visitConstraint(c: Constraint): DependencyGraph = c match {
    case Constraint(cparams, head, body, _) =>
      // Determine if the head predicate has a symbol.
      visitHeadPredicateSymbol(head) match {
        case None => DependencyGraph.Empty
        case Some(headSym) =>
          val dependencies = body flatMap (b => visitDependencyEdge(headSym, b))
          DependencyGraph(dependencies.toSet)
      }
  }

  // TODO: DOC
  private def visitHeadPredicateSymbol(head0: Predicate.Head): Option[Symbol.PredSym] = head0 match {
    case Predicate.Head.Atom(pred, terms, tpe, loc) => Some(pred.sym)
  }

  // TODO: DOC
  private def visitDependencyEdge(head: Symbol.PredSym, body0: Predicate.Body): Option[DependencyEdge] = body0 match {
    case Predicate.Body.Atom(pred, polarity, terms, tpe, loc) => polarity match {
      case Polarity.Positive => Some(DependencyEdge.Positive(head, pred.sym))
      case Polarity.Negative => Some(DependencyEdge.Negative(head, pred.sym))
    }

    case Predicate.Body.Filter(sym, terms, loc) => None

    case Predicate.Body.Functional(sym, term, loc) => None
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
      case Predicate.Body.Atom(_, Polarity.Negative, _, _, _) => true
      case _ => false
    }

    // Collect all the negated and non-negated predicates.
    val negated = c0.body filter isNegative
    val nonNegated = c0.body filterNot isNegative

    // Reassemble the constraint.
    c0.copy(body = nonNegated ::: negated)
  }

}
