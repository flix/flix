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
import ca.uwaterloo.flix.language.ast.{Ast, SourceLocation, Symbol, TypedAst}
import ca.uwaterloo.flix.language.ast.Ast.Polarity
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.Body
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.Body.{Filter, Functional}
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.Head.{False, True}
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.errors.StratificationError
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}
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

  // TODO: It is important to understand that the stratification is different depending on the constraint system.
  // TODO: Since the exact constraints are not known, we cannot actually separate the constraints until at runtime!
  // TODO: At compile time we know the stratification, just not the exact constraints.

  // TODO: Also compute strongly connected components?
  // TODO: Would be a good exampple for flix semantics.

  /**
    * Represents a dependency between two predicate symbols.
    */
  sealed trait DependencyEdge

  object DependencyEdge {

    /**
      * Represents a positive labelled edge.
      */
    case class Positive(head: Symbol.PredSym, body: Symbol.PredSym) extends DependencyEdge

    /**
      * Represents a negative labelled edge.
      */
    case class Negative(head: Symbol.PredSym, body: Symbol.PredSym) extends DependencyEdge

  }

  object DependencyGraph {
    /**
      * The empty dependency graph.
      */
    val Empty: DependencyGraph = DependencyGraph(Set.empty)

  }

  /**
    * Represents a dependency graph; a set of dependency edges.
    */
  case class DependencyGraph(xs: Set[DependencyEdge])

  /**
    * Returns a stratified version of the given AST `root`.
    */
  def run(root: Root)(implicit flix: Flix): Validation[Root, CompilationError] = flix.phase("Stratifier") {
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
  private def visitDef(def0: Def): Validation[Def, CompilationError] =
    visitExp(def0.exp) map {
      case e => def0.copy(exp = e)
    }

  /**
    * Performs stratification of the given expression `exp0`.
    *
    * Returns [[Success]] if the expression is stratified. Otherwise returns [[Failure]] with a [[StratificationError]].
    */
  private def visitExp(exp0: Expression): Validation[Expression, StratificationError] = exp0 match {
    case Expression.Unit(loc) => Expression.Unit(loc).toSuccess
    case Expression.True(loc) => Expression.True(loc).toSuccess
    case Expression.False(loc) => Expression.False(loc).toSuccess
    case Expression.Char(lit, loc) => Expression.Char(lit, loc).toSuccess
    case Expression.Float32(lit, loc) => Expression.Float32(lit, loc).toSuccess
    case Expression.Float64(lit, loc) => Expression.Float64(lit, loc).toSuccess
    case Expression.Int8(lit, loc) => Expression.Int8(lit, loc).toSuccess
    case Expression.Int16(lit, loc) => Expression.Int16(lit, loc).toSuccess
    case Expression.Int32(lit, loc) => Expression.Int32(lit, loc).toSuccess
    case Expression.Int64(lit, loc) => Expression.Int64(lit, loc).toSuccess
    case Expression.BigInt(lit, loc) => Expression.BigInt(lit, loc).toSuccess
    case Expression.Str(lit, loc) => Expression.Str(lit, loc).toSuccess

    case Expression.Wild(tpe, eff, loc) => Expression.Wild(tpe, eff, loc).toSuccess

    case Expression.Var(sym, tpe, eff, loc) => Expression.Var(sym, tpe, eff, loc).toSuccess

    case Expression.Def(sym, tpe, eff, loc) => Expression.Def(sym, tpe, eff, loc).toSuccess

    case Expression.Eff(sym, tpe, eff, loc) => Expression.Eff(sym, tpe, eff, loc).toSuccess

    case Expression.Hole(sym, tpe, eff, loc) => Expression.Hole(sym, tpe, eff, loc).toSuccess

    case Expression.Lambda(fparam, exp, tpe, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.Lambda(fparam, e, tpe, eff, loc)
      }

    case Expression.Apply(exp1, exp2, tpe, eff, loc) =>
      mapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => Expression.Apply(e1, e2, tpe, eff, loc)
      }

    case Expression.Unary(op, exp, tpe, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.Unary(op, e, tpe, eff, loc)
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

    case Expression.Match(exp, rules, tpe, eff, loc) =>
      val rulesVal = traverse(rules) {
        case MatchRule(p, g, b) => visitExp(b).map(MatchRule(p, g, _))
      }

      mapN(visitExp(exp), rulesVal) {
        case (e, rs) => Expression.Match(e, rs, tpe, eff, loc)
      }

    case Expression.Switch(rules, tpe, eff, loc) =>
      val rulesVal = traverse(rules) {
        case (g, b) => mapN(visitExp(g), visitExp(b))((_, _))
      }
      mapN(rulesVal) {
        case rs => Expression.Switch(rs, tpe, eff, loc)
      }

    case Expression.Tag(sym, tag, exp, tpe, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.Tag(sym, tag, e, tpe, eff, loc)
      }

    case Expression.Tuple(elms, tpe, eff, loc) =>
      mapN(traverse(elms)(visitExp)) {
        case es => Expression.Tuple(es, tpe, eff, loc)
      }

    case Expression.RecordEmpty(tpe, eff, loc) =>
      Expression.RecordEmpty(tpe, eff, loc).toSuccess

    case Expression.RecordExtension(base, label, fld, tpe, eff, loc) =>
      mapN(visitExp(base), visitExp(fld)) {
        case (b, f) => Expression.RecordExtension(b, label, f, tpe, eff, loc)
      }

    case Expression.RecordProjection(base, label, tpe, eff, loc) =>
      mapN(visitExp(base)) {
        case b => Expression.RecordProjection(b, label, tpe, eff, loc)
      }

    case Expression.RecordRestriction(base, label, tpe, eff, loc) =>
      mapN(visitExp(base)) {
        case b => Expression.RecordRestriction(b, label, tpe, eff, loc)
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

    case Expression.VectorLit(elms, tpe, eff, loc) =>
      mapN(traverse(elms)(visitExp)) {
        case es => Expression.VectorLit(es, tpe, eff, loc)
      }

    case Expression.VectorNew(elm, len, tpe, eff, loc) =>
      mapN(visitExp(elm)) {
        case e => Expression.VectorNew(e, len, tpe, eff, loc)
      }

    case Expression.VectorLoad(base, index, tpe, eff, loc) =>
      mapN(visitExp(base)) {
        case b => Expression.VectorLoad(b, index, tpe, eff, loc)
      }

    case Expression.VectorStore(base, index, elm, tpe, eff, loc) =>
      mapN(visitExp(base), visitExp(elm)) {
        case (b, e) => Expression.VectorStore(b, index, e, tpe, eff, loc)
      }

    case Expression.VectorLength(base, tpe, eff, loc) =>
      mapN(visitExp(base)) {
        case b => Expression.VectorLength(b, tpe, eff, loc)
      }

    case Expression.VectorSlice(base, startIndex, endIndex, tpe, eff, loc) =>
      mapN(visitExp(base)) {
        case b => Expression.VectorSlice(b, startIndex, endIndex, tpe, eff, loc)
      }

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

    case Expression.Ascribe(exp, tpe, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.Ascribe(e, tpe, eff, loc)
      }

    case Expression.Cast(exp, tpe, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.Cast(e, tpe, eff, loc)
      }

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

    case Expression.NewRelation(sym, tpe, eff, loc) =>
      Expression.NewRelation(sym, tpe, eff, loc).toSuccess

    case Expression.NewLattice(sym, tpe, eff, loc) =>
      Expression.NewLattice(sym, tpe, eff, loc).toSuccess

    case Expression.Constraint(con, tpe, eff, loc) =>
      Expression.Constraint(con, tpe, eff, loc).toSuccess

    case Expression.ConstraintUnion(exp1, exp2, tpe, eff, loc) =>
      mapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => Expression.ConstraintUnion(e1, e2, tpe, eff, loc)
      }

    case Expression.FixpointSolve(exp, _, tpe, eff, loc) =>
      val g = ControlFlowAnalysis.getDependencyGraphFromAbstractValue(exp)
      mapN(visitExp(exp), stratify(g, loc)) {
        case (e, s) => Expression.FixpointSolve(e, s, tpe, eff, loc)
      }

    case Expression.FixpointCheck(exp, _, tpe, eff, loc) =>
      val g = ControlFlowAnalysis.getDependencyGraphFromAbstractValue(exp)
      mapN(visitExp(exp), stratify(g, loc)) {
        case (e, s) => Expression.FixpointCheck(e, s, tpe, eff, loc)
      }

    case Expression.FixpointDelta(exp, _, tpe, eff, loc) =>
      val g = ControlFlowAnalysis.getDependencyGraphFromAbstractValue(exp)
      mapN(visitExp(exp), stratify(g, loc)) {
        case (e, s) => Expression.FixpointDelta(e, s, tpe, eff, loc)
      }

    case Expression.UserError(tpe, eff, loc) =>
      Expression.UserError(tpe, eff, loc).toSuccess

  }

  /**
    * Returns the dependency graph of the given constraint.
    */
  // TODO: Public
  def getDependencyGraph(c: Constraint): DependencyGraph = c match {
    case Constraint(cparams, head, body, loc) =>
      // Determine if the head predicate has a symbol.
      getHeadPredicateSymbol(head) match {
        case None => DependencyGraph.Empty
        case Some(headSym) =>
          val dependencies = body flatMap (b => getDependencyEdge(headSym, b))
          DependencyGraph(dependencies.toSet)
      }
  }

  /**
    * Optionally returns the predicate symbol of the given head predicate `head0`.
    */
  private def getHeadPredicateSymbol(head0: Predicate.Head): Option[Symbol.PredSym] = head0 match {
    case Predicate.Head.True(_) => None
    case Predicate.Head.False(_) => None
    case Predicate.Head.RelAtom(base, sym, terms, loc) => Some(sym)
    case Predicate.Head.LatAtom(base, sym, terms, loc) => Some(sym)
  }

  /**
    * Optionally returns the predicate symbol of the given body predicate `body0`.
    */
  private def getDependencyEdge(head: Symbol.PredSym, body0: Predicate.Body): Option[DependencyEdge] = body0 match {
    case Predicate.Body.RelAtom(base, sym, polarity, terms, loc) => polarity match {
      case Polarity.Positive => Some(DependencyEdge.Positive(head, sym))
      case Polarity.Negative => Some(DependencyEdge.Negative(head, sym))
    }

    case Predicate.Body.LatAtom(base, sym, polarity, terms, loc) => polarity match {
      case Polarity.Positive => Some(DependencyEdge.Positive(head, sym))
      case Polarity.Negative => Some(DependencyEdge.Negative(head, sym))
    }

    case Predicate.Body.Filter(sym, terms, loc) => None

    case Predicate.Body.Functional(sym, term, loc) => None
  }

  /**
    * Returns the union of the two dependency graphs.
    */
  def union(g1: DependencyGraph, g2: DependencyGraph): DependencyGraph =
    DependencyGraph(g1.xs ++ g2.xs)

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
  private def findNegativeCycle(src: Symbol.PredSym, dst: Symbol.PredSym, g: Stratifier.DependencyGraph): List[Symbol.PredSym] = {
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
      case Body.RelAtom(_, _, Polarity.Negative, _, _) => true
      case Body.LatAtom(_, _, Polarity.Negative, _, _) => true
      case _ => false
    }

    // Collect all the negated and non-negated predicates.
    val negated = c0.body filter isNegative
    val nonNegated = c0.body filterNot isNegative

    // Reassemble the constraint.
    c0.copy(body = nonNegated ::: negated)
  }


  /////////////////////////////////////////////////////////////////////////////
  ////////     LEGACY CODE                                       //////////////
  /////////////////////////////////////////////////////////////////////////////

  /**
    * A small case class to allow us to abstract over the possible heads in a
    * constraint
    */
  sealed trait ConstrHead

  object ConstrHead {

    case object True extends ConstrHead

    case object False extends ConstrHead

    case class Atom(sym: /* Symbol.TableSym */ AnyRef) extends ConstrHead

  }

  /**
    * Create a ConstrHead type out of the head of a given constraint
    */
  def makeConstrHead(constr: TypedAst.Constraint): ConstrHead = constr.head match {
    case True(_) => ConstrHead.True
    case False(_) => ConstrHead.False
    case Predicate.Head.RelAtom(_, sym, _, _) => ConstrHead.Atom(sym)
    case Predicate.Head.LatAtom(_, sym, _, _) => ConstrHead.Atom(sym)
  }


  /**
    * Stratify the graph
    */
  def stratifyOld(constraints: List[TypedAst.Constraint], syms: List[ /* Symbol.TableSym */ AnyRef]): Validation[List[AnyRef], StratificationError] = {
    // Implementing as described in Database and Knowledge - Base Systems Volume 1
    // Ullman, Algorithm 3.5 p 133

    /// Start by creating a mapping from predicates to stratum
    var stratumOf: Map[ConstrHead, Int] = (syms.map(x => (ConstrHead.Atom(x), 1))
      ++ List((ConstrHead.True, 1), (ConstrHead.False, 1))).toMap
    val numRules = constraints.size

    // We repeatedly examine the rules. We move the head predicate to
    // the same strata that the body predicate is currently in if it is
    // non negated, or one after the body, if it is. We repeat until
    // there are no changes.
    //
    // If we create more strata than there are rules, then there is no
    // stratification and we error out
    var changes = true
    while (changes) {
      changes = false
      for (pred <- constraints) {
        val headSym = makeConstrHead(pred)
        val currStratum = stratumOf(headSym)

        for (subGoal <- pred.body) {
          subGoal match {
            case Body.RelAtom(_, subGoalSym, Polarity.Positive, _, _) =>
              val newStratum = math.max(currStratum, stratumOf(ConstrHead.Atom(subGoalSym)))
              if (newStratum > numRules) {
                // If we create more stratum than there are rules then
                // we know there must be a negative cycle.
                return StratificationError(???, ???).toFailure
              }
              if (currStratum != newStratum) {
                // Update the strata of the predicate
                stratumOf += (headSym -> newStratum)
                changes = true
              }

            /* copied */
            case Body.LatAtom(_, subGoalSym, Polarity.Positive, _, _) =>
              val newStratum = math.max(currStratum, stratumOf(ConstrHead.Atom(subGoalSym)))
              if (newStratum > numRules) {
                // If we create more stratum than there are rules then
                // we know there must be a negative cycle.
                return StratificationError(???, ???).toFailure
              }
              if (currStratum != newStratum) {
                // Update the strata of the predicate
                stratumOf += (headSym -> newStratum)
                changes = true
              }

            case Body.RelAtom(_, subGoalSym, Polarity.Negative, _, _) =>
              val newStratum = math.max(stratumOf(headSym), stratumOf(ConstrHead.Atom(subGoalSym)) + 1)

              if (newStratum > numRules) {
                // If we create more stratum than there are rules then
                // we know there must be a negative cycle
                return StratificationError(???, ???).toFailure
              }
              if (currStratum != newStratum) {
                // Update the strata of the predicate
                stratumOf += (headSym -> newStratum)
                changes = true
              }

            /* copied */
            case Body.LatAtom(_, subGoalSym, Polarity.Negative, _, _) =>
              val newStratum = math.max(stratumOf(headSym), stratumOf(ConstrHead.Atom(subGoalSym)) + 1)

              if (newStratum > numRules) {
                // If we create more stratum than there are rules then
                // we know there must be a negative cycle
                return StratificationError(???, ???).toFailure
              }
              if (currStratum != newStratum) {
                // Update the strata of the predicate
                stratumOf += (headSym -> newStratum)
                changes = true
              }

            case _: Filter => // Do Nothing
            case _: Functional => // Do Nothing
          }
        }
      }
    }

    // We now have a mapping from predicates to strata, apply it to the
    // list of rules we are given.
    //
    // While we're at it, we also reorder the goals so that negated
    // literals occur after non negated ones. This ensures that the
    // variables in the negated literals will be bound when we evaluate
    // them


    /**
      * Separate a list of strata into the levels found earlier
      */
    def separate(constraints: List[TypedAst.Constraint], currLevel: Int): List[List[TypedAst.Constraint]] = {
      // We consider the True and False atoms to be in the first stratum
      def getLevel(c: TypedAst.Constraint): Int = stratumOf(makeConstrHead(c))

      constraints match {
        case Nil => Nil
        case lst =>
          val currStrata = lst.filter(x => getLevel(x) == currLevel)
          val remStrata = lst.filter(x => getLevel(x) != currLevel)
          currStrata :: separate(remStrata, currLevel + 1)
      }
    }

    val separated = separate(constraints, 1)
    val reordered = separated map {
      _ map reorder
    }

    ???
  }


  /**
    * An edge represents a constraint in the program. It has a weight of -1
    * if it is a negated predicate, else it has a weight of 0
    */
  case class Edge(weight: Int, rule: TypedAst.Constraint)

  /**
    * A graph is an adjacency list of Constraint -> (Constraint, Weight)
    * where weight is either -1 or 0
    */
  class Graph {

    var vertices: Set[ /* Symbol.TableSym */ AnyRef] = Set.empty

    // An adjacency list of from - to edges with weights
    var edges: Map[ /* Symbol.TableSym */ AnyRef, Map[ /* Symbol.TableSym */ AnyRef, Edge]] = Map.empty

    /**
      * Insert an edge from-to with weight into the graph
      */
    def insert(from: /* Symbol.TableSym */ AnyRef, to: /* Symbol.TableSym */ AnyRef, constr: TypedAst.Constraint, weight: Int): Stratifier.Graph = {
      edges = edges.get(from) match {
        case Some(m) => edges + (from -> (m + (to -> Edge(weight, constr))))
        case None => edges + (from -> Map(to -> Edge(weight, constr)))
      }
      vertices += from
      vertices += to
      this
    }
  }

  /**
    * Use the constraints to create an adjacency matrix
    *
    * A constraint of:
    * P :- Q creates an edge Q -> P of weight 0
    * P :- !Q creates an edge Q -> P of weight -1
    */
  def createGraph(constraints: List[TypedAst.Constraint]): Graph = {
    constraints.foldRight(new Graph)((constraint: TypedAst.Constraint, graph: Graph) => constraint.head match {
      case TypedAst.Predicate.Head.RelAtom(_, headSym, _, _) =>
        // As well as creating the graph out of the given constraints, we also add a source node which
        // has a directed edge to all nodes
        graph.insert(null, headSym, constraint, 0)
        constraint.body.foldRight(graph)((pred: TypedAst.Predicate.Body, graph: Graph) => pred match {
          case TypedAst.Predicate.Body.RelAtom(_, predSym, Polarity.Positive, _, _) =>
            graph.insert(headSym, predSym, constraint, 0)
          case TypedAst.Predicate.Body.LatAtom(_, predSym, Polarity.Positive, _, _) =>
            graph.insert(headSym, predSym, constraint, 0)
          case TypedAst.Predicate.Body.RelAtom(_, predSym, Polarity.Negative, _, _) =>
            graph.insert(headSym, predSym, constraint, -1)
          case TypedAst.Predicate.Body.LatAtom(_, predSym, Polarity.Negative, _, _) =>
            graph.insert(headSym, predSym, constraint, -1)
          case _: TypedAst.Predicate.Body.Filter => graph
          case _: TypedAst.Predicate.Body.Functional => graph
        })

      /* copied */
      case TypedAst.Predicate.Head.LatAtom(_, headSym, _, _) =>
        // As well as creating the graph out of the given constraints, we also add a source node which
        // has a directed edge to all nodes
        graph.insert(null, headSym, constraint, 0)
        constraint.body.foldRight(graph)((pred: TypedAst.Predicate.Body, graph: Graph) => pred match {
          case TypedAst.Predicate.Body.RelAtom(_, predSym, Polarity.Positive, _, _) =>
            graph.insert(headSym, predSym, constraint, 0)
          case TypedAst.Predicate.Body.LatAtom(_, predSym, Polarity.Positive, _, _) =>
            graph.insert(headSym, predSym, constraint, 0)
          case TypedAst.Predicate.Body.RelAtom(_, predSym, Polarity.Negative, _, _) =>
            graph.insert(headSym, predSym, constraint, -1)
          case TypedAst.Predicate.Body.LatAtom(_, predSym, Polarity.Negative, _, _) =>
            graph.insert(headSym, predSym, constraint, -1)
          case _: TypedAst.Predicate.Body.Filter => graph
          case _: TypedAst.Predicate.Body.Functional => graph
        })

      case TypedAst.Predicate.Head.True(_) => graph
      case TypedAst.Predicate.Head.False(_) => graph
    })
  }

  /**
    * Returns true iff a graph has a negative cycle
    */
  def findNegCycle(constraints: List[TypedAst.Constraint]): List[TypedAst.Constraint] = {
    // Get the list of constraints
    val g = createGraph(constraints)

    // The Bellman Ford algorithm, we can use it to find negative cycles

    // Initialize the distance and predecessor arrays to default values
    var distPred: Map[ /* Symbol.TableSym */ AnyRef, (Int, TypedAst.Constraint)] = g.vertices.toList.zip(List.fill(g.vertices.size)((1, null))).toMap
    // The source has 0 weight
    distPred = distPred.updated(null, (0, null))

    // Relax the vertices |V|+1 times. The path can't be longer than |V|
    for (_ <- 0 to g.vertices.size + 1) {
      for (fromEntry <- g.edges) {
        for (toEntry <- fromEntry._2) {
          val from = fromEntry._1
          val to = toEntry._1
          val weight = toEntry._2.weight
          // Relax the edge from-to
          if (distPred(from)._1 + weight < distPred(to)._1) {
            distPred = distPred.updated(to, (distPred(from)._1 + weight, toEntry._2.rule))
          }
        }
      }
    }

    // At this point, we must have found the shortest path, so if there is a
    // shorter path, then there is a negative cycle, and if so, we return it
    var witness: List[TypedAst.Constraint] = Nil
    // Look for an edge (u,v) where distance(u) + Weight(u,v) < distance(v)
    for (fromEntry <- g.edges) {
      for (toEntry <- fromEntry._2) {
        val from = fromEntry._1
        val to = toEntry._1
        val weight = toEntry._2.weight
        if (distPred(from)._1 + weight < distPred(to)._1) {
          // We found part of a negative cycle
          // Go backwards from v until we find a cycle
          val firstEdge = distPred(from)._2
          witness = firstEdge :: witness

          var fromConstraint = distPred(firstEdge.head match {
            case Predicate.Head.RelAtom(_, sym, _, _) => sym
            case Predicate.Head.LatAtom(_, sym, _, _) => sym
            case _: False => throw InternalCompilerException("Encountered the False atom while looking for negative cyles which should never happen")
            case _: True => throw InternalCompilerException("Encountered the True atom while looking for negative cyles which should never happen")
          })._2

          while (fromConstraint != null && fromConstraint != firstEdge) {
            witness = fromConstraint :: witness
            fromConstraint = distPred(fromConstraint.head match {
              case Predicate.Head.RelAtom(_, sym, _, _) => sym
              case Predicate.Head.LatAtom(_, sym, _, _) => sym
              case _: False => throw InternalCompilerException("Encountered the False atom while looking for negative cyles which should never happen")
              case _: True => throw InternalCompilerException("Encountered the True atom while looking for negative cyles which should never happen")
            })._2
          }
          return witness
        }
      }
    }
    witness
  }
}
