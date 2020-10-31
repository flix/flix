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
import ca.uwaterloo.flix.language.ast.Ast.{DependencyEdge, DependencyGraph, Polarity}
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.{Ast, Name, SourceLocation, SourcePosition, Type, TypeConstructor}
import ca.uwaterloo.flix.language.errors.StratificationError
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{ParOps, Validation}

import scala.collection.mutable

/**
  * The stratification phase breaks constraints into strata.
  *
  * "Formally, rules are stratified if whenever there is a rule with
  * head predicate p and a negated subgoal with predicate q, there is
  * no path in the dependency graph from p to q" -- Ullman 132
  *
  * Reports a [[StratificationError]] if the constraints cannot be stratified.
  */
object Stratifier extends Phase[Root, Root] {

  /**
    * A type alias for the stratification cache.
    */
  type Cache = mutable.Map[Set[Name.Pred], Ast.Stratification]

  /**
    * Returns a stratified version of the given AST `root`.
    */
  def run(root: Root)(implicit flix: Flix): Validation[Root, CompilationError] = flix.phase("Stratifier") {
    // A cache of stratifications. Only caches successful stratifications.
    val cache: Cache = mutable.Map.empty

    // Check if computation of stratification is disabled.
    if (flix.options.xnostratifier)
      return root.toSuccess

    // Compute an over-approximation of the dependency graph for all constraints in the program.
    val dg = flix.subphase("Compute Dependency Graph") {
      ParOps.parAgg(root.defs, DependencyGraph.empty)({
        case (acc, (sym, decl)) => acc + dependencyGraphOfDef(decl)
      }, _ + _)
    }

    // Compute the stratification at every solve expression in the ast.`
    val defsVal = flix.subphase("Compute Stratification") {
      traverse(root.defs) {
        case (sym, defn) => visitDef(defn)(dg, cache).map(d => sym -> d)
      }
    }

    mapN(defsVal) {
      case ds => root.copy(defs = ds.toMap)
    }
  }

  /**
    * Performs stratification of the given definition `def0`.
    */
  private def visitDef(def0: Def)(implicit dg: DependencyGraph, cache: Cache): Validation[Def, CompilationError] =
    visitExp(def0.exp) map {
      case e => def0.copy(exp = e)
    }

  /**
    * Performs stratification of the given expression `exp0`.
    *
    * Returns [[Success]] if the expression is stratified. Otherwise returns [[Failure]] with a [[StratificationError]].
    */
  private def visitExp(exp0: Expression)(implicit dg: DependencyGraph, cache: Cache): Validation[Expression, StratificationError] = exp0 match {
    case Expression.Unit(_) => exp0.toSuccess

    case Expression.Null(_, _) => exp0.toSuccess

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

    case Expression.Default(_, _) => exp0.toSuccess

    case Expression.Wild(_, _) => exp0.toSuccess

    case Expression.Var(_, _, _) => exp0.toSuccess

    case Expression.Def(_, _, _) => exp0.toSuccess

    case Expression.Sig(_, _, _) => exp0.toSuccess

    case Expression.Hole(_, _, _, _) => exp0.toSuccess

    case Expression.Lambda(fparam, exp, tpe, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.Lambda(fparam, e, tpe, loc)
      }

    case Expression.Apply(exp, exps, tpe, eff, loc) =>
      mapN(visitExp(exp), traverse(exps)(visitExp)) {
        case (e, es) => Expression.Apply(e, es, tpe, eff, loc)
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

    case Expression.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      mapN(visitExp(exp1), visitExp(exp2), visitExp(exp3)) {
        case (e1, e2, e3) => Expression.IfThenElse(e1, e2, e3, tpe, eff, loc)
      }

    case Expression.Stm(exp1, exp2, tpe, eff, loc) =>
      mapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => Expression.Stm(e1, e2, tpe, eff, loc)
      }

    case Expression.Match(exp, rules, tpe, eff, loc) =>
      val matchVal = visitExp(exp)
      val rulesVal = traverse(rules) {
        case MatchRule(pat, guard, body) => mapN(visitExp(guard), visitExp(body)) {
          case (g, b) => MatchRule(pat, g, b)
        }
      }
      mapN(matchVal, rulesVal) {
        case (m, rs) => Expression.Match(m, rs, tpe, eff, loc)
      }

    case Expression.Choose(exps, rules, tpe, eff, loc) =>
      val expsVal = traverse(exps)(visitExp)
      val rulesVal = traverse(rules) {
        case ChoiceRule(pat, exp) => mapN(visitExp(exp))(ChoiceRule(pat, _))
      }
      mapN(expsVal, rulesVal) {
        case (es, rs) => Expression.Choose(es, rs, tpe, eff, loc)
      }

    case Expression.Tag(sym, tag, exp, tpe, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.Tag(sym, tag, e, tpe, eff, loc)
      }

    case Expression.Tuple(elms, tpe, eff, loc) =>
      mapN(traverse(elms)(visitExp)) {
        case es => Expression.Tuple(es, tpe, eff, loc)
      }

    case Expression.RecordEmpty(tpe, loc) =>
      Expression.RecordEmpty(tpe, loc).toSuccess

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

    case Expression.ArrayLength(base, eff, loc) =>
      mapN(visitExp(base)) {
        case b => Expression.ArrayLength(b, eff, loc)
      }

    case Expression.ArrayStore(base, index, elm, loc) =>
      mapN(visitExp(base), visitExp(index), visitExp(elm)) {
        case (b, i, e) => Expression.ArrayStore(b, i, e, loc)
      }

    case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) =>
      mapN(visitExp(base), visitExp(beginIndex), visitExp(endIndex)) {
        case (b, i1, i2) => Expression.ArraySlice(b, i1, i2, tpe, loc)
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

    case Expression.Existential(fparam, exp, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.Existential(fparam, e, loc)
      }

    case Expression.Universal(fparam, exp, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.Universal(fparam, e, loc)
      }

    case Expression.Ascribe(exp, tpe, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.Ascribe(e, tpe, eff, loc)
      }

    case Expression.Cast(exp, tpe, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.Cast(e, tpe, eff, loc)
      }

    case Expression.TryCatch(exp, rules, tpe, eff, loc) =>
      val rulesVal = traverse(rules) {
        case CatchRule(sym, clazz, e) => visitExp(e).map(CatchRule(sym, clazz, _))
      }
      mapN(visitExp(exp), rulesVal) {
        case (e, rs) => Expression.TryCatch(e, rs, tpe, eff, loc)
      }

    case Expression.InvokeConstructor(constructor, args, tpe, eff, loc) =>
      mapN(traverse(args)(visitExp)) {
        case as => Expression.InvokeConstructor(constructor, as, tpe, eff, loc)
      }

    case Expression.InvokeMethod(method, exp, args, tpe, eff, loc) =>
      mapN(visitExp(exp), traverse(args)(visitExp)) {
        case (e, as) => Expression.InvokeMethod(method, e, as, tpe, eff, loc)
      }

    case Expression.InvokeStaticMethod(method, args, tpe, eff, loc) =>
      mapN(traverse(args)(visitExp)) {
        case as => Expression.InvokeStaticMethod(method, as, tpe, eff, loc)
      }

    case Expression.GetField(field, exp, tpe, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.GetField(field, e, tpe, eff, loc)
      }

    case Expression.PutField(field, exp1, exp2, tpe, eff, loc) =>
      mapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => Expression.PutField(field, e1, e2, tpe, eff, loc)
      }

    case Expression.GetStaticField(field, tpe, eff, loc) =>
      Expression.GetStaticField(field, tpe, eff, loc).toSuccess

    case Expression.PutStaticField(field, exp, tpe, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.PutStaticField(field, e, tpe, eff, loc)
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
        case None => None.toSuccess
        case Some(exp) => visitExp(exp) map {
          case e => Some(e)
        }
      }

      mapN(rulesVal, defaultVal) {
        case (rs, d) => Expression.SelectChannel(rs, d, tpe, eff, loc)
      }

    case Expression.Spawn(exp, tpe, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.Spawn(e, tpe, eff, loc)
      }

    case Expression.Lazy(exp, tpe, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.Lazy(e, tpe, loc)
      }

    case Expression.Force(exp, tpe, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.Force(e, tpe, eff, loc)
      }

    case Expression.FixpointConstraintSet(cs0, _, tpe, loc) =>
      // Compute the stratification.
      val stf = stratifyWithCache(dg, tpe, loc)

      mapN(stf) {
        case s =>
          val cs = cs0.map(reorder)
          Expression.FixpointConstraintSet(cs, s, tpe, loc)
      }

    case Expression.FixpointCompose(exp1, exp2, _, tpe, eff, loc) =>
      // Compute the stratification.
      val stf = stratifyWithCache(dg, tpe, loc)

      mapN(visitExp(exp1), visitExp(exp2), stf) {
        case (e1, e2, s) => Expression.FixpointCompose(e1, e2, s, tpe, eff, loc)
      }

    case Expression.FixpointSolve(exp, _, tpe, eff, loc) =>
      // Compute the stratification.
      val stf = stratifyWithCache(dg, tpe, loc)

      mapN(visitExp(exp), stf) {
        case (e, s) => Expression.FixpointSolve(e, s, tpe, eff, loc)
      }

    case Expression.FixpointProject(name, exp, tpe, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.FixpointProject(name, e, tpe, eff, loc)
      }

    case Expression.FixpointEntails(exp1, exp2, tpe, eff, loc) =>
      mapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => Expression.FixpointEntails(e1, e2, tpe, eff, loc)
      }
    case Expression.FixpointFold(name, exp1, exp2, exp3, tpe, eff, loc) =>
      mapN(visitExp(exp1), visitExp(exp2), visitExp(exp3)) {
        case (e1, e2, e3) => Expression.FixpointFold(name, e1, e2, e3, tpe, eff, loc)
      }
  }

  /**
    * Returns the dependency graph of the given definition `def0`.
    */
  private def dependencyGraphOfDef(def0: Def): DependencyGraph = dependencyGraphOfExp(def0.exp)

  /**
    * Returns the dependency graph of the given expression `exp0`.
    */
  private def dependencyGraphOfExp(exp0: Expression): DependencyGraph = exp0 match {
    case Expression.Unit(_) => DependencyGraph.empty

    case Expression.Null(_, _) => DependencyGraph.empty

    case Expression.True(_) => DependencyGraph.empty

    case Expression.False(_) => DependencyGraph.empty

    case Expression.Char(_, _) => DependencyGraph.empty

    case Expression.Float32(_, _) => DependencyGraph.empty

    case Expression.Float64(_, _) => DependencyGraph.empty

    case Expression.Int8(_, _) => DependencyGraph.empty

    case Expression.Int16(_, _) => DependencyGraph.empty

    case Expression.Int32(_, _) => DependencyGraph.empty

    case Expression.Int64(_, _) => DependencyGraph.empty

    case Expression.BigInt(_, _) => DependencyGraph.empty

    case Expression.Str(_, _) => DependencyGraph.empty

    case Expression.Default(_, _) => DependencyGraph.empty

    case Expression.Wild(_, _) => DependencyGraph.empty

    case Expression.Var(_, _, _) => DependencyGraph.empty

    case Expression.Def(_, _, _) => DependencyGraph.empty

    case Expression.Sig(_, _, _) => DependencyGraph.empty

    case Expression.Hole(_, _, _, _) => DependencyGraph.empty

    case Expression.Lambda(_, exp, _, _) =>
      dependencyGraphOfExp(exp)

    case Expression.Apply(exp, exps, _, _, _) =>
      val init = dependencyGraphOfExp(exp)
      exps.foldLeft(init) {
        case (acc, exp) => acc + dependencyGraphOfExp(exp)
      }

    case Expression.Unary(_, exp, _, _, _) =>
      dependencyGraphOfExp(exp)

    case Expression.Binary(_, exp1, exp2, _, _, _) =>
      dependencyGraphOfExp(exp1) + dependencyGraphOfExp(exp2)

    case Expression.Let(_, exp1, exp2, _, _, _) =>
      dependencyGraphOfExp(exp1) + dependencyGraphOfExp(exp2)

    case Expression.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      dependencyGraphOfExp(exp1) + dependencyGraphOfExp(exp2) + dependencyGraphOfExp(exp3)

    case Expression.Stm(exp1, exp2, _, _, _) =>
      dependencyGraphOfExp(exp1) + dependencyGraphOfExp(exp2)

    case Expression.Match(exp, rules, _, _, _) =>
      val dg = dependencyGraphOfExp(exp)
      rules.foldLeft(dg) {
        case (acc, MatchRule(_, g, b)) => acc + dependencyGraphOfExp(g) + dependencyGraphOfExp(b)
      }

    case Expression.Choose(exps, rules, _, _, _) =>
      val dg1 = exps.foldLeft(DependencyGraph.empty) {
        case (acc, exp) => acc + dependencyGraphOfExp(exp)
      }
      val dg2 = rules.foldLeft(DependencyGraph.empty) {
        case (acc, ChoiceRule(_, exp)) => acc + dependencyGraphOfExp(exp)
      }
      dg1 + dg2

    case Expression.Tag(_, _, exp, _, _, _) =>
      dependencyGraphOfExp(exp)

    case Expression.Tuple(elms, _, _, _) =>
      elms.foldLeft(DependencyGraph.empty) {
        case (acc, e) => acc + dependencyGraphOfExp(e)
      }

    case Expression.RecordEmpty(_, _) =>
      DependencyGraph.empty

    case Expression.RecordSelect(base, _, _, _, _) =>
      dependencyGraphOfExp(base)

    case Expression.RecordExtend(_, value, rest, _, _, _) =>
      dependencyGraphOfExp(value) + dependencyGraphOfExp(rest)

    case Expression.RecordRestrict(_, rest, _, _, _) =>
      dependencyGraphOfExp(rest)

    case Expression.ArrayLit(elms, _, _, _) =>
      elms.foldLeft(DependencyGraph.empty) {
        case (acc, e) => acc + dependencyGraphOfExp(e)
      }

    case Expression.ArrayNew(elm, len, _, _, _) =>
      dependencyGraphOfExp(elm) + dependencyGraphOfExp(len)

    case Expression.ArrayLoad(base, index, _, _, _) =>
      dependencyGraphOfExp(base) + dependencyGraphOfExp(index)

    case Expression.ArrayLength(base, _, _) =>
      dependencyGraphOfExp(base)

    case Expression.ArrayStore(base, index, elm, _) =>
      dependencyGraphOfExp(base) + dependencyGraphOfExp(index) + dependencyGraphOfExp(elm)

    case Expression.ArraySlice(base, beginIndex, endIndex, _, _) =>
      dependencyGraphOfExp(base) + dependencyGraphOfExp(beginIndex) + dependencyGraphOfExp(endIndex)

    case Expression.Ref(exp, _, _, _) =>
      dependencyGraphOfExp(exp)

    case Expression.Deref(exp, _, _, _) =>
      dependencyGraphOfExp(exp)

    case Expression.Assign(exp1, exp2, _, _, _) =>
      dependencyGraphOfExp(exp1) + dependencyGraphOfExp(exp2)

    case Expression.Existential(_, exp, _) =>
      dependencyGraphOfExp(exp)

    case Expression.Universal(_, exp, _) =>
      dependencyGraphOfExp(exp)

    case Expression.Ascribe(exp, _, _, _) =>
      dependencyGraphOfExp(exp)

    case Expression.Cast(exp, _, _, _) =>
      dependencyGraphOfExp(exp)

    case Expression.TryCatch(exp, rules, _, _, _) =>
      rules.foldLeft(dependencyGraphOfExp(exp)) {
        case (acc, CatchRule(_, _, e)) => acc + dependencyGraphOfExp(e)
      }

    case Expression.InvokeConstructor(_, args, _, _, _) =>
      args.foldLeft(DependencyGraph.empty) {
        case (acc, e) => acc + dependencyGraphOfExp(e)
      }

    case Expression.InvokeMethod(_, exp, args, _, _, _) =>
      args.foldLeft(dependencyGraphOfExp(exp)) {
        case (acc, e) => acc + dependencyGraphOfExp(e)
      }

    case Expression.InvokeStaticMethod(_, args, _, _, _) =>
      args.foldLeft(DependencyGraph.empty) {
        case (acc, e) => acc + dependencyGraphOfExp(e)
      }

    case Expression.GetField(_, exp, _, _, _) =>
      dependencyGraphOfExp(exp)

    case Expression.PutField(_, exp1, exp2, _, _, _) =>
      dependencyGraphOfExp(exp1) + dependencyGraphOfExp(exp2)

    case Expression.GetStaticField(_, _, _, _) =>
      DependencyGraph.empty

    case Expression.PutStaticField(_, exp, _, _, _) =>
      dependencyGraphOfExp(exp)

    case Expression.NewChannel(exp, _, _, _) =>
      dependencyGraphOfExp(exp)

    case Expression.GetChannel(exp, _, _, _) =>
      dependencyGraphOfExp(exp)

    case Expression.PutChannel(exp1, exp2, _, _, _) =>
      dependencyGraphOfExp(exp1) + dependencyGraphOfExp(exp2)

    case Expression.SelectChannel(rules, default, _, _, _) =>
      val dg = default match {
        case None => DependencyGraph.empty
        case Some(d) => dependencyGraphOfExp(d)
      }

      rules.foldLeft(dg) {
        case (acc, SelectChannelRule(_, exp1, exp2)) => acc + dependencyGraphOfExp(exp1) + dependencyGraphOfExp(exp2)
      }

    case Expression.Spawn(exp, _, _, _) =>
      dependencyGraphOfExp(exp)

    case Expression.Lazy(exp, _, _) =>
      dependencyGraphOfExp(exp)

    case Expression.Force(exp, _, _, _) =>
      dependencyGraphOfExp(exp)

    case Expression.FixpointConstraintSet(cs, _, _, _) =>
      cs.foldLeft(DependencyGraph.empty) {
        case (dg, c) => dg + dependencyGraphOfConstraint(c)
      }

    case Expression.FixpointCompose(exp1, exp2, _, _, _, _) =>
      dependencyGraphOfExp(exp1) + dependencyGraphOfExp(exp2)

    case Expression.FixpointSolve(exp, _, _, _, _) =>
      dependencyGraphOfExp(exp)

    case Expression.FixpointProject(_, exp, _, _, _) =>
      dependencyGraphOfExp(exp)

    case Expression.FixpointEntails(exp1, exp2, _, _, _) =>
      dependencyGraphOfExp(exp1) + dependencyGraphOfExp(exp2)

    case Expression.FixpointFold(_, exp1, exp2, exp3, _, _, _) =>
      dependencyGraphOfExp(exp1) + dependencyGraphOfExp(exp2) + dependencyGraphOfExp(exp3)
  }

  /**
    * Returns the dependency graph of the given constraint `c0`.
    */
  private def dependencyGraphOfConstraint(c0: Constraint): DependencyGraph = c0 match {
    case Constraint(cparams, head, body, _) =>
      getPredicateSym(head) match {
        case None => DependencyGraph.empty
        case Some(headSym) =>
          val dependencies = body flatMap (b => visitDependencyEdge(headSym, b))
          DependencyGraph(dependencies.toSet)
      }
  }

  /**
    * Optionally returns the predicate symbol of the given head atom `head0`.
    */
  private def getPredicateSym(head0: Predicate.Head): Option[Name.Pred] = head0 match {
    case Predicate.Head.Atom(pred, den, terms, tpe, loc) => Some(pred)
    case Predicate.Head.Union(exp, tpe, loc) =>
      // NB: The situation is actually more complicated.
      // If the union expressions evaluates to predicate symbols A, B, C it could
      // be argued that we should add dependency edges for each of these symbols.
      None
  }

  /**
    * Optionally returns a dependency edge of the right type for the given head symbol `head` and body predicate `body0`.
    */
  private def visitDependencyEdge(head: Name.Pred, body0: Predicate.Body): Option[DependencyEdge] = body0 match {
    case Predicate.Body.Atom(pred, den, polarity, terms, tpe, loc) => polarity match {
      case Polarity.Positive => Some(DependencyEdge.Positive(head, pred, loc))
      case Polarity.Negative => Some(DependencyEdge.Negative(head, pred, loc))
    }

    case Predicate.Body.Guard(exp, loc) => None
  }

  /**
    * Computes the stratification of the given dependency graph `dg` for the given row type `tpe` at the given source location `loc`.
    *
    * Uses the given cache and updates it if required.
    */
  private def stratifyWithCache(dg: DependencyGraph, tpe: Type, loc: SourceLocation)(implicit cache: Cache): Validation[Ast.Stratification, StratificationError] = {
    // The key is the set of predicate symbols that occur in the row type.
    val key = predicateSymbolsOf(tpe)

    // Lookup the key in the stratification cache.
    cache.get(key) match {
      case None =>
        // Case 1: Cache miss: Compute the stratification and possibly cache it.

        // Compute the restricted dependency graph.
        val rg = restrict(dg, tpe)

        // Compute the stratification.
        stratify(rg, tpe, loc) match {
          case Validation.Success(stf) =>
            // Cache the stratification.
            cache.put(key, stf)

            // And return it.
            stf.toSuccess
          case Validation.Failure(errors) =>
            // Unable to stratify. Do not cache the result.
            Validation.Failure(errors)
        }
      case Some(stf) =>
        // Case 2: Cache hit: Return the stratification.
        stf.toSuccess
    }
  }

  /**
    * Computes the stratification of the given dependency graph `g` at the given source location `loc`.
    *
    * See Database and Knowledge - Base Systems Volume 1 Ullman, Algorithm 3.5 p 133
    */
  private def stratify(g: DependencyGraph, tpe: Type, loc: SourceLocation): Validation[Ast.Stratification, StratificationError] = {
    //
    // Maintain a mutable map from predicate symbols to their (maximum) stratum number.
    //
    // Any predicate symbol not explicitly in the map has a default value of zero.
    //
    val stratumOf = mutable.Map.empty[Name.Pred, Int]

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
          case DependencyEdge.Positive(headSym, bodySym, _) =>
            // Case 1: The stratum of the head must be in the same or a higher stratum as the body.
            val headStratum = stratumOf.getOrElseUpdate(headSym, 0)
            val bodyStratum = stratumOf.getOrElseUpdate(bodySym, 0)

            if (!(headStratum >= bodyStratum)) {
              // Put the head in the same stratum as the body.
              stratumOf.put(headSym, bodyStratum)
              changed = true
            }

          case DependencyEdge.Negative(headSym, bodySym, edgeLoc) =>
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
                return StratificationError(findNegativeCycle(bodySym, headSym, g, edgeLoc), tpe, loc).toFailure
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
  private def findNegativeCycle(src: Name.Pred, dst: Name.Pred, g: DependencyGraph, loc: SourceLocation): List[(Name.Pred, SourceLocation)] = {
    // Computes a map from symbols to their successors.
    val succ = mutable.Map.empty[Name.Pred, Set[(Name.Pred, SourceLocation)]]
    for (edge <- g.xs) {
      edge match {
        case DependencyEdge.Positive(head, body, loc) =>
          val s = succ.getOrElse(body, Set.empty)
          succ.put(body, s + ((head, loc)))
        case DependencyEdge.Negative(head, body, loc) =>
          val s = succ.getOrElse(body, Set.empty)
          succ.put(body, s + ((head, loc)))
      }
    }

    // We perform a DFS using recursion to find the cycle.

    // A map from symbols to their immediate predecessor in the DFS.
    val pred = mutable.Map.empty[Name.Pred, (Name.Pred, SourceLocation)]

    // A set of previously seen symbols.
    val seen = mutable.Set.empty[Name.Pred]

    // Recursively visit the given symbol.
    def visit(curr: Name.Pred): Unit = {
      // Update the set of previously seen nodes.
      seen.add(curr)

      // Recursively visit each unseen child.
      for ((succ, loc) <- succ.getOrElse(curr, Set.empty)) {
        if (!seen.contains(succ)) {
          pred.update(succ, (curr, loc))
          visit(succ)
        }
      }
    }

    // Compute the predecessor map.
    visit(dst)

    // Recursively constructs a path from `src` and backwards through the graph.
    def unroll(curr: Name.Pred): List[(Name.Pred, SourceLocation)] = pred.get(curr) match {
      case None => Nil
      case Some((prev, loc)) => (prev, loc) :: unroll(prev)
    }

    // Assemble the full path.
    (src, loc) :: unroll(src) ::: (src, loc) :: Nil
  }

  /**
    * Reorders a constraint such that its negated atoms occur last.
    */
  private def reorder(c0: Constraint): Constraint = {
    /**
      * Returns `true` if the body predicate is negated.
      */
    def isNegative(p: Predicate.Body): Boolean = p match {
      case Predicate.Body.Atom(_, _, Polarity.Negative, _, _, _) => true
      case _ => false
    }

    // Collect all the negated and non-negated predicates.
    val negated = c0.body filter isNegative
    val nonNegated = c0.body filterNot isNegative

    // Reassemble the constraint.
    c0.copy(body = nonNegated ::: negated)
  }

  /**
    * Restricts the given dependency graph `dg` to the predicate symbols that occur in the given type `tpe`.
    */
  private def restrict(dg: DependencyGraph, tpe: Type): DependencyGraph = {
    val predSyms = predicateSymbolsOf(tpe)
    dg.restrict(predSyms)
  }

  /**
    * Returns the set of predicate symbols that appears in the given row type `tpe`.
    */
  private def predicateSymbolsOf(tpe: Type): Set[Name.Pred] = tpe.typeConstructors.foldLeft(Set.empty[Name.Pred]) {
    case (acc, TypeConstructor.SchemaExtend(name)) => acc + Name.Pred(name, SourceLocation.Unknown) // TODO: Add source location
    case (acc, _) => acc
  }

}
