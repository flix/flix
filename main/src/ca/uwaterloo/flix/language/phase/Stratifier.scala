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
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.Ast.{ConstraintGraph, MultiEdge, Polarity, TypedPredicate}
import ca.uwaterloo.flix.language.ast.Type.eraseAliases
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.Body
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.errors.StratificationError
import ca.uwaterloo.flix.language.phase.UllmansAlgorithm.DependencyGraph
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps, Validation}

import scala.annotation.tailrec
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
    * Returns a stratified version of the given AST `root`.
    */
  def run(root: Root)(implicit flix: Flix): Validation[Root, CompilationMessage] = flix.phase("Stratifier") {
    // A cache of stratifications. Only caches successful stratifications.
    val cache: Cache = mutable.Map.empty

    // Check if computation of stratification is disabled.
    if (flix.options.xnostratifier)
      return root.toSuccess

    // Compute an over-approximation of the dependency graph for all constraints in the program.
    val dg = flix.subphase("Compute Dependency Graph") {
      ParOps.parAgg(root.defs, ConstraintGraph.empty)({
        case (acc, (_, decl)) => acc + constraintGraphOfDef(decl)
      }, _ + _)
    }

    // Compute the stratification at every datalog expression in the ast.`
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
    * A type alias for the stratification cache.
    */
  private type Cache = mutable.Map[Map[Name.Pred, TypeKey], Ast.Stratification]

  /**
    * The type of the value used to calculate type equality.
    */
  private type TypeKey = Int

  /**
    * Computes the information used for type equality checking.
    * The stratification precision can be changed by changing this
    * function and fixing the `TypeKey` accordingly.
    */
  private def typeKeyOf(tpe: Type): TypeKey = arityOf(tpe)

  /**
    * Computes the arity of a `Relation` or `Lattice` type.
    */
  private def arityOf(tpe: Type): Int = eraseAliases(tpe) match {
    case Type.Apply(Type.Cst(TypeConstructor.Relation | TypeConstructor.Lattice, _), element, _) => element.baseType match {
      case Type.Cst(TypeConstructor.Tuple(arity), _) => arity // Multi-ary
      case Type.Cst(TypeConstructor.Unit, _) => 0 // Nullary
      case _ => 1 // Unary
    }
    case other => throw InternalCompilerException(s"Unexpected non-relation non-lattice type $other")
  }

  /**
    * Performs stratification of the given definition `def0`.
    */
  private def visitDef(def0: Def)(implicit dg: ConstraintGraph, cache: Cache): Validation[Def, CompilationMessage] =
    visitExp(def0.impl.exp) map {
      case e => def0.copy(impl = def0.impl.copy(exp = e))
    }

  /**
    * Performs stratification of the given expression `exp0`.
    *
    * Returns [[Success]] if the expression is stratified. Otherwise returns [[Failure]] with a [[StratificationError]].
    */
  private def visitExp(exp0: Expression)(implicit dg: ConstraintGraph, cache: Cache): Validation[Expression, StratificationError] = exp0 match {
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

    case Expression.Hole(_, _, _) => exp0.toSuccess

    case Expression.Lambda(fparam, exp, tpe, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.Lambda(fparam, e, tpe, loc)
      }

    case Expression.Apply(exp, exps, tpe, eff, loc) =>
      mapN(visitExp(exp), traverse(exps)(visitExp)) {
        case (e, es) => Expression.Apply(e, es, tpe, eff, loc)
      }

    case Expression.Unary(sop, exp, tpe, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.Unary(sop, e, tpe, eff, loc)
      }

    case Expression.Binary(sop, exp1, exp2, tpe, eff, loc) =>
      mapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => Expression.Binary(sop, e1, e2, tpe, eff, loc)
      }

    case Expression.Let(sym, mod, exp1, exp2, tpe, eff, loc) =>
      mapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => Expression.Let(sym, mod, e1, e2, tpe, eff, loc)
      }

    case Expression.LetRec(sym, mod, exp1, exp2, tpe, eff, loc) =>
      mapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => Expression.LetRec(sym, mod, e1, e2, tpe, eff, loc)
      }

    case Expression.LetRegion(sym, exp, tpe, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.LetRegion(sym, e, tpe, eff, loc)
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

    case Expression.RecordSelect(base, field, tpe, eff, loc) =>
      mapN(visitExp(base)) {
        case b => Expression.RecordSelect(b, field, tpe, eff, loc)
      }

    case Expression.RecordExtend(field, value, rest, tpe, eff, loc) =>
      mapN(visitExp(value), visitExp(rest)) {
        case (v, r) => Expression.RecordExtend(field, v, r, tpe, eff, loc)
      }

    case Expression.RecordRestrict(field, rest, tpe, eff, loc) =>
      mapN(visitExp(rest)) {
        case r => Expression.RecordRestrict(field, r, tpe, eff, loc)
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

    case Expression.Ascribe(exp, tpe, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.Ascribe(e, tpe, eff, loc)
      }

    case Expression.Cast(exp, declaredType, declaredEff, tpe, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.Cast(e, declaredType, declaredEff, tpe, eff, loc)
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

    case Expression.FixpointMerge(exp1, exp2, _, tpe, eff, loc) =>
      // Compute the stratification.
      val stf = stratifyWithCache(dg, tpe, loc)

      mapN(visitExp(exp1), visitExp(exp2), stf) {
        case (e1, e2, s) => Expression.FixpointMerge(e1, e2, s, tpe, eff, loc)
      }

    case Expression.FixpointSolve(exp, _, tpe, eff, loc) =>
      // Compute the stratification.
      val stf = stratifyWithCache(dg, tpe, loc)

      mapN(visitExp(exp), stf) {
        case (e, s) => Expression.FixpointSolve(e, s, tpe, eff, loc)
      }

    case Expression.FixpointFilter(pred, exp, tpe, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.FixpointFilter(pred, e, tpe, eff, loc)
      }

    case Expression.FixpointProjectIn(exp, pred, tpe, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.FixpointProjectIn(e, pred, tpe, eff, loc)
      }

    case Expression.FixpointProjectOut(pred, exp, tpe, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.FixpointProjectOut(pred, e, tpe, eff, loc)
      }

    case Expression.Reify(t, tpe, eff, loc) =>
      Expression.Reify(t, tpe, eff, loc).toSuccess

    case Expression.ReifyType(t, k, tpe, eff, loc) =>
      Expression.ReifyType(t, k, tpe, eff, loc).toSuccess

    case Expression.ReifyEff(sym, exp1, exp2, exp3, tpe, eff, loc) =>
      mapN(visitExp(exp1), visitExp(exp2), visitExp(exp3)) {
        case (e1, e2, e3) => Expression.ReifyEff(sym, e1, e2, e3, tpe, eff, loc)
      }

  }

  /**
    * Returns the constraint graph of the given definition `def0`.
    */
  private def constraintGraphOfDef(def0: Def): ConstraintGraph = constraintGraphOfExp(def0.impl.exp)

  /**
    * Returns the constraint graph of the given expression `exp0`.
    */
  private def constraintGraphOfExp(exp0: Expression): ConstraintGraph = exp0 match {
    case Expression.Unit(_) => ConstraintGraph.empty

    case Expression.Null(_, _) => ConstraintGraph.empty

    case Expression.True(_) => ConstraintGraph.empty

    case Expression.False(_) => ConstraintGraph.empty

    case Expression.Char(_, _) => ConstraintGraph.empty

    case Expression.Float32(_, _) => ConstraintGraph.empty

    case Expression.Float64(_, _) => ConstraintGraph.empty

    case Expression.Int8(_, _) => ConstraintGraph.empty

    case Expression.Int16(_, _) => ConstraintGraph.empty

    case Expression.Int32(_, _) => ConstraintGraph.empty

    case Expression.Int64(_, _) => ConstraintGraph.empty

    case Expression.BigInt(_, _) => ConstraintGraph.empty

    case Expression.Str(_, _) => ConstraintGraph.empty

    case Expression.Default(_, _) => ConstraintGraph.empty

    case Expression.Wild(_, _) => ConstraintGraph.empty

    case Expression.Var(_, _, _) => ConstraintGraph.empty

    case Expression.Def(_, _, _) => ConstraintGraph.empty

    case Expression.Sig(_, _, _) => ConstraintGraph.empty

    case Expression.Hole(_, _, _) => ConstraintGraph.empty

    case Expression.Lambda(_, exp, _, _) =>
      constraintGraphOfExp(exp)

    case Expression.Apply(exp, exps, _, _, _) =>
      val init = constraintGraphOfExp(exp)
      exps.foldLeft(init) {
        case (acc, exp) => acc + constraintGraphOfExp(exp)
      }

    case Expression.Unary(_, exp, _, _, _) =>
      constraintGraphOfExp(exp)

    case Expression.Binary(_, exp1, exp2, _, _, _) =>
      constraintGraphOfExp(exp1) + constraintGraphOfExp(exp2)

    case Expression.Let(_, _, exp1, exp2, _, _, _) =>
      constraintGraphOfExp(exp1) + constraintGraphOfExp(exp2)

    case Expression.LetRec(_, _, exp1, exp2, _, _, _) =>
      constraintGraphOfExp(exp1) + constraintGraphOfExp(exp2)

    case Expression.LetRegion(_, exp, _, _, _) =>
      constraintGraphOfExp(exp)

    case Expression.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      constraintGraphOfExp(exp1) + constraintGraphOfExp(exp2) + constraintGraphOfExp(exp3)

    case Expression.Stm(exp1, exp2, _, _, _) =>
      constraintGraphOfExp(exp1) + constraintGraphOfExp(exp2)

    case Expression.Match(exp, rules, _, _, _) =>
      val dg = constraintGraphOfExp(exp)
      rules.foldLeft(dg) {
        case (acc, MatchRule(_, g, b)) => acc + constraintGraphOfExp(g) + constraintGraphOfExp(b)
      }

    case Expression.Choose(exps, rules, _, _, _) =>
      val dg1 = exps.foldLeft(ConstraintGraph.empty) {
        case (acc, exp) => acc + constraintGraphOfExp(exp)
      }
      val dg2 = rules.foldLeft(ConstraintGraph.empty) {
        case (acc, ChoiceRule(_, exp)) => acc + constraintGraphOfExp(exp)
      }
      dg1 + dg2

    case Expression.Tag(_, _, exp, _, _, _) =>
      constraintGraphOfExp(exp)

    case Expression.Tuple(elms, _, _, _) =>
      elms.foldLeft(ConstraintGraph.empty) {
        case (acc, e) => acc + constraintGraphOfExp(e)
      }

    case Expression.RecordEmpty(_, _) =>
      ConstraintGraph.empty

    case Expression.RecordSelect(base, _, _, _, _) =>
      constraintGraphOfExp(base)

    case Expression.RecordExtend(_, value, rest, _, _, _) =>
      constraintGraphOfExp(value) + constraintGraphOfExp(rest)

    case Expression.RecordRestrict(_, rest, _, _, _) =>
      constraintGraphOfExp(rest)

    case Expression.ArrayLit(elms, _, _, _) =>
      elms.foldLeft(ConstraintGraph.empty) {
        case (acc, e) => acc + constraintGraphOfExp(e)
      }

    case Expression.ArrayNew(elm, len, _, _, _) =>
      constraintGraphOfExp(elm) + constraintGraphOfExp(len)

    case Expression.ArrayLoad(base, index, _, _, _) =>
      constraintGraphOfExp(base) + constraintGraphOfExp(index)

    case Expression.ArrayLength(base, _, _) =>
      constraintGraphOfExp(base)

    case Expression.ArrayStore(base, index, elm, _) =>
      constraintGraphOfExp(base) + constraintGraphOfExp(index) + constraintGraphOfExp(elm)

    case Expression.ArraySlice(base, beginIndex, endIndex, _, _) =>
      constraintGraphOfExp(base) + constraintGraphOfExp(beginIndex) + constraintGraphOfExp(endIndex)

    case Expression.Ref(exp, _, _, _) =>
      constraintGraphOfExp(exp)

    case Expression.Deref(exp, _, _, _) =>
      constraintGraphOfExp(exp)

    case Expression.Assign(exp1, exp2, _, _, _) =>
      constraintGraphOfExp(exp1) + constraintGraphOfExp(exp2)

    case Expression.Ascribe(exp, _, _, _) =>
      constraintGraphOfExp(exp)

    case Expression.Cast(exp, _, _, _, _, _) =>
      constraintGraphOfExp(exp)

    case Expression.TryCatch(exp, rules, _, _, _) =>
      rules.foldLeft(constraintGraphOfExp(exp)) {
        case (acc, CatchRule(_, _, e)) => acc + constraintGraphOfExp(e)
      }

    case Expression.InvokeConstructor(_, args, _, _, _) =>
      args.foldLeft(ConstraintGraph.empty) {
        case (acc, e) => acc + constraintGraphOfExp(e)
      }

    case Expression.InvokeMethod(_, exp, args, _, _, _) =>
      args.foldLeft(constraintGraphOfExp(exp)) {
        case (acc, e) => acc + constraintGraphOfExp(e)
      }

    case Expression.InvokeStaticMethod(_, args, _, _, _) =>
      args.foldLeft(ConstraintGraph.empty) {
        case (acc, e) => acc + constraintGraphOfExp(e)
      }

    case Expression.GetField(_, exp, _, _, _) =>
      constraintGraphOfExp(exp)

    case Expression.PutField(_, exp1, exp2, _, _, _) =>
      constraintGraphOfExp(exp1) + constraintGraphOfExp(exp2)

    case Expression.GetStaticField(_, _, _, _) =>
      ConstraintGraph.empty

    case Expression.PutStaticField(_, exp, _, _, _) =>
      constraintGraphOfExp(exp)

    case Expression.NewChannel(exp, _, _, _) =>
      constraintGraphOfExp(exp)

    case Expression.GetChannel(exp, _, _, _) =>
      constraintGraphOfExp(exp)

    case Expression.PutChannel(exp1, exp2, _, _, _) =>
      constraintGraphOfExp(exp1) + constraintGraphOfExp(exp2)

    case Expression.SelectChannel(rules, default, _, _, _) =>
      val dg = default match {
        case None => ConstraintGraph.empty
        case Some(d) => constraintGraphOfExp(d)
      }

      rules.foldLeft(dg) {
        case (acc, SelectChannelRule(_, exp1, exp2)) => acc + constraintGraphOfExp(exp1) + constraintGraphOfExp(exp2)
      }

    case Expression.Spawn(exp, _, _, _) =>
      constraintGraphOfExp(exp)

    case Expression.Lazy(exp, _, _) =>
      constraintGraphOfExp(exp)

    case Expression.Force(exp, _, _, _) =>
      constraintGraphOfExp(exp)

    case Expression.FixpointConstraintSet(cs, _, _, _) =>
      cs.foldLeft(ConstraintGraph.empty) {
        case (dg, c) => dg + constraintGraphOfConstraint(c)
      }

    case Expression.FixpointMerge(exp1, exp2, _, _, _, _) =>
      constraintGraphOfExp(exp1) + constraintGraphOfExp(exp2)

    case Expression.FixpointSolve(exp, _, _, _, _) =>
      constraintGraphOfExp(exp)

    case Expression.FixpointFilter(_, exp, _, _, _) =>
      constraintGraphOfExp(exp)

    case Expression.FixpointProjectIn(exp, _, _, _, _) =>
      constraintGraphOfExp(exp)

    case Expression.FixpointProjectOut(_, exp, _, _, _) =>
      constraintGraphOfExp(exp)

    case Expression.Reify(_, _, _, _) =>
      ConstraintGraph.empty

    case Expression.ReifyType(_, _, _, _, _) =>
      ConstraintGraph.empty

    case Expression.ReifyEff(_, exp1, exp2, exp3, _, _, _) =>
      constraintGraphOfExp(exp1) + constraintGraphOfExp(exp2) + constraintGraphOfExp(exp3)
  }

  /**
    * Returns the constraint graph of the given constraint `c0`.
    */
  private def constraintGraphOfConstraint(c0: Constraint): ConstraintGraph = c0 match {
    case Constraint(_, Predicate.Head.Atom(headSym, _, _, headTpe, _), body, _) =>
      val (pos, neg) = body.foldLeft((Vector.empty[TypedPredicate], Vector.empty[TypedPredicate])) {
        case ((pos, neg), b) => b match {
          case Body.Atom(pred, _, polarity, _, atomTpe, loc) => polarity match {
            case Polarity.Positive => (pos :+ TypedPredicate(pred, atomTpe, loc), neg)
            case Polarity.Negative => (pos, neg :+ TypedPredicate(pred, atomTpe, loc))
          }
          case Body.Guard(_, _) => (pos, neg)
          case Body.Loop(_, _, _) => (pos, neg)
        }
      }
      val edge = MultiEdge((headSym, headTpe), pos, neg)
      ConstraintGraph(Set(edge))
  }

  /**
    * Computes the stratification of the given constraint graph `dg` for the given row type `tpe` at the given source location `loc`.
    *
    * Uses the given cache and updates it if required.
    */
  private def stratifyWithCache(dg: ConstraintGraph, tpe: Type, loc: SourceLocation)(implicit cache: Cache): Validation[Ast.Stratification, StratificationError] = {
    // The key is the set of predicates that occur in the row type.
    val predSyms = predicateSymbolsOf(tpe)

    val key = predSyms.map { case (p, t) => p -> typeKeyOf(t) }

    // Lookup the key in the stratification cache.
    cache.get(key) match {
      case Some(stf) =>
        // Cache hit: Return the stratification.
        stf.toSuccess

      case None =>
        // Cache miss: Compute the stratification and possibly cache it.

        // Compute the restricted constraint graph.
        val rg = dg.restrict(predSyms, (t1, t2) => typeKeyOf(t1) == typeKeyOf(t2))

        // Compute the stratification.
        UllmansAlgorithm.stratify(constraintGraphToDependencyGraph(rg), tpe, loc) match {
          case Validation.Success(stf) =>
            // Cache the stratification.
            cache.put(key, stf)

            // And return it.
            stf.toSuccess
          case Validation.Failure(errors) =>
            // Unable to stratify. Do not cache the result.
            Validation.Failure(errors)
        }
    }
  }

  /**
    * Computes the dependency graph from the Constraint graph.
    */
  private def constraintGraphToDependencyGraph(c: ConstraintGraph): DependencyGraph =
    c.xs.flatMap {
      case MultiEdge((head, _), positives, negatives) =>
        val p = positives.map { case TypedPredicate(b, _, loc) => UllmansAlgorithm.DependencyEdge.Positive(head, b, loc) }
        val n = negatives.map { case TypedPredicate(b, _, loc) => UllmansAlgorithm.DependencyEdge.Negative(head, b, loc) }
        p ++ n
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
    * Returns the map of predicates that appears in the given Schema `tpe`.
    * A non-Schema type will result in an `InternalCompilerException`.
    */
  private def predicateSymbolsOf(tpe: Type): Map[Name.Pred, Type] = {
    @tailrec
    def visitType(tpe: Type, acc: Map[Name.Pred, Type]): Map[Name.Pred, Type] = tpe match {
      case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SchemaRowExtend(pred), _), predType, _), rest, _) =>
        visitType(rest, acc + (pred -> predType))
      case _ => acc
    }

    Type.eraseAliases(tpe) match {
      case Type.Apply(Type.Cst(TypeConstructor.Schema, _), schemaRow, _) => visitType(schemaRow, Map.empty)
      case other => throw InternalCompilerException(s"Unexpected non-schema type $other")
    }
  }

}
