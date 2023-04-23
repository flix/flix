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
import ca.uwaterloo.flix.language.ast.Ast._
import ca.uwaterloo.flix.language.ast.Type.eraseAliases
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.Body
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.errors.StratificationError
import ca.uwaterloo.flix.language.phase.unification.Unification
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.collection.ListMap
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps, Validation}

import scala.annotation.tailrec

/**
  * The stratification phase breaks constraints into strata.
  *
  * "Formally, rules are stratified if whenever there is a rule with
  * head predicate p and a negated subgoal with predicate q, there is
  * no path in the dependency graph from p to q" -- Ullman 132
  *
  * A negated subgoal is generalized here to a subgoal that is negated
  * or fixed, collectively called a strong dependency.
  *
  * Reports a [[StratificationError]] if the constraints cannot be stratified.
  */
object Stratifier {
  /**
    * Returns a stratified version of the given AST `root`.
    */
  def run(root: Root)(implicit flix: Flix): Validation[Root, CompilationMessage] = flix.phase("Stratifier") {
    // Compute an over-approximation of the dependency graph for all constraints in the program.
    val g = flix.subphase("Compute Global Dependency Graph") {
      val defs = root.defs.values.toList
      val instanceDefs = root.instances.values.flatten.flatMap(_.defs)
      ParOps.parAgg(defs ++ instanceDefs, LabelledGraph.empty)({
        case (acc, d) => acc + labelledGraphOfDef(d)
      }, _ + _)
    }

    // Compute the stratification at every datalog expression in the ast.
    val newDefs = flix.subphase("Stratify Defs") {
      val result = ParOps.parMap(root.defs)(kv => visitDef(kv._2)(root, g, flix).map(d => kv._1 -> d))
      Validation.sequence(result)
    }
    val newInstances = flix.subphase("Stratify Instance Defs") {
      val result = ParOps.parMap(root.instances) {
        case (sym, is) =>
          val x = traverse(is)(i => visitInstance(i)(root, g, flix))
          x.map(d => sym -> d)
      }
      Validation.sequence(result)
    }

    mapN(newDefs, newInstances) {
      case (ds, is) => root.copy(defs = ds.toMap, instances = is.toMap)
    }
  }

  /**
    * Performs Stratification of the given instance `i0`.
    */
  private def visitInstance(i0: TypedAst.Instance)(implicit root: Root, g: LabelledGraph, flix: Flix): Validation[TypedAst.Instance, CompilationMessage] =
    traverse(i0.defs)(d => visitDef(d)).map(ds => i0.copy(defs = ds))

  /**
    * Performs stratification of the given definition `def0`.
    */
  private def visitDef(def0: Def)(implicit root: Root, g: LabelledGraph, flix: Flix): Validation[Def, CompilationMessage] =
    visitExp(def0.impl.exp) map {
      case e => def0.copy(impl = def0.impl.copy(exp = e))
    }

  /**
    * Performs stratification of the given expression `exp0`.
    *
    * Returns [[Success]] if the expression is stratified. Otherwise returns [[Failure]] with a [[StratificationError]].
    */
  private def visitExp(exp0: Expression)(implicit root: Root, g: LabelledGraph, flix: Flix): Validation[Expression, StratificationError] = exp0 match {
    case Expression.Cst(_, _, _) => exp0.toSuccess

    case Expression.Wild(_, _) => exp0.toSuccess

    case Expression.Var(_, _, _) => exp0.toSuccess

    case Expression.Def(_, _, _) => exp0.toSuccess

    case Expression.Sig(_, _, _) => exp0.toSuccess

    case Expression.Hole(_, _, _) => exp0.toSuccess

    case Expression.HoleWithExp(exp, tpe, pur, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.HoleWithExp(e, tpe, pur, eff, loc)
      }

    case Expression.OpenAs(sym, exp, tpe, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.OpenAs(sym, e, tpe, loc)
      }

    case Expression.Use(sym, alias, exp, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.Use(sym, alias, e, loc)
      }

    case Expression.Lambda(fparam, exp, tpe, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.Lambda(fparam, e, tpe, loc)
      }

    case Expression.Apply(exp, exps, tpe, pur, eff, loc) =>
      mapN(visitExp(exp), traverse(exps)(visitExp)) {
        case (e, es) => Expression.Apply(e, es, tpe, pur, eff, loc)
      }

    case Expression.Unary(sop, exp, tpe, pur, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.Unary(sop, e, tpe, pur, eff, loc)
      }

    case Expression.Binary(sop, exp1, exp2, tpe, pur, eff, loc) =>
      mapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => Expression.Binary(sop, e1, e2, tpe, pur, eff, loc)
      }

    case Expression.Let(sym, mod, exp1, exp2, tpe, pur, eff, loc) =>
      mapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => Expression.Let(sym, mod, e1, e2, tpe, pur, eff, loc)
      }

    case Expression.LetRec(sym, mod, exp1, exp2, tpe, pur, eff, loc) =>
      mapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => Expression.LetRec(sym, mod, e1, e2, tpe, pur, eff, loc)
      }

    case Expression.Region(_, _) =>
      exp0.toSuccess

    case Expression.Scope(sym, regionVar, exp, tpe, pur, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.Scope(sym, regionVar, e, tpe, pur, eff, loc)
      }

    case Expression.ScopeExit(exp1, exp2, tpe, pur, eff, loc) =>
      mapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => Expression.ScopeExit(e1, e2, tpe, pur, eff, loc)
      }

    case Expression.IfThenElse(exp1, exp2, exp3, tpe, pur, eff, loc) =>
      mapN(visitExp(exp1), visitExp(exp2), visitExp(exp3)) {
        case (e1, e2, e3) => Expression.IfThenElse(e1, e2, e3, tpe, pur, eff, loc)
      }

    case Expression.Stm(exp1, exp2, tpe, pur, eff, loc) =>
      mapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => Expression.Stm(e1, e2, tpe, pur, eff, loc)
      }

    case Expression.Discard(exp, pur, eff, loc) =>
      visitExp(exp) map {
        case e => Expression.Discard(e, pur, eff, loc)
      }

    case Expression.Match(exp, rules, tpe, pur, eff, loc) =>
      val matchVal = visitExp(exp)
      val rulesVal = traverse(rules) {
        case MatchRule(pat, guard, body) => mapN(traverseOpt(guard)(visitExp), visitExp(body)) {
          case (g, b) => MatchRule(pat, g, b)
        }
      }
      mapN(matchVal, rulesVal) {
        case (m, rs) => Expression.Match(m, rs, tpe, pur, eff, loc)
      }

    case Expression.TypeMatch(exp, rules, tpe, pur, eff, loc) =>
      val matchVal = visitExp(exp)
      val rulesVal = traverse(rules) {
        case MatchTypeRule(sym, t, body) => mapN(visitExp(body)) {
          case b => MatchTypeRule(sym, t, b)
        }
      }
      mapN(matchVal, rulesVal) {
        case (m, rs) => Expression.TypeMatch(m, rs, tpe, pur, eff, loc)
      }

    case Expression.RelationalChoose(exps, rules, tpe, pur, eff, loc) =>
      val expsVal = traverse(exps)(visitExp)
      val rulesVal = traverse(rules) {
        case RelationalChoiceRule(pat, exp) => mapN(visitExp(exp))(RelationalChoiceRule(pat, _))
      }
      mapN(expsVal, rulesVal) {
        case (es, rs) => Expression.RelationalChoose(es, rs, tpe, pur, eff, loc)
      }

    case Expression.RestrictableChoose(star, exp, rules, tpe, pur, eff, loc) =>
      val expVal = visitExp(exp)
      val rulesVal = traverse(rules) {
        case RestrictableChoiceRule(pat, body) => mapN(visitExp(body))(RestrictableChoiceRule(pat, _))
      }
      mapN(expVal, rulesVal) {
        case (e, rs) => Expression.RestrictableChoose(star, e, rs, tpe, pur, eff, loc)
      }

    case Expression.Tag(sym, exp, tpe, pur, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.Tag(sym, e, tpe, pur, eff, loc)
      }

    case Expression.RestrictableTag(sym, exp, tpe, pur, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.RestrictableTag(sym, e, tpe, pur, eff, loc)
      }

    case Expression.Tuple(elms, tpe, pur, eff, loc) =>
      mapN(traverse(elms)(visitExp)) {
        case es => Expression.Tuple(es, tpe, pur, eff, loc)
      }

    case Expression.RecordEmpty(tpe, loc) =>
      Expression.RecordEmpty(tpe, loc).toSuccess

    case Expression.RecordSelect(base, field, tpe, pur, eff, loc) =>
      mapN(visitExp(base)) {
        case b => Expression.RecordSelect(b, field, tpe, pur, eff, loc)
      }

    case Expression.RecordExtend(field, value, rest, tpe, pur, eff, loc) =>
      mapN(visitExp(value), visitExp(rest)) {
        case (v, r) => Expression.RecordExtend(field, v, r, tpe, pur, eff, loc)
      }

    case Expression.RecordRestrict(field, rest, tpe, pur, eff, loc) =>
      mapN(visitExp(rest)) {
        case r => Expression.RecordRestrict(field, r, tpe, pur, eff, loc)
      }

    case Expression.ArrayLit(exps, exp, tpe, pur, eff, loc) =>
      mapN(traverse(exps)(visitExp), visitExp(exp)) {
        case (es, e) => Expression.ArrayLit(es, e, tpe, pur, eff, loc)
      }

    case Expression.ArrayNew(exp1, exp2, exp3, tpe, pur, eff, loc) =>
      mapN(visitExp(exp1), visitExp(exp2), visitExp(exp3)) {
        case (e1, e2, e3) => Expression.ArrayNew(e1, e2, e3, tpe, pur, eff, loc)
      }

    case Expression.ArrayLoad(base, index, tpe, pur, eff, loc) =>
      mapN(visitExp(base), visitExp(index)) {
        case (b, i) => Expression.ArrayLoad(b, i, tpe, pur, eff, loc)
      }

    case Expression.ArrayLength(base, pur, eff, loc) =>
      mapN(visitExp(base)) {
        case b => Expression.ArrayLength(b, pur, eff, loc)
      }

    case Expression.ArrayStore(base, index, elm, pur, eff, loc) =>
      mapN(visitExp(base), visitExp(index), visitExp(elm)) {
        case (b, i, e) => Expression.ArrayStore(b, i, e, pur, eff, loc)
      }

    case Expression.VectorLit(exps, tpe, pur, eff, loc) =>
      mapN(traverse(exps)(visitExp)) {
        case es => Expression.VectorLit(es, tpe, pur, eff, loc)
      }

    case Expression.VectorLoad(exp1, exp2, tpe, pur, eff, loc) =>
      mapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => Expression.VectorLoad(e1, e2, tpe, pur, eff, loc)
      }

    case Expression.VectorLength(exp, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.VectorLength(e, loc)
      }

    case Expression.Ref(exp1, exp2, tpe, pur, eff, loc) =>
      mapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => Expression.Ref(e1, e2, tpe, pur, eff, loc)
      }

    case Expression.Deref(exp, tpe, pur, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.Deref(e, tpe, pur, eff, loc)
      }

    case Expression.Assign(exp1, exp2, tpe, pur, eff, loc) =>
      mapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => Expression.Assign(e1, e2, tpe, pur, eff, loc)
      }

    case Expression.Ascribe(exp, tpe, pur, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.Ascribe(e, tpe, pur, eff, loc)
      }

    case Expression.InstanceOf(exp, clazz, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.InstanceOf(e, clazz, loc)
      }

    case Expression.CheckedCast(cast, exp, tpe, pur, eff, loc) =>
      mapN(visitExp(exp))(Expression.CheckedCast(cast, _, tpe, pur, eff, loc))

    case Expression.UncheckedCast(exp, declaredType, declaredPur, declaredEff, tpe, pur, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.UncheckedCast(e, declaredType, declaredPur, declaredEff, tpe, pur, eff, loc)
      }

    case Expression.UncheckedMaskingCast(exp, tpe, pur, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.UncheckedMaskingCast(e, tpe, pur, eff, loc)
      }

    case Expression.Without(exp, sym, tpe, pur, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.Without(e, sym, tpe, pur, eff, loc)
      }

    case Expression.TryCatch(exp, rules, tpe, pur, eff, loc) =>
      val rulesVal = traverse(rules) {
        case CatchRule(sym, clazz, e) => visitExp(e).map(CatchRule(sym, clazz, _))
      }
      mapN(visitExp(exp), rulesVal) {
        case (e, rs) => Expression.TryCatch(e, rs, tpe, pur, eff, loc)
      }

    case Expression.TryWith(exp, sym, rules, tpe, pur, eff, loc) =>
      val rulesVal = traverse(rules) {
        case HandlerRule(op, fparams, e) => visitExp(e).map(HandlerRule(op, fparams, _))
      }
      mapN(visitExp(exp), rulesVal) {
        case (e, rs) => Expression.TryWith(e, sym, rs, tpe, pur, eff, loc)
      }

    case Expression.Do(sym, exps, pur, eff, loc) =>
      mapN(traverse(exps)(visitExp)) {
        case es => Expression.Do(sym, es, pur, eff, loc)
      }

    case Expression.Resume(exp, tpe, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.Resume(e, tpe, loc)
      }

    case Expression.InvokeConstructor(constructor, args, tpe, pur, eff, loc) =>
      mapN(traverse(args)(visitExp)) {
        case as => Expression.InvokeConstructor(constructor, as, tpe, pur, eff, loc)
      }

    case Expression.InvokeMethod(method, exp, args, tpe, pur, eff, loc) =>
      mapN(visitExp(exp), traverse(args)(visitExp)) {
        case (e, as) => Expression.InvokeMethod(method, e, as, tpe, pur, eff, loc)
      }

    case Expression.InvokeStaticMethod(method, args, tpe, pur, eff, loc) =>
      mapN(traverse(args)(visitExp)) {
        case as => Expression.InvokeStaticMethod(method, as, tpe, pur, eff, loc)
      }

    case Expression.GetField(field, exp, tpe, pur, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.GetField(field, e, tpe, pur, eff, loc)
      }

    case Expression.PutField(field, exp1, exp2, tpe, pur, eff, loc) =>
      mapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => Expression.PutField(field, e1, e2, tpe, pur, eff, loc)
      }

    case Expression.GetStaticField(field, tpe, pur, eff, loc) =>
      Expression.GetStaticField(field, tpe, pur, eff, loc).toSuccess

    case Expression.PutStaticField(field, exp, tpe, pur, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.PutStaticField(field, e, tpe, pur, eff, loc)
      }

    case Expression.NewObject(name, clazz, tpe, pur, eff, methods, loc) =>
      mapN(traverse(methods)(visitJvmMethod)) {
        case ms => Expression.NewObject(name, clazz, tpe, pur, eff, ms, loc)
      }

    case Expression.NewChannel(exp1, exp2, tpe, pur, eff, loc) =>
      mapN(visitExp(exp1), visitExp(exp2)) {
        case (r, e) => Expression.NewChannel(r, e, tpe, pur, eff, loc)
      }

    case Expression.GetChannel(exp, tpe, pur, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.GetChannel(e, tpe, pur, eff, loc)
      }

    case Expression.PutChannel(exp1, exp2, tpe, pur, eff, loc) =>
      mapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => Expression.PutChannel(e1, e2, tpe, pur, eff, loc)
      }

    case Expression.SelectChannel(rules, default, tpe, pur, eff, loc) =>
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
        case (rs, d) => Expression.SelectChannel(rs, d, tpe, pur, eff, loc)
      }

    case Expression.Spawn(exp1, exp2, tpe, pur, eff, loc) =>
      mapN(visitExp(exp1), visitExp(exp2)) {
        case (r, e) => Expression.Spawn(r, e, tpe, pur, eff, loc)
      }

    case Expression.Par(exp, loc) =>
      mapN(visitExp(exp))(Expression.Par(_, loc))

    case Expression.ParYield(frags, exp, tpe, pur, eff, loc) =>
      val fragsVal = traverse(frags) {
        case ParYieldFragment(p, e, l) => mapN(visitExp(e)) {
          case e1 => ParYieldFragment(p, e1, l)
        }
      }
      mapN(fragsVal, visitExp(exp)) {
        case (fs, e) => Expression.ParYield(fs, e, tpe, pur, eff, loc)
      }

    case Expression.Lazy(exp, tpe, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.Lazy(e, tpe, loc)
      }

    case Expression.Force(exp, tpe, pur, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.Force(e, tpe, pur, eff, loc)
      }

    case Expression.FixpointConstraintSet(cs0, _, tpe, loc) =>
      // Compute the stratification.
      val stf = stratify(g, tpe, loc)

      mapN(stf) {
        case s =>
          val cs = cs0.map(reorder)
          Expression.FixpointConstraintSet(cs, s, tpe, loc)
      }

    case Expression.FixpointLambda(pparams, exp, _, tpe, pur, eff, loc) =>
      // Compute the stratification.
      val stf = stratify(g, tpe, loc)
      mapN(stf) {
        case s => Expression.FixpointLambda(pparams, exp, s, tpe, pur, eff, loc)
      }

    case Expression.FixpointMerge(exp1, exp2, _, tpe, pur, eff, loc) =>
      // Compute the stratification.
      val stf = stratify(g, tpe, loc)

      mapN(visitExp(exp1), visitExp(exp2), stf) {
        case (e1, e2, s) => Expression.FixpointMerge(e1, e2, s, tpe, pur, eff, loc)
      }

    case Expression.FixpointSolve(exp, _, tpe, pur, eff, loc) =>
      // Compute the stratification.
      val stf = stratify(g, tpe, loc)

      mapN(visitExp(exp), stf) {
        case (e, s) => Expression.FixpointSolve(e, s, tpe, pur, eff, loc)
      }

    case Expression.FixpointFilter(pred, exp, tpe, pur, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.FixpointFilter(pred, e, tpe, pur, eff, loc)
      }

    case Expression.FixpointInject(exp, pred, tpe, pur, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.FixpointInject(e, pred, tpe, pur, eff, loc)
      }

    case Expression.FixpointProject(pred, exp, tpe, pur, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expression.FixpointProject(pred, e, tpe, pur, eff, loc)
      }

    case Expression.Error(m, tpe, pur, eff) =>
      // Note: We must NOT use [[Validation.toSoftFailure]] because
      // that would duplicate the error inside the Validation.
      Validation.SoftFailure(Expression.Error(m, tpe, pur, eff), LazyList.empty)

  }

  private def visitJvmMethod(method: JvmMethod)(implicit root: Root, g: LabelledGraph, flix: Flix): Validation[JvmMethod, StratificationError] = method match {
    case JvmMethod(ident, fparams, exp, tpe, pur, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => JvmMethod(ident, fparams, e, tpe, pur, eff, loc)
      }
  }

  /**
    * Reorders a constraint such that its negated atoms and loop predicates occur last.
    */
  private def reorder(c0: Constraint): Constraint = {
    /**
      * Returns `true` if the body predicate is negated.
      */
    def isNegativeOrLoop(p: Predicate.Body): Boolean = p match {
      case Predicate.Body.Atom(_, _, Polarity.Negative, _, _, _, _) => true
      case Predicate.Body.Functional(_, _, _) => true
      case _ => false
    }

    // Order the predicates from first to last.
    val last = c0.body filter isNegativeOrLoop
    val first = c0.body filterNot isNegativeOrLoop

    // Reassemble the constraint.
    c0.copy(body = first ::: last)
  }

  /**
    * Returns the labelled graph of the given definition `def0`.
    */
  private def labelledGraphOfDef(def0: Def): LabelledGraph =
    labelledGraphOfExp(def0.impl.exp)

  /**
    * Returns the labelled graph of the given expression `exp0`.
    */
  private def labelledGraphOfExp(exp0: Expression): LabelledGraph = exp0 match {
    case Expression.Cst(_, _, _) => LabelledGraph.empty

    case Expression.Wild(_, _) => LabelledGraph.empty

    case Expression.Var(_, _, _) => LabelledGraph.empty

    case Expression.Def(_, _, _) => LabelledGraph.empty

    case Expression.Sig(_, _, _) => LabelledGraph.empty

    case Expression.Hole(_, _, _) => LabelledGraph.empty

    case Expression.HoleWithExp(exp, _, _, _, _) =>
      labelledGraphOfExp(exp)

    case Expression.OpenAs(_, exp, _, _) =>
      labelledGraphOfExp(exp)

    case Expression.Use(_, _, exp, _) =>
      labelledGraphOfExp(exp)

    case Expression.Lambda(_, exp, _, _) =>
      labelledGraphOfExp(exp)

    case Expression.Apply(exp, exps, _, _, _, _) =>
      val init = labelledGraphOfExp(exp)
      exps.foldLeft(init) {
        case (acc, exp) => acc + labelledGraphOfExp(exp)
      }

    case Expression.Unary(_, exp, _, _, _, _) =>
      labelledGraphOfExp(exp)

    case Expression.Binary(_, exp1, exp2, _, _, _, _) =>
      labelledGraphOfExp(exp1) + labelledGraphOfExp(exp2)

    case Expression.Let(_, _, exp1, exp2, _, _, _, _) =>
      labelledGraphOfExp(exp1) + labelledGraphOfExp(exp2)

    case Expression.LetRec(_, _, exp1, exp2, _, _, _, _) =>
      labelledGraphOfExp(exp1) + labelledGraphOfExp(exp2)

    case Expression.Region(_, _) =>
      LabelledGraph.empty

    case Expression.Scope(_, _, exp, _, _, _, _) =>
      labelledGraphOfExp(exp)

    case Expression.ScopeExit(exp1, exp2, _, _, _, _) =>
      labelledGraphOfExp(exp1) + labelledGraphOfExp(exp2)

    case Expression.IfThenElse(exp1, exp2, exp3, _, _, _, _) =>
      labelledGraphOfExp(exp1) + labelledGraphOfExp(exp2) + labelledGraphOfExp(exp3)

    case Expression.Stm(exp1, exp2, _, _, _, _) =>
      labelledGraphOfExp(exp1) + labelledGraphOfExp(exp2)

    case Expression.Discard(exp, _, _, _) =>
      labelledGraphOfExp(exp)

    case Expression.Match(exp, rules, _, _, _, _) =>
      val dg = labelledGraphOfExp(exp)
      rules.foldLeft(dg) {
        case (acc, MatchRule(_, g, b)) => acc + g.map(labelledGraphOfExp).getOrElse(LabelledGraph.empty) + labelledGraphOfExp(b)
      }

    case Expression.TypeMatch(exp, rules, _, _, _, _) =>
      val dg = labelledGraphOfExp(exp)
      rules.foldLeft(dg) {
        case (acc, MatchTypeRule(_, _, b)) => acc + labelledGraphOfExp(b)
      }

    case Expression.RelationalChoose(exps, rules, _, _, _, _) =>
      val dg1 = exps.foldLeft(LabelledGraph.empty) {
        case (acc, exp) => acc + labelledGraphOfExp(exp)
      }
      val dg2 = rules.foldLeft(LabelledGraph.empty) {
        case (acc, RelationalChoiceRule(_, exp)) => acc + labelledGraphOfExp(exp)
      }
      dg1 + dg2

    case Expression.RestrictableChoose(_, exp, rules, _, _, _, _) =>
      val dg1 = labelledGraphOfExp(exp)
      val dg2 = rules.foldLeft(LabelledGraph.empty) {
        case (acc, RestrictableChoiceRule(_, body)) => acc + labelledGraphOfExp(body)
      }
      dg1 + dg2

    case Expression.Tag(_, exp, _, _, _, _) =>
      labelledGraphOfExp(exp)

    case Expression.RestrictableTag(_, exp, _, _, _, _) =>
      labelledGraphOfExp(exp)

    case Expression.Tuple(elms, _, _, _, _) =>
      elms.foldLeft(LabelledGraph.empty) {
        case (acc, e) => acc + labelledGraphOfExp(e)
      }

    case Expression.RecordEmpty(_, _) =>
      LabelledGraph.empty

    case Expression.RecordSelect(base, _, _, _, _, _) =>
      labelledGraphOfExp(base)

    case Expression.RecordExtend(_, value, rest, _, _, _, _) =>
      labelledGraphOfExp(value) + labelledGraphOfExp(rest)

    case Expression.RecordRestrict(_, rest, _, _, _, _) =>
      labelledGraphOfExp(rest)

    case Expression.ArrayLit(elms, exp, _, _, _, _) =>
      elms.foldLeft(labelledGraphOfExp(exp)) {
        case (acc, e) => acc + labelledGraphOfExp(e)
      }

    case Expression.ArrayNew(exp1, exp2, exp3, _, _, _, _) =>
      labelledGraphOfExp(exp1) + labelledGraphOfExp(exp2) + labelledGraphOfExp(exp3)

    case Expression.ArrayLoad(base, index, _, _, _, _) =>
      labelledGraphOfExp(base) + labelledGraphOfExp(index)

    case Expression.ArrayLength(base, _, _, _) =>
      labelledGraphOfExp(base)

    case Expression.ArrayStore(base, index, elm, _, _, _) =>
      labelledGraphOfExp(base) + labelledGraphOfExp(index) + labelledGraphOfExp(elm)

    case Expression.VectorLit(exps, _, _, _, _) =>
      exps.foldLeft(LabelledGraph.empty) {
        case (acc, e) => acc + labelledGraphOfExp(e)
      }

    case Expression.VectorLoad(exp1, exp2, _, _, _, _) =>
      labelledGraphOfExp(exp1) + labelledGraphOfExp(exp2)

    case Expression.VectorLength(exp, _) =>
      labelledGraphOfExp(exp)

    case Expression.Ref(exp1, exp2, _, _, _, _) =>
      labelledGraphOfExp(exp1) + labelledGraphOfExp(exp2)

    case Expression.Deref(exp, _, _, _, _) =>
      labelledGraphOfExp(exp)

    case Expression.Assign(exp1, exp2, _, _, _, _) =>
      labelledGraphOfExp(exp1) + labelledGraphOfExp(exp2)

    case Expression.Ascribe(exp, _, _, _, _) =>
      labelledGraphOfExp(exp)

    case Expression.InstanceOf(exp, _, _) =>
      labelledGraphOfExp(exp)

    case Expression.CheckedCast(_, exp, _, _, _, _) =>
      labelledGraphOfExp(exp)

    case Expression.UncheckedCast(exp, _, _, _, _, _, _, _) =>
      labelledGraphOfExp(exp)

    case Expression.UncheckedMaskingCast(exp, _, _, _, _) =>
      labelledGraphOfExp(exp)

    case Expression.Without(exp, _, _, _, _, _) =>
      labelledGraphOfExp(exp)

    case Expression.TryCatch(exp, rules, _, _, _, _) =>
      rules.foldLeft(labelledGraphOfExp(exp)) {
        case (acc, CatchRule(_, _, e)) => acc + labelledGraphOfExp(e)
      }

    case Expression.TryWith(exp, _, rules, _, _, _, _) =>
      rules.foldLeft(labelledGraphOfExp(exp)) {
        case (acc, HandlerRule(_, _, e)) => acc + labelledGraphOfExp(e)
      }

    case Expression.Do(_, exps, _, _, _) =>
      exps.foldLeft(LabelledGraph.empty) {
        case (acc, exp) => acc + labelledGraphOfExp(exp)
      }

    case Expression.Resume(exp, _, _) =>
      labelledGraphOfExp(exp)

    case Expression.InvokeConstructor(_, args, _, _, _, _) =>
      args.foldLeft(LabelledGraph.empty) {
        case (acc, e) => acc + labelledGraphOfExp(e)
      }

    case Expression.InvokeMethod(_, exp, args, _, _, _, _) =>
      args.foldLeft(labelledGraphOfExp(exp)) {
        case (acc, e) => acc + labelledGraphOfExp(e)
      }

    case Expression.InvokeStaticMethod(_, args, _, _, _, _) =>
      args.foldLeft(LabelledGraph.empty) {
        case (acc, e) => acc + labelledGraphOfExp(e)
      }

    case Expression.GetField(_, exp, _, _, _, _) =>
      labelledGraphOfExp(exp)

    case Expression.PutField(_, exp1, exp2, _, _, _, _) =>
      labelledGraphOfExp(exp1) + labelledGraphOfExp(exp2)

    case Expression.GetStaticField(_, _, _, _, _) =>
      LabelledGraph.empty

    case Expression.PutStaticField(_, exp, _, _, _, _) =>
      labelledGraphOfExp(exp)

    case Expression.NewObject(_, _, _, _, _, _, _) =>
      LabelledGraph.empty

    case Expression.NewChannel(exp1, exp2, _, _, _, _) =>
      labelledGraphOfExp(exp1) + labelledGraphOfExp(exp2)

    case Expression.GetChannel(exp, _, _, _, _) =>
      labelledGraphOfExp(exp)

    case Expression.PutChannel(exp1, exp2, _, _, _, _) =>
      labelledGraphOfExp(exp1) + labelledGraphOfExp(exp2)

    case Expression.SelectChannel(rules, default, _, _, _, _) =>
      val dg = default match {
        case None => LabelledGraph.empty
        case Some(d) => labelledGraphOfExp(d)
      }

      rules.foldLeft(dg) {
        case (acc, SelectChannelRule(_, exp1, exp2)) => acc + labelledGraphOfExp(exp1) + labelledGraphOfExp(exp2)
      }

    case Expression.Spawn(exp1, exp2, _, _, _, _) =>
      labelledGraphOfExp(exp1) + labelledGraphOfExp(exp2)

    case Expression.Par(exp, _) =>
      labelledGraphOfExp(exp)

    case Expression.ParYield(frags, exp, _, _, _, _) =>
      frags.foldLeft(labelledGraphOfExp(exp)) {
        case (acc, ParYieldFragment(_, e, _)) => acc + labelledGraphOfExp(e)
      }

    case Expression.Lazy(exp, _, _) =>
      labelledGraphOfExp(exp)

    case Expression.Force(exp, _, _, _, _) =>
      labelledGraphOfExp(exp)

    case Expression.FixpointConstraintSet(cs, _, _, _) =>
      cs.foldLeft(LabelledGraph.empty) {
        case (dg, c) => dg + labelledGraphOfConstraint(c)
      }

    case Expression.FixpointLambda(_, exp, _, _, _, _, _) =>
      labelledGraphOfExp(exp)

    case Expression.FixpointMerge(exp1, exp2, _, _, _, _, _) =>
      labelledGraphOfExp(exp1) + labelledGraphOfExp(exp2)

    case Expression.FixpointSolve(exp, _, _, _, _, _) =>
      labelledGraphOfExp(exp)

    case Expression.FixpointFilter(_, exp, _, _, _, _) =>
      labelledGraphOfExp(exp)

    case Expression.FixpointInject(exp, _, _, _, _, _) =>
      labelledGraphOfExp(exp)

    case Expression.FixpointProject(_, exp, _, _, _, _) =>
      labelledGraphOfExp(exp)

    case Expression.Error(_, _, _, _) =>
      LabelledGraph.empty

  }

  /**
    * Computes the stratification of the given labelled graph `g` for the given row type `tpe` at the given source location `loc`.
    */
  private def stratify(g: LabelledGraph, tpe: Type, loc: SourceLocation)(implicit root: Root, flix: Flix): Validation[Stratification, StratificationError] = {
    // The key is the set of predicates that occur in the row type.
    val key = predicateSymbolsOf(tpe)

    // Compute the restricted labelled graph.
    val rg = g.restrict(key, labelEq(_, _))

    // Compute the stratification.
    UllmansAlgorithm.stratify(labelledGraphToDependencyGraph(rg), tpe, loc)
  }

  /**
    * Returns the map of predicates that appears in the given Schema `tpe`.
    * A non-Schema type will result in an `InternalCompilerException`.
    */
  private def predicateSymbolsOf(tpe: Type): Map[Name.Pred, Label] = {
    @tailrec
    def visitType(tpe: Type, acc: Map[Name.Pred, Label]): Map[Name.Pred, Label] = tpe match {
      case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SchemaRowExtend(pred), _), predType, _), rest, _) =>
        val (terms, labelDen) = termTypesAndDenotation(predType)
        val label = Label(pred, labelDen, terms.length, terms)
        visitType(rest, acc + (pred -> label))
      case _ => acc
    }

    Type.eraseAliases(tpe) match {
      case Type.Apply(Type.Cst(TypeConstructor.Schema, _), schemaRow, _) => visitType(schemaRow, Map.empty)
      case other => throw InternalCompilerException(s"Unexpected non-schema type: '$other'", other.loc)
    }
  }

  /**
    * Returns the labelled graph of the given constraint `c0`.
    */
  private def labelledGraphOfConstraint(c: Constraint): LabelledGraph = c match {
    case Constraint(_, Predicate.Head.Atom(headPred, den, _, headTpe, _), body0, _) =>
      val (headTerms, _) = termTypesAndDenotation(headTpe)

      // We add all body predicates and the head to the labels of each edge
      val bodyLabels: Vector[Label] = body0.collect {
        case Body.Atom(bodyPred, den, _, _, _, bodyTpe, _) =>
          val (terms, _) = termTypesAndDenotation(bodyTpe)
          Label(bodyPred, den, terms.length, terms)
      }.toVector

      val labels = bodyLabels :+ Label(headPred, den, headTerms.length, headTerms)

      val edges = body0.foldLeft(Vector.empty[LabelledEdge]) {
        case (edges, body) => body match {
          case Body.Atom(bodyPred, _, p, f, _, _, bodyLoc) =>
            edges :+ LabelledEdge(headPred, p, f, labels, bodyPred, bodyLoc)
          case Body.Functional(_, _, _) => edges
          case Body.Guard(_, _) => edges
        }
      }

      LabelledGraph(edges)
  }

  /**
    * Returns the term types of the given relational or latticenal type.
    */
  private def termTypesAndDenotation(tpe: Type): (List[Type], Denotation) = eraseAliases(tpe) match {
    case Type.Apply(Type.Cst(tc, _), t, _) =>
      val den = tc match {
        case TypeConstructor.Relation => Denotation.Relational
        case TypeConstructor.Lattice => Denotation.Latticenal
        case _ => throw InternalCompilerException(s"Unexpected non-denotation type constructor: '$tc'", tpe.loc)
      }
      t.baseType match {
        case Type.Cst(TypeConstructor.Tuple(_), _) => (t.typeArguments, den) // Multi-ary
        case Type.Cst(TypeConstructor.Unit, _) => (Nil, den)
        case _ => (List(t), den) // Unary
      }
    case _: Type.Var =>
      // This could occur when querying or projecting a non-existent predicate
      (Nil, Denotation.Relational)
    case _ => throw InternalCompilerException(s"Unexpected type: '$tpe.'", tpe.loc)
  }

  /**
    * Returns `true` if the two given labels `l1` and `l2` are considered equal.
    */
  private def labelEq(l1: Label, l2: Label)(implicit root: Root, flix: Flix): Boolean = {
    val isEqPredicate = l1.pred == l2.pred
    val isEqDenotation = l1.den == l2.den
    val isEqArity = l1.arity == l2.arity
    val isEqTermTypes = l1.terms.zip(l2.terms).forall {
      case (t1, t2) => Unification.unifiesWith(t1, t2, RigidityEnv.empty, ListMap.empty) // TODO ASSOC-TYPES empty right?
    }

    isEqPredicate && isEqDenotation && isEqArity && isEqTermTypes
  }

  /**
    * Computes the dependency graph from the labelled graph, throwing the labels away.
    * If a labelled edge is either negative or fixed it is transformed to a strong edge.
    */
  private def labelledGraphToDependencyGraph(g: LabelledGraph): UllmansAlgorithm.DependencyGraph =
    g.edges.map {
      case LabelledEdge(head, Polarity.Positive, Fixity.Loose, _, body, loc) =>
        // Positive, loose edges require that the strata of the head is equal to,
        // or below, the strata of the body hence a weak edge.
        UllmansAlgorithm.DependencyEdge.Weak(head, body, loc)
      case LabelledEdge(head, _, _, _, body, loc) =>
        // Edges that are either negatively bound or fixed are strong since they require
        // that the strata of the head is strictly higher than the strata of the body.
        UllmansAlgorithm.DependencyEdge.Strong(head, body, loc)
    }.toSet

}
