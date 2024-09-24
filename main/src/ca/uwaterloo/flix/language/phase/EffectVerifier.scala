/*
 * Copyright 2024 Matthew Lutze
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
import ca.uwaterloo.flix.language.ast.TypedAst.{Def, Expr, Root}
import ca.uwaterloo.flix.language.ast.*
import ca.uwaterloo.flix.language.ast.Type.eraseTopAliases
import ca.uwaterloo.flix.language.ast.shared.Scope
import ca.uwaterloo.flix.language.phase.unification.{Substitution, Unification}
import ca.uwaterloo.flix.util.*
import ca.uwaterloo.flix.util.collection.ListMap

/**
  * Performs a re-checking of the effects in the program.
  *
  * This phase is run after type inference and reconstruction to ensure that the effects are consistent.
  *
  * This phase is only for debugging; inconsistencies indicate a bug in the typer and result in a crash.
  */
object EffectVerifier {

  // We use top scope for simplicity. This is the most relaxed option.
  private implicit val S: Scope = Scope.Top

  /**
    * Verifies the effects in the given root.
    */
  def run(root: Root)(implicit flix: Flix): Unit = {
    if (flix.options.xverifyeffects) {
      // TODO visit sigs and instances
      ParOps.parMapValues(root.defs) {
        case defn => visitDef(defn)(root.eqEnv, flix)
      }
    }
  }

  /**
    * Verifies the effects in the given definition.
    */
  def visitDef(defn: Def)(implicit eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], flix: Flix): Unit = {
    visitExp(defn.exp)
    expectType(defn.spec.eff, defn.exp.eff, defn.exp.loc)
  }

  /**
    * Verifies the effects in the given expression
    */
  def visitExp(e: Expr)(implicit eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], flix: Flix): Unit = e match {
    case Expr.Cst(cst, tpe, loc) => ()
    case Expr.Var(sym, tpe, loc) => ()
    case Expr.Def(sym, tpe, loc) => ()
    case Expr.Sig(sym, tpe, loc) => ()
    case Expr.Hole(sym, tpe, eff, loc) => ()
    case Expr.HoleWithExp(exp, tpe, eff, loc) =>
      visitExp(exp)
    // TODO ?
    case Expr.OpenAs(symUse, exp, tpe, loc) =>
      visitExp(exp)
    case Expr.Use(sym, alias, exp, loc) =>
      visitExp(exp)
    case Expr.Lambda(fparam, exp, tpe, loc) =>
      visitExp(exp)
    case Expr.Apply(exp, exps, tpe, eff, loc) =>
      visitExp(exp)
      exps.foreach(visitExp)
      val expected = Type.mkUnion(Type.eraseTopAliases(exp.tpe).arrowEffectType :: exp.eff :: exps.map(_.eff), loc)
      val actual = eff
      expectType(expected, actual, loc)
    case Expr.ApplyDef(_, exps, itpe, _, eff, loc) =>
      exps.foreach(visitExp)
      val expected = Type.mkUnion(Type.eraseTopAliases(itpe).arrowEffectType :: exps.map(_.eff), loc)
      val actual = eff
      expectType(expected, actual, loc)
    case Expr.Unary(sop, exp, tpe, eff, loc) =>
      visitExp(exp)
      val expected = exp.eff
      val actual = eff
      expectType(expected, actual, loc)
    case Expr.Binary(sop, exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1)
      visitExp(exp2)
      val expected = Type.mkUnion(exp1.eff, exp2.eff, loc)
      val actual = eff
      expectType(expected, actual, loc)
    case Expr.Let(sym, mod, exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1)
      visitExp(exp2)
      val expected = Type.mkUnion(exp1.eff, exp2.eff, loc)
      val actual = eff
      expectType(expected, actual, loc)
    case Expr.LetRec(sym, ann, mod, exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1)
      visitExp(exp2)
      val expected = Type.mkUnion(exp1.eff, exp2.eff, loc)
      val actual = eff
      expectType(expected, actual, loc)
    case Expr.Region(tpe, loc) => ()
    case Expr.Scope(sym, regionVar, exp, tpe, eff, loc) =>
      visitExp(exp)
      val expected = Substitution.singleton(regionVar.sym, Type.Pure)(exp.eff)
      val actual = eff
      expectType(expected, actual, loc)
    case Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      visitExp(exp1)
      visitExp(exp2)
      visitExp(exp3)
      val expected = Type.mkUnion(exp1.eff, exp2.eff, exp3.eff, loc)
      val actual = eff
      expectType(expected, actual, loc)
    case Expr.Stm(exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1)
      visitExp(exp2)
      val expected = Type.mkUnion(exp1.eff, exp2.eff, loc)
      val actual = eff
      expectType(expected, actual, loc)
    case Expr.Discard(exp, eff, loc) =>
      visitExp(exp)
      val expected = exp.eff
      val actual = eff
      expectType(expected, actual, loc)
    case Expr.Match(exp, rules, tpe, eff, loc) =>
      visitExp(exp)
      rules.foreach { r => r.guard.foreach(visitExp); visitExp(r.exp) }
      val expected = Type.mkUnion(exp.eff :: rules.map(_.exp.eff), loc)
      val actual = eff
      expectType(expected, actual, loc)
    case Expr.TypeMatch(exp, rules, tpe, eff, loc) =>
      visitExp(exp)
      rules.foreach { r => visitExp(r.exp) }
      val expected = Type.mkUnion(exp.eff :: rules.map(_.exp.eff), loc)
      val actual = eff
      expectType(expected, actual, loc)
    case Expr.RestrictableChoose(star, exp, rules, tpe, eff, loc) =>
      visitExp(exp)
      rules.foreach { r => visitExp(r.exp) }
      val expected = Type.mkUnion(exp.eff :: rules.map(_.exp.eff), loc)
      val actual = eff
      expectType(expected, actual, loc)
    case Expr.Tag(sym, exp, tpe, eff, loc) =>
      visitExp(exp)
      val expected = exp.eff
      val actual = eff
      expectType(expected, actual, loc)
    case Expr.RestrictableTag(sym, exp, tpe, eff, loc) =>
      visitExp(exp)
      val expected = exp.eff
      val actual = eff
      expectType(expected, actual, loc)
    case Expr.Tuple(elms, tpe, eff, loc) =>
      elms.foreach(visitExp)
      val expected = Type.mkUnion(elms.map(_.eff), loc)
      val actual = eff
      expectType(expected, actual, loc)
    case Expr.RecordEmpty(tpe, loc) => ()
    case Expr.RecordSelect(exp, label, tpe, eff, loc) =>
      visitExp(exp)
      val expected = exp.eff
      val actual = eff
      expectType(expected, actual, loc)
    case Expr.RecordExtend(label, exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1)
      visitExp(exp2)
      val expected = Type.mkUnion(exp1.eff, exp2.eff, loc)
      val actual = eff
      expectType(expected, actual, loc)
    case Expr.RecordRestrict(label, exp, tpe, eff, loc) =>
      visitExp(exp)
      val expected = exp.eff
      val actual = eff
      expectType(expected, actual, loc)
    case Expr.ArrayLit(exps, exp, tpe, eff, loc) =>
      exps.foreach(visitExp)
      visitExp(exp)
      // TODO region stuff
      ()
    case Expr.ArrayNew(exp1, exp2, exp3, tpe, eff, loc) =>
      visitExp(exp1)
      visitExp(exp2)
      visitExp(exp3)
      // TODO region stuff
      ()
    case Expr.ArrayLoad(exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1)
      visitExp(exp2)
      // TODO region stuff
      ()
    case Expr.ArrayLength(exp, eff, loc) =>
      visitExp(exp)
      val expected = exp.eff
      val actual = eff
      expectType(expected, actual, loc)
    case Expr.ArrayStore(exp1, exp2, exp3, eff, loc) =>
      visitExp(exp1)
      visitExp(exp2)
      visitExp(exp3)
      // TODO region stuff
      ()
    case Expr.StructNew(sym, fields, region, tpe, eff, loc) =>
      val expected = Type.mkUnion(fields.map {case (k, v) => v.eff} :+ region.eff, loc)
      val actual = eff
      expectType(expected, actual, loc)
      fields.map {case(k, v) => v}.map(visitExp)
      visitExp(region)
    case Expr.StructGet(e, _, t, _, _) =>
      // JOE TODO region stuff
      visitExp(e)
    case Expr.StructPut(e1, _, e2, t, _, _) =>
      // JOE TODO region stuff
      visitExp(e1)
      visitExp(e2)
    case Expr.VectorLit(exps, tpe, eff, loc) =>
      exps.foreach(visitExp)
      val expected = Type.mkUnion(exps.map(_.eff), loc)
      val actual = eff
      expectType(expected, actual, loc)
    case Expr.VectorLoad(exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1)
      visitExp(exp2)
      val expected = Type.mkUnion(exp1.eff, exp2.eff, loc)
      val actual = eff
      expectType(expected, actual, loc)
    case Expr.VectorLength(exp, loc) =>
      visitExp(exp)
    case Expr.Ascribe(exp, tpe, eff, loc) =>
      visitExp(exp)
    case Expr.InstanceOf(exp, clazz, loc) =>
      visitExp(exp)
    case Expr.CheckedCast(cast, exp, tpe, eff, loc) =>
      visitExp(exp)
    case Expr.UncheckedCast(exp, declaredType, declaredEff, tpe, eff, loc) =>
      visitExp(exp)
    case Expr.UncheckedMaskingCast(exp, tpe, eff, loc) =>
      visitExp(exp)
    case Expr.Without(exp, effUse, tpe, eff, loc) =>
      visitExp(exp)
      val expected = exp.eff
      val actual = eff
      expectType(expected, actual, loc)
    case Expr.TryCatch(exp, rules, tpe, eff, loc) =>
      visitExp(exp)
      rules.foreach { r => visitExp(r.exp) }
      val expected = Type.mkUnion(exp.eff :: rules.map(_.exp.eff), loc)
      val actual = eff
      expectType(expected, actual, loc)
    case Expr.Throw(exp, eff, _, loc) =>
      visitExp(exp)
      expectType(eff, Type.mkUnion(exp.eff, Type.IO, loc), loc)
    case Expr.TryWith(exp, effUse, rules, tpe, eff, loc) =>
      visitExp(exp)
      rules.foreach { r => visitExp(r.exp) }
      // TODO effect stuff
      ()
    case Expr.Do(op, exps, tpe, eff, loc) =>
      exps.foreach(visitExp)
      // TODO effect stuff
      ()
    case Expr.InvokeConstructor(constructor, exps, tpe, eff, loc) =>
      exps.foreach(visitExp)
      // TODO Java stuff
      ()
    case Expr.InvokeMethod(method, exp, exps, tpe, eff, loc) =>
      visitExp(exp)
      exps.foreach(visitExp)
      // TODO Java stuff
      ()
    case Expr.InvokeStaticMethod(method, exps, tpe, eff, loc) =>
      exps.foreach(visitExp)
      // TODO Java stuff
      ()
    case Expr.GetField(field, exp, tpe, eff, loc) =>
      visitExp(exp)
      // TODO Java stuff
      ()
    case Expr.PutField(field, exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1)
      visitExp(exp2)
      // TODO Java stuff
      ()
    case Expr.GetStaticField(field, tpe, eff, loc) =>
      // TODO Java stuff
      ()
    case Expr.PutStaticField(field, exp, tpe, eff, loc) =>
      visitExp(exp)
      // TODO Java stuff
      ()
    case Expr.NewObject(name, clazz, tpe, eff, methods, loc) =>
      methods.foreach { m => visitExp(m.exp) }
      // TODO Java stuff
      ()
    case Expr.NewChannel(exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1)
      visitExp(exp2)
      // TODO region stuff
      ()
    case Expr.GetChannel(exp, tpe, eff, loc) =>
      visitExp(exp)
      // TODO region stuff
      ()
    case Expr.PutChannel(exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1)
      visitExp(exp2)
      // TODO region stuff
      ()
    case Expr.SelectChannel(rules, default, tpe, eff, loc) =>
      rules.foreach { r => visitExp(r.exp) }
      default.foreach { d => visitExp(d) }
      // TODO region stuff
      ()
    case Expr.Spawn(exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1)
      visitExp(exp2)
      // TODO ?
      ()
    case Expr.ParYield(frags, exp, tpe, eff, loc) =>
      frags.foreach { f => visitExp(f.exp) }
      visitExp(exp)
      // TODO ?
      ()
    case Expr.Lazy(exp, tpe, loc) =>
      visitExp(exp)
    case Expr.Force(exp, tpe, eff, loc) =>
      visitExp(exp)
    // TODO ?
    case Expr.FixpointConstraintSet(cs, tpe, loc) =>
    // TODO inner exps
    case Expr.FixpointLambda(pparams, exp, tpe, eff, loc) =>
      visitExp(exp)
      // TODO ?
      ()
    case Expr.FixpointMerge(exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1)
      visitExp(exp2)
      // TODO ?
      ()
    case Expr.FixpointSolve(exp, tpe, eff, loc) =>
      visitExp(exp)
      // TODO ?
      ()
    case Expr.FixpointFilter(pred, exp, tpe, eff, loc) =>
      visitExp(exp)
      // TODO ?
      ()
    case Expr.FixpointInject(exp, pred, tpe, eff, loc) =>
      visitExp(exp)
      // TODO ?
      ()
    case Expr.FixpointProject(pred, exp, tpe, eff, loc) =>
      visitExp(exp)
      // TODO ?
      ()
    case Expr.Error(m, tpe, eff) => ()
  }

  /**
    * Throws an exception if the actual type does not match the expected type.
    */
  private def expectType(expected: Type, actual: Type, loc: SourceLocation)(implicit eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], flix: Flix): Unit = {
    // mark everything as rigid
    val renv = RigidityEnv.ofRigidVars(expected.typeVars.map(_.sym) ++ actual.typeVars.map(_.sym))
    if (!Unification.unifiesWith(expected, actual, renv, eqEnv)) {
      throw InternalCompilerException(s"Expected type $expected but found $actual at $loc", loc)
    }
  }
}
