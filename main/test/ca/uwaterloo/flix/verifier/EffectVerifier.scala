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
package ca.uwaterloo.flix.verifier

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.*
import ca.uwaterloo.flix.language.ast.TypedAst.*
import ca.uwaterloo.flix.language.ast.shared.Scope
import ca.uwaterloo.flix.language.phase.typer.ConstraintSolver2
import ca.uwaterloo.flix.language.phase.unification.EqualityEnv
import ca.uwaterloo.flix.util.*

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
  def verify(root: Root)(implicit flix: Flix): Unit = {
    ParOps.parMapValues(root.defs)(visitDef(_)(root.eqEnv, flix))
    ParOps.parMapValues(root.sigs)(visitSig(_)(root.eqEnv, flix))
    ParOps.parMap(root.instances.values)(visitInstance(_)(root.eqEnv, flix))
  }

  /**
    * Verifies the effects in the given definition.
    */
  def visitDef(defn: Def)(implicit eqEnv: EqualityEnv, flix: Flix): Unit = {
    visitExp(defn.exp)
    expectType(expected = defn.spec.eff, defn.exp.eff, defn.exp.loc)
  }

  /**
    * Verifies the effects in the given signature.
    */
  def visitSig(sig: Sig)(implicit eqEnv: EqualityEnv, flix: Flix): Unit =
    sig.exp match {
      case Some(exp) =>
        visitExp(exp)
        expectType(expected = sig.spec.eff, exp.eff, exp.loc)
      case None => ()
    }

  /**
    * Verifies the effects in the given instance.
    */
  def visitInstance(ins: Instance)(implicit eqEnv: EqualityEnv, flix: Flix): Unit =
    ins.defs.foreach(visitDef)

  /**
    * Verifies the effects in the given expression
    */
  def visitExp(e: Exp)(implicit eqEnv: EqualityEnv, flix: Flix): Unit = e match {
    case Exp.Cst(cst, tpe, loc) => ()
    case Exp.Var(sym, tpe, loc) => ()
    case Exp.Hole(sym, env, tpe, eff, loc) => ()
    case Exp.HoleWithExp(exp, env, tpe, eff, loc) =>
      visitExp(exp)
    // TODO ?
    case Exp.OpenAs(symUse, exp, tpe, loc) =>
      visitExp(exp)
    case Exp.Use(sym, alias, exp, loc) =>
      visitExp(exp)
    case Exp.Lambda(fparam, exp, tpe, loc) =>
      visitExp(exp)
    case Exp.ApplyClo(exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1)
      visitExp(exp2)
      val expected = Type.mkUnion(Type.eraseTopAliases(exp1.tpe).arrowEffectType :: exp1.eff :: exp2.eff :: Nil, loc)
      val actual = eff
      expectType(expected, actual, loc)
    case Exp.ApplyDef(_, exps, _, itpe, _, eff, loc) =>
      exps.foreach(visitExp)
      val expected = Type.mkUnion(Type.eraseTopAliases(itpe).arrowEffectType :: exps.map(_.eff), loc)
      val actual = eff
      expectType(expected, actual, loc)
    case Exp.ApplyLocalDef(_, exps, arrowTpe, _, eff, loc) =>
      exps.foreach(visitExp)
      val expected = Type.mkUnion(Type.eraseTopAliases(arrowTpe).arrowEffectType :: exps.map(_.eff), loc)
      val actual = eff
      expectType(expected, actual, loc)
    case Exp.ApplyOp(op, exps, tpe, eff, loc) =>
      exps.foreach(visitExp)
      // TODO effect stuff
      ()
    case Exp.ApplySig(_, exps, _, _, itpe, _, eff, loc) =>
      exps.foreach(visitExp)
      val expected = Type.mkUnion(Type.eraseTopAliases(itpe).arrowEffectType :: exps.map(_.eff), loc)
      val actual = eff
      expectType(expected, actual, loc)
    case Exp.Unary(sop, exp, tpe, eff, loc) =>
      visitExp(exp)
      val expected = exp.eff
      val actual = eff
      expectType(expected, actual, loc)
    case Exp.Binary(sop, exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1)
      visitExp(exp2)
      val expected = Type.mkUnion(exp1.eff, exp2.eff, loc)
      val actual = eff
      expectType(expected, actual, loc)
    case Exp.Let(sym, exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1)
      visitExp(exp2)
      val expected = Type.mkUnion(exp1.eff, exp2.eff, loc)
      val actual = eff
      expectType(expected, actual, loc)
    case Exp.LocalDef(_, _, exp1, exp2, _, eff, loc) =>
      visitExp(exp1)
      visitExp(exp2)
      val expected = exp2.eff
      val actual = eff
      expectType(expected, actual, loc)
    case Exp.Region(sym, regSym, exp, tpe, eff, loc) =>
      visitExp(exp)
      val expected = Type.purifyRegion(exp.eff, regSym)
      val actual = eff
      expectType(expected, actual, loc)
    case Exp.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      visitExp(exp1)
      visitExp(exp2)
      visitExp(exp3)
      val expected = Type.mkUnion(exp1.eff, exp2.eff, exp3.eff, loc)
      val actual = eff
      expectType(expected, actual, loc)
    case Exp.Stm(exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1)
      visitExp(exp2)
      val expected = Type.mkUnion(exp1.eff, exp2.eff, loc)
      val actual = eff
      expectType(expected, actual, loc)
    case Exp.Discard(exp, eff, loc) =>
      visitExp(exp)
      val expected = exp.eff
      val actual = eff
      expectType(expected, actual, loc)
    case Exp.Match(exp, rules, tpe, eff, loc) =>
      visitExp(exp)
      rules.foreach { r => r.guard.foreach(visitExp); visitExp(r.exp) }
      val expected = Type.mkUnion(exp.eff :: rules.map(_.exp.eff), loc)
      val actual = eff
      expectType(expected, actual, loc)
    case Exp.TypeMatch(exp, rules, tpe, eff, loc) =>
      visitExp(exp)
      rules.foreach { r => visitExp(r.exp) }
      val expected = Type.mkUnion(exp.eff :: rules.map(_.exp.eff), loc)
      val actual = eff
      expectType(expected, actual, loc)
    case Exp.RestrictableChoose(star, exp, rules, tpe, eff, loc) =>
      visitExp(exp)
      rules.foreach { r => visitExp(r.exp) }
      val expected = Type.mkUnion(exp.eff :: rules.map(_.exp.eff), loc)
      val actual = eff
      expectType(expected, actual, loc)
    case Exp.ExtMatch(exp, rules, _, eff, loc) =>
      visitExp(exp)
      rules.foreach(r => visitExp(r.exp))
      val expected = Type.mkUnion(exp.eff :: rules.map(r => r.exp.eff), loc)
      val actual = eff
      expectType(expected, actual, loc)
    case Exp.Tag(symUse, exps, tpe, eff, loc) =>
      exps.foreach(visitExp)
      val expected = Type.mkUnion(exps.map(_.eff), loc)
      val actual = eff
      expectType(expected, actual, loc)
    case Exp.RestrictableTag(symUse, exps, tpe, eff, loc) =>
      exps.foreach(visitExp)
      val expected = Type.mkUnion(exps.map(_.eff), loc)
      val actual = eff
      expectType(expected, actual, loc)
    case Exp.ExtTag(_, exps, _, eff, loc) =>
      exps.foreach(visitExp)
      val expected = Type.mkUnion(exps.map(_.eff), loc)
      val actual = eff
      expectType(expected, actual, loc)
    case Exp.Tuple(elms, tpe, eff, loc) =>
      elms.foreach(visitExp)
      val expected = Type.mkUnion(elms.map(_.eff), loc)
      val actual = eff
      expectType(expected, actual, loc)
    case Exp.RecordSelect(exp, label, tpe, eff, loc) =>
      visitExp(exp)
      val expected = exp.eff
      val actual = eff
      expectType(expected, actual, loc)
    case Exp.RecordExtend(label, exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1)
      visitExp(exp2)
      val expected = Type.mkUnion(exp1.eff, exp2.eff, loc)
      val actual = eff
      expectType(expected, actual, loc)
    case Exp.RecordRestrict(label, exp, tpe, eff, loc) =>
      visitExp(exp)
      val expected = exp.eff
      val actual = eff
      expectType(expected, actual, loc)
    case Exp.ArrayLit(exps, exp, tpe, eff, loc) =>
      exps.foreach(visitExp)
      visitExp(exp)
      // TODO region stuff
      ()
    case Exp.ArrayNew(exp1, exp2, exp3, tpe, eff, loc) =>
      visitExp(exp1)
      visitExp(exp2)
      visitExp(exp3)
      // TODO region stuff
      ()
    case Exp.ArrayLoad(exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1)
      visitExp(exp2)
      // TODO region stuff
      ()
    case Exp.ArrayLength(exp, eff, loc) =>
      visitExp(exp)
      val expected = exp.eff
      val actual = eff
      expectType(expected, actual, loc)
    case Exp.ArrayStore(exp1, exp2, exp3, eff, loc) =>
      visitExp(exp1)
      visitExp(exp2)
      visitExp(exp3)
      // TODO region stuff
      ()
    case Exp.StructNew(sym, fields, region, tpe, eff, loc) =>
      fields.map { case (k, v) => v }.foreach(visitExp)
      visitExp(region)
      // TODO region stuff
      ()
    case Exp.StructGet(e, _, t, _, _) =>
      // JOE TODO region stuff
      visitExp(e)
    case Exp.StructPut(e1, _, e2, t, _, _) =>
      // JOE TODO region stuff
      visitExp(e1)
      visitExp(e2)
    case Exp.VectorLit(exps, tpe, eff, loc) =>
      exps.foreach(visitExp)
      val expected = Type.mkUnion(exps.map(_.eff), loc)
      val actual = eff
      expectType(expected, actual, loc)
    case Exp.VectorLoad(exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1)
      visitExp(exp2)
      val expected = Type.mkUnion(exp1.eff, exp2.eff, loc)
      val actual = eff
      expectType(expected, actual, loc)
    case Exp.VectorLength(exp, loc) =>
      visitExp(exp)
    case Exp.Ascribe(exp, expectedType, expectedEff, tpe, eff, loc) =>
      visitExp(exp)
    case Exp.InstanceOf(exp, clazz, loc) =>
      visitExp(exp)
    case Exp.CheckedCast(cast, exp, tpe, eff, loc) =>
      visitExp(exp)
    case Exp.UncheckedCast(exp, declaredType, declaredEff, tpe, eff, loc) =>
      visitExp(exp)
    case Exp.Unsafe(exp, runEff, tpe, eff, loc) =>
      visitExp(exp)
      val expected = Type.mkDifference(exp.eff, runEff, loc)
      val actual = eff
      expectType(expected, actual, loc)
    case Exp.Without(exp, symUse, tpe, eff, loc) =>
      visitExp(exp)
      val expected = exp.eff
      val actual = eff
      expectType(expected, actual, loc)
    case Exp.TryCatch(exp, rules, tpe, eff, loc) =>
      visitExp(exp)
      rules.foreach { r => visitExp(r.exp) }
      val expected = Type.mkUnion(exp.eff :: rules.map(_.exp.eff), loc)
      val actual = eff
      expectType(expected, actual, loc)
    case Exp.Throw(exp, _, eff, loc) =>
      visitExp(exp)
      expectType(eff, Type.mkUnion(exp.eff, Type.IO, loc), loc)
    case Exp.Handler(symUse, rules, bodyTpe, bodyEff, handledEff, tpe, loc) =>
      rules.foreach { r => visitExp(r.exp) }
      // TODO effect stuff
      ()
    case Exp.RunWith(exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1)
      visitExp(exp2)
      // TODO effect stuff
      ()
    case Exp.InvokeConstructor(constructor, exps, tpe, eff, loc) =>
      exps.foreach(visitExp)
      // TODO Java stuff
      ()
    case Exp.InvokeMethod(method, exp, exps, tpe, eff, loc) =>
      visitExp(exp)
      exps.foreach(visitExp)
      // TODO Java stuff
      ()
    case Exp.InvokeStaticMethod(method, exps, tpe, eff, loc) =>
      exps.foreach(visitExp)
      // TODO Java stuff
      ()
    case Exp.GetField(field, exp, tpe, eff, loc) =>
      visitExp(exp)
      // TODO Java stuff
      ()
    case Exp.PutField(field, exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1)
      visitExp(exp2)
      // TODO Java stuff
      ()
    case Exp.GetStaticField(field, tpe, eff, loc) =>
      // TODO Java stuff
      ()
    case Exp.PutStaticField(field, exp, tpe, eff, loc) =>
      visitExp(exp)
      // TODO Java stuff
      ()
    case Exp.NewObject(name, clazz, tpe, eff, methods, loc) =>
      methods.foreach { m => visitExp(m.exp) }
      // TODO Java stuff
      ()
    case Exp.NewChannel(exp, tpe, eff, loc) =>
      visitExp(exp)
      // TODO region stuff
      ()
    case Exp.GetChannel(exp, tpe, eff, loc) =>
      visitExp(exp)
      // TODO region stuff
      ()
    case Exp.PutChannel(exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1)
      visitExp(exp2)
      // TODO region stuff
      ()
    case Exp.SelectChannel(rules, default, tpe, eff, loc) =>
      rules.foreach { r => visitExp(r.exp) }
      default.foreach { d => visitExp(d) }
      // TODO region stuff
      ()
    case Exp.Spawn(exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1)
      visitExp(exp2)
      // TODO ?
      ()
    case Exp.ParYield(frags, exp, tpe, eff, loc) =>
      frags.foreach { f => visitExp(f.exp) }
      visitExp(exp)
      // TODO ?
      ()
    case Exp.Lazy(exp, tpe, loc) =>
      visitExp(exp)
    case Exp.Force(exp, tpe, eff, loc) =>
      visitExp(exp)
    // TODO ?
    case Exp.FixpointConstraintSet(cs, tpe, loc) =>
    // TODO inner exps
    case Exp.FixpointLambda(pparams, exp, tpe, eff, loc) =>
      visitExp(exp)
      // TODO ?
      ()
    case Exp.FixpointMerge(exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1)
      visitExp(exp2)
      // TODO ?
      ()
    case Exp.FixpointQueryWithProvenance(exps, select, withh, tpe1, eff1, loc1) =>
      exps.foreach(visitExp)
      select match {
        case TypedAst.Predicate.Head.Atom(pred, den, terms, tpe2, loc2) =>
          terms.foreach(visitExp)
      }
      // TODO ?
      ()
    case Exp.FixpointQueryWithSelect(exps, queryExp, selects, from, where, pred, tpe, eff, loc) =>
      exps.foreach(visitExp)
      where.foreach(visitExp)
      // TODO ?
      ()
    case Exp.FixpointSolveWithProject(exps, optPreds, mode, tpe, eff, loc) =>
      exps.foreach(visitExp)
      // TODO ?
      ()
    case Exp.FixpointInjectInto(exps, predsAndArities, tpe, eff, loc) =>
      exps.foreach(visitExp)
      // TODO ?
      ()
    case Exp.Error(m, tpe, eff) => ()
  }

  /**
    * Throws an exception if the actual type does not match the expected type.
    */
  private def expectType(expected: Type, actual: Type, loc: SourceLocation)(implicit eqEnv: EqualityEnv, flix: Flix): Unit = {
    if (!ConstraintSolver2.isEquivalent(expected, actual)) {
      throw InternalCompilerException(s"Expected type $expected but found $actual", loc)
    }
  }
}
