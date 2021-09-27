/*
 * Copyright 2021 Magnus Madsen
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
package ca.uwaterloo.flix.language.phase.extra

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.ast.TypedAst.Expression
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

object CodeQuality {

  sealed trait CodeQualityError

  /**
    * Type checks the given AST root.
    */
  def run(root: TypedAst.Root)(implicit flix: Flix): Validation[TypedAst.Root, CodeQualityError] = flix.phase("CodeQuality") {
    root.toSuccess
  }

  /**
    * Finds the holes and hole contexts in the given expression `exp0`.
    */
  def visitExp(exp0: Expression): List[CodeQualityError] = exp0 match {
    case Expression.Wild(tpe, loc) => Nil

    case Expression.Var(sym, tpe, loc) => Nil

    case Expression.Def(sym, tpe, loc) => Nil

    case Expression.Sig(sym, tpe, loc) => Nil

    case Expression.Hole(sym, tpe, eff, loc) => Map(sym -> HoleContext(sym, tpe))

    case Expression.Unit(loc) => Nil

    case Expression.Null(tpe, loc) => Nil

    case Expression.True(loc) => Nil

    case Expression.False(loc) => Nil

    case Expression.Char(lit, loc) => Nil

    case Expression.Float32(lit, loc) => Nil

    case Expression.Float64(lit, loc) => Nil

    case Expression.Int8(lit, loc) => Nil

    case Expression.Int16(lit, loc) => Nil

    case Expression.Int32(lit, loc) => Nil

    case Expression.Int64(lit, loc) => Nil

    case Expression.BigInt(lit, loc) => Nil

    case Expression.Str(lit, loc) => Nil

    case Expression.Default(tpe, loc) => Nil

    case Expression.Lambda(fparam, exp, tpe, loc) =>
      val env1 = Map(fparam.sym -> fparam.tpe)
      visitExp(exp ++ env1)

    case Expression.Apply(exp, exps, tpe, eff, loc) =>
      val init = visitExp(exp)
      exps.foldLeft(init) {
        case (acc, exp) => acc ++ visitExp(exp)
      }

    case Expression.Unary(sop, exp, tpe, eff, loc) =>
      visitExp(exp)

    case Expression.Binary(sop, exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.Let(sym, _, exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1) ++ visitExp(exp2 + (sym -> exp1.tpe))

    case Expression.LetRegion(_, exp, _, _, _) =>
      visitExp(exp)

    case Expression.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

    case Expression.Stm(exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.Match(matchExp, rules, tpe, eff, loc) =>
      val m = visitExp(matchExp)
      rules.foldLeft(m) {
        case (macc, MatchRule(pat, guard, exp)) =>
          macc ++ visitExp(guard) ++ visitExp(exp, binds(pat) ++ env0)
      }

    case Expression.Choose(exps, rules, tpe, eff, loc) =>
      val m1 = exps.foldLeft(Nil[Symbol.HoleSym, HoleContext]) {
        case (acc, exp) => acc ++ visitExp(exp)
      }
      val m2 = rules.foldLeft(Nil[Symbol.HoleSym, HoleContext]) {
        case (acc, ChoiceRule(pat, exp)) =>
          val env1 = pat.zip(exps).foldLeft(Nil[Symbol.VarSym, Type]) {
            case (acc, (ChoicePattern.Wild(_), exp)) => acc
            case (acc, (ChoicePattern.Absent(_), exp)) => acc
            case (acc, (ChoicePattern.Present(sym, _, _), exp)) => acc + (sym -> exp.tpe)
          }
          acc ++ visitExp(exp ++ env1)
      }
      m1 ++ m2

    case Expression.Tag(sym, tag, exp, tpe, eff, loc) =>
      visitExp(exp)

    case Expression.Tuple(elms, tpe, eff, loc) =>
      elms.foldLeft(Nil[Symbol.HoleSym, HoleContext]) {
        case (macc, elm) => macc ++ visitExp(elm)
      }

    case Expression.RecordEmpty(tpe, loc) => Nil

    case Expression.RecordSelect(base, _, tpe, eff, loc) =>
      visitExp(base)

    case Expression.RecordExtend(_, value, rest, tpe, eff, loc) =>
      visitExp(rest) ++ visitExp(value)

    case Expression.RecordRestrict(_, rest, tpe, eff, loc) =>
      visitExp(rest)

    case Expression.ArrayLit(elms, tpe, eff, loc) =>
      elms.foldLeft(Nil[Symbol.HoleSym, HoleContext]) {
        case (macc, elm) => macc ++ visitExp(elm)
      }

    case Expression.ArrayNew(elm, len, tpe, eff, loc) =>
      visitExp(elm)

    case Expression.ArrayLoad(base, index, tpe, eff, loc) =>
      visitExp(base) ++ visitExp(index)

    case Expression.ArrayStore(base, index, elm, loc) =>
      visitExp(base) ++ visitExp(index) ++ visitExp(elm)

    case Expression.ArrayLength(base, eff, loc) =>
      visitExp(base)

    case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) =>
      visitExp(base) ++ visitExp(beginIndex) ++ visitExp(endIndex)

    case Expression.Ref(exp, tpe, eff, loc) =>
      visitExp(exp)

    case Expression.Deref(exp, tpe, eff, loc) =>
      visitExp(exp)

    case Expression.Assign(exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.Existential(fparam, exp, loc) =>
      visitExp(exp + (fparam.sym -> fparam.tpe))

    case Expression.Universal(fparam, exp, loc) =>
      visitExp(exp + (fparam.sym -> fparam.tpe))

    case Expression.Ascribe(exp, tpe, eff, loc) =>
      visitExp(exp)

    case Expression.Cast(exp, tpe, eff, loc) =>
      visitExp(exp)

    case Expression.TryCatch(exp, rules, tpe, eff, loc) =>
      rules.foldLeft(visitExp(exp)) {
        case (macc, CatchRule(sym, clazz, body)) => macc ++ visitExp(body + (sym -> Type.mkNative(null, loc)))
      }

    case Expression.InvokeConstructor(constructor, args, tpe, eff, loc) =>
      args.foldLeft(Nil[Symbol.HoleSym, HoleContext]) {
        case (macc, arg) => macc ++ visitExp(arg)
      }

    case Expression.InvokeMethod(method, exp, args, tpe, eff, loc) =>
      args.foldLeft(visitExp(exp)) {
        case (macc, arg) => macc ++ visitExp(arg)
      }

    case Expression.InvokeStaticMethod(method, args, tpe, eff, loc) =>
      args.foldLeft(Nil[Symbol.HoleSym, HoleContext]) {
        case (macc, arg) => macc ++ visitExp(arg)
      }

    case Expression.GetField(field, exp, tpe, eff, loc) =>
      visitExp(exp)

    case Expression.PutField(field, exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.GetStaticField(field, tpe, eff, loc) =>
      Nil

    case Expression.PutStaticField(field, exp, tpe, eff, loc) =>
      visitExp(exp)

    case Expression.NewChannel(exp, tpe, eff, loc) => visitExp(exp)

    case Expression.GetChannel(exp, tpe, eff, loc) => visitExp(exp)

    case Expression.PutChannel(exp1, exp2, tpe, eff, loc) => visitExp(exp1) ++ visitExp(exp2)

    case Expression.SelectChannel(rules, default, tpe, eff, loc) =>
      val rs = rules.foldLeft(Nil[Symbol.HoleSym, HoleContext]) {
        case (macc, SelectChannelRule(sym, chan, exp)) => macc ++ visitExp(chan) ++ visitExp(exp)
      }

      val d = default.map(visitExp(_)).getOrElse(Nil)

      rs ++ d

    case Expression.Spawn(exp, tpe, eff, loc) => visitExp(exp)

    case Expression.Lazy(exp, tpe, loc) => visitExp(exp)

    case Expression.Force(exp, tpe, eff, loc) => visitExp(exp)

    case Expression.FixpointConstraintSet(cs, stf, tpe, loc) =>
      cs.foldLeft(Nil[Symbol.HoleSym, HoleContext]) {
        case (macc, c) => macc ++ visitConstraint(c)
      }

    case Expression.FixpointMerge(exp1, exp2, stf, tpe, eff, loc) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.FixpointSolve(exp, stf, tpe, eff, loc) =>
      visitExp(exp)

    case Expression.FixpointFilter(_, exp, tpe, eff, loc) =>
      visitExp(exp)

    case Expression.FixpointProjectIn(exp, _, tpe, eff, loc) =>
      visitExp(exp)

    case Expression.FixpointProjectOut(_, exp, tpe, eff, loc) =>
      visitExp(exp)

    case Expression.Reify(_, _, _, _) =>
      Nil

  }


}
