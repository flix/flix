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
import ca.uwaterloo.flix.language.ast.{SourceLocation, TypedAst}
import ca.uwaterloo.flix.language.ast.TypedAst.{CatchRule, Expression, MatchRule, SelectChannelRule}
import ca.uwaterloo.flix.util.{ParOps, Validation}
import ca.uwaterloo.flix.util.Validation._

object CodeQuality {

  // TODO: DOC
  sealed trait CodeQualityError

  // TODO: DOC
  case class InhibitsLaziness(name: String, loc: SourceLocation) extends CodeQualityError

  // TODO: DOC
  def run(root: TypedAst.Root)(implicit flix: Flix): Validation[TypedAst.Root, CodeQualityError] = flix.phase("CodeQuality") {
    val results = ParOps.parMap(root.defs.values, visitDef)

    if (results.isEmpty)
      root.toSuccess
    else
      Failure(results.flatten.to(LazyList))
  }

  private def visitDef(def0: TypedAst.Def): List[CodeQualityError] = {
    visitExp(def0.impl.exp)
  }


  private def visitExp(exp0: Expression): List[CodeQualityError] = exp0 match {
    case Expression.Wild(tpe, loc) => Nil

    case Expression.Var(sym, tpe, loc) => Nil

    case Expression.Def(sym, tpe, loc) => Nil

    case Expression.Sig(sym, tpe, loc) => Nil

    case Expression.Hole(sym, tpe, eff, loc) => Nil

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
      visitExp(exp)

    case Expression.Apply(exp, exps, tpe, eff, loc) =>
      val init = visitExp(exp)
      exps.foldLeft(init) {
        case (acc, exp) => acc ++ visitExp(exp)
      }

    case Expression.Unary(sop, exp, tpe, eff, loc) =>
      visitExp(exp)

    case Expression.Binary(sop, exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.Let(sym, _, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

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
          macc ++ visitExp(guard) ++ visitExp(exp)
      }

    case Expression.Choose(exps, rules, tpe, eff, loc) =>
      visitExps(exps) // TODO

    case Expression.Tag(sym, tag, exp, tpe, eff, loc) =>
      visitExp(exp)

    case Expression.Tuple(elms, tpe, eff, loc) =>
      visitExps(elms)

    case Expression.RecordEmpty(tpe, loc) => Nil

    case Expression.RecordSelect(base, _, tpe, eff, loc) =>
      visitExp(base)

    case Expression.RecordExtend(_, value, rest, tpe, eff, loc) =>
      visitExp(rest) ++ visitExp(value)

    case Expression.RecordRestrict(_, rest, tpe, eff, loc) =>
      visitExp(rest)

    case Expression.ArrayLit(elms, tpe, eff, loc) =>
      visitExps(elms)

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
      visitExp(exp)

    case Expression.Universal(fparam, exp, loc) =>
      visitExp(exp)

    case Expression.Ascribe(exp, tpe, eff, loc) =>
      visitExp(exp)

    case Expression.Cast(exp, tpe, eff, loc) =>
      visitExp(exp)

    case Expression.TryCatch(exp, rules, tpe, eff, loc) =>
      visitExp(exp) ++ rules.flatMap {
        case CatchRule(_, _, exp) => visitExp(exp)
      }

    case Expression.InvokeConstructor(constructor, args, tpe, eff, loc) =>
      visitExps(args)

    case Expression.InvokeMethod(method, exp, args, tpe, eff, loc) =>
      args.foldLeft(visitExp(exp)) {
        case (macc, arg) => macc ++ visitExp(arg)
      }

    case Expression.InvokeStaticMethod(method, args, tpe, eff, loc) =>
      visitExps(args)

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

    case Expression.SelectChannel(rules, default, _, _, _) =>
      rules.flatMap {
        case SelectChannelRule(_, chan, exp) => visitExp(chan) ++ visitExp(exp)
      } ++ default.map(visitExp).getOrElse(Nil)

    case Expression.Spawn(exp, _, _, _) =>
      visitExp(exp)

    case Expression.Lazy(exp, _, _) =>
      visitExp(exp)

    case Expression.Force(exp, _, _, _) =>
      visitExp(exp)

    case Expression.FixpointConstraintSet(cs, _, _, _) =>
      Nil // TODO

    case Expression.FixpointMerge(exp1, exp2, _, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.FixpointSolve(exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.FixpointFilter(_, exp, _, _, _) =>
      visitExp(exp)

    case Expression.FixpointProjectIn(exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.FixpointProjectOut(_, exp, _, _, _) =>
      visitExp(exp)

    case Expression.Reify(_, _, _, _) =>
      Nil

  }

  private def visitExps(exps: List[Expression]): List[CodeQualityError] =
    exps.flatMap(visitExp)


}
