/*
 *  Copyright 2022 Magnus Madsen
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
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{ParOps, Validation}

/**
  * The region phase ensures that regions do not escape outside of their scope.
  *
  * It does so by keep tracking of every region variable in scope and ensure that every
  * rigid Boolean type variable that occurs anywhere belongs is in scope.
  */
object Regions {

  def run(root: Root)(implicit flix: Flix): Validation[Root, CompilationMessage] = flix.phase("Regions") {
    val defsVal = Validation.sequence(ParOps.parMap(root.defs)(kv => visitDef(kv._2).map(d => kv._1 -> d)))

    // TODO: Instances

    mapN(defsVal) {
      case ds => root.copy(defs = ds.toMap)
    }
  }

  private def visitDef(def0: Def)(implicit flix: Flix): Validation[Def, CompilationMessage] =
    mapN(visitExp(def0.impl.exp)(Nil, flix)) {
      case e => def0
    }

  private def visitExp(exp0: Expression)(implicit scope: List[Symbol.RegionSym], flix: Flix): Validation[Unit, CompilationMessage] = exp0 match {
    case Expression.Unit(_) => ().toSuccess

    case Expression.Null(_, _) => ().toSuccess

    case Expression.True(_) => ().toSuccess

    case Expression.False(_) => ().toSuccess

    case Expression.Char(_, _) => ().toSuccess

    case Expression.Float32(_, _) => ().toSuccess

    case Expression.Float64(_, _) => ().toSuccess

    case Expression.Int8(_, _) => ().toSuccess

    case Expression.Int16(_, _) => ().toSuccess

    case Expression.Int32(_, _) => ().toSuccess

    case Expression.Int64(_, _) => ().toSuccess

    case Expression.BigInt(_, _) => ().toSuccess

    case Expression.Str(_, _) => ().toSuccess

    case Expression.Default(_, _) => ().toSuccess

    case Expression.Wild(_, _) => ().toSuccess

    case Expression.Var(_, tpe, loc) => checkType(tpe, loc)

    case Expression.Def(_, _, _) => ().toSuccess

    case Expression.Sig(_, _, _) => ().toSuccess

    case Expression.Hole(_, _, _) => ().toSuccess

    case Expression.Lambda(_, exp, tpe, loc) =>
      flatMapN(visitExp(exp)) {
        case e => checkType(tpe, loc)
      }

    case Expression.Apply(exp, exps, tpe, _, loc) =>
      flatMapN(visitExp(exp), traverse(exps)(visitExp)) {
        case (e, es) => checkType(tpe, loc)
      }

    case Expression.Unary(_, exp, tpe, _, loc) =>
      flatMapN(visitExp(exp)) {
        case _ => checkType(tpe, loc)
      }

    case Expression.Binary(_, exp1, exp2, tpe, _, loc) =>
      flatMapN(visitExp(exp1), visitExp(exp2)) {
        case (_, _) => checkType(tpe, loc)
      }

    case Expression.Let(_, _, exp1, exp2, tpe, _, loc) =>
      flatMapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => checkType(tpe, loc)
      }

    case Expression.LetRec(_, _, exp1, exp2, tpe, _, loc) =>
      flatMapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => checkType(tpe, loc)
      }

    case Expression.Region(_, _) =>
      ().toSuccess

    case Expression.Scope(sym, _, _, exp, tpe, _, loc) =>
      flatMapN(visitExp(exp)(sym :: scope, flix)) {
        case e => checkType(tpe, loc)
      }

    case Expression.IfThenElse(exp1, exp2, exp3, tpe, _, loc) =>
      flatMapN(visitExp(exp1), visitExp(exp2), visitExp(exp3)) {
        case (e1, e2, e3) => checkType(tpe, loc)
      }

    case Expression.Stm(exp1, exp2, tpe, _, loc) =>
      flatMapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => checkType(tpe, loc)
      }

    case Expression.Discard(exp, _, _) => visitExp(exp)

    case Expression.Match(exp, rules, tpe, _, loc) =>
      val matchVal = visitExp(exp)
      val rulesVal = traverse(rules) {
        case MatchRule(pat, guard, body) => flatMapN(visitExp(guard), visitExp(body)) {
          case (g, b) => ().toSuccess
        }
      }
      flatMapN(matchVal, rulesVal) {
        case (m, rs) => checkType(tpe, loc)
      }

    case Expression.Choose(exps, rules, tpe, _, loc) =>
      val expsVal = traverse(exps)(visitExp)
      val rulesVal = traverse(rules) {
        case ChoiceRule(pat, exp) => flatMapN(visitExp(exp))(_ => ().toSuccess)
      }
      flatMapN(expsVal, rulesVal) {
        case (es, rs) => checkType(tpe, loc)
      }

    case Expression.Tag(sym, _, exp, tpe, _, loc) =>
      flatMapN(visitExp(exp)) {
        case e => checkType(tpe, loc)
      }

    case Expression.Tuple(elms, tpe, _, loc) =>
      flatMapN(traverse(elms)(visitExp)) {
        case es => checkType(tpe, loc)
      }

    case Expression.RecordEmpty(_, _) =>
      ().toSuccess

    case Expression.RecordSelect(exp, _, tpe, _, loc) =>
      flatMapN(visitExp(exp)) {
        case b => checkType(tpe, loc)
      }

    case Expression.RecordExtend(_, exp1, exp2, tpe, _, loc) =>
      flatMapN(visitExp(exp1), visitExp(exp2)) {
        case (v, r) => checkType(tpe, loc)
      }

    case Expression.RecordRestrict(_, exp, tpe, _, loc) =>
      flatMapN(visitExp(exp)) {
        case r => checkType(tpe, loc)
      }

    case Expression.ArrayLit(exps, exp, tpe, _, loc) =>
      flatMapN(traverse(exps)(visitExp), visitExp(exp)) {
        case (es, e) => checkType(tpe, loc)
      }

    case Expression.ArrayNew(exp1, exp2, exp3, tpe, _, loc) =>
      flatMapN(visitExp(exp1), visitExp(exp2), visitExp(exp3)) {
        case (e1, e2, e3) => checkType(tpe, loc)
      }

    case Expression.ArrayLoad(exp1, exp2, tpe, _, loc) =>
      flatMapN(visitExp(exp1), visitExp(exp2)) {
        case (b, i) => checkType(tpe, loc)
      }

    case Expression.ArrayLength(exp, _, loc) =>
      flatMapN(visitExp(exp)) {
        case b => ().toSuccess
      }

    case Expression.ArrayStore(exp1, exp2, exp3, loc) =>
      flatMapN(visitExp(exp1), visitExp(exp2), visitExp(exp3)) {
        case (b, i, e) => ().toSuccess
      }

    case Expression.ArraySlice(exp1, exp2, exp3, tpe, loc) =>
      flatMapN(visitExp(exp1), visitExp(exp2), visitExp(exp3)) {
        case (b, i1, i2) => checkType(tpe, loc)
      }

    case Expression.Ref(exp1, exp2, tpe, _, loc) =>
      flatMapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => checkType(tpe, loc)
      }

    case Expression.Deref(exp, tpe, _, loc) =>
      flatMapN(visitExp(exp)) {
        case e => checkType(tpe, loc)
      }

    case Expression.Assign(exp1, exp2, tpe, _, loc) =>
      flatMapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => checkType(tpe, loc)
      }

    case Expression.Ascribe(exp, tpe, _, loc) =>
      flatMapN(visitExp(exp)) {
        case e => checkType(tpe, loc)
      }

    case Expression.Cast(exp, _, _, tpe, _, loc) =>
      flatMapN(visitExp(exp)) {
        case e => checkType(tpe, loc)
      }

    case Expression.TryCatch(exp, rules, tpe, _, loc) =>
      val rulesVal = traverse(rules) {
        case CatchRule(sym, clazz, e) => visitExp(e).map(_ => ())
      }
      flatMapN(visitExp(exp), rulesVal) {
        case (e, rs) => checkType(tpe, loc)
      }

    case Expression.InvokeConstructor(_, exps, tpe, _, loc) =>
      flatMapN(traverse(exps)(visitExp)) {
        case as => checkType(tpe, loc)
      }

    case Expression.InvokeMethod(_, exp, exps, tpe, _, loc) =>
      flatMapN(visitExp(exp), traverse(exps)(visitExp)) {
        case (e, as) => checkType(tpe, loc)
      }

    case Expression.InvokeStaticMethod(_, exps, tpe, _, loc) =>
      flatMapN(traverse(exps)(visitExp)) {
        case as => checkType(tpe, loc)
      }

    case Expression.GetField(_, exp, tpe, _, loc) =>
      flatMapN(visitExp(exp)) {
        case e => checkType(tpe, loc)
      }

    case Expression.PutField(_, exp1, exp2, tpe, _, loc) =>
      flatMapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => checkType(tpe, loc)
      }

    case Expression.GetStaticField(_, tpe, _, loc) =>
      ().toSuccess

    case Expression.PutStaticField(_, exp, tpe, _, loc) =>
      flatMapN(visitExp(exp)) {
        case e => checkType(tpe, loc)
      }

    case Expression.NewChannel(exp, tpe, _, loc) =>
      flatMapN(visitExp(exp)) {
        case e => checkType(tpe, loc)
      }

    case Expression.GetChannel(exp, tpe, _, loc) =>
      flatMapN(visitExp(exp)) {
        case e => checkType(tpe, loc)
      }

    case Expression.PutChannel(exp1, exp2, tpe, _, loc) =>
      flatMapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => checkType(tpe, loc)
      }

    case Expression.SelectChannel(rules, default, tpe, _, loc) =>
      val rulesVal = traverse(rules) {
        case SelectChannelRule(sym, chan, exp) => flatMapN(visitExp(chan), visitExp(exp)) {
          case (c, e) => ().toSuccess
        }
      }

      val defaultVal = default match {
        case None => None.toSuccess
        case Some(exp) => visitExp(exp) map {
          case e => Some(e)
        }
      }

      flatMapN(rulesVal, defaultVal) {
        case (rs, d) => checkType(tpe, loc)
      }

    case Expression.Spawn(exp, tpe, _, loc) =>
      flatMapN(visitExp(exp)) {
        case e => checkType(tpe, loc)
      }

    case Expression.Lazy(exp, tpe, loc) =>
      flatMapN(visitExp(exp)) {
        case e => checkType(tpe, loc)
      }

    case Expression.Force(exp, tpe, _, loc) =>
      flatMapN(visitExp(exp)) {
        case e => checkType(tpe, loc)
      }

    case Expression.FixpointConstraintSet(cs0, _, tpe, loc) =>
      ().toSuccess // TODO

    case Expression.FixpointLambda(_, exp, _, tpe, _, loc) =>
      flatMapN(visitExp(exp)) {
        case e => checkType(tpe, loc)
      }

    case Expression.FixpointMerge(exp1, exp2, _, tpe, _, loc) =>
      flatMapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => checkType(tpe, loc)
      }

    case Expression.FixpointSolve(exp, _, tpe, _, loc) =>
      flatMapN(visitExp(exp)) {
        case e => checkType(tpe, loc)
      }

    case Expression.FixpointFilter(_, exp, tpe, _, loc) =>
      flatMapN(visitExp(exp)) {
        case e => checkType(tpe, loc)
      }

    case Expression.FixpointProjectIn(exp, _, tpe, _, loc) =>
      flatMapN(visitExp(exp)) {
        case e => checkType(tpe, loc)
      }

    case Expression.FixpointProjectOut(_, exp, tpe, _, loc) =>
      flatMapN(visitExp(exp)) {
        case e => checkType(tpe, loc)
      }

    case Expression.Reify(_, tpe, _, loc) =>
      checkType(tpe, loc)

    case Expression.ReifyType(_, k, tpe, _, loc) =>
      checkType(tpe, loc)

    case Expression.ReifyEff(_, exp1, exp2, exp3, tpe, _, loc) =>
      flatMapN(visitExp(exp1), visitExp(exp2), visitExp(exp3)) {
        case (e1, e2, e3) => checkType(tpe, loc)
      }

  }

  /**
    * Ensures that no region escapes inside `tpe`.
    */
  private def checkType(tpe: Type, loc: SourceLocation)(implicit scope: List[Symbol.RegionSym]): Validation[Unit, CompilationMessage] = {
    // Compute the region variables that escape.
    val escapes = tpe.regionSyms -- scope

    // Return an error if a region variable escapes.
    if (escapes.nonEmpty) {
      val sym = escapes.head
      return TypeError.RegionEscapes(sym, tpe, loc).toFailure
    }

    // Otherwise return success.
    ().toSuccess
  }

}
