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
import ca.uwaterloo.flix.language.ast.{Kind, Rigidity, SourceLocation, Type}
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{ParOps, Validation}

import scala.collection.immutable.SortedSet

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

  private def visitExp(exp0: Expression)(implicit scope: List[Type.KindedVar], flix: Flix): Validation[Unit, CompilationMessage] = exp0 match {
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

    case Expression.Scope(_, regionVar, exp, tpe, _, loc) =>
      flatMapN(visitExp(exp)(regionVar :: scope, flix)) {
        case e => checkType(tpe, loc)
      }

    case Expression.IfThenElse(exp1, exp2, exp3, tpe, _, loc) =>
      flatMapN(visitExp(exp1), visitExp(exp2), visitExp(exp3)) {
        case (e1, e2, e3) => checkType(tpe, loc)
      }

    case Expression.Stm(exp1, exp2, tpe, eff, loc) =>
      flatMapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => checkType(tpe, loc)
      }

    case Expression.Match(exp, rules, tpe, eff, loc) =>
      val matchVal = visitExp(exp)
      val rulesVal = traverse(rules) {
        case MatchRule(pat, guard, body) => flatMapN(visitExp(guard), visitExp(body)) {
          case (g, b) => ().toSuccess
        }
      }
      flatMapN(matchVal, rulesVal) {
        case (m, rs) => checkType(tpe, loc)
      }

    case Expression.Choose(exps, rules, tpe, eff, loc) =>
      val expsVal = traverse(exps)(visitExp)
      val rulesVal = traverse(rules) {
        case ChoiceRule(pat, exp) => flatMapN(visitExp(exp))(_ => ().toSuccess)
      }
      flatMapN(expsVal, rulesVal) {
        case (es, rs) => checkType(tpe, loc)
      }

    case Expression.Tag(sym, tag, exp, tpe, eff, loc) =>
      flatMapN(visitExp(exp)) {
        case e => checkType(tpe, loc)
      }

    case Expression.Tuple(elms, tpe, eff, loc) =>
      flatMapN(traverse(elms)(visitExp)) {
        case es => checkType(tpe, loc)
      }

    case Expression.RecordEmpty(tpe, loc) =>
      ().toSuccess

    case Expression.RecordSelect(base, field, tpe, eff, loc) =>
      flatMapN(visitExp(base)) {
        case b => checkType(tpe, loc)
      }

    case Expression.RecordExtend(field, value, rest, tpe, eff, loc) =>
      flatMapN(visitExp(value), visitExp(rest)) {
        case (v, r) => checkType(tpe, loc)
      }

    case Expression.RecordRestrict(field, rest, tpe, eff, loc) =>
      flatMapN(visitExp(rest)) {
        case r => checkType(tpe, loc)
      }

    case Expression.ArrayLit(exps, exp, tpe, eff, loc) =>
      flatMapN(traverse(exps)(visitExp), visitExp(exp)) {
        case (es, e) => checkType(tpe, loc)
      }

    case Expression.ArrayNew(exp1, exp2, exp3, tpe, eff, loc) =>
      flatMapN(visitExp(exp1), visitExp(exp2), visitExp(exp3)) {
        case (e1, e2, e3) => checkType(tpe, loc)
      }

    case Expression.ArrayLoad(base, index, tpe, eff, loc) =>
      flatMapN(visitExp(base), visitExp(index)) {
        case (b, i) => checkType(tpe, loc)
      }

    case Expression.ArrayLength(base, eff, loc) =>
      flatMapN(visitExp(base)) {
        case b => ().toSuccess
      }

    case Expression.ArrayStore(base, index, elm, loc) =>
      flatMapN(visitExp(base), visitExp(index), visitExp(elm)) {
        case (b, i, e) => ().toSuccess
      }

    case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) =>
      flatMapN(visitExp(base), visitExp(beginIndex), visitExp(endIndex)) {
        case (b, i1, i2) => checkType(tpe, loc)
      }

    case Expression.Ref(exp1, exp2, tpe, eff, loc) =>
      flatMapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => checkType(tpe, loc)
      }

    case Expression.Deref(exp, tpe, eff, loc) =>
      flatMapN(visitExp(exp)) {
        case e => checkType(tpe, loc)
      }

    case Expression.Assign(exp1, exp2, tpe, eff, loc) =>
      flatMapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => checkType(tpe, loc)
      }

    case Expression.Ascribe(exp, tpe, eff, loc) =>
      flatMapN(visitExp(exp)) {
        case e => checkType(tpe, loc)
      }

    case Expression.Cast(exp, declaredType, declaredEff, tpe, eff, loc) =>
      flatMapN(visitExp(exp)) {
        case e => checkType(tpe, loc)
      }

    case Expression.TryCatch(exp, rules, tpe, eff, loc) =>
      val rulesVal = traverse(rules) {
        case CatchRule(sym, clazz, e) => visitExp(e).map(_ => ())
      }
      flatMapN(visitExp(exp), rulesVal) {
        case (e, rs) => checkType(tpe, loc)
      }

    case Expression.InvokeConstructor(constructor, args, tpe, eff, loc) =>
      flatMapN(traverse(args)(visitExp)) {
        case as => checkType(tpe, loc)
      }

    case Expression.InvokeMethod(method, exp, args, tpe, eff, loc) =>
      flatMapN(visitExp(exp), traverse(args)(visitExp)) {
        case (e, as) => checkType(tpe, loc)
      }

    case Expression.InvokeStaticMethod(method, args, tpe, eff, loc) =>
      flatMapN(traverse(args)(visitExp)) {
        case as => checkType(tpe, loc)
      }

    case Expression.GetField(field, exp, tpe, eff, loc) =>
      flatMapN(visitExp(exp)) {
        case e => checkType(tpe, loc)
      }

    case Expression.PutField(field, exp1, exp2, tpe, eff, loc) =>
      flatMapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => checkType(tpe, loc)
      }

    case Expression.GetStaticField(field, tpe, eff, loc) =>
      ().toSuccess

    case Expression.PutStaticField(field, exp, tpe, eff, loc) =>
      flatMapN(visitExp(exp)) {
        case e => checkType(tpe, loc)
      }

    case Expression.NewChannel(exp, tpe, eff, loc) =>
      flatMapN(visitExp(exp)) {
        case e => checkType(tpe, loc)
      }

    case Expression.GetChannel(exp, tpe, eff, loc) =>
      flatMapN(visitExp(exp)) {
        case e => checkType(tpe, loc)
      }

    case Expression.PutChannel(exp1, exp2, tpe, eff, loc) =>
      flatMapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => checkType(tpe, loc)
      }

    case Expression.SelectChannel(rules, default, tpe, eff, loc) =>
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

    case Expression.Spawn(exp, tpe, eff, loc) =>
      flatMapN(visitExp(exp)) {
        case e => checkType(tpe, loc)
      }

    case Expression.Lazy(exp, tpe, loc) =>
      flatMapN(visitExp(exp)) {
        case e => checkType(tpe, loc)
      }

    case Expression.Force(exp, tpe, eff, loc) =>
      flatMapN(visitExp(exp)) {
        case e => checkType(tpe, loc)
      }

    case Expression.FixpointConstraintSet(cs0, stf, tpe, loc) =>
      ().toSuccess // TODO

    case Expression.FixpointMerge(exp1, exp2, stf, tpe, eff, loc) =>
      flatMapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => checkType(tpe, loc)
      }

    case Expression.FixpointSolve(exp, stf, tpe, eff, loc) =>
      flatMapN(visitExp(exp)) {
        case e => checkType(tpe, loc)
      }

    case Expression.FixpointFilter(pred, exp, tpe, eff, loc) =>
      flatMapN(visitExp(exp)) {
        case e => checkType(tpe, loc)
      }

    case Expression.FixpointProjectIn(exp, pred, tpe, eff, loc) =>
      flatMapN(visitExp(exp)) {
        case e => checkType(tpe, loc)
      }

    case Expression.FixpointProjectOut(pred, exp, tpe, eff, loc) =>
      flatMapN(visitExp(exp)) {
        case e => checkType(tpe, loc)
      }

    case Expression.Reify(t, tpe, eff, loc) =>
      checkType(tpe, loc)

    case Expression.ReifyType(t, k, tpe, eff, loc) =>
      checkType(tpe, loc)

    case Expression.ReifyEff(sym, exp1, exp2, exp3, tpe, eff, loc) =>
      flatMapN(visitExp(exp1), visitExp(exp2), visitExp(exp3)) {
        case (e1, e2, e3) => checkType(tpe, loc)
      }

  }

  /**
    * Ensures that no region escapes inside `tpe`.
    */
  private def checkType(tpe: Type, loc: SourceLocation)(implicit scope: List[Type.KindedVar]): Validation[Unit, CompilationMessage] = {
    // Compute the region variables that escape.
    val escapes = regionVarsOf(tpe) -- scope

    // Return an error if a region variable escapes.
    if (escapes.nonEmpty) {
      val rvar = escapes.head
      return TypeError.RegionVarEscapes(rvar, tpe, loc).toFailure
    }

    // Otherwise return success.
    ().toSuccess
  }

  /**
    * Returns all region variables in the given type `tpe`.
    */
  private def regionVarsOf(tpe: Type): SortedSet[Type.KindedVar] = tpe.typeVars.filter {
    case tvar =>
      val isBool = tvar.sym.kind == Kind.Bool
      val isRigid = tvar.sym.rigidity == Rigidity.Rigid
      isBool && isRigid
  }

}
