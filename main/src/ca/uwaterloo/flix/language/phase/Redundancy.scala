/*
 *  Copyright 2019 Magnus Madsen
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
import ca.uwaterloo.flix.language.ast.{BinaryOperator, SourceLocation, Symbol, TypedAst}
import ca.uwaterloo.flix.language.ast.TypedAst.{Expression, MatchRule, Pattern}
import ca.uwaterloo.flix.language.errors.RedundancyError
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

object Redundancy extends Phase[TypedAst.Root, TypedAst.Root] {

  case class Used(defSyms: Set[Symbol.DefnSym], varSyms: Set[Symbol.VarSym]) {
    // TODO: EffSym
    // TODO: EnumSym
    // TODO: Cases
    // TODO: RelSym
    // TODO: LatSym

    def +(that: Used): Used = ???
  }

  def run(root: TypedAst.Root)(implicit flix: Flix): Validation[TypedAst.Root, RedundancyError] = flix.phase("Redundancy") {
    val defs = root.defs.map { case (_, v) => visitDef(v, root) }

    for {
      _ <- sequence(defs)
    } yield root

  }

  private def visitDef(defn: TypedAst.Def, root: TypedAst.Root): Validation[TypedAst.Def, RedundancyError] = {
    for {
      _ <- visitExp(defn.exp, Map.empty)
      _ <- constantFoldExp(defn.exp, Map.empty)
    } yield defn
  }

  private def visitExp(e0: TypedAst.Expression, c: Map[Symbol.VarSym, AbsVal]): Validation[TypedAst.Expression, RedundancyError] = e0 match {
    case Expression.Unit(loc) => e0.toSuccess
    case Expression.True(loc) => e0.toSuccess
    case Expression.False(loc) => e0.toSuccess
    case Expression.Char(lit, loc) => e0.toSuccess
    case Expression.Float32(lit, loc) => e0.toSuccess
    case Expression.Float64(lit, loc) => e0.toSuccess
    case Expression.Int8(lit, loc) => e0.toSuccess
    case Expression.Int16(lit, loc) => e0.toSuccess
    case Expression.Int32(lit, loc) => e0.toSuccess
    case Expression.Int64(lit, loc) => e0.toSuccess
    case Expression.BigInt(lit, loc) => e0.toSuccess
    case Expression.Str(lit, loc) => e0.toSuccess
    case Expression.Wild(tpe, eff, loc) => e0.toSuccess
    case Expression.Var(sym, tpe, eff, loc) => e0.toSuccess
    case Expression.Def(sym, tpe, eff, loc) => e0.toSuccess
    case Expression.Eff(sym, tpe, eff, loc) => e0.toSuccess
    case Expression.Hole(sym, tpe, eff, loc) => e0.toSuccess
    case Expression.Lambda(fparam, exp, tpe, eff, loc) => e0.toSuccess
    case Expression.Apply(exp1, exp2, tpe, eff, loc) => e0.toSuccess
    case Expression.Unary(op, exp, tpe, eff, loc) => e0.toSuccess
    case Expression.Binary(op, exp1, exp2, tpe, eff, loc) => e0.toSuccess

    case Expression.Let(sym, exp1, exp2, tpe, eff, loc) =>
      for {
        _ <- visitExp(exp1, c)
        _ <- visitExp(exp2, c)
      } yield e0

    case Expression.LetRec(sym, exp1, exp2, tpe, eff, loc) => e0.toSuccess

    case Expression.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>

      val current = eval(exp1)
      for {
        _ <- compatible(c, current, exp1.loc)
        _ <- visitExp(exp2, current)
      } yield e0

    case Expression.Match(exp, rules, tpe, eff, loc) => e0.toSuccess
    case Expression.Switch(rules, tpe, eff, loc) => e0.toSuccess
    case Expression.Tag(sym, tag, exp, tpe, eff, loc) => e0.toSuccess
    case Expression.Tuple(elms, tpe, eff, loc) => e0.toSuccess
    case Expression.RecordEmpty(tpe, eff, loc) => e0.toSuccess
    case Expression.RecordSelect(exp, label, tpe, eff, loc) => e0.toSuccess
    case Expression.RecordExtend(label, value, rest, tpe, eff, loc) => e0.toSuccess
    case Expression.RecordRestrict(label, rest, tpe, eff, loc) => e0.toSuccess
    case Expression.ArrayLit(elms, tpe, eff, loc) => e0.toSuccess
    case Expression.ArrayNew(elm, len, tpe, eff, loc) => e0.toSuccess
    case Expression.ArrayLoad(base, index, tpe, eff, loc) => e0.toSuccess
    case Expression.ArrayLength(base, tpe, eff, loc) => e0.toSuccess
    case Expression.ArrayStore(base, index, elm, tpe, eff, loc) => e0.toSuccess
    case Expression.ArraySlice(base, beginIndex, endIndex, tpe, eff, loc) => e0.toSuccess
    case Expression.VectorLit(elms, tpe, eff, loc) => e0.toSuccess
    case Expression.VectorNew(elm, len, tpe, eff, loc) => e0.toSuccess
    case Expression.VectorLoad(base, index, tpe, eff, loc) => e0.toSuccess
    case Expression.VectorStore(base, index, elm, tpe, eff, loc) => e0.toSuccess
    case Expression.VectorLength(base, tpe, eff, loc) => e0.toSuccess
    case Expression.VectorSlice(base, startIndex, endIndex, tpe, eff, loc) => e0.toSuccess
    case Expression.Ref(exp, tpe, eff, loc) => e0.toSuccess
    case Expression.Deref(exp, tpe, eff, loc) => e0.toSuccess
    case Expression.Assign(exp1, exp2, tpe, eff, loc) => e0.toSuccess
    case Expression.HandleWith(exp, bindings, tpe, eff, loc) => e0.toSuccess
    case Expression.Existential(fparam, exp, eff, loc) => e0.toSuccess
    case Expression.Universal(fparam, exp, eff, loc) => e0.toSuccess
    case Expression.Ascribe(exp, tpe, eff, loc) => e0.toSuccess
    case Expression.Cast(exp, tpe, eff, loc) => e0.toSuccess
    case Expression.NativeConstructor(constructor, args, tpe, eff, loc) => e0.toSuccess
    case Expression.TryCatch(exp, rules, tpe, eff, loc) => e0.toSuccess
    case Expression.NativeField(field, tpe, eff, loc) => e0.toSuccess
    case Expression.NativeMethod(method, args, tpe, eff, loc) => e0.toSuccess
    case Expression.NewChannel(exp, tpe, eff, loc) => e0.toSuccess
    case Expression.GetChannel(exp, tpe, eff, loc) => e0.toSuccess
    case Expression.PutChannel(exp1, exp2, tpe, eff, loc) => e0.toSuccess
    case Expression.SelectChannel(rules, default, tpe, eff, loc) => e0.toSuccess
    case Expression.Spawn(exp, tpe, eff, loc) => e0.toSuccess
    case Expression.Sleep(exp, tpe, eff, loc) => e0.toSuccess
    case Expression.FixpointConstraint(c, tpe, eff, loc) => e0.toSuccess
    case Expression.FixpointCompose(exp1, exp2, tpe, eff, loc) => e0.toSuccess
    case Expression.FixpointSolve(exp, tpe, eff, loc) => e0.toSuccess
    case Expression.FixpointProject(pred, exp, tpe, eff, loc) => e0.toSuccess
    case Expression.FixpointEntails(exp1, exp2, tpe, eff, loc) => e0.toSuccess
    case Expression.UserError(tpe, eff, loc) => e0.toSuccess
  }

  sealed trait Value

  object Value {

    case object Bool extends Value

  }


  private def eval(e0: TypedAst.Expression): Map[Symbol.VarSym, AbsVal] = e0 match {
    case Expression.Binary(op, Expression.Var(sym, _, _, _), Expression.Int32(v, _), tpe, eff, loc) => op match {
      case BinaryOperator.Equal => Map(sym -> AbsVal.Range(v, v))
      case _ => Map.empty
    }
    case _ => Map.empty
  }

  private def compatible(m1: Map[Symbol.VarSym, Redundancy.AbsVal], m2: Map[Symbol.VarSym, Redundancy.AbsVal], loc: SourceLocation): Validation[Unit, RedundancyError] = {
    val commonKeys = m1.keys.filter(k2 => m2.contains(k2))

    commonKeys.headOption match {
      case None =>
        ().toSuccess
      case Some(key) =>
        val v1 = m1(key)
        val v2 = m2(key)

        compat(v1, v2, loc)
    }
  }

  private def compat(v1: AbsVal, v2: AbsVal, loc: SourceLocation): Validation[Unit, RedundancyError] = (v1, v2) match {
    case (AbsVal.Range(b1, e1), AbsVal.Range(b2, e2)) =>
      if (e1 < b2 || e2 < b1)
        RedundancyError.Dead(loc).toFailure
      else
        ().toSuccess

  }

  // TODO: We could store an upper, lower bound, and equal. At least for integers.
  // TODO: For other types it would just be equal and not equal.
  // TODO: Might even be for entire expressions, like xs.length > 0, xs.length == 0, and not just variables.

  sealed trait AbsVal

  object AbsVal {

    case class Range(b: Int, e: Int) extends AbsVal

  }


  // TODO
  private def isPure(e: Expression): Boolean = ???


  private def constantFoldExp(e0: TypedAst.Expression, env0: Map[Symbol.VarSym, Pattern]): Validation[Value, RedundancyError] = e0 match {

    case Expression.Let(sym, exp1, exp2, tpe, eff, loc) =>
      for {
        _ <- constantFoldExp(exp1, env0)
        _ <- constantFoldExp(exp2, env0)
      } yield Value.Bool

    case Expression.Match(exp, rules, tpe, eff, loc) => exp match {
      case Expression.Var(sym, _, _, _) =>
        val rs = traverse(rules) {
          case MatchRule(pat, guard, body) =>
            env0.get(sym) match {
              case None =>
                constantFoldExp(body, env0 + (sym -> pat))

              case Some(pat2) =>
                mapN(unify(pat, pat2)) {
                  case _ => constantFoldExp(body, env0 + (sym -> pat))
                }
            }
        }

        mapN(rs) {
          case _ => Value.Bool
        }
    }
    case _ => Value.Bool.toSuccess
  }

  // TODO: Need notion of stable expression which should be used instead of variable symbol.
  sealed trait StableExp

  object StableExp {

    case class StableVar() extends StableExp

    case class StableApply() extends StableExp

  }


  private def unify(p1: Pattern, p2: Pattern): Validation[Unit, RedundancyError] = (p1, p2) match {
    case (Pattern.Tag(_, tag1, pat1, _, _), Pattern.Tag(_, tag2, pat2, _, _)) =>
      if (tag1 == tag2)
        unify(pat1, pat2)
      else
        RedundancyError.ImpossibleMatch(p1.loc, p2.loc).toFailure
    case _ => ().toSuccess
  }
}
