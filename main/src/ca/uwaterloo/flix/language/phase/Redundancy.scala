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
import ca.uwaterloo.flix.language.ast.{BinaryOperator, SourceLocation, Symbol, Type, TypedAst}
import ca.uwaterloo.flix.language.ast.TypedAst.{Expression, FormalParam, MatchRule, Pattern, TypeParam}
import ca.uwaterloo.flix.language.errors.RedundancyError
import ca.uwaterloo.flix.language.errors.RedundancyError.{Dead, UnusedFormalParam, UnusedTypeParam, UnusedVarSym}
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

object Redundancy extends Phase[TypedAst.Root, TypedAst.Root] {

  object Used {
    /**
      * Represents the empty set of used symbols.
      */
    val empty: Validation[Used, RedundancyError] = Used(Set.empty, Set.empty).toSuccess

    /**
      * Returns a `used` object where the given variable `sym` is marked as used.
      */
    def of(sym: Symbol.VarSym): Validation[Used, RedundancyError] = Used(Set.empty, Set(sym)).toSuccess

  }

  case class Used(defSyms: Set[Symbol.DefnSym], varSyms: Set[Symbol.VarSym]) {
    // TODO: EffSym
    // TODO: EnumSym
    // TODO: Cases
    // TODO: RelSym
    // TODO: LatSym

    // TODO: Optimize for empty case.
    def ++(that: Used): Used = Used(this.defSyms ++ that.defSyms, this.varSyms ++ that.varSyms)

    def -(sym: Symbol.VarSym): Used = copy(varSyms = varSyms - sym)
  }

  def run(root: TypedAst.Root)(implicit flix: Flix): Validation[TypedAst.Root, RedundancyError] = flix.phase("Redundancy") {
    val defs = root.defs.map { case (_, v) => visitDef(v, root) }

    for {
      _ <- sequence(defs)
    } yield root

  }

  private def visitDef(defn: TypedAst.Def, root: TypedAst.Root): Validation[TypedAst.Def, RedundancyError] = {
    for {
      u <- usedExp(defn.exp)
      _ <- checkFparams(defn, u)
      _ <- checkUnusedTypeparams(defn)
      _ <- constantFoldExp(defn.exp, Map.empty)
    } yield defn
  }

  private def checkFparams(defn: TypedAst.Def, used: Redundancy.Used): Validation[List[Unit], RedundancyError] = {
    traverse(defn.fparams) {
      case FormalParam(sym, _, _, _) if unused(sym, used) => UnusedFormalParam(sym, Some(defn.sym)).toFailure
      case FormalParam(_, _, _, _) => ().toSuccess
    }
  }

  private def checkUnusedTypeparams(defn: TypedAst.Def): Validation[List[Unit], RedundancyError] = {
    traverse(defn.tparams) {
      case TypeParam(name, tvar, _) if unused(tvar, defn.tpe.typeVars) => UnusedTypeParam(name, defn.sym).toFailure
      case TypeParam(_, _, _) => ().toSuccess
    }
  }

  /**
    * Returns symbols used in the given expression `e0`.
    */
  private def usedExp(e0: TypedAst.Expression): Validation[Used, RedundancyError] = e0 match {
    case Expression.Unit(_) => Used.empty

    case Expression.True(_) => Used.empty

    case Expression.False(_) => Used.empty

    case Expression.Char(_, _) => Used.empty

    case Expression.Float32(_, _) => Used.empty

    case Expression.Float64(_, _) => Used.empty

    case Expression.Int8(_, _) => Used.empty

    case Expression.Int16(_, _) => Used.empty

    case Expression.Int32(_, _) => Used.empty

    case Expression.Int64(_, _) => Used.empty

    case Expression.BigInt(_, _) => Used.empty

    case Expression.Str(_, _) => Used.empty

    case Expression.Wild(_, _, _) => Used.empty

    case Expression.Var(sym, tpe, eff, loc) => Used.of(sym)

    case Expression.Def(sym, tpe, eff, loc) => ??? // TODO

    case Expression.Eff(sym, tpe, eff, loc) => ??? // TODO

    case Expression.Hole(sym, tpe, eff, loc) => ??? // TODO

    case Expression.Lambda(fparam, exp, tpe, eff, loc) => ??? // TODO

    case Expression.Apply(exp1, exp2, _, _, _) =>
      val us1 = usedExp(exp1)
      val us2 = usedExp(exp2)
      mapN(us1, us2)(_ ++ _)

    case Expression.Unary(op, exp, _, _, _) => ??? // TODO

    case Expression.Binary(_, exp1, exp2, _, _, _) =>
      val us1 = usedExp(exp1)
      val us2 = usedExp(exp2)
      mapN(us1, us2)(_ ++ _)

    case Expression.Let(sym, exp1, exp2, _, _, _) =>
      val us1 = usedExp(exp1)
      val us2 = usedExp(exp2)
      flatMapN(us1, us2) {
        case (u1, u2) if unused(sym, u2) => UnusedVarSym(sym).toFailure
        case (u1, u2) => ((u1 ++ u2) - sym).toSuccess
      }

    case Expression.LetRec(sym, exp1, exp2, tpe, eff, loc) => ??? // TODO

    case Expression.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      val us1 = usedExp(exp1)
      val us2 = usedExp(exp2)
      val us3 = usedExp(exp3)
      mapN(us1, us2, us3)(_ ++ _ ++ _)

    case Expression.Match(exp, rules, tpe, eff, loc) => Used.empty // TODO

    case Expression.Switch(rules, tpe, eff, loc) => ??? // TODO

    case Expression.Tag(sym, tag, exp, tpe, eff, loc) =>
      Used.empty // TODO

    case Expression.Tuple(elms, tpe, eff, loc) => ??? // TODO
    case Expression.RecordEmpty(tpe, eff, loc) => ??? // TODO
    case Expression.RecordSelect(exp, label, tpe, eff, loc) => ??? // TODO
    case Expression.RecordExtend(label, value, rest, tpe, eff, loc) => ??? // TODO
    case Expression.RecordRestrict(label, rest, tpe, eff, loc) => ??? // TODO
    case Expression.ArrayLit(elms, tpe, eff, loc) => ??? // TODO
    case Expression.ArrayNew(elm, len, tpe, eff, loc) => ??? // TODO
    case Expression.ArrayLoad(base, index, tpe, eff, loc) => ??? // TODO
    case Expression.ArrayLength(base, tpe, eff, loc) => ??? // TODO
    case Expression.ArrayStore(base, index, elm, tpe, eff, loc) => ??? // TODO
    case Expression.ArraySlice(base, beginIndex, endIndex, tpe, eff, loc) => ??? // TODO
    case Expression.VectorLit(elms, tpe, eff, loc) => ??? // TODO
    case Expression.VectorNew(elm, len, tpe, eff, loc) => ??? // TODO
    case Expression.VectorLoad(base, index, tpe, eff, loc) => ??? // TODO
    case Expression.VectorStore(base, index, elm, tpe, eff, loc) => ??? // TODO
    case Expression.VectorLength(base, tpe, eff, loc) => ??? // TODO
    case Expression.VectorSlice(base, startIndex, endIndex, tpe, eff, loc) => ??? // TODO
    case Expression.Ref(exp, tpe, eff, loc) => ??? // TODO
    case Expression.Deref(exp, tpe, eff, loc) => ??? // TODO
    case Expression.Assign(exp1, exp2, tpe, eff, loc) => ??? // TODO
    case Expression.HandleWith(exp, bindings, tpe, eff, loc) => ??? // TODO
    case Expression.Existential(fparam, exp, eff, loc) => ??? // TODO
    case Expression.Universal(fparam, exp, eff, loc) => ??? // TODO
    case Expression.Ascribe(exp, tpe, eff, loc) => ??? // TODO
    case Expression.Cast(exp, tpe, eff, loc) => ??? // TODO
    case Expression.NativeConstructor(constructor, args, tpe, eff, loc) => ??? // TODO
    case Expression.TryCatch(exp, rules, tpe, eff, loc) => ??? // TODO
    case Expression.NativeField(field, tpe, eff, loc) => ??? // TODO
    case Expression.NativeMethod(method, args, tpe, eff, loc) => ??? // TODO
    case Expression.NewChannel(exp, tpe, eff, loc) => ??? // TODO
    case Expression.GetChannel(exp, tpe, eff, loc) => ??? // TODO
    case Expression.PutChannel(exp1, exp2, tpe, eff, loc) => ??? // TODO
    case Expression.SelectChannel(rules, default, tpe, eff, loc) => ??? // TODO
    case Expression.Spawn(exp, tpe, eff, loc) => ??? // TODO
    case Expression.Sleep(exp, tpe, eff, loc) => ??? // TODO
    case Expression.FixpointConstraint(c, tpe, eff, loc) => ??? // TODO
    case Expression.FixpointCompose(exp1, exp2, tpe, eff, loc) => ??? // TODO
    case Expression.FixpointSolve(exp, tpe, eff, loc) => ??? // TODO
    case Expression.FixpointProject(pred, exp, tpe, eff, loc) => ??? // TODO
    case Expression.FixpointEntails(exp1, exp2, tpe, eff, loc) => ??? // TODO
    case Expression.UserError(tpe, eff, loc) => ??? // TODO
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

  // TODO: Code like f(x), and f(x) is redundant if both are pure... This is just common sub-expression elimination.

  // TODO: Need notion of stable expression which should be used instead of variable symbol.
  sealed trait StableExp

  object StableExp {

    // TODO: Should literals be considered stable?


    //
    //    case Expression.Unit(loc) => ??? // TODO
    //    case Expression.True(loc) => ??? // TODO
    //    case Expression.False(loc) => ??? // TODO
    //    case Expression.Char(lit, loc) => ??? // TODO
    //    case Expression.Float32(lit, loc) => ??? // TODO
    //    case Expression.Float64(lit, loc) => ??? // TODO
    //    case Expression.Int8(lit, loc) => ??? // TODO
    //    case Expression.Int16(lit, loc) => ??? // TODO
    //    case Expression.Int32(lit, loc) => ??? // TODO
    //    case Expression.Int64(lit, loc) => ??? // TODO
    //    case Expression.BigInt(lit, loc) => ??? // TODO
    //    case Expression.Str(lit, loc) => ??? // TODO


    case class Var(sym: Symbol.VarSym) extends StableExp

    case class Def(sym: Symbol.DefnSym) extends StableExp

    case class Apply(exp1: StableExp, exp2: StableExp) extends StableExp

    //    case Expression.Unary(op, exp, tpe, eff, loc) => ??? // TODO
    //    case Expression.Binary(op, exp1, exp2, tpe, eff, loc) => ??? // TODO
    //
    //    case Expression.Match(exp, rules, tpe, eff, loc) => ??? // TODO
    //    case Expression.Switch(rules, tpe, eff, loc) => ??? // TODO
    //    case Expression.Tag(sym, tag, exp, tpe, eff, loc) => ??? // TODO
    //    case Expression.Tuple(elms, tpe, eff, loc) => ??? // TODO
    //    case Expression.RecordEmpty(tpe, eff, loc) => ??? // TODO
    //    case Expression.RecordSelect(exp, label, tpe, eff, loc) => ??? // TODO
    //    case Expression.RecordExtend(label, value, rest, tpe, eff, loc) => ??? // TODO
    //    case Expression.RecordRestrict(label, rest, tpe, eff, loc) => ??? // TODO
    //    case Expression.ArrayLit(elms, tpe, eff, loc) => ??? // TODO
    //    case Expression.ArrayNew(elm, len, tpe, eff, loc) => ??? // TODO
    //    case Expression.ArrayLoad(base, index, tpe, eff, loc) => ??? // TODO
    //    case Expression.ArrayLength(base, tpe, eff, loc) => ??? // TODO
    //    case Expression.ArrayStore(base, index, elm, tpe, eff, loc) => ??? // TODO
    //    case Expression.ArraySlice(base, beginIndex, endIndex, tpe, eff, loc) => ??? // TODO
    //    case Expression.VectorLit(elms, tpe, eff, loc) => ??? // TODO
    //    case Expression.VectorNew(elm, len, tpe, eff, loc) => ??? // TODO
    //    case Expression.VectorLoad(base, index, tpe, eff, loc) => ??? // TODO
    //    case Expression.VectorStore(base, index, elm, tpe, eff, loc) => ??? // TODO
    //    case Expression.VectorLength(base, tpe, eff, loc) => ??? // TODO
    //    case Expression.VectorSlice(base, startIndex, endIndex, tpe, eff, loc) => ??? // TODO
    //    case Expression.Ref(exp, tpe, eff, loc) => ??? // TODO
    //    case Expression.Deref(exp, tpe, eff, loc) => ??? // TODO
    //    case Expression.Assign(exp1, exp2, tpe, eff, loc) => ??? // TODO
    //    case Expression.HandleWith(exp, bindings, tpe, eff, loc) => ??? // TODO
    //    case Expression.NewChannel(exp, tpe, eff, loc) => ??? // TODO
    //    case Expression.GetChannel(exp, tpe, eff, loc) => ??? // TODO
    //    case Expression.PutChannel(exp1, exp2, tpe, eff, loc) => ??? // TODO
    //    case Expression.SelectChannel(rules, default, tpe, eff, loc) => ??? // TODO
    //    case Expression.Spawn(exp, tpe, eff, loc) => ??? // TODO

  }


  private def unify(p1: Pattern, p2: Pattern): Validation[Unit, RedundancyError] = (p1, p2) match {
    case (Pattern.Tag(_, tag1, pat1, _, _), Pattern.Tag(_, tag2, pat2, _, _)) =>
      if (tag1 == tag2)
        unify(pat1, pat2)
      else
        RedundancyError.ImpossibleMatch(p1.loc, p2.loc).toFailure
    case _ => ().toSuccess
  }

  private def unused(sym: Symbol.VarSym, used: Redundancy.Used): Boolean = !used.varSyms.contains(sym)

  private def unused(sym: Type.Var, used: Set[Type.Var]): Boolean = !used.contains(sym)
}
