/*
 * Copyright 2018 Magnus Madsen
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
import ca.uwaterloo.flix.language.ast
import ca.uwaterloo.flix.language.ast.Symbol._
import ca.uwaterloo.flix.language.ast.{Type, TypeConstructor}
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.errors.EffectError
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.Validation

/**
  * A phase that computes the effect of every expression in the program.
  */
object Effects extends Phase[Root, Root] {

  /**
    * Performs effect inference on the given AST `root`.
    */
  def run(root: Root)(implicit flix: Flix): Validation[Root, EffectError] = flix.phase("Effects") {

    // TODO: Implement Effects.

    // TODO: Need not only effects, but also whether a result must be used or not.



//    root.defs.map {
//      case (sym, defn) => visitDef(defn)(flix, root)
//    }

    return root.toSuccess

  }

  private def visitDef(defn: Def)(implicit flix: Flix, root: Root): Def = {
    inferExp(defn.exp)
    defn
  }


  private def unifyM(typ1: Typ, typ2: Typ): InferMonad[(Typ, ast.Eff)] = InferMonad(null)

  private def inferExp(exp0: Expression)(implicit flix: Flix, root: Root): InferMonad[(Typ, ast.Eff)] = exp0 match {

    case Expression.Unit(_) => pure(Typ.Prim)

    case Expression.True(_) => pure(Typ.Prim)

    case Expression.False(_) => pure(Typ.Prim)

    case Expression.Char(lit, loc) => ???

    case Expression.Float32(lit, loc) => ???

    case Expression.Float64(lit, loc) => ???

    case Expression.Int8(lit, loc) => ???

    case Expression.Int16(lit, loc) => ???

    case Expression.Int32(_, _) => pure(Typ.Prim)

    case Expression.Int64(lit, loc) => ???

    case Expression.BigInt(lit, loc) => ???

    case Expression.Str(lit, loc) => ???

    case Expression.Wild(tpe, eff, loc) => ???

    case Expression.Var(sym, tpe, eff, loc) => pure(Typ.Prim)

    case Expression.Def(sym, tpe, eff, loc) => ???

    case Expression.Eff(sym, tpe, eff, loc) => ???

    case Expression.Hole(sym, tpe, eff, loc) => ???

    case Expression.Lambda(fparam, exp, tpe, eff, loc) =>
      val argumentType = translate(fparam.tpe)
      for {
        (typ1, eff1) <- inferExp(exp)
      } yield
        (Typ.Lambda(argumentType, eff1, typ1), ast.Eff.Pure)

    case Expression.Apply(exp1, exp2, _, _, _) =>
      val freshResultType: Typ = Typ.Var()
      val freshResultEff: ast.Eff = null
      for {
        (inferredLambdaType, inferredLambdaEffect) <- inferExp(exp1)
        (inferredArgumentType, inferredArgumentEffect) <- inferExp(exp2)
        expectedLambdaType <- unifyM(inferredLambdaType, Typ.Lambda(inferredArgumentType, freshResultEff, freshResultType))
      }
        yield
          ???

    case Expression.Unary(op, exp, tpe, eff, loc) => ???

    case Expression.Binary(op, exp1, exp2, tpe, eff, loc) => ???

    case Expression.Let(sym, exp1, exp2, tpe, eff, loc) =>
      for {
        (typ1, eff1) <- inferExp(exp1)
        (typ2, eff2) <- inferExp(exp2)
      } yield ???

    case Expression.LetRec(sym, exp1, exp2, tpe, eff, loc) => ???

    case Expression.IfThenElse(exp1, exp2, exp3, _, _, _) => ???

    case Expression.Stm(exp1, exp2, _, _, _) =>
      for {
        (typ1, eff1) <- inferExp(exp1)
        (typ2, eff2) <- inferExp(exp2)
      } yield ???

    case Expression.Match(exp, rules, tpe, eff, loc) => ???

    case Expression.Switch(rules, tpe, eff, loc) => ???

    case Expression.Tag(sym, tag, exp, tpe, eff, loc) => ???

    case Expression.Tuple(elms, tpe, eff, loc) => ???

    case Expression.RecordEmpty(tpe, eff, loc) => ???

    case Expression.RecordSelect(exp, label, tpe, eff, loc) => ???

    case Expression.RecordExtend(label, value, rest, tpe, eff, loc) => ???

    case Expression.RecordRestrict(label, rest, tpe, eff, loc) => ???

    case Expression.ArrayLit(elms, tpe, eff, loc) => ???

    case Expression.ArrayNew(elm, len, tpe, eff, loc) => ???

    case Expression.ArrayLoad(base, index, tpe, eff, loc) => ???

    case Expression.ArrayLength(base, tpe, eff, loc) => ???

    case Expression.ArrayStore(base, index, elm, tpe, eff, loc) => ???

    case Expression.ArraySlice(base, beginIndex, endIndex, tpe, eff, loc) => ???

    case Expression.VectorLit(elms, tpe, eff, loc) => ???

    case Expression.VectorNew(elm, len, tpe, eff, loc) => ???

    case Expression.VectorLoad(base, index, tpe, eff, loc) => ???

    case Expression.VectorStore(base, index, elm, tpe, eff, loc) => ???

    case Expression.VectorLength(base, tpe, eff, loc) => ???

    case Expression.VectorSlice(base, startIndex, endIndex, tpe, eff, loc) => ???

    case Expression.Ref(exp, tpe, eff, loc) => ???

    case Expression.Deref(exp, tpe, eff, loc) => ???

    case Expression.Assign(exp1, exp2, tpe, eff, loc) => ???

    case Expression.HandleWith(exp, bindings, tpe, eff, loc) => ???

    case Expression.Existential(fparam, exp, eff, loc) => ???

    case Expression.Universal(fparam, exp, eff, loc) => ???

    case Expression.Ascribe(exp, tpe, eff, loc) => ???

    case Expression.Cast(exp, tpe, eff, loc) => ???

    case Expression.NativeConstructor(constructor, args, tpe, eff, loc) => ???

    case Expression.TryCatch(exp, rules, tpe, eff, loc) => ???

    case Expression.NativeField(field, tpe, eff, loc) => ???

    case Expression.NativeMethod(method, args, tpe, eff, loc) => ???

    case Expression.NewChannel(exp, _, _, _) =>
      for {
        (typ, eff) <- inferExp(exp)
      } yield (Typ.Channel(typ), eff)

    case Expression.GetChannel(exp, tpe, eff, loc) => ???

    case Expression.PutChannel(exp1, exp2, tpe, eff, loc) => ???

    case Expression.SelectChannel(rules, default, tpe, eff, loc) => ???

    case Expression.ProcessSpawn(exp, tpe, eff, loc) => ???

    case Expression.ProcessSleep(exp, tpe, eff, loc) => ???

    case Expression.ProcessPanic(msg, tpe, eff, loc) => ???

    case Expression.FixpointConstraint(c, tpe, eff, loc) => ???

    case Expression.FixpointCompose(exp1, exp2, tpe, eff, loc) => ???

    case Expression.FixpointSolve(exp, tpe, eff, loc) => ???

    case Expression.FixpointProject(pred, exp, tpe, eff, loc) => ???

    case Expression.FixpointEntails(exp1, exp2, tpe, eff, loc) => ???
  }

  private def reassembleExp(e: Expression): Expression = ???

  private def pure(typ: Typ): InferMonad[(Typ, ast.Eff)] = InferMonad((typ, ast.Eff.Pure))

  private def freshEffVar(): Typ = ???


  private sealed trait Typ

  private object Typ {

    case class Var() extends Typ

    case object Prim extends Typ

    case class Channel(t: Typ) extends Typ

    case class Lambda(t1: Typ, eff: ast.Eff, t2: Typ) extends Typ

  }

  private def translate(value: Type): Typ = value match {
    case Type.Cst(TypeConstructor.Int32) => Typ.Prim
    case _ => ???
  }


  private case class Substitution() {

  }

  private case class InferMonad[T](t: T) {

    def map[B](f: T => B): InferMonad[B] = InferMonad(f(this.t))

    def flatMap[B](f: T => InferMonad[B]): InferMonad[B] = f(this.t)

    def withFilter(f: T => Boolean): InferMonad[T] = this // TODO

  }

}
