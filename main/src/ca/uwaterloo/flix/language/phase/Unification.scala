/*
 *  Copyright 2016 Magnus Madsen
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

import ca.uwaterloo.flix.language.ast.Type
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.util.{InternalCompilerException, Result}

object Unification {

  /**
    * A substitution is a map from type variables to types.
    */
  object Substitution {
    /**
      * Returns the empty substitution.
      */
    val empty: Substitution = Substitution(Map.empty)

    /**
      * Returns the singleton substitution mapping `x` to `tpe`.
      */
    def singleton(x: Type.Var, tpe: Type): Substitution = Substitution(Map(x -> tpe))
  }

  case class Substitution(m: Map[Type.Var, Type]) {

    // TODO: Remove in the future.
    val invariant = (m.forall {
      case (tvar, tpe) => m.get(tvar) match {
        case Some(t: Type.Var) => !m.contains(t)
        case _ => true
      }
    })

    if (!invariant)
      throw new RuntimeException

    /**
      * Applies `this` substitution to the given type `tpe`.
      */
    def apply(tpe: Type): Type = tpe match {
      case x: Type.Var =>
        m.get(x) match {
          case None => x
          case Some(y) if x.kind == tpe.kind => y
          case Some(y) if x.kind != tpe.kind => throw InternalCompilerException(s"Expected kind `${x.kind}' but got `${tpe.kind}'.")
        }
      case Type.Unit => Type.Unit
      case Type.Bool => Type.Bool
      case Type.Char => Type.Char
      case Type.Float32 => Type.Float32
      case Type.Float64 => Type.Float64
      case Type.Int8 => Type.Int8
      case Type.Int16 => Type.Int16
      case Type.Int32 => Type.Int32
      case Type.Int64 => Type.Int64
      case Type.BigInt => Type.BigInt
      case Type.Str => Type.Str
      case Type.Native => Type.Native
      case Type.Arrow => Type.Arrow
      case Type.FTuple(l) => Type.FTuple(l)
      case Type.FOpt => Type.FOpt
      case Type.FList => Type.FList
      case Type.FVec => Type.FVec
      case Type.FSet => Type.FSet
      case Type.FMap => Type.FMap
      case Type.Enum(name, cases) => Type.Enum(name, cases.foldLeft(Map.empty[String, Type]) {
        case (macc, (tag, t)) => macc + (tag -> apply(t))
      })
      case Type.Apply(t1, t2) => Type.Apply(apply(t1), apply(t2))
      case Type.Tuple(elms) => Type.Tuple(elms map apply)
      case Type.Lambda(args, retTpe) => Type.Lambda(args map apply, apply(retTpe))
    }

    /**
      * Applies `this` substitution to the given types `ts`.
      */
    def apply(ts: List[Type]): List[Type] = ts.map(t => apply(t))

    /**
      * Returns the left-biased  composition of `this` substitution with `that` substitution.
      */
    def ++(that: Substitution): Substitution = {
      Substitution(this.m ++ that.m.filter(kv => !this.m.contains(kv._1)))
    }

    /**
      * Returns the composition of `this` substitution with `that` substitution.
      */
    def @@(that: Substitution): Substitution = {
      val m = that.m.map {
        case (x, t) => x -> this.apply(t)
      }
      Substitution(m) ++ this
    }

  }

  /**
    * Returns the most general unifier of the two given types `tpe1` and `tpe2`.
    *
    * Returns [[Failure]] if the two types cannot be unified.
    */
  def unify(tpe1: Type, tpe2: Type): Result[Substitution, TypeError] = (tpe1, tpe2) match {
    case (x: Type.Var, _) => unifyVar(x, tpe2)
    case (_, x: Type.Var) => unifyVar(x, tpe1)
    case (Type.Unit, Type.Unit) => Result.Ok(Substitution.empty)
    case (Type.Bool, Type.Bool) => Result.Ok(Substitution.empty)
    case (Type.Char, Type.Char) => Result.Ok(Substitution.empty)
    case (Type.Float32, Type.Float32) => Result.Ok(Substitution.empty)
    case (Type.Float64, Type.Float64) => Result.Ok(Substitution.empty)
    case (Type.Int8, Type.Int8) => Result.Ok(Substitution.empty)
    case (Type.Int16, Type.Int16) => Result.Ok(Substitution.empty)
    case (Type.Int32, Type.Int32) => Result.Ok(Substitution.empty)
    case (Type.Int64, Type.Int64) => Result.Ok(Substitution.empty)
    case (Type.BigInt, Type.BigInt) => Result.Ok(Substitution.empty)
    case (Type.Str, Type.Str) => Result.Ok(Substitution.empty)
    case (Type.Native, Type.Native) => Result.Ok(Substitution.empty)
    case (Type.Arrow, Type.Arrow) => Result.Ok(Substitution.empty)
    case (Type.FTuple(l1), Type.FTuple(l2)) if l1 == l2 => Result.Ok(Substitution.empty)
    case (Type.FOpt, Type.FOpt) => Result.Ok(Substitution.empty)
    case (Type.FList, Type.FList) => Result.Ok(Substitution.empty)
    case (Type.FVec, Type.FVec) => Result.Ok(Substitution.empty)
    case (Type.FSet, Type.FSet) => Result.Ok(Substitution.empty)
    case (Type.FMap, Type.FMap) => Result.Ok(Substitution.empty)
    case (Type.Enum(name1, cases1), Type.Enum(name2, cases2)) if name1 == name2 =>
      val ts1 = cases1.values.toList
      val ts2 = cases2.values.toList
      unify(ts1, ts2)

    case (Type.Apply(t11, t12), Type.Apply(t21, t22)) =>
      unify(t11, t21) match {
        case Result.Ok(subst1) => unify(subst1(t12), subst1(t22)) match {
          case Result.Ok(subst2) => Result.Ok(subst2 @@ subst1)
          case Result.Err(e) => Result.Err(e)
        }
        case Result.Err(e) => Result.Err(e)
      }

    case (Type.Tuple(elms1), Type.Tuple(elms2)) =>
      unify(elms1, elms2)

    case (Type.Lambda(arguments1, returnType1), Type.Lambda(arguments2, returnType2)) =>
      unify(arguments1, arguments2) match {
        case Result.Ok(subst1) => unify(subst1(returnType1), subst1(returnType2)) match {
          case Result.Ok(subst2) => Result.Ok(subst2 @@ subst1)
          case Result.Err(e) => Result.Err(e)
        }
        case Result.Err(e) => Result.Err(e)
      }
    case _ => Result.Err(TypeError.UnificationError(tpe1, tpe2))
  }

  /**
    * Unifies the two given lists of types `ts1` and `ts2`.
    */
  def unify(ts1: List[Type], ts2: List[Type]): Result[Substitution, TypeError] = (ts1, ts2) match {
    case (Nil, Nil) => Result.Ok(Substitution.empty)
    case (tpe1 :: rs1, tpe2 :: rs2) => unify(tpe1, tpe2) match {
      case Result.Ok(subst1) => unify(subst1(rs1), subst1(rs2)) match {
        case Result.Ok(subst2) => Result.Ok(subst2 @@ subst1)
        case Result.Err(e) => Result.Err(e)
      }
      case Result.Err(e) => Result.Err(e)
    }
    case _ => throw InternalCompilerException(s"Mismatched type lists: `$ts1' and `$ts2'.")
  }

  /**
    * Unifies the given variable `x` with the given type `tpe`.
    *
    * Performs the so-called occurs-check to ensure that the substitution is kind-preserving.
    */
  def unifyVar(x: Type.Var, tpe: Type): Result[Substitution, TypeError] = {
    if (x == tpe) {
      return Result.Ok(Substitution.empty)
    }
    if (tpe.typeVars contains x) {
      return Result.Err(TypeError.OccursCheck())
    }
    if (x.kind != tpe.kind) {
      return Result.Err(TypeError.KindError())
    }
    Result.Ok(Substitution.singleton(x, tpe))
  }


  /**
    * TODO: DOC
    */
  trait InferMonad[A] {

    def get: A = this match {
      case Success(a, s) => a
      case Failure(e) => throw new RuntimeException()
    }

    def isSuccess: Boolean = this match {
      case x: Success[A] => true
      case x: Failure[A] => false
    }

    def map[B](f: A => B): InferMonad[B]

    def flatMap[B](f: A => InferMonad[B]): InferMonad[B]
  }

  /**
    * TODO: DOC
    */
  case class Success[A](a: A, s: Substitution) extends InferMonad[A] {
    /**
      * TODO: DOC
      */
    def map[B](f: A => B): InferMonad[B] = Success(f(a), s)

    /**
      * TODO: DOC
      */
    def flatMap[B](f: A => InferMonad[B]): InferMonad[B] = f(a) match {
      case Success(a1, s1) => Success(a1, s1 @@ s)
      case Failure(e) => Failure(e)
    }
  }

  case class Failure[A](e: TypeError) extends InferMonad[A] {

    def map[B](f: (A) => B): InferMonad[B] = Failure(e)

    def flatMap[B](f: (A) => InferMonad[B]): InferMonad[B] = Failure(e)

  }

  /**
    * TODO: DOC
    */
  def liftM[A](a: A): InferMonad[A] = Success(a, Substitution.empty)

  /**
    * TODO: DOC
    */
  def liftM[A](a: A, s: Substitution): InferMonad[A] = Success(a, s)

  /**
    * TODO: DOC
    */
  def failM[A](e: TypeError): InferMonad[A] = Failure(e)

  /**
    * TODO: DOC
    */
  def sequenceM[A](xs: List[InferMonad[A]]): InferMonad[List[A]] = xs match {
    case Nil => liftM(Nil)
    case y :: ys => y flatMap {
      case r => sequenceM(ys) map {
        case rs => r :: rs
      }
    }
  }

  /**
    * TODO: DOC
    */
  def unifyM(tpe1: Type, tpe2: Type): InferMonad[Type] = unify(tpe1, tpe2) match {
    case Result.Ok(subst) => Success(subst(tpe1), subst)
    case Result.Err(e) => Failure(e)
  }

  /**
    * TODO: DOC
    */
  def unifyM(tpe1: Type, tpe2: Type, tpe3: Type): InferMonad[Type] = {
    for (
      tpe <- unifyM(tpe1, tpe2);
      res <- unifyM(tpe, tpe3)
    ) yield res
  }

  /**
    * TODO: DOC
    */
  def unifyM(tpe1: Type, tpe2: Type, tpe3: Type, tpe4: Type): InferMonad[Type] =
  for (
    tpe <- unifyM(tpe1, tpe2, tpe3);
    res <- unifyM(tpe, tpe4)
  ) yield res

  // TODO
  def unifyM(ts: List[Type]): InferMonad[Type] = {
    def visit(tpe0: Type, xs: List[Type]): InferMonad[Type] = xs match {
      case Nil => liftM(tpe0)
      case tpe :: ys => for (
        intermediate <- unifyM(tpe0, tpe);
        resultType <- visit(intermediate, ys)
      ) yield resultType
    }
    visit(ts.head, ts.tail)
  }

  def unifyM(xs: List[Type], ys: List[Type]): InferMonad[List[Type]] =
    sequenceM((xs zip ys).map {
      case (x, y) => unifyM(x, y)
    })


}
