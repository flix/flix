/*
 * Copyright 2022 Matthew Lutze
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
package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.language.ast.{RigidityEnv, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.util.InternalCompilerException
import ca.uwaterloo.flix.util.collection.Bimap

import scala.collection.immutable.SortedSet

/**
  * A type class for Boolean Formulas.
  */
trait BoolAlg[F] {

  /**
    * Returns `true` if `f` represents TRUE.
    */
  def isTrue(f: F): Boolean

  /**
    * Returns `true` if `f` represents FALSE.
    */
  def isFalse(f: F): Boolean

  /**
    * Returns `true` if `f` represents a variable.
    */
  def isVar(f: F): Boolean

  /**
    * Returns a representation of TRUE.
    */
  def mkTrue: F

  /**
    * Returns a representation of FALSE.
    */
  def mkFalse: F

  /**
    * Returns a representation of the variable with the given `id`.
    */
  def mkVar(id: Int): F

  /**
    * Returns a representation of the complement of `f`.
    */
  def mkNot(f: F): F

  /**
    * Returns a representation of the disjunction of `f1` and `f2`.
    */
  def mkOr(f1: F, f2: F): F

  /**
    * Returns a representation of the conjunction of `f1` and `f2`.
    */
  def mkAnd(f1: F, f2: F): F

  /**
    * Returns a representation of the formula `f1 xor f2`.
    */
  def mkXor(f1: F, f2: F): F = mkOr(mkAnd(f1, mkNot(f2)), mkAnd(mkNot(f1), f2))

  /**
    * Returns the set of free variables in `f`.
    */
  def freeVars(f: F): SortedSet[Int]

  /**
    * Applies the function `fn` to every variable in `f`.
    */
  def map(f: F)(fn: Int => F): F

  /**
    * Returns `true` if formula is satisfiable and `false` otherwise.
    */
  def satisfiable(f: F): Boolean

  /**
    * Returns a representation equivalent to `f` (but potentially smaller).
    */
  def minimize(f: F): F

  /**
    * Returns an environment built from the given types mapping between type variables and formula variables.
    *
    * This environment should be used in the functions [[toType]] and [[fromType]].
    */
  def getEnv(fs: List[Type]): Bimap[Symbol.KindedTypeVarSym, Int]

  /**
    * Returns a rigidity environment on formulas that is equivalent to the given one on types.
    */
  def liftRigidityEnv(renv: RigidityEnv, env: Bimap[Symbol.KindedTypeVarSym, Int]): SortedSet[Int] = {
    renv.s.flatMap(env.getForward)
  }

  /**
    * Converts the given formula f into a type.
    */
  def toType(f: F, env: Bimap[Symbol.KindedTypeVarSym, Int]): Type

  /**
    * Converts the given type t into a formula.
    */
  def fromType(t: Type, env: Bimap[Symbol.KindedTypeVarSym, Int]): F = Type.eraseTopAliases(t) match {
    case Type.Var(sym, _) => env.getForward(sym) match {
      case None => throw InternalCompilerException(s"Unexpected unbound variable: '$sym'.")
      case Some(x) => mkVar(x)
    }
    case Type.True => mkTrue
    case Type.False => mkFalse
    case Type.Apply(Type.Cst(TypeConstructor.Not, _), tpe1, _) => mkNot(fromType(tpe1, env))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.And, _), tpe1, _), tpe2, _) => mkAnd(fromType(tpe1, env), fromType(tpe2, env))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Or, _), tpe1, _), tpe2, _) => mkOr(fromType(tpe1, env), fromType(tpe2, env))
    case _ => throw InternalCompilerException(s"Unexpected type: '$t'.")
  }

}
