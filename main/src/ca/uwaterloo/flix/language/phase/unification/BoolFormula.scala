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
trait BoolFormula[F] {
  def mkTrue: F

  def mkFalse: F

  def mkAnd(f1: F, f2: F): F

  def mkOr(f1: F, f2: F): F

  def mkNot(f1: F): F

  def mkVar(id: Int): F

  /**
    * Maps the function over the formula.
    * The function is executed for each variable in the formula.
    */
  def map(g: Int => F, f: F): F

  /**
    * Returns an environment built from the given types.
    *
    * This environment is used to convert between formulas and types.
    */
  def getEnv(fs: List[Type]): Bimap[Symbol.KindedTypeVarSym, Int]

  /**
    * Converts the given formula into a type.
    */
  def toType(f: F, env: Bimap[Symbol.KindedTypeVarSym, Int]): Type

  /**
    * Converts the given type into a formula.
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

  /**
    * Returns a rigidity environment on formulas that is equivalent to the given one on types.
    */
  def liftRigidityEnv(renv: RigidityEnv, env: Bimap[Symbol.KindedTypeVarSym, Int]): SortedSet[Int] = {
    renv.s.flatMap(env.getForward)
  }

  /**
    * Returns the set of free variables in the formula.
    */
  def freeVars(f: F): SortedSet[Int]

  /**
    * Returns an equivalent formula of equal or lesser size.
    */
  def minimize(f: F): F
}
