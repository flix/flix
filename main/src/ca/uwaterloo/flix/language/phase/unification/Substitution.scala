/*
 *  Copyright 2020 Magnus Madsen
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
package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.language.ast.{Type, TypeConstructor}
import ca.uwaterloo.flix.util.InternalCompilerException

/**
  * Companion object for the [[Substitution]] class.
  */
object Substitution {
  /**
    * Returns the empty substitution.
    */
  val empty: Substitution = Substitution(Map.empty, Set.empty, Set.empty)

  /**
    * Returns the singleton substitution mapping the type variable `x` to `tpe`.
    */
  def singleton(x: Type.Var, tpe: Type): Substitution = {
    // Ensure that we do not add any x -> x mappings.
    tpe match {
      case y: Type.Var if x.id == y.id => empty
      case Type.Cst(TypeConstructor.Pure) => Substitution(Map.empty, Set(x), Set.empty)
      case Type.Cst(TypeConstructor.Impure) => Substitution(Map.empty, Set.empty, Set(x))
      case _ => Substitution(Map(x -> tpe), Set.empty, Set.empty)
    }
  }
}

/**
  * A substitution is a map from type variables to types.
  */
case class Substitution(m: Map[Type.Var, Type], trueVars: Set[Type.Var], falseVars: Set[Type.Var]) {

  /**
    * Returns `true` if `this` is the empty substitution.
    */
  val isEmpty: Boolean = m.isEmpty && trueVars.isEmpty && falseVars.isEmpty

  /**
    * Returns `true` if the domain of `this` substitution contains a mapping for `x`.
    */
  def contains(x: Type.Var): Boolean = this.trueVars.contains(x) || this.falseVars.contains(x) || this.m.contains(x)

  /**
    * Applies `this` substitution to the given type `tpe0`.
    */
  def apply(tpe0: Type): Type = {
    // NB: The order of cases has been determined by code coverage analysis.
    def visit(t: Type): Type =
      t match {
        case x: Type.Var =>
          // Determine if the type variable is `True`.
          if (trueVars.contains(x))
            Type.Pure
          // Determine if the type variable is `False`.
          else if (falseVars.contains(x))
            Type.Impure
          else
          // Lookup the type variable in the map.
            m.getOrElse(x, x)

        case Type.Cst(tc) => t

        case Type.Apply(t1, t2) =>
          val y = visit(t2)
          visit(t1) match {
            // Simplify boolean equations.
            case Type.Cst(TypeConstructor.Not) => BoolUnification.mkNot(y)
            case Type.Apply(Type.Cst(TypeConstructor.And), x) => BoolUnification.mkAnd(x, y)
            case Type.Apply(Type.Cst(TypeConstructor.Or), x) => BoolUnification.mkOr(x, y)
            case x => Type.Apply(x, y)
          }
        case Type.Arrow(l, eff) => Type.Arrow(l, visit(eff))
        case Type.Lambda(tvar, tpe) => throw InternalCompilerException(s"Unexpected type '$tpe0'.")
      }

    // Optimization: Return the type if the substitution is empty. Otherwise visit the type.
    if (isEmpty) tpe0 else visit(tpe0)
  }

  /**
    * Applies `this` substitution to the given types `ts`.
    */
  def apply(ts: List[Type]): List[Type] = if (isEmpty) ts else ts map apply

  /**
    * Returns the left-biased composition of `this` substitution with `that` substitution.
    */
  def ++(that: Substitution): Substitution = {
    if (this.isEmpty) {
      that
    } else if (that.isEmpty) {
      this
    } else {
      Substitution(
        this.m ++ that.m.filter(kv => !this.contains(kv._1)),
        this.trueVars ++ that.trueVars,
        this.falseVars ++ that.falseVars
      )
    }
  }

  /**
    * Returns the composition of `this` substitution with `that` substitution.
    */
  def @@(that: Substitution): Substitution = {
    // Case 1: Return `that` if `this` is empty.
    if (this.isEmpty) {
      return that
    }

    // Case 2: Return `this` if `that` is empty.
    if (that.isEmpty) {
      return this
    }

    // Case 3: Merge the two substitutions.

    // NB: Use of mutability improve performance.
    import scala.collection.mutable

    // We start with all bindings in `this`.
    var newTypeMap = this.m

    // Compute all newly true variables.
    val newTrueVars = mutable.Set.empty[Type.Var]

    // Compute all newly false variables.
    val newFalseVars = mutable.Set.empty[Type.Var]

    // Add all bindings in `that`. (Applying the current substitution).
    for ((x, t) <- that.m) {
      this.apply(t) match {
        case Type.Cst(TypeConstructor.Pure) =>
          newTrueVars.add(x)
        case Type.Cst(TypeConstructor.Impure) =>
          newFalseVars.add(x)
        case tpe =>
          newTypeMap = newTypeMap.updated(x, tpe)
      }
    }

    // Reassemble the substitution.
    // Note: Order of ++ determined by profiling.
    Substitution(newTypeMap, that.trueVars ++ this.trueVars ++ newTrueVars, that.falseVars ++ this.falseVars ++ newFalseVars)
  }

  /**
    * Returns a human-readable representation of the substitution.
    */
  override def toString: String = f"t = ${trueVars.size}%3d, f = ${falseVars.size}%3d, m = ${m.size}%3d, subst = $m"

}
