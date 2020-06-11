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
      case Type.Cst(Type.Pure) => Substitution(Map.empty, Set(x), Set.empty)
      case Type.Cst(Type.Impure) => Substitution(Map.empty, Set.empty, Set(x))
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
    * Applies `this` substitution to the given type `tpe0`.
    */
  def apply(tpe0: Type): Type = {
    // NB: The order of cases has been determined by code coverage analysis.
    def visit(t: Type): Type =
      t match {
        case x: Type.Var =>
          if (trueVars.contains(x))
            Type.Pure
          else if (falseVars.contains(x))
            Type.Impure
          else
            m.getOrElse(x, x)
        case Type.Cst(tc) => t
        case Type.Apply(t1, t2) =>
          (visit(t1), visit(t2)) match {
            // Simplify boolean equations.
            case (Type.Cst(TypeConstructor.Not), x) => BoolUnification.mkNot(x)
            case (Type.Apply(Type.Cst(TypeConstructor.And), x), y) => BoolUnification.mkAnd(x, y)
            case (Type.Apply(Type.Cst(TypeConstructor.Or), x), y) => BoolUnification.mkOr(x, y)
            case (x, y) => Type.Apply(x, y)
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
        this.m ++ that.m.filter(kv => !this.m.contains(kv._1)),
        this.trueVars ++ that.trueVars,
        this.falseVars ++ that.falseVars
      )
    }
  }

  /**
    * Returns the composition of `this` substitution with `that` substitution.
    */
  def @@(that: Substitution): Substitution = {
    if (this.isEmpty) {
      that
    } else if (that.isEmpty) {
      this
    } else {
      var newTypeMap = Map.empty[Type.Var, Type]
      var newTrueVars = this.trueVars ++ that.trueVars
      var newFalseVars = this.falseVars ++ that.falseVars

      for ((tvar, tpe) <- that.m) {
        val t = this.apply(tpe)
        t match {
          case Type.Cst(TypeConstructor.Pure) =>
            newTrueVars = newTrueVars + tvar
          case Type.Cst(TypeConstructor.Impure) =>
            newFalseVars = newFalseVars + tvar
          case _ =>
            newTypeMap = newTypeMap.updated(tvar, t)
        }
      }
      Substitution(newTypeMap, newTrueVars, newFalseVars) ++ this
    }
  }

  override def toString: String = s"t = ${trueVars.size}, f = ${falseVars.size}, m = $m"

}
