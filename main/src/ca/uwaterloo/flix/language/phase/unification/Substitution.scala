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

import ca.uwaterloo.flix.language.ast.{Ast, Scheme, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.util.InternalCompilerException

/**
  * Companion object for the [[Substitution]] class.
  */
object Substitution {
  /**
    * Returns the empty substitution.
    */
  val empty: Substitution = Substitution(Map.empty)

  /**
    * Returns the singleton substitution mapping the type variable `x` to `tpe`.
    */
  def singleton(x: Type.Var, tpe: Type): Substitution = {
    // Ensure that we do not add any x -> x mappings.
    tpe match {
      case y: Type.Var if x.id == y.id => empty
      case _ => Substitution(Map(x -> tpe))
    }
  }

}

/**
  * A substitution is a map from type variables to types.
  */
case class Substitution(m: Map[Type.Var, Type]) {

  /**
    * Returns `true` if `this` is the empty substitution.
    */
  val isEmpty: Boolean = m.isEmpty

  /**
    * Applies `this` substitution to the given type `tpe0`.
    */
  def apply(tpe0: Type): Type = {
    // NB: The order of cases has been determined by code coverage analysis.
    def visit(t: Type): Type =
      t match {
        case x: Type.Var => m.get(x) match {
          case None => x
          case Some(t0) => t0 match {
            // NB: This small trick is used to propagate variable names.
            case tr: Type.Var => tr.copy(text = x.text)
            case tr => tr
          }
        }
        case Type.Cst(tc, _) => t
        case Type.Apply(t1, t2) =>
          val y = visit(t2)
          visit(t1) match {
            // Simplify boolean equations.
            case Type.Cst(TypeConstructor.Not, _) => BoolUnification.mkNot(y)
            case Type.Apply(Type.Cst(TypeConstructor.And, _), x) => BoolUnification.mkAnd(x, y)
            case Type.Apply(Type.Cst(TypeConstructor.Or, _), x) => BoolUnification.mkOr(x, y)
            case x => Type.Apply(x, y)
          }
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
    * Applies `this` substitution to the given type constraint `tc`.
    */
  def apply(tc: Ast.TypeConstraint): Ast.TypeConstraint = if (isEmpty) tc else tc.copy(arg = apply(tc.arg))

  /**
    * Applies `this` substitution to the given type scheme `sc`.
    *
    * NB: Throws an InternalCompilerException if quantifiers are present in the substitution.
    */
  def apply(sc: Scheme): Scheme = sc match {
    case Scheme(quantifiers, constraints, base) =>
      if (sc.quantifiers.exists(m.contains)) {
        throw InternalCompilerException("Quantifier in substitution.")
      }
      Scheme(quantifiers, constraints.map(apply), apply(base))
  }

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
        this.m ++ that.m.filter(kv => !this.m.contains(kv._1))
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
    val newTypeMap = mutable.Map.empty[Type.Var, Type]

    // Add all bindings in `that`. (Applying the current substitution).
    for ((x, t) <- that.m) {
      newTypeMap.update(x, this.apply(t))
    }

    // Add all bindings in `this` that are not in `that`.
    for ((x, t) <- this.m) {
      if (!that.m.contains(x)) {
        newTypeMap.update(x, t)
      }
    }

    Substitution(newTypeMap.toMap) ++ this
  }

}
