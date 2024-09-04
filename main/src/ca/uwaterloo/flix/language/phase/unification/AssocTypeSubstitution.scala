/*
 * Copyright 2023 Matthew Lutze
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

import ca.uwaterloo.flix.language.ast.{Ast, Scheme, Symbol, Type, TypeConstructor}

/**
  * Companion object for the [[AssocTypeSubstitution]] class.
  */
object AssocTypeSubstitution {
  /**
    * Returns the empty substitution.
    */
  val empty: AssocTypeSubstitution = AssocTypeSubstitution(Map.empty)

  /**
    * Returns the singleton substitution mapping the type variable `x` to `tpe`.
    */
  def singleton(assoc: Symbol.AssocTypeSym, tvar: Symbol.KindedTypeVarSym, tpe: Type): AssocTypeSubstitution = {
    tpe match {
      // avoid x -> x mappings
      case Type.AssocType(Ast.AssocTypeConstructor(assoc1, _), Type.Var(tvar1, _), _, _) if assoc == assoc1 && tvar == tvar1 => AssocTypeSubstitution.empty
      case _ => AssocTypeSubstitution(Map((assoc, tvar) -> tpe))
    }
  }

}

/**
  * A substitution is a map from type variables to types.
  */
case class AssocTypeSubstitution(m: Map[(Symbol.AssocTypeSym, Symbol.KindedTypeVarSym), Type]) {

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
        case x: Type.Var => x
        case Type.Cst(_, _) => t
        case Type.Apply(t1, t2, loc) =>
          val y = visit(t2)
          visit(t1) match {
            // Simplify boolean equations.
            case Type.Cst(TypeConstructor.Complement, _) => Type.mkComplement(y, loc)
            case Type.Apply(Type.Cst(TypeConstructor.Union, _), x, _) => Type.mkUnion(x, y, loc)
            case Type.Apply(Type.Cst(TypeConstructor.Intersection, _), x, _) => Type.mkIntersection(x, y, loc)

            // Simplify set expressions
            case Type.Cst(TypeConstructor.CaseComplement(sym), _) => Type.mkCaseComplement(y, sym, loc)
            case Type.Apply(Type.Cst(TypeConstructor.CaseIntersection(sym), _), x, _) => Type.mkCaseIntersection(x, y, sym, loc)
            case Type.Apply(Type.Cst(TypeConstructor.CaseUnion(sym), _), x, _) => Type.mkCaseUnion(x, y, sym, loc)

            // Else just apply
            case x => Type.Apply(x, y, loc)
          }
        case Type.Alias(sym, args0, tpe0, loc) =>
          val args = args0.map(visit)
          val tpe = visit(tpe0)
          Type.Alias(sym, args, tpe, loc)
        case Type.AssocType(Ast.AssocTypeConstructor(assoc, _), Type.Var(tvar, _), _, _) =>
          m.getOrElse((assoc, tvar), t)
        case Type.AssocType(cst, args0, kind, loc) =>
          val args = args0.map(visit)
          Type.AssocType(cst, args, kind, loc)
        case Type.JvmToType(tpe0, loc) =>
          val tpe = visit(tpe0)
          Type.JvmToType(tpe, loc)
        case Type.UnresolvedJvmType(template0, loc) =>
          val template = template0.map(visit)
          Type.UnresolvedJvmType(template, loc)

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
  def apply(tc: Ast.TraitConstraint): Ast.TraitConstraint = if (isEmpty) tc else tc.copy(arg = apply(tc.arg))

  /**
    * Applies `this` substitution to the given type scheme `sc`.
    *
    * NB: Throws an InternalCompilerException if quantifiers are present in the substitution.
    */
  def apply(sc: Scheme): Scheme = sc match {
    case Scheme(quantifiers, tconstrs, econstrs, base) =>
      Scheme(quantifiers, tconstrs.map(apply), econstrs.map(apply), apply(base))
  }

  /**
    * Applies `this` substitution to the given pair of types `ts`.
    */
  def apply(ec: Ast.EqualityConstraint): Ast.EqualityConstraint = if (isEmpty) ec else ec match {
    case Ast.EqualityConstraint(cst, t1, t2, loc) => Ast.EqualityConstraint(cst, apply(t1), apply(t2), loc)
  }

  /**
    * Applies `this` substitution to the given pair of types `ts`.
    */
  def apply(ec: Ast.BroadEqualityConstraint): Ast.BroadEqualityConstraint = if (isEmpty) ec else ec match {
    case Ast.BroadEqualityConstraint(t1, t2) => Ast.BroadEqualityConstraint(apply(t1), apply(t2))
  }

  /**
    * Returns the left-biased composition of `this` substitution with `that` substitution.
    */
  def ++(that: AssocTypeSubstitution): AssocTypeSubstitution = {
    if (this.isEmpty) {
      that
    } else if (that.isEmpty) {
      this
    } else {
      AssocTypeSubstitution(
        this.m ++ that.m.filter(kv => !this.m.contains(kv._1))
      )
    }
  }

  /**
    * Returns the composition of `this` substitution with `that` substitution.
    */
  def @@(that: AssocTypeSubstitution): AssocTypeSubstitution = {
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
    val newTypeMap = mutable.Map.empty[(Symbol.AssocTypeSym, Symbol.KindedTypeVarSym), Type]

    // Add all bindings in `that`. (Applying the current substitution).
    for ((x, t) <- that.m) {
      // minimize case set formulas if present
      val tpe = this.apply(t)
      newTypeMap.update(x, tpe)
    }

    // Add all bindings in `this` that are not in `that`.
    for ((x, t) <- this.m) {
      if (!that.m.contains(x)) {
        newTypeMap.update(x, t)
      }
    }

    AssocTypeSubstitution(newTypeMap.toMap)
  }
}
