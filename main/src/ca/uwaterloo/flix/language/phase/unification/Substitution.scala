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

import ca.uwaterloo.flix.language.ast.{Ast, Kind, Scheme, SourceLocation, Symbol, Type, TypeConstructor}
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
  def singleton(x: Symbol.KindedTypeVarSym, tpe: Type): Substitution = {
    // Ensure that we do not add any x -> x mappings.
    tpe match {
      case y: Type.Var if x.id == y.sym.id => empty
      case y: Type.Var if y.sym.text isStrictlyLessPreciseThan x.text => Substitution(Map(x -> y.withText(x.text)))
      case y: Type.Var if x.text isStrictlyLessPreciseThan y.sym.text => Substitution(Map(x.withText(y.sym.text) -> y))
      case _ => Substitution(Map(x -> tpe))
    }
  }

}

/**
  * A substitution is a map from type variables to types.
  */
case class Substitution(m: Map[Symbol.KindedTypeVarSym, Type]) {

  /**
    * Returns `true` if `this` is the empty substitution.
    */
  val isEmpty: Boolean = m.isEmpty

  /**
    * Applies `this` substitution to the given type `tpe0`.
    */
  def apply(tpe0: Type): Type = {
    // NB: The order of cases has been determined by code coverage analysis.
    // insideCaseSet means that we are inside a case set formula and should not minimize yet
    def visit(insideCaseSet: Boolean, t: Type): Type =
      t match {
        case x: Type.Var => m.getOrElse(x.sym, x)
        case Type.Cst(tc, _) => t
        case Type.Apply(t1, t2, loc) =>
          // only trigger minimization once for each formula
          val y =
            if (!insideCaseSet && isCaseSet(t2)) minimizeSetType(visit(insideCaseSet = true, t2))
            else visit(insideCaseSet, t2)
          val z =
            if (!insideCaseSet && isCaseSet(t1)) minimizeSetType(visit(insideCaseSet = true, t1))
            else visit(insideCaseSet, t1)
          z match {
            // Simplify boolean equations.
            case Type.Cst(TypeConstructor.Not, _) => Type.mkNot(y, loc)
            case Type.Apply(Type.Cst(TypeConstructor.And, _), x, _) => Type.mkAnd(x, y, loc)
            case Type.Apply(Type.Cst(TypeConstructor.Or, _), x, _) => Type.mkOr(x, y, loc)

            // Simplify set expressions
            case Type.Cst(TypeConstructor.Complement, _) => SetUnification.mkComplement(y)
            case Type.Apply(Type.Cst(TypeConstructor.Intersection, _), x, _) => SetUnification.mkIntersection(x, y)
            case Type.Apply(Type.Cst(TypeConstructor.Union, _), x, _) => SetUnification.mkUnion(x, y)

            case Type.Cst(TypeConstructor.CaseComplement(sym), _) => Type.mkCaseComplement(y, sym, loc)
            case Type.Apply(Type.Cst(TypeConstructor.CaseIntersection(sym), _), x, _) => Type.mkCaseIntersection(x, y, sym, loc)
            case Type.Apply(Type.Cst(TypeConstructor.CaseUnion(sym), _), x, _) => Type.mkCaseUnion(x, y, sym, loc)

            // Else just apply
            case x => Type.Apply(x, y, loc)
          }
        case Type.Alias(sym, args0, tpe0, loc) =>
          val args = args0.map(visit(insideCaseSet, _))
          val tpe = visit(insideCaseSet, tpe0)
          Type.Alias(sym, args, tpe, loc)
      }

    // Optimization: Return the type if the substitution is empty. Otherwise visit the type.
    if (isEmpty) tpe0
    else minimizeSetType(visit(isCaseSet(tpe0), tpe0))
  }

  /**
    * Returns a minimized type if the type is of kind `CaseSet`, otherwise
    * do nothing.
    */
  private def minimizeSetType(tpe: Type): Type = tpe.kind match {
    case Kind.CaseSet(enumSym) =>
      // TODO RESTR-VARS: undo this universe hack
      val univ = List(
        new Symbol.RestrictableCaseSym(enumSym, "Cst", SourceLocation.Unknown),
        new Symbol.RestrictableCaseSym(enumSym, "Var", SourceLocation.Unknown),
        new Symbol.RestrictableCaseSym(enumSym, "Not", SourceLocation.Unknown),
        new Symbol.RestrictableCaseSym(enumSym, "And", SourceLocation.Unknown),
        new Symbol.RestrictableCaseSym(enumSym, "Or", SourceLocation.Unknown),
        new Symbol.RestrictableCaseSym(enumSym, "Xor", SourceLocation.Unknown)
      )
      val (env, univ2) = SetFormula.mkEnv(List(tpe), univ)
      val something = SetFormula.fromCaseType(tpe, env, univ2)
      SetFormula.toCaseType(SetFormula.minimize(something)(univ2), enumSym, env, SourceLocation.Unknown)
    case _ => tpe
  }

  private def isCaseSet(tpe: Type): Boolean = tpe.kind match {
    case Kind.CaseSet(_) => true
    case _ => false
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
        throw InternalCompilerException("Quantifier in substitution.", base.loc)
      }
      Scheme(quantifiers, constraints.map(apply), apply(base))
  }

  /**
    * Removes the binding for the given type variable `tvar` (if it exists).
    */
  def unbind(tvar: Symbol.KindedTypeVarSym): Substitution = Substitution(m - tvar)

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
    val newTypeMap = mutable.Map.empty[Symbol.KindedTypeVarSym, Type]

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
