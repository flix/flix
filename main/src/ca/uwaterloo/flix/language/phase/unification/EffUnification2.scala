/*
 *  Copyright 2024 Magnus Madsen
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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{Rigidity, RigidityEnv, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.phase.unification.FastBoolUnification.{ConflictException, Equation, Term, TooComplexException}
import ca.uwaterloo.flix.language.phase.unification.UnificationError.TooComplex
import ca.uwaterloo.flix.util.{InternalCompilerException, Result}
import ca.uwaterloo.flix.util.collection.Bimap

import scala.collection.mutable

object EffUnification2 {

  /**
    * Returns the most general unifier of the pairwise unification equations in `l`.
   *
   * @param l the list of unification equations.
   * @param renv the rigidity environment.
   * @param loc the source location of the entire equation system, e.g. the entire function body.
    */
  def unifyAll(l: List[(Type, Type, SourceLocation)], renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix): Result[Substitution, UnificationError] = {
    // Compute a bi-directional map from type variables to ints.
    implicit val bimap: Bimap[Symbol.KindedTypeVarSym, Int] = mkBidirectionalVarMap(l)

    // Translate all unification problems from equations on types to equations on terms.
    val equations = l.map {
      case (tpe1, tpe2, loc) => toEquation(tpe1, tpe2, loc)(renv, bimap)
    }

    // Compute the most-general unifier of all the equations.
    FastBoolUnification.solveAll(equations) match {
      case Result.Ok(subst) => Result.Ok(fromBoolSubst(subst))

      case Result.Err((ex: ConflictException, _, _)) =>
        val tpe1 = fromTerm(ex.x, ex.loc)
        val tpe2 = fromTerm(ex.y, ex.loc)
        Result.Err(UnificationError.MismatchedEffects(tpe1, tpe2))

      case Result.Err((ex: TooComplexException, _, _)) =>
        Result.Err(UnificationError.TooComplex(ex.msg, loc))
    }
  }

  /**
    * Returns a bi-directional map from type variables to ints computed from the given list of unification equations `l`.
    */
  private def mkBidirectionalVarMap(l: List[(Type, Type, SourceLocation)]): Bimap[Symbol.KindedTypeVarSym, Int] = {
    // Find all type variables that occur in anywhere in `l`.
    val allVars = mutable.Set.empty[Symbol.KindedTypeVarSym]
    for ((t1, t2, _) <- l) {
      allVars ++= t1.typeVars
      allVars ++= t2.typeVars
    }

    // Construct the forward map from type variables to ints.
    val forward = allVars.foldLeft(Map.empty[Symbol.KindedTypeVarSym, Int]) {
      case (macc, tvar) => macc + (tvar -> tvar.id)
    }

    // Construct the reverse map from ints to type variables.
    val backward = allVars.foldLeft(Map.empty[Int, Symbol.KindedTypeVarSym]) {
      case (macc, tvar) => macc + (tvar.id -> tvar)
    }
    Bimap(forward, backward)
  }

  /**
    * Translates the given unification equation on types `p` into a unification equation on terms.
    */
  private def toEquation(p: (Type, Type, SourceLocation))(implicit renv: RigidityEnv, m: Bimap[Symbol.KindedTypeVarSym, Int]): Equation = {
    val (tpe1, tpe2, loc) = p
    Equation.mk(toTerm(tpe1), toTerm(tpe2), loc)
  }

  /**
    * Returns the given type `t` as term.
    *
    * Uses the given bimap `m` to map type variables to term variables.
    *
    * The rigidity environment `renv` is used to map rigid type variables to constants and flexible type variables to term variables.
    */
  private def toTerm(t: Type)(implicit renv: RigidityEnv, m: Bimap[Symbol.KindedTypeVarSym, Int]): Term = Type.eraseTopAliases(t) match {
    case Type.Pure => Term.True
    case Type.Univ => Term.False

    case Type.Var(sym, _) => m.getForward(sym) match {
      case None => throw InternalCompilerException(s"Unexpected unbound type variable: '$t'.", t.loc)
      case Some(x) => renv.get(sym) match {
        case Rigidity.Flexible => Term.Var(x) // A flexible variable is a real variable.
        case Rigidity.Rigid => Term.Cst(x) // A rigid variable is a constant.
      }
    }

    case Type.Apply(Type.Cst(TypeConstructor.Complement, _), tpe1, _) => Term.mkNot(toTerm(tpe1))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Union, _), tpe1, _), tpe2, _) => Term.mkAnd(toTerm(tpe1), toTerm(tpe2))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Intersection, _), tpe1, _), tpe2, _) => Term.mkOr(toTerm(tpe1), toTerm(tpe2))

    case _ => throw InternalCompilerException(s"Unexpected type: '$t'.", t.loc)
  }

  /**
    * Returns a regular type substitution obtained from the given Boolean substitution `s`.
    */
  private def fromBoolSubst(s: FastBoolUnification.BoolSubstitution)(implicit m: Bimap[Symbol.KindedTypeVarSym, Int]): Substitution = {
    Substitution(s.m.foldLeft(Map.empty[Symbol.KindedTypeVarSym, Type]) {
      case (macc, (k, v)) =>
        val tvar = m.getBackward(k).get
        macc + (tvar -> fromTerm(v, tvar.loc))
    })
  }

  /**
    * Returns the given term `t` as a type.
    *
    * Uses the given bimap `m` to map term variables to type variables.
    * Uses the given source location `loc` as the source location for all sub-terms.
    *
    * Both constants and variables are mapped back to type variables. The rigidity environment, in the type world,
    * distinguishes their rigidity or flexibility.
    */
  private def fromTerm(t: Term, loc: SourceLocation)(implicit m: Bimap[Symbol.KindedTypeVarSym, Int]): Type = t match {
    case Term.True => Type.Pure
    case Term.False => Type.Univ
    case Term.Cst(c) => Type.Var(m.getBackward(c).get, loc) // Safe: We never introduce new variables.
    case Term.Var(x) => Type.Var(m.getBackward(x).get, loc) // Safe: We never introduce new variables.
    case Term.Not(t) => Type.mkComplement(fromTerm(t, loc), loc)
    case Term.And(csts, vars, rest) =>
      val ts = csts.toList.map(fromTerm(_, loc)) ++ vars.toList.map(fromTerm(_, loc)) ++ rest.map(fromTerm(_, loc))
      Type.mkUnion(ts, loc)
    case Term.Or(ts) => Type.mkIntersection(ts.map(fromTerm(_, loc)), loc)
  }

}
