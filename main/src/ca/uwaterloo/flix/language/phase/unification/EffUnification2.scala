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
import ca.uwaterloo.flix.language.ast.{Ast, Kind, Rigidity, RigidityEnv, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.phase.unification.FastBoolUnification.{ConflictException, Equation, Term, TooComplexException}
import ca.uwaterloo.flix.util.collection.Bimap
import ca.uwaterloo.flix.util.{InternalCompilerException, Result}

import scala.collection.mutable

object EffUnification2 {

  /**
    * Returns the most general unifier of the pairwise unification equations in `l`.
    *
    * @param l    the list of unification equations.
    * @param renv the rigidity environment.
    * @param loc  the source location of the entire equation system, e.g. the entire function body.
    */
  def unifyAll(l: List[(Type, Type, SourceLocation)], renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix): Result[Substitution, UnificationError] = {
    // Compute a bi-directional from type variables to ints.
    implicit val bimap: Bimap[Operand, Int] = mkBidirectionalOperandMap(l)

    // Translate all unification problems from equations on types to equations on terms.
    val equations = l.map {
      case (tpe1, tpe2, loc) => toEquation(tpe1, tpe2, loc)(renv, bimap)
    }

    // Compute the most-general unifier of all the term equations.
    FastBoolUnification.solveAll(equations) match {
      case Result.Ok(subst) => Result.Ok(fromBoolSubst(subst))

      case Result.Err((ex: ConflictException, _, _)) =>
        val tpe1 = fromTerm(ex.x, ex.loc)
        val tpe2 = fromTerm(ex.y, ex.loc)
        Result.Err(UnificationError.MismatchedEffects(tpe1, tpe2))

      case Result.Err((ex: TooComplexException, _, _)) =>
        Result.Err(UnificationError.TooComplex(ex.size, loc))
    }
  }

  /**
    * Returns a bi-directional from type variables to ints computed from the given list of unification equations `l`.
    */
  private def mkBidirectionalOperandMap(l: List[(Type, Type, SourceLocation)])(implicit flix: Flix): Bimap[Operand, Int] = {
    // Find all type variables that occur in anywhere in `l`.
    val allOperands = mutable.Set.empty[Operand]
    for ((t1, t2, _) <- l) {
      allOperands ++= extractOperands(t1)
      allOperands ++= extractOperands(t2)
    }

    // Construct the map from type variables to ints.
    val forward = allOperands.foldLeft(Map.empty[Operand, Int]) {
      case (macc, op) => macc + (op -> flix.genSym.freshId())
    }

    // Construct the reverse map from ints to type variables.
    val backward = forward.map {
      case (k, v) => (v, k)
    }

    Bimap(forward, backward)
  }

  private def extractOperands(t: Type): Set[Operand] = t match {
    case Type.Var(sym, _) => Set(Operand.Var(sym))
    case Type.Cst(TypeConstructor.Effect(sym), _) => Set(Operand.Eff(sym))
    case Type.Apply(tpe1, tpe2, _) => extractOperands(tpe1) ++ extractOperands(tpe2)
    case Type.Alias(_, _, tpe, _) => extractOperands(tpe)
    case Type.AssocType(Ast.AssocTypeConstructor(sym, _), Type.Var(tvarSym, _), _, _) => Set(Operand.Assoc(sym, tvarSym))
    case Type.AssocType(_, _, _, loc) => throw InternalCompilerException("Unexpected unreduced associated type: " + t, loc)
    case Type.Cst(_, _) => Set.empty
  }

  /**
    * Translates the given unification equation on types `p` into a unification equation on terms.
    */
  private def toEquation(p: (Type, Type, SourceLocation))(implicit renv: RigidityEnv, m: Bimap[Operand, Int]): Equation = {
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
  private def toTerm(t: Type)(implicit renv: RigidityEnv, m: Bimap[Operand, Int]): Term = Type.eraseTopAliases(t) match {
    case Type.Pure => Term.True
    case Type.Univ => Term.False

    case t: Type.Var => m.getForward(Operand.Var(t.sym)) match {
      case None => throw InternalCompilerException(s"Unexpected unbound type variable: '$t'.", t.loc)
      case Some(x) => renv.get(t.sym) match {
        case Rigidity.Flexible => Term.Var(x) // A flexible variable is a real variable.
        case Rigidity.Rigid => Term.Cst(x) // A rigid variable is a constant.
      }
    }

    case Type.Cst(TypeConstructor.Effect(sym), _) => m.getForward(Operand.Eff(sym)) match {
      case None => throw InternalCompilerException(s"Unexpected unbound type effect: '$t'.", t.loc)
      case Some(x) => Term.Cst(x)
    }

    case Type.AssocType(Ast.AssocTypeConstructor(sym, _), Type.Var(tvarSym, _), _, _) if renv.isRigid(tvarSym) =>
      m.getForward(Operand.Assoc(sym, tvarSym)) match {
        case None => throw InternalCompilerException(s"Unexpected unbound associcated type: '$t'.", t.loc)
        case Some(x) => Term.Cst(x)
      }

    case Type.Apply(Type.Cst(TypeConstructor.Complement, _), tpe1, _) => Term.mkNot(toTerm(tpe1))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Union, _), tpe1, _), tpe2, _) => Term.mkAnd(toTerm(tpe1), toTerm(tpe2))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Intersection, _), tpe1, _), tpe2, _) => Term.mkOr(toTerm(tpe1), toTerm(tpe2))

    case _ => throw InternalCompilerException(s"Unexpected type: '$t'.", t.loc)
  }

  /**
    * Returns a Boolean substitution as a regular type substitution.
    */
  private def fromBoolSubst(s: FastBoolUnification.BoolSubstitution)(implicit m: Bimap[Operand, Int]): Substitution = {
    val map = s.m.map {
      case (k0, v0) =>
        val k = m.getBackward(k0) match {
          case Some(Operand.Var(sym)) => sym
          case other => throw InternalCompilerException("unexpected missing or non-var mapping: " + other, SourceLocation.Unknown)
        }
        val v = fromTerm(v0, SourceLocation.Unknown)
        (k, v)
    }
    Substitution(map)
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
  private def fromTerm(t: Term, loc: SourceLocation)(implicit m: Bimap[Operand, Int]): Type = t match {
    case Term.True => Type.Pure
    case Term.False => Type.Univ
    case Term.Cst(c) => m.getBackward(c) match {
      case Some(Operand.Assoc(sym, tvar)) => Type.AssocType(Ast.AssocTypeConstructor(sym, loc), Type.Var(tvar, loc), Kind.Eff, loc)
      case Some(Operand.Eff(sym)) => Type.Cst(TypeConstructor.Effect(sym), loc)
      case Some(Operand.Var(sym)) => Type.Var(sym, loc)
      case None => throw InternalCompilerException("unexpected unbound constant: " + c, loc)
    }
    case Term.Var(x) => m.getBackward(x) match {
      case Some(Operand.Var(sym)) => Type.Var(sym, loc)
      case _ => throw InternalCompilerException("unexpected misbound variable: " + x, loc)
    }
    case Term.Not(t) => Type.mkComplement(fromTerm(t, loc), loc)
    case Term.And(csts, vars, rest) =>
      val ts = csts.toList.map(fromTerm(_, loc)) ++ vars.toList.map(fromTerm(_, loc)) ++ rest.map(fromTerm(_, loc))
      Type.mkUnion(ts, loc)
    case Term.Or(ts) => Type.mkIntersection(ts.map(fromTerm(_, loc)), loc)
  }

  private sealed trait Operand

  private object Operand {
    case class Var(sym: Symbol.KindedTypeVarSym) extends Operand

    case class Eff(sym: Symbol.EffectSym) extends Operand

    case class Assoc(sym: Symbol.AssocTypeSym, tvar: Symbol.KindedTypeVarSym) extends Operand
  }

}
