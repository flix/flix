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
import ca.uwaterloo.flix.language.phase.unification.FastBoolUnification.{Equation, Term, solveAll}
import ca.uwaterloo.flix.util.{InternalCompilerException, Result}
import ca.uwaterloo.flix.util.collection.Bimap

import scala.collection.mutable

object EffUnification2 {

  /**
    * Returns the most general unifier of the pairwise unification equations in `l`.
    */
  def unifyAll(l: List[(Type, Type)], renv0: RigidityEnv)(implicit flix: Flix): Result[Substitution, UnificationError] = {
    // Compute a bi-directional from type variables to ints.
    implicit val bimap: Bimap[Type.Var, Int] = mkBidirectionalVarMap(l)

    // Translate all unification problems from equations on types to equations on terms.
    val equations = l.map(mkEquation(_, renv0))

    // Compute the most-general unifier of all the term equations.
    solveAll(equations) match {
      case Result.Ok(subst) => Result.Ok(toTypeSubst(subst))

      case Result.Err(ex) =>
        val tpe1 = toType(ex.x, SourceLocation.Unknown) // TODO: Add more precise source locations.
        val tpe2 = toType(ex.y, SourceLocation.Unknown) // TODO: Add more precise source locations.
        Result.Err(UnificationError.MismatchedEffects(tpe1, tpe2))
    }
  }

  /**
    * Returns a bi-directional from type variables to ints computed from the given list of unification equations `l`.
    */
  private def mkBidirectionalVarMap(l: List[(Type, Type)]): Bimap[Type.Var, Int] = {
    // Find all type variables that occur in anywhere in `l`.
    val allVars = mutable.Set.empty[Type.Var]
    for ((t1, t2) <- l) {
      allVars ++= t1.typeVars
      allVars ++= t2.typeVars
    }

    // Construct the map from type variables to ints.
    val forward = allVars.foldLeft(Map.empty[Type.Var, Int]) {
      case (macc, tvar) => macc + (tvar -> tvar.sym.id)
    }

    // Construct the reverse map from ints to type variables.
    val backward = allVars.foldLeft(Map.empty[Int, Type.Var]) {
      case (macc, tvar) => macc + (tvar.sym.id -> tvar)
    }
    Bimap(forward, backward)
  }

  /**
    * Translates the given unification equation on types `p` into a unification equation on terms.
    */
  private def mkEquation(p: (Type, Type), renv0: RigidityEnv)(implicit bimap: Bimap[Type.Var, Int]): Equation = {
    val (tpe1, tpe2) = p
    Equation.mk(fromType(tpe1, bimap, renv0), fromType(tpe2, bimap, renv0))
  }

  private def toTypeSubst(s: FastBoolUnification.BoolSubstitution)(implicit bimap: Bimap[Type.Var, Int]): Substitution = {
    Substitution(s.m.foldLeft(Map.empty[Symbol.KindedTypeVarSym, Type]) {
      case (macc, (k, v)) => macc + (bimap.getBackward(k).get.sym -> toType(v, SourceLocation.Unknown))
    })
  }

  private def toType(t0: Term, loc: SourceLocation)(implicit m: Bimap[Type.Var, Int]): Type = t0 match {
    case Term.True => Type.Pure
    case Term.False => Type.Univ
    case Term.Cst(c) => m.getBackward(c).get
    case Term.Var(x) => m.getBackward(x).get
    case Term.Not(t) => Type.mkComplement(toType(t, loc), loc)
    case Term.And(csts, vars, rest) =>
      val ts = csts.toList.map(toType(_, loc)) ++ vars.toList.map(toType(_, loc)) ++ rest.map(toType(_, loc))
      Type.mkIntersection(ts, loc)
    case Term.Or(ts) => Type.mkUnion(ts.map(toType(_, loc)), loc)
  }

  private def fromType(t: Type, env: Bimap[Type.Var, Int], renv: RigidityEnv): Term = Type.eraseTopAliases(t) match {
    case t: Type.Var => env.getForward(t) match {
      case None => throw InternalCompilerException(s"Unexpected unbound variable: '$t'.", t.loc)
      case Some(x) => renv.get(t.sym) match {
        case Rigidity.Flexible => Term.Var(x)
        case Rigidity.Rigid => Term.Cst(x)
      }
    }
    case Type.Cst(TypeConstructor.Effect(sym), _) => throw InternalCompilerException("Not yet", t.loc) // TODO
    case Type.Pure => Term.True
    case Type.Univ => Term.False
    case Type.Apply(Type.Cst(TypeConstructor.Complement, _), tpe1, _) => Term.mkNot(fromType(tpe1, env, renv))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Union, _), tpe1, _), tpe2, _) => Term.mkAnd(fromType(tpe1, env, renv), fromType(tpe2, env, renv))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Intersection, _), tpe1, _), tpe2, _) => Term.mkOr(fromType(tpe1, env, renv), fromType(tpe2, env, renv))
    case Type.Cst(TypeConstructor.Error(_), _) => Term.True
    case _ => throw InternalCompilerException(s"Unexpected type: '$t'.", t.loc)
  }


}
