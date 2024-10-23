/*
 *  Copyright 2024 Jonathan Lindegaard Starup
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

import ca.uwaterloo.flix.language.ast.Ast.AssocTypeConstructor
import ca.uwaterloo.flix.language.ast.shared.Scope
import ca.uwaterloo.flix.language.ast.{Kind, RigidityEnv, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.phase.unification.set.Equation.Status
import ca.uwaterloo.flix.language.phase.unification.set.SetUnification.Options
import ca.uwaterloo.flix.language.phase.unification.set.{Equation, SetFormula, SetSubstitution, SetUnification}
import ca.uwaterloo.flix.util.collection.Bimap
import ca.uwaterloo.flix.util.{InternalCompilerException, Result}

object EffUnification3 {

  /**
    * Returns a most general substitution solution to `l`.
    *
    * @param l     the list of equations.
    * @param scope the current region scope.
    * @param renv  the rigidity environment.
    * @param loc   the source location of the entire equation system, e.g. the entire function body.
    */
  def unifyAll(l: List[(Type, Type, SourceLocation)], scope: Scope, renv: RigidityEnv, loc: SourceLocation, opts: Options = Options.default): (List[(Type, Type, SourceLocation)], Substitution) = {
    // Compute a bi-directional map from type variables to integers.
    implicit val bimap: Bimap[Atom, Int] = mkBidirectionalVarMap(l)(scope, renv)

    // Translate all unification problems from equations on types to equations on formula.
    // We could solve all the valid equations but we just give up if any equation is inconvertible.
    val equations = try l.map {
      case (tpe1, tpe2, loc) => toEquation(tpe1, tpe2, loc)(scope, renv, bimap)
    } catch {
      case InvalidType => return (l, Substitution.empty)
    }

    // Compute the most-general unifier of all the equations.
    val (setEqs, setSubst) = SetUnification.solve(equations)(SetUnification.SolverListener.doNothing, opts)
    val eqs = setEqs.map(eq => (fromSetFormula(eq.f1, eq.loc), fromSetFormula(eq.f2, eq.loc), eq.loc))
    val subst = fromSetSubst(setSubst)
    (eqs, subst)
  }

  def unify(tpe1: Type, tpe2: Type, scope: Scope, renv: RigidityEnv): Result[Option[Substitution], UnificationError] = {
    implicit val bimap: Bimap[Atom, Int] = mkBidirectionalVarMap(tpe1, tpe2)(scope, renv)

    val equation = try {
      toEquation(tpe1, tpe2, tpe1.loc)(scope, renv, bimap)
    } catch {
      case InvalidType => return Result.Ok(None)
    }

    SetUnification.solve(List(equation))(SetUnification.SolverListener.doNothing, Options.default) match {
      case (Nil, subst) =>
        Result.Ok(Some(fromSetSubst(subst)))

      case (eq :: _, _) =>
        // Equations are leftover.
        val Equation(f1, f2, status, loc) = eq
        status match {
          case Status.Pending => throw InternalCompilerException(s"Unexpected pending formula $eq", eq.loc)
          case Status.Unsolvable =>
            Result.Err(UnificationError.MismatchedEffects(fromSetFormula(f1, loc), fromSetFormula(f2, loc)))
          case Status.Timeout(msg) =>
            Result.Err(UnificationError.TooComplex(msg, eq.loc))
        }

    }
  }

  /**
    * Returns a bi-directional map from type variables to ints computed from the given list of
    * unification equations `l`.
    */
  private def mkBidirectionalVarMap(l: List[(Type, Type, SourceLocation)])(implicit scope: Scope, renv: RigidityEnv): Bimap[Atom, Int] = {
    // Find all atoms that occur in anywhere in `l`.
    val atoms = l.foldLeft(Set.empty[Atom]) {
      case (acc, (t1, t2, _)) => acc ++ getAtoms(t1) ++ getAtoms(t2)
    }

    // Construct the map from atoms to ints.
    mkBidirectionalVarMap(atoms)
  }

  private def mkBidirectionalVarMap(tpe1: Type, tpe2: Type)(implicit scope: Scope, renv: RigidityEnv): Bimap[Atom, Int] =
    mkBidirectionalVarMap(getAtoms(tpe1) ++ getAtoms(tpe2))

  private def mkBidirectionalVarMap(atoms: Set[Atom]): Bimap[Atom, Int] =
    Bimap.from(atoms.toList.zipWithIndex)

  /**
    * Returns the set of [[Atom]] that occur in the given [[Type]] or throws [[InvalidType]].
    *
    * This should collect atoms top-down according to [[Atom.fromType]] and ignore everything else.
    */
  private def getAtoms(t: Type)(implicit scope: Scope, renv: RigidityEnv): Set[Atom] = t match {
    case Type.Var(sym, _) if renv.isRigid(sym) => Set(Atom.RigidVar(sym))
    case Type.Var(sym, _) => Set(Atom.FlexVar(sym))
    case Type.Cst(TypeConstructor.Effect(sym), _) => Set(Atom.Eff(sym))
    case Type.Cst(TypeConstructor.Error(id, _), _) => Set(Atom.Error(id))
    case Type.Apply(tpe1, tpe2, _) => getAtoms(tpe1) ++ getAtoms(tpe2)
    case Type.Alias(_, _, tpe, _) => getAtoms(tpe)
    case assoc@Type.AssocType(_, _, _, _) => getAssocAtoms(assoc).toSet
    case _ => Set.empty
  }

  private def getAssocAtoms(t: Type)(implicit scope: Scope, renv: RigidityEnv): Option[Atom] = t match {
    case Type.Var(sym, _) if renv.isRigid(sym) => Some(Atom.RigidVar(sym))
    case Type.AssocType(AssocTypeConstructor(sym, _), arg, _, _) =>
      getAssocAtoms(arg).map(Atom.Assoc(sym, _))
    case Type.Alias(_, _, tpe, _) => getAssocAtoms(tpe)
    case _ => None
  }

  /**
    * Translates the given unification equation on [[Type]] `p` into a unification equation on [[SetFormula]].
    *
    * Throws [[InvalidType]] for invalid types.
    */
  private def toEquation(p: (Type, Type, SourceLocation))(implicit scope: Scope, renv: RigidityEnv, m: Bimap[Atom, Int]): Equation = {
    val (tpe1, tpe2, loc) = p
    Equation.mk(toSetFormula(tpe1), toSetFormula(tpe2), loc)
  }

  /**
    * Returns the given type `t` as [[SetFormula]].
    *
    * Uses the given bimap `m` to map type variables to formula variables.
    *
    * The rigidity environment `renv` is used to map rigid type variables to constants and flexible type variables to formula variables.
    *
    * Throws [[InvalidType]] if `t` is not valid.
    */
  private def toSetFormula(t: Type)(implicit scope: Scope, renv: RigidityEnv, m: Bimap[Atom, Int]): SetFormula = t match {
    case Type.Univ => SetFormula.Univ
    case Type.Pure => SetFormula.Empty

    case tpe@Type.Var(_, _) =>
      val atom = Atom.fromType(tpe)
      m.getForward(atom) match {
        case None => throw InternalCompilerException(s"Unexpected unbound type variable: '$t'.", t.loc)
        case Some(x) => atom match {
          case Atom.FlexVar(_) => SetFormula.Var(x) // A flexible variable is a real variable.
          case Atom.RigidVar(_) => SetFormula.Cst(x) // A rigid variable is a constant.
          case _ => throw InternalCompilerException(s"Unexpected atom rep. ($atom) of variable ($tpe)", tpe.loc)
        }
      }

    case tpe@Type.Cst(TypeConstructor.Effect(_), _) => m.getForward(Atom.fromType(tpe)) match {
      case None => throw InternalCompilerException(s"Unexpected unbound effect: '$t'.", t.loc)
      case Some(x) => SetFormula.mkElemSet(x)
    }

    case tpe@Type.AssocType(_, _, _, _) => m.getForward(Atom.fromType(tpe)) match {
      case None => throw InternalCompilerException(s"Unexpected unbound associated type: '$t'.", t.loc)
      case Some(x) => SetFormula.Cst(x)
    }

    case tpe@Type.Cst(TypeConstructor.Error(_, _), _) => m.getForward(Atom.fromType(tpe)) match {
      case None => throw InternalCompilerException(s"Unexpected unbound error type: '$t'.", t.loc)
      case Some(x) => SetFormula.Var(x)
    }

    case Type.Apply(Type.Cst(TypeConstructor.Complement, _), tpe1, _) =>
      SetFormula.mkCompl(toSetFormula(tpe1))

    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Union, _), tpe1, _), tpe2, _) =>
      SetFormula.mkUnion(toSetFormula(tpe1), toSetFormula(tpe2))

    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Intersection, _), tpe1, _), tpe2, _) =>
      SetFormula.mkInter(toSetFormula(tpe1), toSetFormula(tpe2))

    case Type.Alias(_, _, tpe, _) => toSetFormula(tpe)

    case _ => throw InvalidType
  }

  /** Returns a regular type substitution obtained from the given Boolean substitution `s`. */
  private def fromSetSubst(s: SetSubstitution)(implicit m: Bimap[Atom, Int]): Substitution = {
    Substitution(s.m.foldLeft(Map.empty[Symbol.KindedTypeVarSym, Type]) {
      case (macc, (k, SetFormula.Var(x))) if k == x => macc
      case (macc, (k, v)) =>
        m.getBackward(k).get match {
          // Case 1: A proper var. Add it to the substitution.
          case Atom.FlexVar(sym) =>
            macc + (sym -> fromSetFormula(v, sym.loc))
          // Case 2: An error type. Don't add it to the substitution.
          case Atom.Error(_) =>
            macc
          // Case 3: An invalid atom. Crash.
          case other => throw InternalCompilerException(s"unexpected non-variable mapped to variable: $other", SourceLocation.Unknown)
        }
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
  private def fromSetFormula(t: SetFormula, loc: SourceLocation)(implicit m: Bimap[Atom, Int]): Type = t match {
    case SetFormula.Univ => Type.Univ
    case SetFormula.Empty => Type.Pure
    case SetFormula.Cst(c) => Atom.toType(m.getBackward(c).get, loc) // Safe: We never introduce new variables.
    case SetFormula.Var(x) => Atom.toType(m.getBackward(x).get, loc) // Safe: We never introduce new variables.
    case SetFormula.ElemSet(x) => Type.mkUnion(x.toList.map(m.getBackward(_).get).map(Atom.toType(_, loc)), loc)
    case SetFormula.Compl(t) => Type.mkComplement(fromSetFormula(t, loc), loc)
    case inter@SetFormula.Inter(_, _, _, _, _, _, _) =>
      Type.mkIntersection(inter.mapSubformulas(fromSetFormula(_, loc)), loc)
    case union@SetFormula.Union(_, _, _, _, _, _, _) =>
      Type.mkUnion(union.mapSubformulas(fromSetFormula(_, loc)), loc)
  }

  /**
    * Atomic terms that may appear in the algebra of effects when associated types are fully reduced.
    *
    * atom ::= VarFlex | VarRigid | Eff | Error | atomAssoc
    * atomAssoc ::= Assoc atomAssoc | VarRigid
    */
  private sealed trait Atom

  private object Atom {
    /** Representing a flexible variable. */
    case class FlexVar(sym: Symbol.KindedTypeVarSym) extends Atom

    /** Representing a rigid variable. */
    case class RigidVar(sym: Symbol.KindedTypeVarSym) extends Atom

    /** Representing an effect constant. */
    case class Eff(sym: Symbol.EffectSym) extends Atom

    /** Represents an associated effect. */
    case class Assoc(sym: Symbol.AssocTypeSym, arg: Atom) extends Atom

    /** Represents an error type. */
    case class Error(id: Int) extends Atom

    /** Returns the [[Atom]] representation of the given [[Type]] or throws [[InvalidType]]. */
    def fromType(t: Type)(implicit scope: Scope, renv: RigidityEnv): Atom = t match {
      case Type.Var(sym, _) if renv.isRigid(sym) => Atom.RigidVar(sym)
      case Type.Var(sym, _) => Atom.FlexVar(sym)
      case Type.Cst(TypeConstructor.Effect(sym), _) => Atom.Eff(sym)
      case assoc@Type.AssocType(_, _, _, _) => assocFromType(assoc)
      case Type.Cst(TypeConstructor.Error(id, _), _) => Atom.Error(id)
      case Type.Alias(_, _, tpe, _) => fromType(tpe)
      case _ => throw InvalidType
    }

    /** Returns the [[Atom]] representation of the given [[Type]] or throws [[InvalidType]]. */
    private def assocFromType(t: Type)(implicit scope: Scope, renv: RigidityEnv): Atom = t match {
      case Type.Var(sym, _) if renv.isRigid(sym) => Atom.RigidVar(sym)
      case Type.AssocType(AssocTypeConstructor(sym, _), arg, _, _) => Atom.Assoc(sym, assocFromType(arg))
      case Type.Alias(_, _, tpe, _) => assocFromType(tpe)
      case _ => throw InvalidType
    }


    /** Returns the [[Type]] represented by the given [[Atom]]. */
    def toType(s: Atom, loc: SourceLocation)(implicit m: Bimap[Atom, Int]): Type = s match {
      case Atom.Eff(sym) => Type.Cst(TypeConstructor.Effect(sym), loc)
      case Atom.RigidVar(sym) => Type.Var(sym, loc)
      case Atom.FlexVar(sym) => Type.Var(sym, loc)
      case Atom.Assoc(sym, arg0) =>
        Type.AssocType(AssocTypeConstructor(sym, loc), toType(arg0, loc), Kind.Eff, loc)
      case Atom.Error(id) => Type.Cst(TypeConstructor.Error(id, Kind.Eff), loc)
    }
  }

  private case object InvalidType extends RuntimeException

}
