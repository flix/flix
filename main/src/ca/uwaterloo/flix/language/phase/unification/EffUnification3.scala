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
import ca.uwaterloo.flix.language.ast.{Kind, Rigidity, RigidityEnv, SourceLocation, Symbol, Type, TypeConstructor}
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
  def unifyAll(l: List[(Type, Type, SourceLocation)], scope: Scope, renv: RigidityEnv, loc: SourceLocation, opts: Options = Options.default): Result[Substitution, UnificationError] = {
    // Compute a bi-directional map from type variables to integers.
    implicit val bimap: Bimap[Atom, Int] = mkBidirectionalVarMap(l)

    // Translate all unification problems from equations on types to equations on terms.
    val equations = l.map {
      case (tpe1, tpe2, loc) => toEquation(tpe1, tpe2, loc)(scope, renv, bimap)
    }

    // Compute the most-general unifier of all the equations.
    SetUnification.solve(equations)(SetUnification.SolverListener.doNothing, opts) match {
      case (Nil, subst) =>
        Result.Ok(fromSetSubst(subst))

      case (eq :: _, _) =>
        // Equations are leftover.
        eq.status match {
          case Status.Pending => throw InternalCompilerException(s"Unexpected pending formula $eq", eq.loc)
          case Status.Unsolvable =>
            val tpe1 = fromSetFormula(eq.f1, eq.loc)
            val tpe2 = fromSetFormula(eq.f2, eq.loc)
            Result.Err(UnificationError.MismatchedEffects(tpe1, tpe2))
          case Status.Timeout(msg) =>
            Result.Err(UnificationError.TooComplex(msg, eq.loc))
        }
    }
  }

  def unify(tpe1: Type, tpe2: Type, scope: Scope, renv: RigidityEnv): Result[Option[Substitution], UnificationError] = {
    implicit val bimap: Bimap[Atom, Int] = try {
      mkBidirectionalVarMap(tpe1, tpe2)
    } catch {
      case InternalCompilerException(_, _) => return Result.Ok(None)
    }

    val equation = try {
      toEquation(tpe1, tpe2, tpe1.loc)(scope, renv, bimap)
    } catch {
      case InternalCompilerException(_, _) => return Result.Ok(None)
    }

    SetUnification.solve(List(equation))(SetUnification.SolverListener.doNothing, Options.default) match {
      case (Nil, subst) =>
        Result.Ok(Some(fromSetSubst(subst)))

      case (eq :: _, _) =>
        // Equations are leftover.
        eq.status match {
          case Status.Pending => throw InternalCompilerException(s"Unexpected pending formula $eq", eq.loc)
          case Status.Unsolvable =>
            val tpe1 = fromSetFormula(eq.f1, eq.loc)
            val tpe2 = fromSetFormula(eq.f2, eq.loc)
            Result.Err(UnificationError.MismatchedEffects(tpe1, tpe2))
          case Status.Timeout(msg) =>
            Result.Err(UnificationError.TooComplex(msg, eq.loc))
        }

    }
  }

  /**
    * Returns a bi-directional map from type variables to ints computed from the given list of
    * unification equations `l`.
    */
  private def mkBidirectionalVarMap(l: List[(Type, Type, SourceLocation)]): Bimap[Atom, Int] = {
    // Find all atoms that occur in anywhere in `l`.
    val atoms = l.foldLeft(Set.empty[Atom]) {
      case (acc, (t1, t2, _)) => acc ++ getAtoms(t1) ++ getAtoms(t2)
    }

    // Construct the map from atoms to ints.
    mkBidirectionalVarMap(atoms)
  }

  private def mkBidirectionalVarMap(tpe1: Type, tpe2: Type): Bimap[Atom, Int] = {
    mkBidirectionalVarMap(getAtoms(tpe1) ++ getAtoms(tpe2))
  }


  private def mkBidirectionalVarMap(atoms: Set[Atom]): Bimap[Atom, Int] = {
    Bimap.from(atoms.toList.zipWithIndex)
  }

  /** Returns the set of atoms that occur in the given type. */
  private def getAtoms(t: Type): Set[Atom] = t match {
    case Type.Var(sym, _) => Set(Atom.Var(sym))
    case Type.Cst(TypeConstructor.Effect(sym), _) => Set(Atom.Eff(sym))
    case Type.Cst(TypeConstructor.Error(id, kind), _) => Set(Atom.Error(id, kind))
    case Type.Cst(_, _) => Set.empty
    case Type.Apply(tpe1, tpe2, _) => getAtoms(tpe1) ++ getAtoms(tpe2)
    case Type.Alias(_, _, tpe, _) => getAtoms(tpe)
    case Type.AssocType(AssocTypeConstructor(sym, _), arg, kind, _) => Set(Atom.Assoc(sym, getAssocArg(arg), kind))
    case Type.JvmToEff(_, loc) => throw InternalCompilerException("dont know", loc)
    case Type.JvmToType(_, loc) => throw InternalCompilerException("dont know", loc)
    case Type.UnresolvedJvmType(_, loc) => throw InternalCompilerException("dont know", loc)
  }

  /**
    * Returns a atom corresponding to the type t.
    *
    * Fails if t is not a valid argument to a simplified associated typed.
    */
  private def getAssocArg(t: Type): Atom = t match {
    case Type.Var(sym, _) => Atom.Var(sym)
    case Type.AssocType(AssocTypeConstructor(sym, _), arg, kind, _) => Atom.Assoc(sym, getAssocArg(arg), kind)
    case Type.Alias(_, _, tpe, _) => getAssocArg(tpe)
    case tpe => throw InternalCompilerException(s"unexpected associate type argument: $tpe", tpe.loc)
  }

  /**
    * Translates the given unification equation on types `p` into a unification equation on terms.
    */
  private def toEquation(p: (Type, Type, SourceLocation))(implicit scope: Scope, renv: RigidityEnv, m: Bimap[Atom, Int]): Equation = {
    val (tpe1, tpe2, loc) = p
    Equation.mk(toSetFormula(tpe1), toSetFormula(tpe2), loc)
  }

  /**
    * Returns the given type `t` as term.
    *
    * Uses the given bimap `m` to map type variables to term variables.
    *
    * The rigidity environment `renv` is used to map rigid type variables to constants and flexible type variables to term variables.
    */
  private def toSetFormula(t: Type)(implicit scope: Scope, renv: RigidityEnv, m: Bimap[Atom, Int]): SetFormula = Type.eraseTopAliases(t) match {
    case Type.Univ => SetFormula.Univ
    case Type.Pure => SetFormula.Empty

    case tpe@Type.Var(sym, _) => m.getForward(toAtom(tpe)) match {
      case None => throw InternalCompilerException(s"Unexpected unbound type variable: '$t'.", t.loc)
      case Some(x) => renv.get(sym) match {
        case Rigidity.Flexible => SetFormula.Var(x) // A flexible variable is a real variable.
        case Rigidity.Rigid => SetFormula.Cst(x) // A rigid variable is a constant.
      }
    }

    case tpe@Type.Cst(TypeConstructor.Effect(_), _) => m.getForward(toAtom(tpe)) match {
      case None => throw InternalCompilerException(s"Unexpected unbound effect: '$t'.", t.loc)
      case Some(x) => SetFormula.mkElemSet(x)
    }

    case tpe: Type.AssocType => m.getForward(toAtom(tpe)) match {
      case None => throw InternalCompilerException(s"Unexpected unbound associated type: '$t'.", t.loc)
      case Some(x) => SetFormula.Cst(x)
    }

    case tpe@Type.Cst(TypeConstructor.Error(_, _), _) => m.getForward(toAtom(tpe)) match {
      case None => throw InternalCompilerException(s"Unexpected unbound error type: '$t'.", t.loc)
      case Some(x) => SetFormula.Var(x)
    }

    case Type.Apply(Type.Cst(TypeConstructor.Complement, _), tpe1, _) => SetFormula.mkCompl(toSetFormula(tpe1))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Union, _), tpe1, _), tpe2, _) => SetFormula.mkUnion(toSetFormula(tpe1), toSetFormula(tpe2))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Intersection, _), tpe1, _), tpe2, _) => SetFormula.mkInter(toSetFormula(tpe1), toSetFormula(tpe2))

    case _ => throw InternalCompilerException(s"Unexpected type: '$t'.", t.loc)
  }

  /** Returns the Atom representation of the given Type. */
  private def toAtom(t: Type)(implicit scope: Scope, renv: RigidityEnv): Atom = Type.eraseTopAliases(t) match {
    case Type.Var(sym, _) => Atom.Var(sym)
    case Type.Cst(TypeConstructor.Effect(sym), _) => Atom.Eff(sym)
    case Type.AssocType(AssocTypeConstructor(sym, _), arg0, kind, _) =>
      val arg = rigidToAtom(arg0)
      Atom.Assoc(sym, arg, kind)
    case Type.Cst(TypeConstructor.Error(id, kind), _) => Atom.Error(id, kind)
    case tpe => throw InternalCompilerException(s"Unexpected non-atom type: $tpe", tpe.loc)
  }

  /** Returns the Atom representation of the given Type. */
  private def rigidToAtom(t: Type)(implicit scope: Scope, renv: RigidityEnv): Atom = Type.eraseTopAliases(t) match {
    case tpe@Type.Var(sym, _) =>
      if (renv.isRigid(sym)) Atom.Var(sym)
      else throw InternalCompilerException(s"Unexpected non-atom type: $tpe", tpe.loc)
    case Type.Cst(TypeConstructor.Effect(sym), _) => Atom.Eff(sym)
    case Type.AssocType(AssocTypeConstructor(sym, _), arg0, kind, _) =>
      val arg = rigidToAtom(arg0)
      Atom.Assoc(sym, arg, kind)
    case Type.Cst(TypeConstructor.Error(id, kind), _) => Atom.Error(id, kind)
    case tpe => throw InternalCompilerException(s"Unexpected non-atom type: $tpe", tpe.loc)
  }

  /** Returns a regular type substitution obtained from the given Boolean substitution `s`. */
  private def fromSetSubst(s: SetSubstitution)(implicit m: Bimap[Atom, Int]): Substitution = {
    Substitution(s.m.foldLeft(Map.empty[Symbol.KindedTypeVarSym, Type]) {
      case (macc, (k, SetFormula.Var(x))) if k == x => macc
      case (macc, (k, v)) =>
        m.getBackward(k).get match {
          // Case 1: A proper var. Add it to the substitution.
          case Atom.Var(sym) =>
            macc + (sym -> fromSetFormula(v, sym.loc))
          // Case 2: An error type. Don't add it to the substitution.
          case Atom.Error(_, _) =>
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
    case SetFormula.Cst(c) => fromAtom(m.getBackward(c).get, loc) // Safe: We never introduce new variables.
    case SetFormula.Var(x) => fromAtom(m.getBackward(x).get, loc) // Safe: We never introduce new variables.
    case SetFormula.ElemSet(x) => Type.mkUnion(x.toList.map(m.getBackward(_).get).map(fromAtom(_, loc)), loc)
    case SetFormula.Compl(t) => Type.mkComplement(fromSetFormula(t, loc), loc)
    case SetFormula.Inter(posElem, posCsts, posVars, negElem, negCsts, negVars, rest) =>
      val ts = posElem.iterator.map(fromSetFormula(_, loc)) ++
        posCsts.iterator.map(fromSetFormula(_, loc)) ++
        posVars.iterator.map(fromSetFormula(_, loc)) ++
        negElem.iterator.map(SetFormula.mkCompl(_)).map(fromSetFormula(_, loc)) ++
        negCsts.iterator.map(SetFormula.mkCompl(_)).map(fromSetFormula(_, loc)) ++
        negVars.iterator.map(SetFormula.mkCompl(_)).map(fromSetFormula(_, loc)) ++
        rest.iterator.map(fromSetFormula(_, loc))
      Type.mkIntersection(ts.toList, loc)
    case SetFormula.Union(posElem, posCsts, posVars, negElem, negCsts, negVars, rest) =>
      val ts = posElem.iterator.map(fromSetFormula(_, loc)) ++
        posCsts.iterator.map(fromSetFormula(_, loc)) ++
        posVars.iterator.map(fromSetFormula(_, loc)) ++
        negElem.iterator.map(SetFormula.mkCompl(_)).map(fromSetFormula(_, loc)) ++
        negCsts.iterator.map(SetFormula.mkCompl(_)).map(fromSetFormula(_, loc)) ++
        negVars.iterator.map(SetFormula.mkCompl(_)).map(fromSetFormula(_, loc)) ++
        rest.iterator.map(fromSetFormula(_, loc))
      Type.mkUnion(ts.toList, loc)
  }

  /** Returns the Type represented by the given Atom. */
  private def fromAtom(s: Atom, loc: SourceLocation)(implicit m: Bimap[Atom, Int]): Type = s match {
    case Atom.Eff(sym) => Type.Cst(TypeConstructor.Effect(sym), loc)
    case Atom.Var(sym) => Type.Var(sym, loc)
    case Atom.Assoc(sym, arg0, kind) =>
      val arg = fromAtom(arg0, loc)
      Type.AssocType(AssocTypeConstructor(sym, loc), arg, kind, loc)
    case Atom.Error(id, kind) => Type.Cst(TypeConstructor.Error(id, kind), loc)
  }

  /**
    * Atomic terms that may appear in the algebra of effects when associated types are fully reduced.
    */
  private sealed trait Atom

  private object Atom {
    /** An atom representing a variable. */
    case class Var(sym: Symbol.KindedTypeVarSym) extends Atom

    /** An atom representing an effect constant. */
    case class Eff(sym: Symbol.EffectSym) extends Atom

    /** An atom representing an associated effect. */
    case class Assoc(sym: Symbol.AssocTypeSym, arg: Atom, kind: Kind) extends Atom

    /** An atom representing an invalid type. */
    case class Error(id: Int, kind: Kind) extends Atom
  }

}
