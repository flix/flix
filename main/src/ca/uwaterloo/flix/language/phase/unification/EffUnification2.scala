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
import ca.uwaterloo.flix.language.ast.Ast.AssocTypeConstructor
import ca.uwaterloo.flix.language.ast.{Ast, Kind, Rigidity, RigidityEnv, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.phase.unification.FastSetUnification.Term.mkCompl
import ca.uwaterloo.flix.language.phase.unification.FastSetUnification.{ConflictException, Equation, Term, TooComplexException}
import ca.uwaterloo.flix.util.collection.Bimap
import ca.uwaterloo.flix.util.{InternalCompilerException, Result}

import scala.io.AnsiColor

object EffUnification2 {

  /**
    * Returns the most general unifier of the pairwise unification equations in `l`.
    *
    * @param l    the list of unification equations.
    * @param renv the rigidity environment.
    * @param loc  the source location of the entire equation system, e.g. the entire function body.
    */
  def unifyAll(l: List[(Type, Type, SourceLocation)], renv: RigidityEnv, loc: SourceLocation): Result[Substitution, UnificationError] = {
    // Compute a bi-directional map from type variables to ints.
    implicit val bimap: Bimap[Atom, Int] = mkBidirectionalVarMap(l)

    // Translate all unification problems from equations on types to equations on terms.
    val equations = l.map {
      case (tpe1, tpe2, loc) => toEquation(tpe1, tpe2, loc)(renv, bimap)
    }

    // Compute the most-general unifier of all the equations.
    FastSetUnification.Solver.solve(equations) match {
      case Result.Ok(subst) => Result.Ok(fromSetSubst(subst))

      case Result.Err((ex: ConflictException, _, _)) =>
        val tpe1 = fromTerm(ex.x, ex.loc)
        val tpe2 = fromTerm(ex.y, ex.loc)
        Result.Err(UnificationError.MismatchedEffects(tpe1, tpe2))

      case Result.Err((ex: TooComplexException, _, _)) =>
        Result.Err(UnificationError.TooComplex(ex.msg, loc))
    }
  }

  def unifyHelper(tpe1: Type, tpe2: Type, renv: RigidityEnv)(implicit flix: Flix): Result[(Substitution, List[Ast.BroadEqualityConstraint]), UnificationError] = {
    val neww = unify(tpe1, tpe2, tpe1.loc, renv).map {
      case None => (Substitution.empty, List(Ast.BroadEqualityConstraint(tpe1, tpe2)))
      case Some(subst) => (subst, Nil)
    }
    // `old` is by-name, so don't let-bind it.
    Checking.compare(checkThings = true, tpe1, tpe2, old = EffUnification.unify(tpe1, tpe2, renv), neww = neww, renv)
    neww
  }

  def unify(tpe1: Type, tpe2: Type, loc: SourceLocation, renv: RigidityEnv): Result[Option[Substitution], UnificationError] = {
//    (tpe1, tpe2) match {
//      case (t1@Type.Var(x, _), t2) if renv.isFlexible(x) && !t2.typeVars.contains(t1) =>
//        return Result.Ok(Some(Substitution.singleton(x, t2)))
//
//      case (t1, t2@Type.Var(x, _)) if renv.isFlexible(x) && !t1.typeVars.contains(t2) =>
//        return Result.Ok(Some(Substitution.singleton(x, t1)))
//
//      case _ => ()
//    }

    implicit val bimap: Bimap[Atom, Int] = try {mkBidirectionalVarMap(tpe1, tpe2)} catch {
      case InternalCompilerException(_, _) => return Result.Ok(None)
    }

    val equation = try {
      toEquation(tpe1, tpe2, loc)(renv, bimap)
    } catch {
      case InternalCompilerException(_, _) => return Result.Ok(None)
    }

    FastSetUnification.Solver.solve(List(equation)) match {
      case Result.Ok(subst) => Result.Ok(Some(fromSetSubst(subst)))

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

  /**
    * Returns the set of atoms that occur in the given type.
    */
  private def getAtoms(t: Type): Set[Atom] = t match {
    case Type.Var(sym, _) => Set(Atom.Var(sym))
    case Type.Cst(TypeConstructor.Effect(sym), _) => Set(Atom.Eff(sym))
    case Type.Cst(TypeConstructor.Error(id, kind), _) => Set(Atom.Error(id, kind))
    case Type.Cst(_, _) => Set.empty
    case Type.Apply(tpe1, tpe2, _) => getAtoms(tpe1) ++ getAtoms(tpe2)
    case Type.Alias(_, _, tpe, _) => getAtoms(tpe)
    case Type.AssocType(AssocTypeConstructor(sym, _), arg, kind, _) => Set(Atom.Assoc(sym, getAssocArg(arg), kind))
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
  private def toEquation(p: (Type, Type, SourceLocation))(implicit renv: RigidityEnv, m: Bimap[Atom, Int]): Equation = {
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
  private def toTerm(t: Type)(implicit renv: RigidityEnv, m: Bimap[Atom, Int]): Term = Type.eraseTopAliases(t) match {
    case Type.Univ => Term.Univ
    case Type.Pure => Term.Empty

    case tpe@Type.Var(sym, _) => m.getForward(toAtom(tpe)) match {
      case None => throw InternalCompilerException(s"Unexpected unbound type variable: '$t'.", t.loc)
      case Some(x) => renv.get(sym) match {
        case Rigidity.Flexible => Term.Var(x) // A flexible variable is a real variable.
        case Rigidity.Rigid => Term.Cst(x) // A rigid variable is a constant.
      }
    }

    case tpe@Type.Cst(TypeConstructor.Effect(_), _) => m.getForward(toAtom(tpe)) match {
      case None => throw InternalCompilerException(s"Unexpected unbound effect: '$t'.", t.loc)
      case Some(x) => Term.mkElemSet(x)
    }

    case tpe: Type.AssocType => m.getForward(toAtom(tpe)) match {
      case None => throw InternalCompilerException(s"Unexpected unbound associated type: '$t'.", t.loc)
      case Some(x) => Term.Cst(x)
    }

    case tpe@Type.Cst(TypeConstructor.Error(_, _), _) => m.getForward(toAtom(tpe)) match {
      case None => throw InternalCompilerException(s"Unexpected unbound error type: '$t'.", t.loc)
      case Some(x) => Term.Var(x)
    }

    case Type.Apply(Type.Cst(TypeConstructor.Complement, _), tpe1, _) => Term.mkCompl(toTerm(tpe1))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Union, _), tpe1, _), tpe2, _) => Term.mkUnion(toTerm(tpe1), toTerm(tpe2))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Intersection, _), tpe1, _), tpe2, _) => Term.mkInter(toTerm(tpe1), toTerm(tpe2))

    case _ => throw InternalCompilerException(s"Unexpected type: '$t'.", t.loc)
  }

  /**
    * Returns the Atom representation of the given Type.
    */
  private def toAtom(t: Type)(implicit renv: RigidityEnv): Atom = Type.eraseTopAliases(t) match {
    case Type.Var(sym, _) => Atom.Var(sym)
    case Type.Cst(TypeConstructor.Effect(sym), _) => Atom.Eff(sym)
    case Type.AssocType(AssocTypeConstructor(sym, _), arg0, kind, _) =>
      val arg = rigidToAtom(arg0)
      Atom.Assoc(sym, arg, kind)
    case Type.Cst(TypeConstructor.Error(id, kind), _) => Atom.Error(id, kind)
    case tpe => throw InternalCompilerException(s"Unexpected non-atom type: $tpe", tpe.loc)
  }

  /**
    * Returns the Atom representation of the given Type.
    */
  private def rigidToAtom(t: Type)(implicit renv: RigidityEnv): Atom = Type.eraseTopAliases(t) match {
    case tpe@Type.Var(sym, _) =>
      if (renv.isRigid(sym)) Atom.Var(sym)
      else throw InternalCompilerException(s"Unexpected non-atom type: $tpe", tpe.loc)
    // This is omitted to align with the "wrong" behaviour of the existing solver
    //
    //    case Type.Cst(TypeConstructor.Effect(sym), _) => Atom.Eff(sym)
    //    case Type.AssocType(AssocTypeConstructor(sym, _), arg0, kind, _) =>
    //      val arg = rigidToAtom(arg0)
    //      Atom.Assoc(sym, arg, kind)
    //    case Type.Cst(TypeConstructor.Error(id, kind), _) => Atom.Error(id, kind)
    case tpe => throw InternalCompilerException(s"Unexpected non-atom type: $tpe", tpe.loc)
  }

  /**
    * Returns a regular type substitution obtained from the given Boolean substitution `s`.
    */
  private def fromSetSubst(s: FastSetUnification.SetSubstitution)(implicit m: Bimap[Atom, Int]): Substitution = {
    Substitution(s.m.foldLeft(Map.empty[Symbol.KindedTypeVarSym, Type]) {
      case (macc, (k, Term.Var(x))) if k == x => macc
      case (macc, (k, v)) =>
        m.getBackward(k).get match {
          // Case 1: A proper var. Add it to the substitution.
          case Atom.Var(sym) =>
            macc + (sym -> fromTerm(v, sym.loc))
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
  private def fromTerm(t: Term, loc: SourceLocation)(implicit m: Bimap[Atom, Int]): Type = t match {
    case Term.Univ => Type.Univ
    case Term.Empty => Type.Pure
    case Term.Cst(c) => fromAtom(m.getBackward(c).get, loc) // Safe: We never introduce new variables.
    case Term.Var(x) => fromAtom(m.getBackward(x).get, loc) // Safe: We never introduce new variables.
    case Term.ElemSet(x) => Type.mkUnion(x.toList.map(m.getBackward(_).get).map(fromAtom(_, loc)), loc)
    case Term.Compl(t) => Type.mkComplement(fromTerm(t, loc), loc)
    case Term.Inter(posElem, posCsts, posVars, negElem, negCsts, negVars, rest) =>
      val ts = posElem.iterator.map(fromTerm(_, loc)) ++
        posCsts.iterator.map(fromTerm(_, loc)) ++
        posVars.iterator.map(fromTerm(_, loc)) ++
        negElem.iterator.map(mkCompl(_)).map(fromTerm(_, loc)) ++
        negCsts.iterator.map(mkCompl(_)).map(fromTerm(_, loc)) ++
        negVars.iterator.map(mkCompl(_)).map(fromTerm(_, loc)) ++
        rest.iterator.map(fromTerm(_, loc))
      Type.mkIntersection(ts.toList, loc)
    case Term.Union(posElem, posCsts, posVars, negElem, negCsts, negVars, rest) =>
      val ts = posElem.iterator.map(fromTerm(_, loc)) ++
        posCsts.iterator.map(fromTerm(_, loc)) ++
        posVars.iterator.map(fromTerm(_, loc)) ++
        negElem.iterator.map(mkCompl(_)).map(fromTerm(_, loc)) ++
        negCsts.iterator.map(mkCompl(_)).map(fromTerm(_, loc)) ++
        negVars.iterator.map(mkCompl(_)).map(fromTerm(_, loc)) ++
        rest.iterator.map(fromTerm(_, loc))
      Type.mkUnion(ts.toList, loc)
  }

  /**
    * Returns the Type represented by the given Atom.
    */
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
    /**
      * An atom representing a variable.
      */
    case class Var(sym: Symbol.KindedTypeVarSym) extends Atom

    /**
      * An atom representing an effect constant.
      */
    case class Eff(sym: Symbol.EffectSym) extends Atom

    /**
      * An atom representing an associated effect.
      *
      * The argument must either be a variable or another valid associated effect.
      */
    case class Assoc(sym: Symbol.AssocTypeSym, arg: Atom, kind: Kind) extends Atom

    /**
      * An atom representing an invalid type.
      */
    case class Error(id: Int, kind: Kind) extends Atom
  }

  private object Checking {

    type UnifiedI = (Substitution, List[Ast.BroadEqualityConstraint])
    type Unified = Result[UnifiedI, UnificationError]

    def compare(checkThings: Boolean, tpe1: Type, tpe2: Type, old: => Unified, neww: Unified, renv: RigidityEnv): Unit = {
      if (!checkThings) () else {
        handleResults(old, neww, tpe1, tpe2, renv)
      }
//      for {
//        (sOld, restOld) <- old
//        (sNeww, newwRest) <- neww
//      } yield {
//        if (sOld.isEmpty && sNeww.isEmpty) () else {
//          if (sOld.m.keySet != sNeww.m.keySet) {
//            println(s"\nold:\n$sOld\n$restOld\nnew:\n$sNeww\n$newwRest\nfrom\n$tpe1 ~~~ $tpe2\nwith\n$renv")
//            scala.io.StdIn.readLine()
//            ()
//          }
//          for (k <- sOld.m.keySet) {
//            sNeww.m.get(k).foreach(ki => checkEq(tpeOld = sOld.m(k), tpeNew = ki, SourceLocation.Unknown, renv))
//
//          }
//        }
//      }
    }

    private def checkUnifiedI(old: UnifiedI, neww: UnifiedI, tpe1: Type, tpe2: Type, renv: RigidityEnv): Unit = {
      (old, neww) match {
        case ((_, rest1), (_, rest2)) if rest1 != rest2 =>
          println()
          println(s"-- Rest Disagree! -- ${tpe1.loc}")
          printEq(tpe1, tpe2, renv)
          println(s"old rest:")
          printConstraints(rest1)
          println(s"new rest:")
          printConstraints(rest2)
          halt()
        case _ => ()
      }
    }

    private def checkSubst(old: Substitution, neww: Substitution, tpe1: Type, tpe2: Type, renv: RigidityEnv): Unit = {
      checkGeneralSubst(general = old, specific = neww, tpe1, tpe2, renv)
    }

    /**
      * Checks that `general` is a more general substitution than `specific` by checking that, for
      * all variables(*1), there exists a new substitution(*2) `fix`, where
      * `fix(general(x)) = specific(x)`.
      *
      *   - (*1): all variables are limited to variables that appear in either substitution on either
      *     side. The variables not included in this will always have the solution that `fix` uses
      *     the identity mapping.
      *   - (*2): How do we find this substitution? we have to ask the old or the new solver which
      *     gives different correctness implications - trying both is best.
      */
    private def checkGeneralSubst(general: Substitution, specific: Substitution, tpe1: Type, tpe2: Type, renv: RigidityEnv): Unit = {
      // the set of relevant variables to the two substitutions
      val fvs = freeVars(general) ++ freeVars(specific)
      ()
    }

    private def freeVars(s: Substitution): Set[Symbol.KindedTypeVarSym] = {
      s.m.foldLeft(Set.empty[Symbol.KindedTypeVarSym]){
        case (acc, (sym, tpe)) => acc + sym ++ tpe.typeVars.map(_.sym)
      }
    }

    private def handleResults(old: Unified, neww: Unified, tpe1: Type, tpe2: Type, renv: RigidityEnv): Unit = {
      (old, neww) match {
        case (Result.Ok(v1), Result.Ok(v2)) =>
          checkUnifiedI(v1, v2, tpe1, tpe2, renv)
        case (Result.Err(_), Result.Err(_)) =>
          // we don't assert anything about simultaneous errors - it's ok
          ()
        case (Result.Ok(v), Result.Err(err)) =>
          println()
          println(s"-- Results Disagree! -- ${tpe1.loc}")
          printEq(tpe1, tpe2, renv)
          println(s"old Ok:")
          printUnifiedI(v)
          println(s"new Err:")
          println(err)
          halt()
        case (Result.Err(err), Result.Ok(v)) =>
          println()
          println(s"-- Results Disagree! -- ${tpe1.loc}")
          printEq(tpe1, tpe2, renv)
          println(s"old Err:")
          println(err)
          println(s"new Ok:")
          printUnifiedI(v)
          halt()
      }
    }

    private def halt(): Unit = {
      println()
      print(s"${AnsiColor.GREEN}<press enter to continue ..>${AnsiColor.RESET}")
      scala.io.StdIn.readLine()
    }

    private def printEq(tpe1: Type, tpe2: Type, renv: RigidityEnv): Unit = {
      println(s"$tpe1 ~ $tpe2")
      println(s"\t(${renv.s.mkString(",")})")
    }

    private def printConstraints(eqs: List[Ast.BroadEqualityConstraint]): Unit = {
      println(eqs.mkString("{", ",", "}"))
    }

    private def printUnifiedI(u: UnifiedI): Unit = {
      val (subst, eqs) = u
      if (eqs.isEmpty) println(subst.m)
      else if (subst.isEmpty) printConstraints(eqs)
      else {
        println(subst.m)
        print("and ")
        printConstraints(eqs)
      }
    }

  }

}
