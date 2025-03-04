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

import ca.uwaterloo.flix.api.{Flix, FlixEvent}
import ca.uwaterloo.flix.language.ast.shared.Scope
import ca.uwaterloo.flix.language.ast.shared.SymUse.AssocTypeSymUse
import ca.uwaterloo.flix.language.ast.{Kind, RegionAction, RegionFlavor, RigidityEnv, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.phase.typer.TypeConstraint
import ca.uwaterloo.flix.language.phase.typer.TypeConstraint.Provenance
import ca.uwaterloo.flix.language.phase.unification.set.Equation.Status
import ca.uwaterloo.flix.language.phase.unification.set.{Equation, SetFormula, SetSubstitution, SetUnification}
import ca.uwaterloo.flix.language.phase.unification.zhegalkin.Zhegalkin
import ca.uwaterloo.flix.util.collection.SortedBimap
import ca.uwaterloo.flix.util.{InternalCompilerException, Result}

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet

object EffUnification3 {

  /**
    * Controls whether to enable solve-and-retry for subeffecting.
    */
  var EnableSmartSubeffecting: Boolean = true

  /**
    * Computes an MGU for **ALL* equations in `eqns`.
    *
    * Returns `Result.Ok(s)` if *ALL* equations in `eqns` where solvable.
    * The returned substitution `s` is an MGU for `eqns`.
    *
    * Returns `Result.Err(l)` if *a single equation* in `eqns` is unsolvable.
    * The returned list `l` is a non-empty list of equations that were unsolvable (i.e. in conflict).
    * The equations in `l` are derived from `eqns` but are not a strict subset of `eqns`.
    * That is, an equation in `l` may not directly correspond to any equation in `eqns`. However, their source locations are valid.
    *
    * Returns `Result.Err(eqns)` if `eqns` contains an equation that is ill-kinded. Hence, it is better to handle ill-kinded equations elsewhere.
    *
    * Note: Treats `Type.Error` as a constant, i.e. only equal to itself. Hence, it is better to drop equations that contain `Type.Error`.
    */
  def unifyAll(eqs: List[TypeConstraint.Equality], scope: Scope, renv: RigidityEnv)(implicit flix: Flix): Result[Substitution, List[TypeConstraint]] = {
    // Performance: Nothing to do if the equation list is empty
    if (eqs.isEmpty) {
      return Result.Ok(Substitution.empty)
    }

    // Add to implicit context.
    implicit val scopeImplicit: Scope = scope
    implicit val renvImplicit: RigidityEnv = renv
    implicit val listener: SetUnification.SolverListener = SetUnification.SolverListener.DoNothing

    // Choose a unique number for each atom.
    implicit val bimap: SortedBimap[Atom, Int] = mkBidirectionalVarMap(getAtomsFromConstraints(eqs))

    //
    // Phase 1: Try to solve without subeffecting.
    //
    if (EnableSmartSubeffecting) {
      try {
        val equations = toEquations(eqs, withSlack = false)
        val (unsolvedEqns, resultSubst) = SetUnification.solve(equations)
        if (unsolvedEqns.isEmpty) {
          // We have found a valid solution without subeffecting. Return it immediately.
          return Result.Ok(fromSetSubst(resultSubst)(withSlack = false, m = bimap))
        }
        // Otherwise we fall through.
      } catch {
        case InvalidType(_) => // We fall through.
      }
    }

    //
    // Phase 2: With subeffecting.
    //
    try {
      val equations = toEquations(eqs, withSlack = true)
      flix.emitEvent(FlixEvent.SolveEffEquations(equations))
      val (unsolvedEqns, resultSubst) = SetUnification.solve(equations)
      if (unsolvedEqns.isEmpty) {
        Result.Ok(fromSetSubst(resultSubst)(withSlack = true, m = bimap))
      } else {
        Result.Err(fromSetEquations(unsolvedEqns))
      }
    } catch {
      case InvalidType(_) =>
        // The effect equations are invalid.
        // We don't call these conflicted because TypeReduction may make these valid later.
        Result.Err(eqs)
    }
  }

  /** Returns a [[SortedBimap]] with each [[Atom]] having a unique number. */
  private def mkBidirectionalVarMap(atoms: SortedSet[Atom]): SortedBimap[Atom, Int] =
    SortedBimap.from(atoms.toList.zipWithIndex)

  /** Returns the union of [[Atom]]s for each [[Type]] in `eqs` using [[Atom.getAtoms]]. */
  private def getAtomsFromConstraints(eqs: List[TypeConstraint.Equality])(implicit scope: Scope, renv: RigidityEnv): SortedSet[Atom] = {
    eqs.foldLeft(SortedSet.empty[Atom]) {
      case (acc, TypeConstraint.Equality(t1, t2, _)) => acc ++ Atom.getAtoms(t1) ++ Atom.getAtoms(t2)
    }
  }

  /**
    * Returns the given list of type equations as a list of set equations.
    */
  private def toEquations(l: List[TypeConstraint.Equality], withSlack: Boolean)(implicit scope: Scope, renv: RigidityEnv, m: SortedBimap[Atom, Int]): List[Equation] =
    l.map(e => toEquation(e, withSlack))

  /**
    * Translates `eq` into a equation of [[SetFormula]].
    *
    * Throws [[InvalidType()]] for types not convertible to [[SetFormula]].
    */
  private def toEquation(eq: TypeConstraint.Equality, withSlack: Boolean)(implicit scope: Scope, renv: RigidityEnv, m: SortedBimap[Atom, Int]): Equation = {
    val TypeConstraint.Equality(tpe1, tpe2, prov) = eq
    Equation.mk(toSetFormula(tpe1)(withSlack = withSlack, scope, renv, m), toSetFormula(tpe2)(withSlack = withSlack, scope, renv, m), prov.loc)
  }

  /**
    * Returns `t` as a [[SetFormula]].
    *
    * Throws [[InvalidType()]] if `t` is not valid.
    */
  private def toSetFormula(t: Type)(implicit withSlack: Boolean, scope: Scope, renv: RigidityEnv, m: SortedBimap[Atom, Int]): SetFormula = t match {
    case Type.Univ => SetFormula.Univ
    case Type.Pure => SetFormula.Empty

    case tpe@Type.Var(_, _) =>
      val atom = Atom.fromType(tpe)
      m.getForward(atom) match {
        case None => throw InternalCompilerException(s"Unexpected unbound type variable: '$tpe'.", tpe.loc)
        case Some(x) => atom match {
          case Atom.VarFlex(sym) =>
            if (sym.isSlack && !withSlack) {
              // Special Case: We have a slack variable and we want to ignore it. Return the empty set.
              SetFormula.Empty
            } else {
              // General Case: A flexible type variable is a real Set variable.
              SetFormula.Var(x)
            }
          case Atom.VarRigid(_) => SetFormula.Cst(x) // A rigid variable is a constant.
          case _ => throw InternalCompilerException(s"Unexpected atom representation ($atom) of variable ($tpe)", tpe.loc)
        }
      }

    case tpe@Type.Cst(TypeConstructor.Effect(_), _) => m.getForward(Atom.fromType(tpe)) match {
      case None => throw InternalCompilerException(s"Unexpected unbound effect: '$tpe'.", tpe.loc)
      case Some(x) => SetFormula.mkElemSet(x)
    }

    case tpe@Type.Cst(TypeConstructor.RegionId(_), _) => m.getForward(Atom.fromType(tpe)) match {
      case None => throw InternalCompilerException(s"Unexpected unbound effect: '$tpe'.", tpe.loc)
      case Some(x) => SetFormula.mkElemSet(x)
    }

    case tpe@Type.AssocType(_, _, _, _) => m.getForward(Atom.fromType(tpe)) match {
      case None => throw InternalCompilerException(s"Unexpected unbound associated type: '$tpe'.", tpe.loc)
      case Some(x) => SetFormula.Cst(x)
    }

    case tpe@Type.Apply(Type.Cst(TypeConstructor.RegionToEff(_), _), _, _) => m.getForward(Atom.fromType(tpe)) match {
      case None => throw InternalCompilerException(s"Unexpected unbound region: '$tpe'.", tpe.loc)
      case Some(x) => SetFormula.Cst(x)
    }

    case tpe@Type.Cst(TypeConstructor.Error(_, _), _) => m.getForward(Atom.fromType(tpe)) match {
      case None => throw InternalCompilerException(s"Unexpected unbound error type: '$tpe'.", tpe.loc)
      case Some(x) => SetFormula.Var(x)
    }

    case Type.Apply(Type.Cst(TypeConstructor.Complement, _), tpe1, _) =>
      SetFormula.mkCompl(toSetFormula(tpe1))

    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Union, _), tpe1, _), tpe2, _) =>
      SetFormula.mkUnion2(toSetFormula(tpe1), toSetFormula(tpe2))

    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Intersection, _), tpe1, _), tpe2, _) =>
      SetFormula.mkInter(toSetFormula(tpe1), toSetFormula(tpe2))

    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Difference, _), tpe1, _), tpe2, _) =>
      SetFormula.mkDiff2(toSetFormula(tpe1), toSetFormula(tpe2))

    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SymmetricDiff, _), tpe1, _), tpe2, _) =>
      val f1 = toSetFormula(tpe1)
      val f2 = toSetFormula(tpe2)
      SetFormula.mkXor2(f1, f2)

    case Type.Alias(_, _, tpe, _) => toSetFormula(tpe)

    case _ => throw InvalidType(t)
  }

  /** Returns [[Substitution]] where each mapping in `s` is converted to [[Type]]. */
  private def fromSetSubst(s: SetSubstitution)(implicit withSlack: Boolean, m: SortedBimap[Atom, Int]): Substitution = {
    Substitution(s.m.foldLeft(Map.empty[Symbol.KindedTypeVarSym, Type]) {
      case (macc, (k, SetFormula.Var(x))) if k == x => macc
      case (macc, (k, v)) =>
        m.getBackward(k) match {
          case Some(Atom.VarFlex(sym)) =>
            // A proper var. Add it to the substitution.
            if (sym.isSlack && !withSlack) {
              // Special Case: The slack variable was set to the empty set.
              macc + (sym -> Type.Pure)
            } else {
              // General Case: The map determines the type variable.
              macc + (sym -> fromSetFormula(v, sym.loc))
            }

          case Some(Atom.Error(_)) =>
            // An error type. Don't add it to the substitution.
            macc

          case Some(a) =>
            // An invalid atom. Crash.
            throw InternalCompilerException(s"Unexpected atom '$a' for key '$k'.", SourceLocation.Unknown)

          case None =>
            // An unbound identifier. Crash.
            throw InternalCompilerException(s"Unexpected unbound variable '$k'.", SourceLocation.Unknown)
        }
    })
  }

  /**
    * Returns the given list of equations as a list of type equalities.
    */
  private def fromSetEquations(l: List[Equation])(implicit m: SortedBimap[Atom, Int]): List[TypeConstraint] =
    l.map {
      case Equation(f1, f2, status, loc) =>
        val t1 = fromSetFormula(f1, loc)
        val t2 = fromSetFormula(f2, loc)
        val prov = status match {
          case Status.Pending => Provenance.Match(t1, t2, loc)
          case Status.Unsolvable => Provenance.Match(t1, t2, loc)
          case Status.Timeout(msg) => Provenance.Timeout(msg, loc)
        }
        TypeConstraint.Conflicted(t1, t2, prov)
    }

  /**
    * Returns `f` as a type with location `loc`.
    *
    * Both constants and variables are mapped back to generic type variables. The rigidity
    * environment, in the type world, distinguishes their rigidity or flexibility.
    */
  private def fromSetFormula(f: SetFormula, loc: SourceLocation)(implicit m: SortedBimap[Atom, Int]): Type = f match {
    case SetFormula.Univ => Type.Univ
    case SetFormula.Empty => Type.Pure
    case SetFormula.Cst(c) => m.getBackward(c) match {
      case Some(atom) => Atom.toType(atom, loc)
      case None => throw InternalCompilerException(s"Unexpected unbound constant identifier '$c'", loc)
    }
    case SetFormula.Var(x) => m.getBackward(x) match {
      case Some(atom) => Atom.toType(atom, loc)
      case None => throw InternalCompilerException(s"Unexpected unbound variable identifier '$x'", loc)
    }
    case SetFormula.ElemSet(s) =>
      val elementTypes = s.toList.map(e => m.getBackward(e) match {
        case Some(atom) => Atom.toType(atom, loc)
        case None => throw InternalCompilerException(s"Unexpected unbound element identifier '$e'", loc)
      })
      Type.mkUnion(elementTypes, loc)
    case SetFormula.Compl(f1) => Type.mkComplement(fromSetFormula(f1, loc), loc)

    case SetFormula.Inter(l) =>
      Type.mkIntersection(l.toList.map(fromSetFormula(_, loc)), loc)

    case SetFormula.Union(l) =>
      Type.mkUnion(l.toList.map(fromSetFormula(_, loc)), loc)

    case SetFormula.Xor(other) =>
      Type.mkSymmetricDiff(other.map(fromSetFormula(_, loc)), loc)
  }

  /**
    * Atomic effects that can be represented as atoms in [[SetFormula]]. Atomic effects are
    * variables, effects, errors, or simple associated effects, described in the grammar below.
    *
    * atom ::= VarFlex | VarRigid | Eff | Error | atomAssoc
    * atomAssoc ::= Assoc atomAssoc | VarRigid
    */
  private sealed trait Atom extends Ordered[Atom] {
    override def compare(that: Atom): Int = (this, that) match {
      case (Atom.VarFlex(sym1), Atom.VarFlex(sym2)) => sym1.id - sym2.id
      case (Atom.VarRigid(sym1), Atom.VarRigid(sym2)) => sym1.id - sym2.id
      case (Atom.Eff(sym1), Atom.Eff(sym2)) => sym1.compare(sym2)
      case (Atom.RegionId(sym1), Atom.RegionId(sym2)) => sym1.compare(sym2)
      case (Atom.RegionIdToRegion(flav1, arg1), Atom.RegionIdToRegion(flav2, arg2)) =>
        val flavCmp = flav1.compare(flav2)
        if (flavCmp != 0) flavCmp else arg1.compare(arg2)
      case (Atom.Assoc(sym1, arg1), Atom.Assoc(sym2, arg2)) =>
        val symCmp = sym1.compare(sym2)
        if (symCmp != 0) symCmp else arg1.compare(arg2)
      case (Atom.RegionToEff(action1, arg1), Atom.RegionToEff(action2, arg2)) =>
        val actionCmp = action1.compare(action2)
        if (actionCmp != 0) actionCmp else arg1.compare(arg2)
      case (Atom.Error(id1), Atom.Error(id2)) => id1 - id2
      case _ =>
        def ordinal(a: Atom): Int = a match {
          case Atom.VarFlex(_) => 0
          case Atom.VarRigid(_) => 1
          case Atom.RegionId(_) => 2
          case Atom.Eff(_) => 3
          case Atom.Assoc(_, _) => 4
          case Atom.RegionToEff(_, _) => 5
          case Atom.RegionIdToRegion(_, _) => 6
          case Atom.Error(_) => 7
        }

        ordinal(this) - ordinal(that)
    }
  }

  private object Atom {
    /** Representing a flexible variable. */
    case class VarFlex(sym: Symbol.KindedTypeVarSym) extends Atom

    /** Representing a rigid variable. */
    case class VarRigid(sym: Symbol.KindedTypeVarSym) extends Atom

    /** Representing an effect constant. */
    case class Eff(sym: Symbol.EffectSym) extends Atom

    /** Represents an associated effect. */
    case class Assoc(sym: Symbol.AssocTypeSym, arg: Atom) extends Atom

    /** Represents a region identifier. */
    case class RegionId(sym: Symbol.RegionSym) extends Atom

    /** Represents a conversion from region ID to region */
    case class RegionIdToRegion(flav: RegionFlavor, arg: Atom) extends Atom

    /** Represents a region converted to an effect. */
    case class RegionToEff(action: Option[RegionAction], arg: Atom) extends Atom

    /** Represents an error type. */
    case class Error(id: Int) extends Atom

    /** Returns the [[Atom]] representation of `t` or throws [[InvalidType()]]. */
    def fromType(t: Type)(implicit scope: Scope, renv: RigidityEnv): Atom = t match {
      case Type.Var(sym, _) if renv.isRigid(sym) => Atom.VarRigid(sym)
      case Type.Var(sym, _) => Atom.VarFlex(sym)
      case Type.Cst(TypeConstructor.Effect(sym), _) => Atom.Eff(sym)
      case Type.Cst(TypeConstructor.RegionId(sym), _) => Atom.RegionId(sym)
      case Type.AssocType(AssocTypeSymUse(sym, _), arg, _, _) => Atom.Assoc(sym, fromNestedType(arg))
      case Type.Apply(Type.Cst(TypeConstructor.RegionToEff(action), _), tpe2, _) => Atom.RegionToEff(action, fromNestedType(tpe2))
      case Type.Apply(Type.Cst(TypeConstructor.RegionIdToRegion(action), _), tpe2, _) => Atom.RegionIdToRegion(action, fromNestedType(tpe2))
      case Type.Cst(TypeConstructor.Error(id, _), _) => Atom.Error(id)
      case Type.Alias(_, _, tpe, _) => fromType(tpe)
      case _ => throw InvalidType(t)
    }

    /** Returns the [[Atom]] representation of `t` or throws [[InvalidType()]]. */
    private def assocFromType(t: Type)(implicit scope: Scope, renv: RigidityEnv): Atom = t match {
      case Type.Var(sym, _) if renv.isRigid(sym) => Atom.VarRigid(sym)
      case Type.AssocType(AssocTypeSymUse(sym, _), arg, _, _) => Atom.Assoc(sym, assocFromType(arg))
      case Type.Alias(_, _, tpe, _) => assocFromType(tpe)
      case _ => throw InvalidType(t)
    }

    /**
      * Returns the set of valid [[Atom]]s that occur in `t` (according to [[Atom.fromType]]).
      * Invalid or unrelated types are ignored.
      *
      * The validity of atoms are checked top-down, so even though `MyTrait.MyType[x]` is a valid
      * atom where `x` is rigid, `getAtoms(MyTrait.MyType[MyTrait.MyType[x] ∪ IO])` will return
      * `Set.empty` since the outermost associated type is not valid. This behaviour aligns with the
      * needs of [[toSetFormula]].
      *
      * Examples:
      *   - `getAtoms(Crash ∪ ef) = Set(Eff(Crash), VarFlex(ef))` (if [[RigidityEnv.isRigid]] is
      *     false for `ef`)
      *   - `getAtoms(Indexable.Aef[Error]) = Set.empty`
      */
    def getAtoms(t: Type)(implicit scope: Scope, renv: RigidityEnv): SortedSet[Atom] = t match {
      case Type.Var(sym, _) if renv.isRigid(sym) => SortedSet(Atom.VarRigid(sym))
      case Type.Var(sym, _) => SortedSet(Atom.VarFlex(sym))
      case Type.Cst(TypeConstructor.Effect(sym), _) => SortedSet(Atom.Eff(sym))
      case Type.Cst(TypeConstructor.RegionId(sym), _) => SortedSet(Atom.RegionId(sym))
      case Type.Cst(TypeConstructor.Error(id, _), _) => SortedSet(Atom.Error(id))
      case regToEff@Type.Apply(Type.Cst(TypeConstructor.RegionToEff(_), _), _, _) => SortedSet.from(getNestedAtoms(regToEff))
      case regIdToRegion@Type.Apply(Type.Cst(TypeConstructor.RegionIdToRegion(_), _), _, _) => SortedSet.from(getNestedAtoms(regIdToRegion))
      case Type.Apply(tpe1, tpe2, _) => getAtoms(tpe1) ++ getAtoms(tpe2)
      case Type.Alias(_, _, tpe, _) => getAtoms(tpe)
      case assoc@Type.AssocType(_, _, _, _) => SortedSet.from(getNestedAtoms(assoc))
      case _ => SortedSet.empty
    }

    /**
      * Returns the [[Atom]] of `t` if it is a valid associated [[Atom]] (according to
      * [[Atom.assocFromType]]). Invalid or unrelated types return [[None]].
      *
      * // MATT update this doc
      */
    private def getNestedAtoms(t: Type)(implicit scope: Scope, renv: RigidityEnv): Option[Atom] = t match {
      case Type.Var(sym, _) if renv.isRigid(sym) => Some(Atom.VarRigid(sym))
      case Type.AssocType(AssocTypeSymUse(sym, _), arg, _, _) =>
        getNestedAtoms(arg).map(Atom.Assoc(sym, _))
      case Type.Alias(_, _, tpe, _) => getNestedAtoms(tpe)
      case Type.Apply(Type.Cst(TypeConstructor.RegionToEff(action), _), arg, _) =>
        getNestedAtoms(arg).map(Atom.RegionToEff(action, _))
      case Type.Apply(Type.Cst(TypeConstructor.RegionIdToRegion(action), _), arg, _) =>
        getNestedAtoms(arg).map(Atom.RegionIdToRegion(action, _))
      case Type.Cst(TypeConstructor.RegionId(sym), _) => Some(Atom.RegionId(sym))
      case _ => None
    }

    /**
      * Returns the [[Type]] represented by `atom` with location `loc`. The kind of errors and
      * associated types are set to be [[Kind.Eff]].
      */
    def toType(atom: Atom, loc: SourceLocation)(implicit m: SortedBimap[Atom, Int]): Type = atom match {
      case Atom.Eff(sym) => Type.Cst(TypeConstructor.Effect(sym), loc)
      case Atom.RegionId(sym) => Type.Cst(TypeConstructor.RegionId(sym), loc)
      case Atom.RegionIdToRegion(flav, arg0) =>
        Type.Apply(Type.Cst(TypeConstructor.RegionIdToRegion(flav), loc), toType(arg0, loc), loc)
      case Atom.RegionToEff(action, arg0) =>
        Type.Apply(Type.Cst(TypeConstructor.RegionToEff(action), loc), toType(arg0, loc), loc)
      case Atom.VarRigid(sym) => Type.Var(sym, loc)
      case Atom.VarFlex(sym) => Type.Var(sym, loc)
      case Atom.Assoc(sym, arg0) =>
        Type.AssocType(AssocTypeSymUse(sym, loc), toType(arg0, loc), Kind.Eff, loc)
      case Atom.Error(id) => Type.Cst(TypeConstructor.Error(id, Kind.Eff), loc)
    }
  }

  /**
    * Simplifies the given effect `tpe` by conversion to - and back from - a Zhegalkin polynomial.
    *
    * WARNING:
    * - The type `tpe` *MUST* have kind `Eff`.
    * - The type `tpe` *MUST* be well-kinded. Do not use this function for ill-kinded effects!
    *
    * The type `tpe` may contain `Type.Error`.
    */
  def simplify(tpe: Type): Type = try {
    // Check if type is too complex to simplify via Zhegalkin polynomials.
    if (tpe.typeVars.size > SetUnification.MaxVars) {
      // The type is too complex, we return it unchanged.
      return tpe
    }

    // We can use an arbitrary scope and renv because we don't do any unification.
    try {
      implicit val scope: Scope = Scope.Top
      implicit val renv: RigidityEnv = RigidityEnv.empty
      implicit val bimap: SortedBimap[Atom, Int] = mkBidirectionalVarMap(Atom.getAtoms(tpe))

        val f0 = toSetFormula(tpe)(withSlack = false, scope, renv, bimap)
        val z = Zhegalkin.toZhegalkin(f0)
        val f1 = Zhegalkin.toSetFormula(z)

    fromSetFormula(f1, tpe.loc)
  } catch {
    case _: InvalidType =>
      // The type is invalid. We cannot simplify it.
      tpe
  }

  /**
    * An exception used for partial functions that convert [[Type]] into [[Atom]].
    *
    * This exception should not leak outside this phase - it should always be caught. It is used to
    * avoid having [[Option]] types on recursive functions.
    */
  private case class InvalidType(tpe: Type) extends RuntimeException

}
