/*
 * Copyright 2024 Matthew Lutze
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
package ca.uwaterloo.flix.language.phase.typer

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.shared.*
import ca.uwaterloo.flix.language.ast.{Kind, RigidityEnv, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.phase.typer.TypeConstraint.Provenance
import ca.uwaterloo.flix.language.phase.typer.TypeReduction2.reduce
import ca.uwaterloo.flix.language.phase.unification.*
import ca.uwaterloo.flix.util.collection.ListOps
import ca.uwaterloo.flix.util.{ChaosMonkey, Result}

import scala.annotation.tailrec

/**
  * The constraint solver reduces a collection of constraints by iteratively applying reduction rules.
  * The result of constraint solving is a substitution and a list of constraints that could not be resolved.
  */
object ConstraintSolver2 {

  /**
    * A container for a constraint set and a substitution tree.
    *
    * This class provides several methods for manipulating the constraints.
    */
  // Invariant: the constraints always have the tree applied
  final class Soup(private val constrs: List[TypeConstraint], private val tree: SubstitutionTree) {

    /**
      * Transforms the constraint set by applying a one-to-many constraint function.
      */
    def flatMap(f: TypeConstraint => List[TypeConstraint]): Soup = {
      val newConstrs = flatMapConserve(constrs)(f)
      renew(newConstrs, tree)
    }

    /**
      * Transforms the constraint set by applying a one-to-one constraint function.
      */
    def map(f: TypeConstraint => TypeConstraint): Soup = {
      val newConstrs = ListOps.mapWithReuse(constrs)(f)
      renew(newConstrs, tree)
    }

    /**
      * Transforms the constraint set by applying a one-to-many constraint function,
      * composing the result with the substitution tree.
      */
    def flatMapSubst(f: TypeConstraint => (List[TypeConstraint], SubstitutionTree)): Soup = {
      val (newConstrs, moreTree) = foldSubstitution(constrs)(f)
      renew(newConstrs, moreTree @@ tree)
    }

    /**
      * Transforms the entire set of constraints with the given many-to-many constraint function.
      */
    def blockApply(f: List[TypeConstraint] => (List[TypeConstraint], SubstitutionTree)): Soup = {
      val (newConstrs, moreTree) = f(constrs)
      renew(newConstrs, moreTree @@ tree)
    }

    /**
      * Returns the constraints and substitution tree.
      */
    def get: (List[TypeConstraint], SubstitutionTree) = (constrs, tree)

    /**
      * Performs the function `f` on the constraints until no progress is made.
      */
    @tailrec
    def exhaustively(progress: Progress)(f: (Soup, Progress) => Soup): Soup = {
      val innerProgress = Progress()
      val res = f(this, innerProgress)
      if (innerProgress.query()) {
        progress.markProgress()
        res.exhaustively(progress)(f)
      } else {
        res
      }
    }

    /**
      * Creates a new [[Soup]], but reuses this one if the arguments are the same.
      */
    private def renew(newConstrs: List[TypeConstraint], newTree: SubstitutionTree): Soup = {
      if ((constrs eq newConstrs) && (tree eq newTree)) {
        this
      } else {
        new Soup(newConstrs, newTree)
      }
    }

  }

  /**
    * Unifies the given type fully, reducing all generated constraints.
    *
    * Returns None if the type are not unifiable.
    */
  def fullyUnify(tpe1: Type, tpe2: Type, scope: RegionScope, renv: RigidityEnv)(implicit eqenv: EqualityEnv, flix: Flix): Option[Substitution] = {
    // unification is now defined as taking a single constraint and applying rules until it's done
    val constr = TypeConstraint.Equality(tpe1, tpe2, Provenance.Match(tpe1, tpe2, SourceLocation.Unknown))
    implicit val r: RigidityEnv = renv
    implicit val s: RegionScope = scope
    solveAllTypes(List(constr)) match {
      // Case 1: No constraints left. Success.
      case (Nil, subst) => Some(subst)

      // Case 2: Leftover constraints. Failure
      case (_ :: _, _) => None
    }
  }

  /**
    * Returns `true` if `tpe1` and `tpe2` are equivalent types.
    */
  def isEquivalent(tpe1: Type, tpe2: Type)(implicit eqenv: EqualityEnv, flix: Flix): Boolean = {
    // Mark everything as rigid.
    val renv = RigidityEnv.ofRigidVars(tpe1.typeVars.map(_.sym) ++ tpe2.typeVars.map(_.sym))
    ConstraintSolver2.fullyUnify(tpe1, tpe2, RegionScope.Top, renv)(eqenv, flix).isDefined
  }

  /**
    * Solves the given constraint set as far as possible.
    */
  def solveAll(constrs0: List[TypeConstraint], initialSubst: SubstitutionTree)(implicit scope: RegionScope, renv: RigidityEnv, trenv: TraitEnv, eqenv: EqualityEnv, flix: Flix): (List[TypeConstraint], SubstitutionTree) = {
    val constrs = constrs0.map(initialSubst.apply)
    val soup = new Soup(constrs, initialSubst)
    val progress = Progress()
    val res = soup.exhaustively(progress)(solveOne)
    res.get
  }

  /**
    * Solves the given constraint set as far as possible.
    *
    * The constraint set must contain only equality constraints.
    */
  def solveAllTypes(constrs0: List[TypeConstraint])(implicit scope: RegionScope, renv: RigidityEnv, eqenv: EqualityEnv, flix: Flix): (List[TypeConstraint], Substitution) = {
    solveAll(constrs0, SubstitutionTree.empty)(scope, renv, TraitEnv(Map.empty), eqenv, flix) match {
      case (constrs, subst) => (constrs, subst.root)
    }
  }

  /**
    * Iterates once over all reduction rules to apply them to the constraint set.
    */
  private def solveOne(soup: Soup, progress: Progress)(implicit scope: RegionScope, renv: RigidityEnv, trenv: TraitEnv, eqenv: EqualityEnv, flix: Flix): Soup = {
    soup
      .exhaustively(progress) {
        (soup, progress) =>
          val s1 = soup
            .flatMap(simplify(_, progress))
            .flatMapSubst(makeSubstitution(_, progress))
          // Performance: Simplification is deterministic, so if the soup is unchanged
          // (object identity), repeating it is a no-op and can be skipped.
          // N.B.: We cannot use `progress` to detect this, since alias unwrapping in
          // type reduction changes constraints without marking progress.
          val s3 =
            if (s1 eq soup)
              s1
            else {
              val s2 = s1.flatMap(simplify(_, progress))
              if (s2 eq s1) s2 else s2.flatMap(simplify(_, progress))
            }
          s3
            .flatMapSubst(recordUnification(_, progress))
            .flatMapSubst(schemaUnification(_, progress))
            .map(purifyEmptyRegion(_, progress))
      }
      .blockApply(blockEffectUnification(_, progress))
      .flatMapSubst(caseSetUnification(_, progress))
      .flatMapSubst(booleanUnification(_, progress))
      .flatMap(contextReduction(_, progress))
  }

  /**
    * Simplifies the given constraint by applying the local rules in a single traversal:
    *
    *   - (appU): breaks down equalities over syntactic types: `τ₁[τ₂] ~ τ₃[τ₄]` becomes `τ₁ ~ τ₃, τ₂ ~ τ₄`
    *   - (reflU): eliminates trivial equalities: `τ ~ τ` becomes `∅`
    *   - eliminates constraints containing unrecoverable errors (see [[isEliminable]])
    *   - (redU): reduces the types in the constraint
    *
    * The rules are applied in the listed order to each constraint:
    * applications are broken down exhaustively, and then the remaining
    * rules are applied to each resulting constraint.
    *
    * This fuses what would otherwise be one traversal per rule into a single traversal,
    * preserving the order in which rules are applied to any given constraint.
    */
  private def simplify(constr: TypeConstraint, progress: Progress)(implicit scope: RegionScope, renv: RigidityEnv, eqenv: EqualityEnv, flix: Flix): List[TypeConstraint] = constr match {
    case TypeConstraint.Equality(tpe1, tpe2, prov) => (tpe1, tpe2) match {
      // (appU)
      case (Type.Apply(t11, t12, _), Type.Apply(t21, t22, _)) if isSyntactic(tpe1.kind) && isSyntactic(tpe2.kind) =>
        progress.markProgress()
        simplify(TypeConstraint.Equality(t11, t21, prov), progress) ::: simplify(TypeConstraint.Equality(t12, t22, prov), progress)

      case _ =>
        // (reflU)
        if (tpe1 == tpe2) {
          progress.markProgress()
          Nil
        } else if (isEliminable(tpe1) || isEliminable(tpe2)) {
          Nil
        } else {
          // (redU)
          val (r1, cs1) = reduce(tpe1)(scope, renv, progress, eqenv, flix)
          val (r2, cs2) = reduce(tpe2)(scope, renv, progress, eqenv, flix)
          // Performance: Reuse this, if possible.
          if ((r1 eq tpe1) && (r2 eq tpe2) && cs1.isEmpty && cs2.isEmpty)
            constr :: Nil
          else
            TypeConstraint.Equality(r1, r2, prov) :: cs1 ::: cs2
        }
    }

    case TypeConstraint.Trait(sym, tpe, loc) =>
      if (isEliminable(tpe)) {
        Nil
      } else {
        // (redU)
        val (r, cs) = reduce(tpe)(scope, renv, progress, eqenv, flix)
        // Performance: Reuse this, if possible.
        if ((r eq tpe) && cs.isEmpty)
          constr :: Nil
        else
          TypeConstraint.Trait(sym, r, loc) :: cs
      }

    case TypeConstraint.Purification(sym, eff1, eff2, prov, nested0) =>
      // (redU)
      val (r1, cs1) = reduce(eff1)(scope, renv, progress, eqenv, flix)
      val (r2, cs2) = reduce(eff2)(scope, renv, progress, eqenv, flix)
      val nested = flatMapConserve(nested0)(simplify(_, progress)(scope.enter(sym), renv, eqenv, flix))
      // Performance: Reuse this, if possible.
      if ((r1 eq eff1) && (r2 eq eff2) && (nested eq nested0) && cs1.isEmpty && cs2.isEmpty)
        constr :: Nil
      else
        TypeConstraint.Purification(sym, r1, r2, prov, nested) :: cs1 ::: cs2

    case TypeConstraint.Conflicted(tpe1, tpe2, prov) =>
      if (isEliminable(tpe1) || isEliminable(tpe2)) {
        Nil
      } else {
        // (redU)
        val (r1, cs1) = reduce(tpe1)(scope, renv, progress, eqenv, flix)
        val (r2, cs2) = reduce(tpe2)(scope, renv, progress, eqenv, flix)
        // Performance: Reuse this, if possible.
        if ((r1 eq tpe1) && (r2 eq tpe2) && cs1.isEmpty && cs2.isEmpty)
          constr :: Nil
        else
          TypeConstraint.Conflicted(r1, r2, prov) :: cs1 ::: cs2
      }

    case TypeConstraint.EffConflicted(_) => constr :: Nil
  }

  /**
    * Purifies empty regions in the constraint set.
    *
    * {{{
    *   φ₁ ~ φ₂[r ↦ Pure] ∧ ∅
    * }}}
    *
    * becomes
    *
    * {{{
    *   φ₁ ~ φ₃{r ↦ Pure}
    * }}}
    *
    * where `{ }` represents actual substitution
    */
  private def purifyEmptyRegion(constr: TypeConstraint, progress: Progress): TypeConstraint = constr match {
    case TypeConstraint.Purification(sym, eff1, eff2, prov, Nil) =>
      progress.markProgress()
      val purified = Type.purifyRegion(eff2, sym)
      TypeConstraint.Equality(eff1, purified, prov)
    case c@TypeConstraint.Purification(sym, eff1, eff2, prov, nested0) =>
      val nested = ListOps.mapWithReuse(nested0)(purifyEmptyRegion(_, progress))
      // Performance: Reuse this, if possible.
      if (nested eq nested0)
        c
      else
        TypeConstraint.Purification(sym, eff1, eff2, prov, nested)
    case c => c
  }

  /**
    * Returns true if type constraints containing this type can be eliminated.
    * They can be eliminated if:
    * - the type is syntactic and IS an error type, or
    * - the type is non-syntactic and CONTAINS an error type
    *
    * We don't eliminate constraints with errors that are part of syntactic types.
    * This is to allow partial solutions. For example, if we have:
    * {{{
    *   a -> Error ~ String -> Bool
    * }}}
    * then we don't eliminate this constraint because we can later break it down to
    * {{{
    *       a ~ String
    *   Error ~ Bool
    * }}}
    * then `a ~ String` can be solved and `Error ~ Bool` can be eliminated.
    *
    * If the types are non-syntactic, then we eliminate the constraint.
    * For example, if we have:
    * {{{
    *   IO ∪ Error ~ IO ∩ a
    * }}}
    * then we eliminate the constraint because we cannot simply break it down.
    *
    * Note: Eliminating an error constraint does not count as progress.
    */
  private def isEliminable(tpe: Type): Boolean = {
    if (isSyntactic(tpe.kind)) {
      // Syntactic kinds can only be eliminated after breaking them up.
      tpe match {
        case Type.Cst(TypeConstructor.Error(_, _), _) => true
        case _ => false
      }
    } else {
      // Non-syntactic kinds don't get broken up, so the must be eliminated eagerly.
      Type.hasError(tpe)
    }
  }

  /**
    * Performs context reduction on the given type constraint.
    *
    * Removes a constraint T[τ] if the trait environment contains it.
    *
    * Replaces a constraint with its premises if there is an implication for the constraint in the environment.
    *
    * {{{
    *   Ord[List[τ]]
    * }}}
    *
    * becomes
    *
    * {{{
    *   Ord[τ]
    * }}}
    *
    * given
    *
    * {{{
    *   instance Ord[List[a]] with Ord[a]
    * }}}
    */
  private def contextReduction(constr: TypeConstraint, progress: Progress)(implicit scope: RegionScope, renv0: RigidityEnv, trenv: TraitEnv, eqenv: EqualityEnv, flix: Flix): List[TypeConstraint] = constr match {
    // Case 1: Non-trait constraint. Do nothing.
    case c: TypeConstraint.Equality => List(c)
    case c: TypeConstraint.Conflicted => List(c)
    case c: TypeConstraint.EffConflicted => List(c)

    case c@TypeConstraint.Purification(sym, eff1, eff2, prov, nested0) =>
      val nested = flatMapConserve(nested0)(contextReduction(_, progress)(scope.enter(sym), renv0, trenv, eqenv, flix))
      // Performance: Reuse this, if possible.
      if (nested eq nested0)
        c :: Nil
      else
        List(TypeConstraint.Purification(sym, eff1, eff2, prov, nested))

    // Case 2: Trait constraint. Perform context reduction.
    case c@TypeConstraint.Trait(sym, tpe, loc) =>

      // Get all the instances from the context
      val insts = trenv.getInstance(sym, tpe)

      // Find the instance that matches
      val matches = insts.flatMap {
        case Instance(instTparams, instTpe0, instTconstrs0, instEconstrs0) =>
          // We fully rigidify `tpe`, because we need the substitution to go from instance type to constraint type.
          // For example, if our constraint is ToString[Map[Int32, a]] and our instance is ToString[Map[k, v]],
          // then we want the substitution to include "v -> a" but NOT "a -> v".
          val renv = tpe.typeVars.map(_.sym).foldLeft(renv0)(_.markRigid(_))

          // Refresh the flexible variables in the instance
          // (variables may be rigid if the instance comes from a constraint on the definition)
          val instVarMap = instTparams.map {
            case fromSym => fromSym -> Type.freshVar(fromSym.kind, fromSym.loc)
          }.toMap
          val instSubst = Substitution(instVarMap)
          val instTpe = instSubst(instTpe0)
          val instTconstrs = instTconstrs0.map(instSubst.apply)
          val instEconstrs = instEconstrs0.map(instSubst.apply)

          // Instantiate all the instance constraints according to the substitution.
          fullyUnify(tpe, instTpe, scope, renv).map {
            case subst => (instTconstrs.map(subst.apply), instEconstrs.map(subst.apply))
          }
      }

      matches match {
        // Case 1: No match. Throw the constraint back in the pool.
        case None => List(c)

        // Case 2: One match. Use the instance constraints.
        case Some((newTconstrs, newEconstrs)) =>
          progress.markProgress()
          val tTypeConstrs = newTconstrs.map(traitConstraintToTypeConstraint(_, loc))
          val eTypeConstrs = newEconstrs.map(equalityConstraintToTypeConstraint(_, loc))
          tTypeConstrs ++ eTypeConstrs
      }
  }

  /**
    * Performs case set unification on the given constraint.
    */
  private def caseSetUnification(constr: TypeConstraint, progress: Progress)(implicit scope: RegionScope, renv: RigidityEnv, flix: Flix): (List[TypeConstraint], SubstitutionTree) = constr match {
    case c@TypeConstraint.Equality(tpe1, tpe2, _) => (tpe1.kind, tpe2.kind) match {
      case (Kind.CaseSet(sym1), Kind.CaseSet(sym2)) if sym1 == sym2 =>
        CaseSetZhegalkinUnification.unify(tpe1, tpe2, renv, sym1.universe, sym1) match {
          case Some(subst) => (Nil, SubstitutionTree.shallow(subst))
          case None => (List(c), SubstitutionTree.empty)
        }
      case _ => (List(c), SubstitutionTree.empty)
    }

    case c@TypeConstraint.Purification(sym, eff1, eff2, prov, nested0) =>
      val (nested, branch) = foldSubstitution(nested0)(caseSetUnification(_, progress)(scope.enter(sym), renv, flix))
      // Performance: Reuse this, if possible.
      if (branch.isEmpty && (nested eq nested0)) {
        (c :: Nil, SubstitutionTree.empty)
      } else {
        val tree = SubstitutionTree.oneBranch(sym, branch)
        val cs = List(TypeConstraint.Purification(sym, eff1, eff2, prov, nested))
        (cs, tree)
      }

    case c => (List(c), SubstitutionTree.empty)
  }

  /**
    * Performs Boolean unification on the given constraint.
    */
  private def booleanUnification(constr: TypeConstraint, progress: Progress)(implicit scope: RegionScope, renv: RigidityEnv, flix: Flix): (List[TypeConstraint], SubstitutionTree) = constr match {
    case c@TypeConstraint.Equality(tpe1, tpe2, _) if tpe1.kind == Kind.Bool && tpe2.kind == Kind.Bool =>
      // BoolUnification is all-or-nothing:
      // Either we get a substitution and have nothing left over
      // Or we have leftovers but the substitution is empty.
      BoolUnification.unify(tpe1, tpe2, renv) match {
        case Some(subst) => (Nil, SubstitutionTree.shallow(subst))
        case None => (List(c), SubstitutionTree.empty)
      }

    case c@TypeConstraint.Purification(sym, eff1, eff2, prov, nested0) =>
      val (nested, branch) = foldSubstitution(nested0)(booleanUnification(_, progress)(scope.enter(sym), renv, flix))
      // Performance: Reuse this, if possible.
      if (branch.isEmpty && (nested eq nested0)) {
        (c :: Nil, SubstitutionTree.empty)
      } else {
        val tree = SubstitutionTree.oneBranch(sym, branch)
        val cs = List(TypeConstraint.Purification(sym, eff1, eff2, prov, nested))
        (cs, tree)
      }

    case c => (List(c), SubstitutionTree.empty)
  }

  /**
    * Performs effect unification on all the given constraints.
    */
  private def blockEffectUnification(constrs0: List[TypeConstraint], progress: Progress)(implicit scope: RegionScope, renv: RigidityEnv, flix: Flix): (List[TypeConstraint], SubstitutionTree) = {

    // Separate out the effect unification stuff
    val (eqConstrs, rest0) = constrs0.partitionMap {
      case eq@TypeConstraint.Equality(tpe1, _, _) if tpe1.kind == Kind.Eff => Left(eq)
      case other => Right(other)
    }

    // First solve all the top-level constraints together
    val (leftovers1, subst1) = EffUnification3.unifyAll(eqConstrs, scope, renv) match {
      case Result.Ok(subst) =>
        // If we solved everything, then we can use the new substitution.
        // We only mark progress if there was something to solve.
        if (eqConstrs.nonEmpty) {
          progress.markProgress()
        }
        (Nil, subst)
      case Result.Err(unsolved) =>
        // We try to compute user-friendly error message
        // If unsuccessful we return original unsolved constraints
        val errors = EffectProvenance.getErrors(constrs0)
        val c = errors match {
          case Some(effErrors) => effErrors
          case None => unsolved
        }
        (c, Substitution.empty)
    }

    val tree0 = SubstitutionTree.shallow(subst1)

    // Performance: If there were no equality constraints, reuse the original list.
    val rest0a = if (eqConstrs.isEmpty) constrs0 else rest0

    // Apply the substitution to the remaining constraints
    val rest1 = ListOps.mapWithReuse(rest0a)(tree0.apply)

    // Now we separate the purification constraints and recurse on those individually
    var branches = Map.empty[Symbol.RegionSym, SubstitutionTree]
    val rest = ListOps.mapWithReuse(rest1) {
      // If it's a purification constraint, solve the nested constraints
      // and put the substitution in the tree
      case c@TypeConstraint.Purification(sym, eff1, eff2_0, prov, nested0) =>
        val nested1 = ListOps.mapWithReuse(nested0)(tree0.apply)
        val (nested, subst2) = blockEffectUnification(nested1, progress)(scope.enter(sym), renv, flix)
        branches = branches + (sym -> subst2)

        // apply the inner substitution to the to-be-purified effect
        val eff2 = subst2.root(eff2_0)
        // Performance: Reuse this, if possible.
        if ((nested eq nested0) && (eff2 eq eff2_0))
          c
        else
          TypeConstraint.Purification(sym, eff1, eff2, prov, nested)

      // Otherwise no change
      case c => c
    }

    val tree = SubstitutionTree.mk(tree0.root, branches)
    val constrs = leftovers1 ++ rest

    (constrs, tree)
  }

  /**
    * Performs record row unification on the given type constraint.
    */
  private def recordUnification(constr: TypeConstraint, progress: Progress)(implicit scope: RegionScope, renv: RigidityEnv, eqEnv: EqualityEnv, flix: Flix): (List[TypeConstraint], SubstitutionTree) = constr match {
    case TypeConstraint.Equality(tpe1, tpe2, prov) if tpe1.kind == Kind.RecordRow && tpe2.kind == Kind.RecordRow =>
      RecordConstraintSolver.solve(tpe1, tpe2, scope, renv, prov)(progress, flix) match {
        case (constrs, subst) => (constrs, SubstitutionTree.shallow(subst))
      }

    case c@TypeConstraint.Purification(sym, eff1, eff2, prov, nested0) =>
      val (nested, branch) = foldSubstitution(nested0)(recordUnification(_, progress)(scope.enter(sym), renv, eqEnv, flix))
      // Performance: Reuse this, if possible.
      if (branch.isEmpty && (nested eq nested0)) {
        (c :: Nil, SubstitutionTree.empty)
      } else {
        val tree = SubstitutionTree.oneBranch(sym, branch)
        val cs = List(TypeConstraint.Purification(sym, eff1, eff2, prov, nested))
        (cs, tree)
      }

    case c => (List(c), SubstitutionTree.empty)
  }

  /**
    * Performs schema row unification on the given type constraint.
    */
  private def schemaUnification(constr: TypeConstraint, progress: Progress)(implicit scope: RegionScope, renv: RigidityEnv, eqEnv: EqualityEnv, flix: Flix): (List[TypeConstraint], SubstitutionTree) = constr match {
    case TypeConstraint.Equality(tpe1, tpe2, prov) if tpe1.kind == Kind.SchemaRow && tpe2.kind == Kind.SchemaRow =>
      SchemaConstraintSolver.solve(tpe1, tpe2, scope, renv, prov)(progress, flix) match {
        case (constrs, subst) => (constrs, SubstitutionTree.shallow(subst))
      }

    case c@TypeConstraint.Purification(sym, eff1, eff2, prov, nested0) =>
      val (nested, branch) = foldSubstitution(nested0)(schemaUnification(_, progress)(scope.enter(sym), renv, eqEnv, flix))
      // Performance: Reuse this, if possible.
      if (branch.isEmpty && (nested eq nested0)) {
        (c :: Nil, SubstitutionTree.empty)
      } else {
        val tree = SubstitutionTree.oneBranch(sym, branch)
        val cs = List(TypeConstraint.Purification(sym, eff1, eff2, prov, nested))
        (cs, tree)
      }

    case c => (List(c), SubstitutionTree.empty)
  }

  /**
    * Builds a substitution from constraints where one side is a free variable.
    *
    * {{{
    *   τ ~ α
    * }}}
    *
    * becomes
    *
    * {{{
    *   α ↦ τ
    * }}}
    */
  // (varU)
  private def makeSubstitution(constr: TypeConstraint, progress: Progress)(implicit scope: RegionScope, renv: RigidityEnv): (List[TypeConstraint], SubstitutionTree) = constr match {
    case TypeConstraint.Equality(Type.Var(sym, _), tpe2, _) if canSubstitute(sym, tpe2) =>
      progress.markProgress()
      (Nil, SubstitutionTree.singleton(sym, tpe2))

    case TypeConstraint.Equality(tpe1, Type.Var(sym, _), _) if canSubstitute(sym, tpe1) =>
      progress.markProgress()
      (Nil, SubstitutionTree.singleton(sym, tpe1))

    case c0@TypeConstraint.Purification(sym, eff1, eff2, prov, nested0) =>
      val (nested, branch) = foldSubstitution(nested0)(makeSubstitution(_, progress)(scope.enter(sym), renv))
      // Performance: Reuse this, if possible.
      if (branch.isEmpty && (nested eq nested0)) {
        (c0 :: Nil, SubstitutionTree.empty)
      } else {
        val c = TypeConstraint.Purification(sym, eff1, eff2, prov, nested)
        val tree = SubstitutionTree.oneBranch(sym, branch)
        (List(c), tree)
      }

    case c => (List(c), SubstitutionTree.empty)
  }

  /**
    * Returns true if it is valid to create a substitution from the given type variable to the given type.
    */
  private def canSubstitute(sym: Symbol.KindedTypeVarSym, tpe: Type)(implicit scope: RegionScope, renv: RigidityEnv): Boolean = {
    renv.isFlexible(sym) &&
      sym.kind == tpe.kind &&
      hasIdempotentSubstitution(sym.kind) &&
      !tpe.typeVars.exists { tvar => tvar.sym == sym } &&
      !Type.hasJvmType(tpe)
  }

  /**
    * Returns true if substitutions over the given kind are idempotent.
    *
    * This means that the substitution can be applied multiple times to a type without changing the result.
    */
  private def hasIdempotentSubstitution(k: Kind) = k match {
    case Kind.Eff => false
    case Kind.Bool => false
    case Kind.CaseSet(_) => false
    case _ => true
  }

  /**
    * Folds over the constraints with the given constraint/substitution processing function.
    *
    * Ensures that the resulting substitution has been applied to all constraints.
    */
  private def foldSubstitution(constrs: List[TypeConstraint])(f: TypeConstraint => (List[TypeConstraint], SubstitutionTree)): (List[TypeConstraint], SubstitutionTree) = {
    var subst = SubstitutionTree.empty
    var changed = false
    val buf = List.newBuilder[TypeConstraint]
    var cur = constrs
    while (cur.nonEmpty) {
      val constr = cur.head
      val (cs, s) = f(subst(constr))
      if (!s.isEmpty) {
        subst = s @@ subst
      }
      cs match {
        // Performance: Reuse the original constraint, if possible.
        case c :: Nil if c eq constr => buf += c
        case _ =>
          changed = true
          buf ++= cs
      }
      cur = cur.tail
    }
    val newConstrs = if (changed) buf.result() else constrs
    if (subst.isEmpty)
      (newConstrs, subst)
    else
      (newConstrs.map(subst.apply), subst) // apply the substitution to all constraints
  }

  /**
    * Applies the one-to-many constraint function `f` to each constraint.
    *
    * Returns the original list if `f` leaves every constraint unchanged.
    */
  private def flatMapConserve(constrs: List[TypeConstraint])(f: TypeConstraint => List[TypeConstraint]): List[TypeConstraint] = {
    var changed = false
    val buf = List.newBuilder[TypeConstraint]
    var cur = constrs
    while (cur.nonEmpty) {
      val constr = cur.head
      f(constr) match {
        // Performance: Reuse the original constraint, if possible.
        case c :: Nil if c eq constr => buf += c
        case cs =>
          changed = true
          buf ++= cs
      }
      cur = cur.tail
    }
    if (changed) buf.result() else constrs
  }

  /**
    * Converts a syntactic type constraint into a semantic type constraint.
    *
    * Replaces the location with the given location.
    */
  private def traitConstraintToTypeConstraint(constr: TraitConstraint, loc: SourceLocation): TypeConstraint = constr match {
    case TraitConstraint(head, arg, _) => TypeConstraint.Trait(head.sym, arg, loc)
  }

  /**
    * Converts a syntactic equality constraint into a semantic type constraint.
    *
    * Replaces the location with the given location.
    */
  private def equalityConstraintToTypeConstraint(constr: EqualityConstraint, loc: SourceLocation): TypeConstraint = constr match {
    case EqualityConstraint(cst, tpe1, tpe2, _) =>
      val assoc = Type.AssocType(cst, tpe1, tpe2.kind, tpe1.loc)
      TypeConstraint.Equality(assoc, tpe2, Provenance.Match(tpe1, tpe2, loc))
  }

  /**
    * Returns true if the kind should be unified syntactically.
    */
  @tailrec
  private def isSyntactic(k: Kind): Boolean = k match {
    case Kind.Star => true
    case Kind.Predicate => true

    case Kind.Arrow(_, k2) => isSyntactic(k2)

    case Kind.Wild => false
    case Kind.WildCaseSet => false
    case Kind.Eff => false
    case Kind.Bool => false
    case Kind.RecordRow => false
    case Kind.SchemaRow => false
    case Kind.Jvm => false
    case Kind.CaseSet(_) => false
    case Kind.Error => false
  }
}
