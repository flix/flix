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
import ca.uwaterloo.flix.language.ast.Ast.{Instance, TraitContext}
import ca.uwaterloo.flix.language.ast.shared.Scope
import ca.uwaterloo.flix.language.ast.{Ast, Kind, RigidityEnv, SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.language.phase.typer.TypeReduction2.reduce
import ca.uwaterloo.flix.language.phase.unification.*
import ca.uwaterloo.flix.util.collection.{ListMap, MapOps}
import ca.uwaterloo.flix.util.{InternalCompilerException, Result}

/**
  * The constraint solver reduces a collection of constraints by iteratively applying reduction rules.
  * The result of constraint solving is a substitution and a list of constraints that could not be resolved.
  */
object ConstraintSolver2 {

  sealed trait TypeConstraint

  object TypeConstraint {
    /**
      * A constraint indicating the equivalence of two types.
      * {{{
      *   tpe1 ~ tpe2
      * }}}
      */
    case class Equality(tpe1: Type, tpe2: Type) extends TypeConstraint

    /**
      * A constraint indicating that the given type is a member of the given trait.
      * {{{
      *   sym[tpe]
      * }}}
      */
    case class Trait(sym: Symbol.TraitSym, tpe: Type) extends TypeConstraint

    /**
      * A constraint indicating that:
      *   - `eff1` is equivalent to `eff2` when the region `sym` is purified in `eff2`, and
      *   - the nested constraints all hold
      *
      * This constraint arises when exiting a region.
      * All nested constraints must be resolved before determining the equality of `eff1` and `eff2`,
      * because the nested constraints influence `eff2`.
      *
      * {{{
      *   eff1 ~ eff2[sym ↦ Pure] ∧ nested
      * }}}
      */
    case class Purification(sym: Symbol.KindedTypeVarSym, eff1: Type, eff2: Type, nested: List[TypeConstraint]) extends TypeConstraint
  }

  /**
    * A mutable class used for tracking whether progress has been made.
    */
  case class Progress(private var progressMade: Boolean = false) {
    def markProgress(): Unit = {
      progressMade = true
    }

    def query(): Boolean = {
      progressMade
    }
  }

  /**
    * A container for a constraint set and a substitution tree.
    *
    * This class provides several methods for manipulating the constraints.
    */
  // Invariant: the constraints always have the tree applied
  class Soup private(private val constrs: List[TypeConstraint], private val tree: SubstitutionTree) {

    /**
      * Transforms the constraint set by applying a one-to-many constraint function.
      */
    def flatMap(f: TypeConstraint => List[TypeConstraint]): Soup = {
      val newConstrs = constrs.flatMap(f)
      new Soup(newConstrs, tree)
    }

    /**
      * Transforms the constraint set by applying a one-to-one constraint function.
      */
    def map(f: TypeConstraint => TypeConstraint): Soup = {
      val newConstrs = constrs.map(f)
      new Soup(newConstrs, tree)
    }

    /**
      * Transforms the constraint set by applying a one-to-many constraint function,
      * composing the result with the substitution tree.
      */
    def flatMapSubst(f: TypeConstraint => (List[TypeConstraint], SubstitutionTree)): Soup = {
      val (newConstrs, moreTree) = foldSubstitution(constrs)(f)
      new Soup(newConstrs, moreTree @@ tree)
    }

    /**
      * Returns the constraints and substitution tree.
      */
    def get: (List[TypeConstraint], SubstitutionTree) = (constrs, tree)

    /**
      * Returns the constraints and the root substitution of the substitution tree.
      */
    def getShallow: (List[TypeConstraint], Substitution) = (constrs, tree.root)
  }

  object Soup {
    /**
      * Creates a new [[Soup]] from the given constraints.
      */
    def of(constrs: List[TypeConstraint]): Soup = new Soup(constrs, SubstitutionTree.empty)
  }

  /**
    * The substitution tree represents the substitutions that apply to different region scopes.
    * It is structured as a map rather than a proper tree,
    * since regions are uniquely identified by their region variable.
    *
    * @param root     the substitutions at the top level
    * @param branches a map from region variables to the substitutions for those regions
    */
  case class SubstitutionTree(root: Substitution, branches: Map[Symbol.KindedTypeVarSym, SubstitutionTree]) {

    /**
      * Applies this substitution tree to the given type constraint.
      */
    def apply(constr: TypeConstraint): TypeConstraint = constr match {
      case TypeConstraint.Equality(tpe1, tpe2) => TypeConstraint.Equality(root(tpe1), root(tpe2))
      case TypeConstraint.Trait(sym, tpe) => TypeConstraint.Trait(sym, root(tpe))
      case TypeConstraint.Purification(sym, eff1, eff2, nested) =>
        // Use the root substitution for the external effects.
        // Use the appropriate branch substitution for the nested constraints.
        // MATT what to do if sym not in branches?
        TypeConstraint.Purification(sym, root(eff1), root(eff2), nested.map(branches(sym).apply))
    }

    /**
      * Composes this substitution tree with the given substitution tree.
      */
    def @@(that: SubstitutionTree): SubstitutionTree = that match {
      case SubstitutionTree(thatRoot, thatBranches) =>
        val newRoot = root @@ thatRoot
        val newBranches = MapOps.unionWith(branches, thatBranches)(_ @@ _)
        SubstitutionTree(newRoot, newBranches)
    }
  }

  object SubstitutionTree {
    /**
      * The empty substitution tree.
      */
    val empty: SubstitutionTree = SubstitutionTree(Substitution.empty, Map.empty)

    /**
      * Returns a substitution tree mapping one key to one value.
      */
    def singleton(key: Symbol.KindedTypeVarSym, value: Type): SubstitutionTree = SubstitutionTree(Substitution.singleton(key, value), Map.empty)

    /**
      * Returns a substitution tree containing one branch.
      */
    def oneBranch(sym: Symbol.KindedTypeVarSym, tree: SubstitutionTree): SubstitutionTree = {
      SubstitutionTree(Substitution.empty, Map(sym -> tree))
    }
  }

  /**
    * Solves the given constraint set as far as possible.
    */
  def goAll(constrs0: List[TypeConstraint])(implicit scope: Scope, renv: RigidityEnv, trenv: Map[Symbol.TraitSym, TraitContext], eqenv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], flix: Flix): (List[TypeConstraint], SubstitutionTree) = {
    var constrs = constrs0
    var subst = SubstitutionTree.empty
    var progressMade = true
    while (progressMade) {
      implicit val tracker: Progress = Progress()
      val (newConstrs, newSubst) = goOne(constrs)
      // invariant: the new subst is already applied to all the newConstrs
      constrs = newConstrs
      subst = newSubst @@ subst
      progressMade = tracker.query()
    }
    (constrs, subst)
  }

  /**
    * Solves the given constraint set as far as possible.
    *
    * The constraint set must contain only equality constraints.
    */
  def goAllTypes(constrs0: List[TypeConstraint])(implicit scope: Scope, renv: RigidityEnv, eqenv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], flix: Flix): (List[TypeConstraint], Substitution) = {
    var constrs = constrs0
    var subst = Substitution.empty
    var progressMade = true
    while (progressMade) {
      implicit val tracker: Progress = Progress()
      val (newConstrs, newSubst) = goTypes(constrs)
      // invariant: the new subst is already applied to all the newConstrs
      constrs = newConstrs
      subst = newSubst @@ subst
      progressMade = tracker.query()
    }
    (constrs, subst)
  }

  /**
    * Iterates once over all reduction rules to apply them to the constraint set.
    */
  private def goOne(constrs: List[TypeConstraint])(implicit tracker: Progress, scope: Scope, renv: RigidityEnv, trenv: Map[Symbol.TraitSym, TraitContext], eqenv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], flix: Flix): (List[TypeConstraint], SubstitutionTree) = {
    Soup.of(constrs)
      .flatMap(breakDownConstraints)
      .flatMap(eliminateIdentities)
      .map(reduceTypes)
      .map(purifyEmptyRegion)
      .flatMapSubst(makeSubstitution)
      .flatMapSubst(effectUnification)
      .flatMapSubst(recordUnification)
      .flatMapSubst(schemaUnification)
      .flatMap(contextReduction)
      .get
  }

  /**
    * Iterates once over all reduction rules to apply them to the constraint set.
    *
    * Only applies rules relevant to equality constraints.
    */
  private def goTypes(constrs: List[TypeConstraint])(implicit tracker: Progress, scope: Scope, renv: RigidityEnv, eqenv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], flix: Flix): (List[TypeConstraint], Substitution) = {
    Soup.of(constrs)
      .flatMap(breakDownConstraints)
      .flatMap(eliminateIdentities)
      .map(reduceTypes)
      .flatMapSubst(effectUnification)
      .flatMapSubst(makeSubstitution)
      .flatMapSubst(recordUnification)
      .flatMapSubst(schemaUnification)
      .getShallow
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
  private def purifyEmptyRegion(constr: TypeConstraint): TypeConstraint = constr match {
    case TypeConstraint.Purification(sym, eff1, eff2, Nil) =>
      val purified = Substitution.singleton(sym, Type.Pure)(eff2)
      TypeConstraint.Equality(eff1, purified)
    case TypeConstraint.Purification(sym, eff1, eff2, nested0) =>
      val nested = nested0.map(purifyEmptyRegion)
      TypeConstraint.Purification(sym, eff1, eff2, nested)
    case c: TypeConstraint.Trait => c
    case c: TypeConstraint.Equality => c
  }

  /**
    * Breaks down equality constraints over syntactic types.
    *
    * {{{
    *   τ₁[τ₂] ~ τ₃[τ₄]
    * }}}
    *
    * becomes
    *
    * {{{
    *   τ₁ ~ τ₃, τ₂[τ₄]
    * }}}
    */
  // (appU)
  private def breakDownConstraints(constr: TypeConstraint)(implicit tracker: Progress): List[TypeConstraint] = constr match {
    case TypeConstraint.Equality(t1@Type.Apply(tpe11, tpe12, _), t2@Type.Apply(tpe21, tpe22, _)) if isSyntactic(t1.kind) && isSyntactic(t2.kind) =>
      tracker.markProgress()
      List(TypeConstraint.Equality(tpe11, tpe21), TypeConstraint.Equality(tpe12, tpe22))

    case TypeConstraint.Purification(sym, eff1, eff2, nested0) =>
      val nested = nested0.flatMap(breakDownConstraints)
      List(TypeConstraint.Purification(sym, eff1, eff2, nested))

    case c => List(c)
  }

  /**
    * Eliminates constraints that are the same on the left and right
    *
    * {{{
    *   τ ~ τ
    * }}}
    *
    * becomes
    *
    * {{{
    *   ∅
    * }}}
    */
  // (reflU)
  private def eliminateIdentities(constr: TypeConstraint)(implicit tracker: Progress): List[TypeConstraint] = constr match {
    case c@TypeConstraint.Equality(tpe1, tpe2) =>
      if (tpe1 == tpe2) {
        tracker.markProgress()
        Nil
      } else {
        List(c)
      }

    case TypeConstraint.Purification(sym, eff1, eff2, nested0) =>
      val nested = nested0.flatMap(eliminateIdentities)
      List(TypeConstraint.Purification(sym, eff1, eff2, nested))

    case c: TypeConstraint.Trait =>
      List(c)
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
  private def contextReduction(constr: TypeConstraint)(implicit tracker: Progress, scope: Scope, renv0: RigidityEnv, trenv: Map[Symbol.TraitSym, TraitContext], eqenv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], flix: Flix): List[TypeConstraint] = constr match {
    // Case 1: Non-trait constraint. Do nothing.
    case c: TypeConstraint.Equality => List(c)

    case TypeConstraint.Purification(sym, eff1, eff2, nested0) =>
      val nested = nested0.flatMap(contextReduction(_)(tracker, scope.enter(sym), renv0, trenv, eqenv, flix))
      List(TypeConstraint.Purification(sym, eff1, eff2, nested))

    // Case 2: Trait constraint. Perform context reduction.
    case c@TypeConstraint.Trait(sym, tpe) =>

      // Get all the instances from the context
      val TraitContext(supers, insts) = trenv(sym)

      // Find the instance that matches
      val matches = insts.flatMap {
        case Instance(instTpe, instConstrs) =>
          // We fully rigidify `tpe`, because we need the substitution to go from instance type to constraint type.
          // For example, if our constraint is ToString[Map[Int32, a]] and our instance is ToString[Map[k, v]],
          // then we want the substitution to include "v -> a" but NOT "a -> v".
          val renv = tpe.typeVars.map(_.sym).foldLeft(renv0)(_.markRigid(_))

          // Instantiate all the instance constraints according to the substitution.
          fullyUnify(tpe, instTpe, renv).map {
            case subst => instConstrs.map(subst.apply)
          }
      }

      // TODO CONSTR-SOLVER-2 ought to be exactly 0 or 1; should check in Resolver
      matches match {
        // Case 1: No match. Throw the constraint back in the pool.
        case Nil => List(c)

        // Case 2: One match. Use the instance constraints.
        case newConstrs :: Nil => newConstrs.map(traitConstraintToTypeConstraint)

        // Case 3: Multiple matches. Throw the constraint back in the pool.
        // TODO CONSTR-SOLVER-2 Right resiliency strategy?
        case _ :: _ :: _ => List(c)
      }
  }

  /**
    * Unifies the given type fully, reducing all generated constraints.
    *
    * Returns None if the type are not unifiable.
    */
  def fullyUnify(tpe1: Type, tpe2: Type, renv: RigidityEnv)(implicit scope: Scope, eqenv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], flix: Flix): Option[Substitution] = {
    // unification is now defined as taking a single constraint and applying rules until it's done
    val constr = TypeConstraint.Equality(tpe1, tpe2)
    implicit val r = renv
    goAllTypes(List(constr)) match {
      // Case 1: No constraints left. Success.
      case (Nil, subst) => Some(subst)

      // Case 2: Leftover constraints. Failure
      case (_ :: _, _) => None
    }
  }

  /**
    * Performs effect unification on the given type constraint.
    */
  private def effectUnification(constr: TypeConstraint)(implicit tracker: Progress, scope: Scope, renv: RigidityEnv, flix: Flix): (List[TypeConstraint], SubstitutionTree) = constr match {
    case c@TypeConstraint.Equality(tpe1, tpe2) if tpe1.kind == Kind.Eff && tpe2.kind == Kind.Eff =>
      EffUnification.unify(tpe1, tpe2, renv) match {
        case Result.Ok((subst, newConstrs)) => (newConstrs.map(broadEqualityConstraintToTypeConstraint), SubstitutionTree(subst, Map()))
        case Result.Err(e) => (List(c), SubstitutionTree.empty)
      }

    case TypeConstraint.Purification(sym, eff1, eff2, nested0) =>
      val (nested, branch) = foldSubstitution(nested0)(effectUnification(_)(tracker, scope.enter(sym), renv, flix))
      val tree = SubstitutionTree.oneBranch(sym, branch)
      val cs = List(TypeConstraint.Purification(sym, eff1, eff2, nested))
      (cs, tree)

    case c => (List(c), SubstitutionTree.empty)
  }

  /**
    * Performs record row unification on the given type constraint.
    */
  private def recordUnification(constr: TypeConstraint)(implicit scope: Scope, tracker: Progress, renv: RigidityEnv, eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], flix: Flix): (List[TypeConstraint], SubstitutionTree) = constr match {
    case TypeConstraint.Equality(tpe1, tpe2) if tpe1.kind == Kind.RecordRow && tpe2.kind == Kind.RecordRow =>
      RecordConstraintSolver2.solve(tpe1, tpe2) match {
        case (constrs, subst) => (constrs, SubstitutionTree(subst, Map()))
      }

    case TypeConstraint.Purification(sym, eff1, eff2, nested0) =>
      val (nested, branch) = foldSubstitution(nested0)(recordUnification(_)(scope.enter(sym), tracker, renv, eqEnv, flix))
      val tree = SubstitutionTree.oneBranch(sym, branch)
      val cs = List(TypeConstraint.Purification(sym, eff1, eff2, nested))
      (cs, tree)

    case c => (List(c), SubstitutionTree.empty)
  }

  /**
    * Performs schema row unification on the given type constraint.
    */
  private def schemaUnification(constr: TypeConstraint)(implicit scope: Scope, tracker: Progress, renv: RigidityEnv, eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], flix: Flix): (List[TypeConstraint], SubstitutionTree) = constr match {
    case TypeConstraint.Equality(tpe1, tpe2) if tpe1.kind == Kind.SchemaRow && tpe2.kind == Kind.SchemaRow =>
      SchemaConstraintSolver2.solve(tpe1, tpe2) match {
        case (constrs, subst) => (constrs, SubstitutionTree(subst, Map()))
      }

    case TypeConstraint.Purification(sym, eff1, eff2, nested0) =>
      val (nested, branch) = foldSubstitution(nested0)(schemaUnification(_)(scope.enter(sym), tracker, renv, eqEnv, flix))
      val tree = SubstitutionTree.oneBranch(sym, branch)
      val cs = List(TypeConstraint.Purification(sym, eff1, eff2, nested))
      (cs, tree)

    case c => (List(c), SubstitutionTree.empty)
  }

  /**
    * Performs reduction on the types in the given type constraints.
    */
  // (redU)
  private def reduceTypes(constr: TypeConstraint)(implicit scope: Scope, tracker: Progress, renv0: RigidityEnv, eqenv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], flix: Flix): TypeConstraint = constr match {
    case TypeConstraint.Equality(tpe1, tpe2) => TypeConstraint.Equality(reduce(tpe1), reduce(tpe2))
    case TypeConstraint.Trait(sym, tpe) => TypeConstraint.Trait(sym, reduce(tpe))
    case TypeConstraint.Purification(sym, eff1, eff2, nested) => TypeConstraint.Purification(sym, reduce(eff1), reduce(eff2), nested)
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
  // TODO CONSTR-SOLVER-2 make private
  def makeSubstitution(constr: TypeConstraint)(implicit scope: Scope, tracker: Progress, renv: RigidityEnv): (List[TypeConstraint], SubstitutionTree) = constr match {
    case TypeConstraint.Equality(Type.Var(sym, _), tpe2) if !renv.isRigid(sym) && sym.kind == tpe2.kind =>
      tracker.markProgress()
      (Nil, SubstitutionTree.singleton(sym, tpe2))

    case TypeConstraint.Equality(tpe1, Type.Var(sym, _)) if !renv.isRigid(sym) && tpe1.kind == sym.kind =>
      tracker.markProgress()
      (Nil, SubstitutionTree.singleton(sym, tpe1))

    case c: TypeConstraint.Equality => (List(c), SubstitutionTree.empty)

    case c: TypeConstraint.Trait => (List(c), SubstitutionTree.empty)

    case TypeConstraint.Purification(sym, eff1, eff2, nested) =>
      val (cs, branch) = foldSubstitution(nested)(makeSubstitution(_)(scope.enter(sym), tracker, renv))
      val c = TypeConstraint.Purification(sym, eff1, eff2, cs)
      val tree = SubstitutionTree.oneBranch(sym, branch)
      (List(c), tree)
  }

  /**
    * Folds over the constraints with the given constraint/substitution processing function.
    *
    * Ensures that the resulting substitution has been applied to all constraints.
    */
  private def foldSubstitution(constrs: List[TypeConstraint])(f: TypeConstraint => (List[TypeConstraint], SubstitutionTree)): (List[TypeConstraint], SubstitutionTree) = {
    var subst = SubstitutionTree.empty
    val newConstrs = constrs.flatMap {
      constr =>
        val (cs, s) = f(subst(constr))
        subst = s @@ subst
        cs
    }.map(subst.apply) // apply the substitution to all constraints
    (newConstrs, subst)
  }

  /**
    * Converts a syntactic type constraint into a semantic type constraint.
    */
  def traitConstraintToTypeConstraint(constr: Ast.TraitConstraint): TypeConstraint = constr match {
    case Ast.TraitConstraint(head, arg, loc) => TypeConstraint.Trait(head.sym, arg)
  }

  /**
    * Converts a broad equality constraint to a type constraint.
    */
  def broadEqualityConstraintToTypeConstraint(constr: Ast.BroadEqualityConstraint): TypeConstraint = constr match {
    case Ast.BroadEqualityConstraint(tpe1, tpe2) => TypeConstraint.Equality(tpe1, tpe2)
  }

  /**
    * Converts a type constraint to a broad equality constraint.
    *
    * The type constraint must be an equality constraint.
    */
  def unsafeTypeConstraintToBroadEqualityConstraint(constr: TypeConstraint): Ast.BroadEqualityConstraint = constr match {
    case TypeConstraint.Equality(tpe1, tpe2) => Ast.BroadEqualityConstraint(tpe1, tpe2)
    case c => throw InternalCompilerException("unexpected constraint: " + c, SourceLocation.Unknown)
  }

  /**
    * Returns true if the kind should be unified syntactically.
    */
  private def isSyntactic(k: Kind): Boolean = k match {
    case Kind.Wild => false // MATT ?
    case Kind.WildCaseSet => false
    case Kind.Star => true
    case Kind.Eff => false
    case Kind.Bool => false
    case Kind.RecordRow => false
    case Kind.SchemaRow => false
    case Kind.Predicate => false
    case Kind.Jvm => false
    case Kind.CaseSet(_) => false
    case Kind.Arrow(_, k2) => isSyntactic(k2)
    case Kind.Error => false
  }
}
