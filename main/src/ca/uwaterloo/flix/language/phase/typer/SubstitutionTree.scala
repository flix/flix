/*
 * Copyright 2024 Matthew Lutze
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.phase.typer

import ca.uwaterloo.flix.language.ast.{Symbol, Type}
import ca.uwaterloo.flix.language.phase.unification.Substitution
import ca.uwaterloo.flix.util.collection.{ListOps, MapOps}

/**
  * The substitution tree represents the substitutions that apply to different region scopes.
  * It is structured as a map rather than a proper tree,
  * since regions are uniquely identified by their region variable.
  *
  * Invariant: any binding found in the root is also found in all the branches.
  *
  * @param root     the substitutions at the top level
  * @param branches a map from region variables to the substitutions for those regions
  */
class SubstitutionTree private(val root: Substitution, val branches: Map[Symbol.RegionSym, SubstitutionTree]) {

  def isEmpty: Boolean = root.isEmpty && branches.forall { case (_, tree) => tree.isEmpty }

  /**
    * Applies this substitution tree to the given type constraint.
    */
  def apply(constr: TypeConstraint): TypeConstraint = {
    if (root.isEmpty) {
      // Performance: The substitution is empty. Nothing to be done.
      return constr
    }

    constr match {
      case eq@TypeConstraint.Equality(tpe1, tpe2, _) =>
        // Check whether the substitution has to be applied.
        val t1 = if (tpe1.typeVars.isEmpty) tpe1 else root(tpe1)
        val t2 = if (tpe2.typeVars.isEmpty) tpe2 else root(tpe2)
        // Performance: Reuse eq, if possible.
        eq.renew(t1, t2)

      case TypeConstraint.Trait(sym, tpe, loc) =>
        // Check whether the substitution has to be applied.
        val t = if (tpe.typeVars.isEmpty) tpe else root(tpe)
        // Performance: Reuse this, if possible.
        if (t eq tpe)
          constr
        else
          TypeConstraint.Trait(sym, t, loc)

      case TypeConstraint.Purification(sym, eff1, eff2, prov, nested) =>
        // Use the root substitution for the external effects.
        // Use the appropriate branch substitution for the nested constraints.

        // We use the root as a fallback (as all branches are supersets of root)
        val subtree = branches.getOrElse(sym, this)
        // Check whether the substitution has to be applied.
        val e1 = if (eff1.typeVars.isEmpty) eff1 else root(eff1)
        val e2 = if (eff2.typeVars.isEmpty) eff2 else subtree(eff2)
        val ns = ListOps.mapWithReuse(nested)(subtree.apply)
        // Performance: Reuse this, if possible.
        if ((e1 eq eff1) && (e2 eq eff2) && (ns eq nested))
          constr
        else
          TypeConstraint.Purification(sym, e1, e2, prov, ns)

      case TypeConstraint.Conflicted(tpe1, tpe2, prov) =>
        // Check whether the substitution has to be applied.
        val t1 = if (tpe1.typeVars.isEmpty) tpe1 else root(tpe1)
        val t2 = if (tpe2.typeVars.isEmpty) tpe2 else root(tpe2)
        // Performance: Reuse this, if possible.
        if ((t1 eq tpe1) && (t2 eq tpe2))
          constr
        else
          TypeConstraint.Conflicted(t1, t2, prov)

      case TypeConstraint.EffConflicted(_) => constr
    }
  }

  /**
    * Applies the substitution at the root of the substitution tree to the given type.
    */
  def apply(t: Type): Type = root.apply(t)

  /**
    * Returns the tree obtained by applying `f` to every bound type, in the root and in every branch.
    *
    * Performance: Returns `this` if `f` returns every bound type unchanged (by reference).
    */
  def mapTypes(f: Type => Type): SubstitutionTree = {
    val newRoot = root.mapTypes(f)
    var newBranches = branches
    for ((sym, tree) <- branches) {
      val t = tree.mapTypes(f)
      if (!(t eq tree)) {
        newBranches = newBranches.updated(sym, t)
      }
    }
    if ((newRoot eq root) && (newBranches eq branches)) this else new SubstitutionTree(newRoot, newBranches)
  }

  /**
    * Composes this substitution tree with the given substitution tree.
    */
  def @@(that: SubstitutionTree): SubstitutionTree = that match {
    case SubstitutionTree(thatRoot, thatBranches) =>
      // Performance: Check if `this` or `that` is empty.
      if (this.isEmpty) {
        return that
      }
      if (that.isEmpty) {
        return this
      }
      val newRoot = root @@ thatRoot
      val newBranches = MapOps.unionWith(branches, thatBranches)(_ @@ _)
      SubstitutionTree.mk(newRoot, newBranches)
  }

  /**
    * Returns the size of the largest type in the tree.
    */
  def maxSize: Int = branches.values.map(_.maxSize).maxOption.getOrElse(0).max(root.maxSize)
}

object SubstitutionTree {

  /**
    * Destructs the tree for pattern matching.
    */
  def unapply(tree: SubstitutionTree): Option[(Substitution, Map[Symbol.RegionSym, SubstitutionTree])] = Some(tree.root, tree.branches)

  /**
    * Creates a new substitution tree from the given root and branches.
    *
    * Ensures the invariant holds: All substitutions in the root are present and applied in the branches.
    */
  def mk(root: Substitution, branches: Map[Symbol.RegionSym, SubstitutionTree]): SubstitutionTree = {
    propogate(Substitution.empty, root, branches)
  }

  /**
    * Combines the given substitution with the root and applies the resulting substitution to the branches.
    *
    * Ensures the invariant holds: All substitutions in the root are present and applied in the branches.
    */
  private def propogate(subst: Substitution, root: Substitution, branches: Map[Symbol.RegionSym, SubstitutionTree]): SubstitutionTree = {
    val newRoot = subst @@ root
    val newBranches = MapOps.mapValues(branches) {
      case SubstitutionTree(subRoot, subBranches) => propogate(newRoot, subRoot, subBranches)
    }
    new SubstitutionTree(newRoot, newBranches)
  }

  /**
    * The empty substitution tree.
    */
  val empty: SubstitutionTree = new SubstitutionTree(Substitution.empty, Map.empty)

  /**
    * Returns a substitution tree mapping one key to one value.
    */
  def singleton(key: Symbol.KindedTypeVarSym, value: Type): SubstitutionTree = new SubstitutionTree(Substitution.singleton(key, value), Map.empty)

  /**
    * Returns a substitution tree containing one branch.
    */
  def oneBranch(sym: Symbol.RegionSym, tree: SubstitutionTree): SubstitutionTree = {
    new SubstitutionTree(Substitution.empty, Map(sym -> tree))
  }

  /**
    * Returns a substitution tree with no branches.
    */
  def shallow(subst: Substitution): SubstitutionTree = {
    if (subst.isEmpty) {
      SubstitutionTree.empty
    } else {
      new SubstitutionTree(subst, Map.empty)
    }
  }
}

