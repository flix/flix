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
import ca.uwaterloo.flix.language.ast.Type.JvmMember
import ca.uwaterloo.flix.language.ast.shared.Scope
import ca.uwaterloo.flix.language.ast.{Ast, Kind, RigidityEnv, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.phase.typer.TypeConstraint as OldTypeConstraint
import ca.uwaterloo.flix.language.phase.unification.*
import ca.uwaterloo.flix.util.{InternalCompilerException, Result}
import ca.uwaterloo.flix.util.collection.{ListMap, MapOps}

// MATT docs
// MATT go through every transformer thing and make sure tracker is used
object ConstraintSolver2 {

  // MATT docs
  sealed trait TypeConstraint

  object TypeConstraint {
    case class Equality(tpe1: Type, tpe2: Type) extends TypeConstraint

    case class Trait(sym: Symbol.TraitSym, tpe: Type) extends TypeConstraint

    case class Purification(sym: Symbol.KindedTypeVarSym, eff1: Type, eff2: Type, nested: ConstraintSet) extends TypeConstraint
  }

  // MATT docs
  case class Tracker(private var progress: Boolean = false) {
    def markProgress(): Unit = {
      progress = true
    }

    def maybeMarkProgress(p: Boolean): Unit = {
      progress |= p
    }

    def query(): Boolean = {
      progress
    }
  }

  // Invariant: the constraints always have the tree applied
  class Soup private(private val constrs: ConstraintSet, private val tree: SubstitutionTree) {

    /**
      * Transforms the constraint set by applying a one-to-many constraint function.
      */
    def flatMap(f: TypeConstraint => ConstraintSet): Soup = {
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
    def flatMapSubst(f: TypeConstraint => (ConstraintSet, SubstitutionTree)): Soup = {
      val (newConstrs, moreTree) = foldSubstitution(constrs)(f)
      new Soup(newConstrs, moreTree @@ tree)
    }

    /**
      * Transforms the constraint set by applying a many-to-many constraint function,
      * composing the result with the substitution tree.
      */
    def blockSubst(f: ConstraintSet => (ConstraintSet, SubstitutionTree)): Soup = {
      val (newConstrs, moreTree) = f(constrs)
      new Soup(newConstrs, moreTree @@ tree)
    }

    def get: (ConstraintSet, SubstitutionTree) = (constrs, tree)

    def getShallow: (ConstraintSet, Substitution) = (constrs, tree.root)
  }

  object Soup {
    def of(constrs: ConstraintSet): Soup = new Soup(constrs, SubstitutionTree.empty)
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
    def apply(constr: TypeConstraint): TypeConstraint = constr match {
      case TypeConstraint.Equality(tpe1, tpe2) => TypeConstraint.Equality(root(tpe1), root(tpe2))
      case TypeConstraint.Trait(sym, tpe) => TypeConstraint.Trait(sym, root(tpe))
      case TypeConstraint.Purification(sym, eff1, eff2, nested) =>
        // MATT docs
        // MATT what to do if sym not in branches?
        TypeConstraint.Purification(sym, root(eff1), root(eff2), nested.map(branches(sym).apply))
    }

    def @@(that: SubstitutionTree): SubstitutionTree = that match {
      case SubstitutionTree(thatRoot, thatBranches) =>
        val newRoot = root @@ thatRoot
        val newBranches = MapOps.unionWith(branches, thatBranches)(_ @@ _)
        SubstitutionTree(newRoot, newBranches)
    }
  }

  object SubstitutionTree {
    val empty: SubstitutionTree = SubstitutionTree(Substitution.empty, Map.empty)

    def singleton(key: Symbol.KindedTypeVarSym, value: Type): SubstitutionTree = SubstitutionTree(Substitution.singleton(key, value), Map.empty)

    def oneBranch(sym: Symbol.KindedTypeVarSym, tree: SubstitutionTree): SubstitutionTree = {
      SubstitutionTree(Substitution.empty, Map(sym -> tree))
    }
  }

  type ConstraintSet = List[TypeConstraint]
  type TraitEnv = Map[Symbol.TraitSym, TraitContext]
  type EqualityEnv = ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef]

  def goAll(constrs0: ConstraintSet)(implicit scope: Scope, renv: RigidityEnv, trenv: TraitEnv, eqenv: EqualityEnv, flix: Flix): (ConstraintSet, SubstitutionTree) = {
    var constrs = constrs0
    var subst = SubstitutionTree.empty
    var progressMade = true
    while (progressMade) {
      implicit val tracker: Tracker = Tracker()
      val (newConstrs, newSubst) = goOne(constrs)
      // invariant: the new subst is already applied to all the newConstrs
      constrs = newConstrs
      subst = compose2(newSubst, subst)
      progressMade = tracker.query()
    }
    (constrs, subst)
  }

  def goAllTypes(constrs0: ConstraintSet)(implicit scope: Scope, renv: RigidityEnv, eqenv: EqualityEnv, flix: Flix): (ConstraintSet, Substitution) = {
    var constrs = constrs0
    var subst = Substitution.empty
    var progressMade = true
    while (progressMade) {
      implicit val tracker: Tracker = Tracker()
      val (newConstrs, newSubst) = goTypes(constrs)
      // invariant: the new subst is already applied to all the newConstrs
      constrs = newConstrs
      subst = newSubst @@ subst
      progressMade = tracker.query()
    }
    (constrs, subst)
  }

  // MATT docs
  def goOne(constrs: ConstraintSet)(implicit tracker: Tracker, scope: Scope, renv: RigidityEnv, trenv: TraitEnv, eqenv: EqualityEnv, flix: Flix): (ConstraintSet, SubstitutionTree) = {
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

  // MATT docs
  def goTypes(constrs: ConstraintSet)(implicit tracker: Tracker, scope: Scope, renv: RigidityEnv, eqenv: EqualityEnv, flix: Flix): (ConstraintSet, Substitution) = {
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

  // MATT docs
  def purifyEmptyRegion(constr: TypeConstraint): TypeConstraint = constr match {
    case TypeConstraint.Purification(sym, eff1, eff2, Nil) =>
      val purified = Substitution.singleton(sym, Type.Pure)(eff2)
      TypeConstraint.Equality(eff1, purified)
    case TypeConstraint.Purification(sym, eff1, eff2, nested0) =>
      val nested = nested0.map(purifyEmptyRegion)
      TypeConstraint.Purification(sym, eff1, eff2, nested)
    case c: TypeConstraint.Trait => c
    case c: TypeConstraint.Equality => c
  }

  // (appU)
  def breakDownConstraints(constr: TypeConstraint)(implicit tracker: Tracker): ConstraintSet = constr match {
    // TODO make sure we're looking at a syntactic type
    case TypeConstraint.Equality(t1@Type.Apply(tpe11, tpe12, _), t2@Type.Apply(tpe21, tpe22, _)) if isSyntactic(t1.kind) && isSyntactic(t2.kind) =>
      tracker.markProgress()
      List(TypeConstraint.Equality(tpe11, tpe21), TypeConstraint.Equality(tpe12, tpe22))

    case TypeConstraint.Purification(sym, eff1, eff2, nested0) =>
      val nested = nested0.flatMap(breakDownConstraints)
      List(TypeConstraint.Purification(sym, eff1, eff2, nested))

    case c => List(c)
  }

  // Eliminates constraints that are the same on the left and right
  // TODO examples
  // (reflU)
  def eliminateIdentities(constr: TypeConstraint)(implicit tracker: Tracker): ConstraintSet = constr match {
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

  // MATT docs
  def contextReduction(constr: TypeConstraint)(implicit tracker: Tracker, scope: Scope, renv0: RigidityEnv, trenv: TraitEnv, eqenv: EqualityEnv, flix: Flix): ConstraintSet = constr match {
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
        case newConstrs :: Nil => newConstrs.map(toTypeConstraint)

        // Case 3: Multiple matches. Throw the constraint back in the pool.
        // TODO CONSTR-SOLVER-2 Right resiliency strategy?
        case _ :: _ :: _ => List(c)
      }
  }

  // TODO docs
  def fullyUnify(tpe1: Type, tpe2: Type, renv: RigidityEnv)(implicit tracker: Tracker, scope: Scope, eqenv: EqualityEnv, flix: Flix): Option[Substitution] = {
    // unification is now defined as taking a single constraint and applying rules until it's done
    val constr = TypeConstraint.Equality(tpe1, tpe2)
    implicit val r = renv
    goTypes(List(constr)) match {
      // Case 1: No constraints left. Success.
      case (Nil, subst) => Some(subst)

      // Case 2: Leftover constraints. Failure
      case (_ :: _, _) => None
    }
  }

  // Resolve all effect constraints in the set
  // (bool or something)
  // TODO examples
  // MATT do we need to ensure this happens only when it's just effects left to solve?

  def effectUnification(constr: TypeConstraint)(implicit tracker: Tracker, scope: Scope, renv: RigidityEnv, flix: Flix): (ConstraintSet, SubstitutionTree) = constr match {
    case c@TypeConstraint.Equality(tpe1, tpe2) if tpe1.kind == Kind.Eff && tpe2.kind == Kind.Eff =>
      EffUnification.unify(tpe1, tpe2, renv) match {
        case Result.Ok((subst, newConstrs)) => (newConstrs.map(toTypeConstraint2), SubstitutionTree(subst, Map()))
        case Result.Err(e) => (List(c), SubstitutionTree.empty)
      }

    case TypeConstraint.Purification(sym, eff1, eff2, nested0) =>
      val (nested, branch) = foldSubstitution(nested0)(effectUnification(_)(tracker, scope.enter(sym), renv, flix))
      val tree = SubstitutionTree.oneBranch(sym, branch)
      val cs = List(TypeConstraint.Purification(sym, eff1, eff2, nested))
      (cs, tree)

    case c => (List(c), SubstitutionTree.empty)
  }

  // MATT docs
  // MATT abstract over unification
  def recordUnification(constr: TypeConstraint)(implicit scope: Scope, tracker: Tracker, renv: RigidityEnv, eqEnv: EqualityEnv, flix: Flix): (ConstraintSet, SubstitutionTree) = constr match {
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

  // MATT docs
  def schemaUnification(constr: TypeConstraint)(implicit scope: Scope, tracker: Tracker, renv: RigidityEnv, eqEnv: EqualityEnv, flix: Flix): (ConstraintSet, SubstitutionTree) = constr match {
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

  // MATT extract this to type reduction
  def reduce(tpe0: Type)(implicit scope: Scope, tracker: Tracker, renv0: RigidityEnv, eqenv: EqualityEnv, flix: Flix): Type = tpe0 match {
    case t: Type.Var => t

    case t: Type.Cst => t

    case Type.Apply(tpe1, tpe2, loc) =>
      // TODO CONSTR-SOLVER-2 this is recursive. Might be a hot-spot for performance?
      val t1 = reduce(tpe1)
      val t2 = reduce(tpe2)
      Type.Apply(t1, t2, loc)

    case Type.Alias(cst, args, tpe, loc) => tpe

    case Type.AssocType(Ast.AssocTypeConstructor(sym, _), tpe, kind, loc) =>

      // Get all the associated types from the context
      val assocs = eqenv(sym)

      // Find the instance that matches
      val matches = assocs.flatMap {
        case Ast.AssocTypeDef(assocTpe, ret) =>
          // We fully rigidify `tpe`, because we need the substitution to go from instance type to constraint type.
          // For example, if our constraint is ToString[Map[Int32, a]] and our instance is ToString[Map[k, v]],
          // then we want the substitution to include "v -> a" but NOT "a -> v".
          val renv = tpe.typeVars.map(_.sym).foldLeft(renv0)(_.markRigid(_))

          // Instantiate all the instance constraints according to the substitution.
          fullyUnify(tpe, assocTpe, renv).map {
            case subst => subst(ret)
          }
      }

      // TODO CONSTR-SOLVER-2 ought to be exactly 0 or 1; should check in Resolver
      matches match {
        // Case 1: No match. Can't reduce the type.
        case Nil => tpe0

        // Case 2: One match. Use it.
        case newTpe :: Nil =>
          tracker.markProgress()
          newTpe

        // Case 3: Multiple matches. Give back the original type.
        // TODO CONSTR-SOLVER-2 Right resiliency strategy?
        case _ :: _ :: _ => tpe0
      }

    case Type.JvmToType(tpe, loc) =>
      reduce(tpe) match {
        case Type.Cst(TypeConstructor.JvmConstructor(constructor), _) =>
          tracker.markProgress()
          Type.getFlixType(constructor.getDeclaringClass)

        case Type.Cst(TypeConstructor.JvmMethod(method), _) =>
          tracker.markProgress()
          Type.getFlixType(method.getReturnType)

        case Type.Cst(TypeConstructor.JvmField(field), _) =>
          tracker.markProgress()
          Type.getFlixType(field.getType)

        case t => Type.JvmToType(t, loc)
      }

    case Type.JvmToEff(tpe, loc) =>
      reduce(tpe) match {
        case Type.Cst(TypeConstructor.JvmConstructor(constructor), _) =>
          tracker.markProgress()
          BaseEffects.getConstructorEffs(constructor, loc)

        case Type.Cst(TypeConstructor.JvmMethod(method), _) =>
          tracker.markProgress()
          BaseEffects.getMethodEffs(method, loc)

        case t => Type.JvmToType(t, loc)
      }

    // MATT how to organize?
    case unresolved@Type.UnresolvedJvmType(member, loc) =>
      member.map(reduce) match {
        case JvmMember.JvmConstructor(clazz, tpes) =>
          TypeReduction.lookupConstructor(clazz, tpes) match {
            case TypeReduction.JavaConstructorResolution.Resolved(constructor) =>
              tracker.markProgress()
              Type.Cst(TypeConstructor.JvmConstructor(constructor), loc)
            case _ => unresolved
          }

        case JvmMember.JvmField(tpe, name) =>
          TypeReduction.lookupField(tpe, name.name) match {
            case TypeReduction.JavaFieldResolution.Resolved(field) =>
              tracker.markProgress()
              Type.Cst(TypeConstructor.JvmField(field), loc)
            case _ => unresolved
          }

        case JvmMember.JvmMethod(tpe, name, tpes) =>
          TypeReduction.lookupMethod(tpe, name.name, tpes) match {
            case TypeReduction.JavaMethodResolution.Resolved(method) =>
              tracker.markProgress()
              Type.Cst(TypeConstructor.JvmMethod(method), loc)
            case _ => unresolved
          }

        case JvmMember.JvmStaticMethod(clazz, name, tpes) =>
          TypeReduction.lookupStaticMethod(clazz, name.name, tpes) match {
            case TypeReduction.JavaMethodResolution.Resolved(method) =>
              tracker.markProgress()
              Type.Cst(TypeConstructor.JvmMethod(method), loc)
            case _ => unresolved
          }
      }
  }

  // (redU)
  def reduceTypes(constr: TypeConstraint)(implicit scope: Scope, tracker: Tracker, renv0: RigidityEnv, eqenv: EqualityEnv, flix: Flix): TypeConstraint = constr match {
    case TypeConstraint.Equality(tpe1, tpe2) => TypeConstraint.Equality(reduce(tpe1), reduce(tpe2))
    case TypeConstraint.Trait(sym, tpe) => TypeConstraint.Trait(sym, reduce(tpe))
    case TypeConstraint.Purification(sym, eff1, eff2, nested) => TypeConstraint.Purification(sym, reduce(eff1), reduce(eff2), nested)
  }

  // Build a substitution from any variable constraints
  // (varU)
  // TODO examples
  // MATT docs
  def makeSubstitution(constr: TypeConstraint)(implicit scope: Scope, tracker: Tracker, renv: RigidityEnv): (ConstraintSet, SubstitutionTree) = constr match {
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

  // MATT docs
  def foldSubstitution(constrs: ConstraintSet)(f: TypeConstraint => (ConstraintSet, SubstitutionTree)): (ConstraintSet, SubstitutionTree) = {
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
  def toTypeConstraint(constr: Ast.TraitConstraint): TypeConstraint = constr match {
    case Ast.TraitConstraint(head, arg, loc) => TypeConstraint.Trait(head.sym, arg)
  }

  // MATT docs
  def toTypeConstraint2(constr: Ast.BroadEqualityConstraint): TypeConstraint = constr match {
    case Ast.BroadEqualityConstraint(tpe1, tpe2) => TypeConstraint.Equality(tpe1, tpe2)
  }

  // MATT docs
  def compose2(tree1: SubstitutionTree, tree2: SubstitutionTree): SubstitutionTree = (tree1, tree2) match {
    case (SubstitutionTree(root1, branches1), SubstitutionTree(root2, branches2)) =>
      val root = root1 @@ root2
      val branches = MapOps.unionWith(branches1, branches2)(compose2)
      SubstitutionTree(root, branches)
  }

  // MATT docs
  def toTypeConstraint3(constr: OldTypeConstraint): TypeConstraint = constr match {
    case OldTypeConstraint.Equality(tpe1, tpe2, prov) => TypeConstraint.Equality(tpe1, tpe2)
    case OldTypeConstraint.Trait(sym, tpe, loc) => TypeConstraint.Trait(sym, tpe)
    case OldTypeConstraint.Purification(sym, eff1, eff2, prov, nested) => TypeConstraint.Purification(sym, eff1, eff2, nested.map(toTypeConstraint3))
  }

  // MATT docs
  def unsafeToBroadEqualityConstraint(constr: TypeConstraint): Ast.BroadEqualityConstraint = constr match {
    case TypeConstraint.Equality(tpe1, tpe2) => Ast.BroadEqualityConstraint(tpe1, tpe2)
    case c => throw InternalCompilerException("unexpected constraint: " + c, SourceLocation.Unknown)
  }

  /**
    * Returns true if the kind should be unified syntactically.
    */
  def isSyntactic(k: Kind): Boolean = k match {
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
