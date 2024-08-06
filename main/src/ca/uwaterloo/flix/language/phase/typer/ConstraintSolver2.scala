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

import ca.uwaterloo.flix.language.ast.Ast.{Instance, TraitContext}
import ca.uwaterloo.flix.language.ast.{Ast, RigidityEnv, Symbol, Type}
import ca.uwaterloo.flix.language.phase.unification._
import ca.uwaterloo.flix.util.collection.ListMap

import scala.util.chaining.scalaUtilChainingOps

object ConstraintSolver2 {

  sealed trait TypeConstraint

  object TypeConstraint {
    case class Equality(tpe1: Type, tpe2: Type) extends TypeConstraint

    case class Trait(sym: Symbol.TraitSym, tpe: Type) extends TypeConstraint
  }

  case class Tracker(private var progress: Boolean = false) extends AnyVal {
    def markProgress(): Unit = {
      progress = true
    }

    def query(): Boolean = {
      progress
    }
  }

  type ConstraintSet = List[TypeConstraint]
  type TraitEnv = Map[Symbol.TraitSym, TraitContext]
  type EqualityEnv = ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef]

  def goAll(constrs0: ConstraintSet)(implicit renv: RigidityEnv, trenv: TraitEnv, eqenv: EqualityEnv): (ConstraintSet, Substitution) = {
    var constrs = constrs0
    var subst = Substitution.empty
    var progressMade = true
    while (progressMade) {
      implicit val tracker: Tracker = Tracker()
      val (newConstrs, newSubst) = goOne(constrs)
      // invariant: the new subst is already applied to all the newConstrs
      constrs = newConstrs
      subst = newSubst @@ subst
      progressMade = tracker.query()
    }
    (constrs, subst)
  }

  def goOne(constrs: ConstraintSet)(implicit tracker: Tracker, renv: RigidityEnv, trenv: TraitEnv, eqenv: EqualityEnv): (ConstraintSet, Substitution) = {
    constrs
      .pipe(breakDownConstraints)
      .pipe(eliminateIdentities)
      .pipe(reduceTypes)
      .pipe(contextReduction)
      .pipe(makeSubstitution)
  }


  // Breaks down constraints syntactically
  // TODO examples
  // (appU)
  def breakDownConstraints(constrs: ConstraintSet)(implicit tracker: Tracker): ConstraintSet = {
    def breakDownConstraint(constr: TypeConstraint): ConstraintSet = constr match {
      // TODO make sure we're looking at a syntactic type
      case TypeConstraint.Equality(Type.Apply(tpe11, tpe12, _), Type.Apply(tpe21, tpe22, _)) =>
        tracker.markProgress()
        List(TypeConstraint.Equality(tpe11, tpe21), TypeConstraint.Equality(tpe12, tpe22))

      case c: TypeConstraint => List(c)
    }

    constrs.flatMap(breakDownConstraint)
  }

  // Eliminates constraints that are the same on the left and right
  // TODO examples
  // (reflU)
  def eliminateIdentities(constrs: ConstraintSet)(implicit tracker: Tracker): ConstraintSet = {
    constrs.filter {
      case TypeConstraint.Equality(tpe1, tpe2) =>
        if (tpe1 == tpe2) {
          // Case 1: Identical types. Remove and mark progress.
          tracker.markProgress()
          false
        } else {
          // Case 2: Different types. Don't remove.
          true
        }
      case TypeConstraint.Trait(_, _) => true
    }
  }

  // Reduces trait constraints
  // TODO examples
  // (bchainE) (?)
  def contextReduction(constrs: ConstraintSet)(implicit tracker: Tracker, renv0: RigidityEnv, trenv: TraitEnv, eqenv: EqualityEnv): ConstraintSet = {
    def contextReduction1(constr: TypeConstraint): ConstraintSet = constr match {
      // Case 1: Equality constraint. Do nothing.
      case c: TypeConstraint.Equality => List(c)

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

    constrs.flatMap(contextReduction1)
  }

  // TODO docs
  def fullyUnify(tpe1: Type, tpe2: Type, renv: RigidityEnv)(implicit trenv: TraitEnv, eqenv: EqualityEnv): Option[Substitution] = {
    // unification is now defined as taking a single constraint and applying rules until it's done
    val constr = TypeConstraint.Equality(tpe1, tpe2)
    implicit val r = renv
    goAll(List(constr)) match {
      // Case 1: No constraints left. Success.
      case (Nil, subst) => Some(subst)

      // Case 2: Leftover constraints. Failure
      case (_ :: _, _) => None
    }
  }

  // Resolve all effect constraints in the set
  // (bool or something)
  // TODO examples
  def effectUnification(constrs: ConstraintSet)(implicit tracker: Tracker): (ConstraintSet, Substitution) = ???

  // Evaluate type constraints as far as possible
  // (redU)
  // TODO examples
  def reduceTypes(constrs: ConstraintSet)(implicit tracker: Tracker, renv0: RigidityEnv, trenv: TraitEnv, eqenv: EqualityEnv): ConstraintSet = {
    def reduce(tpe0: Type): Type = tpe0 match {
      case t: Type.Var => t
      case t: Type.Cst => t
      case Type.Apply(tpe1, tpe2, loc) =>
        ??? // TODO recursive? I think with Bools it has to be recursive since they don't get broken up, but with syntactic types it should not be recursive
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
          case newTpe :: Nil => newTpe

          // Case 3: Multiple matches. Give back the original type.
          // TODO CONSTR-SOLVER-2 Right resiliency strategy?
          case _ :: _ :: _ => tpe0
        }
    }

    def reduceTypes1(constr: TypeConstraint): TypeConstraint = constr match {
      case TypeConstraint.Equality(tpe1, tpe2) => TypeConstraint.Equality(reduce(tpe1), reduce(tpe2))
      case TypeConstraint.Trait(sym, tpe) => TypeConstraint.Trait(sym, reduce(tpe))
    }

    constrs.map(reduceTypes1)
  }

  // Build a substitution from any variable constraints
  // (varU)
  // TODO examples
  def makeSubstitution(constrs: ConstraintSet)(implicit renv: RigidityEnv): (ConstraintSet, Substitution) = {

    def makeSubstitution1(constr: TypeConstraint): Either[TypeConstraint, Substitution] = constr match {
      // TODO occurs check
      // TODO full simplification check
      case TypeConstraint.Equality(Type.Var(sym, _), tpe2) if !renv.isRigid(sym) => Right(Substitution.singleton(sym, tpe2))
      case TypeConstraint.Equality(tpe1, Type.Var(sym, _)) if !renv.isRigid(sym) => Right(Substitution.singleton(sym, tpe1))
      case c: TypeConstraint.Equality => Left(c)
      case c: TypeConstraint.Trait => Left(c)
    }

    var subst = Substitution.empty
    val newConstrs = constrs.flatMap {
      constr =>
        makeSubstitution1(applySubst(subst)(constr)) match {
          // Case 1: No substitution. Keep the substituted constraint.
          case Left(newConstr) =>
            Some(newConstr)
          // Case 2: Substitution. Compose it with the growing substitution.
          case Right(newSubst) =>
            subst = newSubst @@ subst
            None
        }
    }

    (newConstrs, subst)
  }

  def toTypeConstraint(constr: Ast.TypeConstraint): TypeConstraint = constr match {
    case Ast.TypeConstraint(head, arg, loc) => TypeConstraint.Trait(head.sym, arg)
  }

  def applySubst(subst: Substitution)(constr: TypeConstraint): TypeConstraint = constr match {
    case TypeConstraint.Equality(tpe1, tpe2) => TypeConstraint.Equality(subst(tpe1), subst(tpe2))
    case TypeConstraint.Trait(sym, tpe) => TypeConstraint.Trait(sym, tpe)
  }

}
