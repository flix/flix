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
import ca.uwaterloo.flix.language.ast.{Ast, Kind, KindedAst, RigidityEnv, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.language.phase.typer.TypeConstraint.Provenance
import ca.uwaterloo.flix.language.phase.typer.TypeReduction.{JavaConstructorResolutionResult, JavaMethodResolutionResult}
import ca.uwaterloo.flix.language.phase.unification.Unification.getUnderOrOverAppliedError
import ca.uwaterloo.flix.language.phase.unification._
import ca.uwaterloo.flix.util.Result.Err
import ca.uwaterloo.flix.util.collection.{ListMap, ListOps}
import ca.uwaterloo.flix.util.{InternalCompilerException, Result, Validation}

import scala.annotation.tailrec
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

  def goAll(constrs0: ConstraintSet): (ConstraintSet, Substitution) = {
    var constrs = constrs0
    var subst = Substitution.empty
    var progressMade = true
    while (progressMade) {
      val tracker: Tracker = Tracker()
      val (newConstrs, newSubst) = goOne(constrs)(tracker)
      // invariant: the new subst is already applied to all the newConstrs
      constrs = newConstrs
      subst = newSubst @@ subst
      progressMade = tracker.query()
    }
    (constrs, subst)
  }

  def goOne(constrs: ConstraintSet)(implicit tracker: Tracker, traitEnv: TraitEnv): (ConstraintSet, Substitution) = {
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
  def contextReduction(constrs: ConstraintSet)(implicit tracker: Tracker, traitEnv: TraitEnv): ConstraintSet = {
    def contextReduction1(constr: TypeConstraint): ConstraintSet = constr match {
      // Case 1: Equality constraint. Do nothing.
      case c: TypeConstraint.Equality => List(c)

      // Case 2: Trait constraint. Perform context reduction.
      case c@TypeConstraint.Trait(sym, tpe) =>

        // Get all the instances from the context
        val TraitContext(supers, insts) = traitEnv(sym)

        // Find the instance that matches
        val matches = insts.flatMap {
          case Instance(instTpe, instConstrs) =>
            // TODO need a renv here. tpe must be fully rigid because we need to subst inst -> tpe
            // Instantiate all the instance constraints according to the substitution.
            fullyUnify(tpe, instTpe).map {
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
  def fullyUnify(tpe1: Type, tpe2: Type)(implicit tracker: Tracker): Option[Substitution] = {
    // unification is now defined as taking a single constraint and applying rules until it's done
    val constr = TypeConstraint.Equality(tpe1, tpe2)
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
  def reduceTypes(constrs: ConstraintSet)(implicit tracker: Tracker, eqenv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef]): ConstraintSet = {
    def reduce(tpe: Type): Type = tpe match {
      case t: Type.Var => t
      case t: Type.Cst => t
      case Type.Apply(tpe1, tpe2, loc) =>
        ??? // TODO recursive? I think with Bools it has to be recursive since they don't get broken up, but with syntactic types it should not be recursive
      case Type.Alias(cst, args, tpe, loc) => tpe
      case Type.AssocType(cst, arg, kind, loc) =>
        ??? // similar to trait stuff: look up in env etc.
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
  def makeSubstitution(constrs: ConstraintSet): (ConstraintSet, Substitution) = ???

  def toTypeConstraint(constr: Ast.TypeConstraint): TypeConstraint = constr match {
    case Ast.TypeConstraint(head, arg, loc) => TypeConstraint.Trait(head.sym, arg)
  }

}
