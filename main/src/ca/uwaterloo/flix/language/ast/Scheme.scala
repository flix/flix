/*
 * Copyright 2016 Magnus Madsen
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

package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.debug.{Audience, FormatScheme}
import ca.uwaterloo.flix.language.phase.unification.UnificationError.UnfulfilledConstraint
import ca.uwaterloo.flix.language.phase.unification.{ContextReduction, Substitution, Unification, UnificationError}
import ca.uwaterloo.flix.util.Result.{ToErr, ToOk}
import ca.uwaterloo.flix.util.collection.MultiMap
import ca.uwaterloo.flix.util.{InternalCompilerException, Result}

object Scheme {

  /**
    * A common super-type that controls how quantified and free variables are instantiated.
    */
  sealed trait InstantiateMode

  object InstantiateMode {

    /**
      * Instantiated variables are marked as flexible. Free variables are left unchanged.
      */
    case object Flexible extends InstantiateMode

    /**
      * Instantiated variables are marked as rigid. Free variables are marked as rigid (regardless of their prior rigidity).
      */
    case object Rigid extends InstantiateMode

    /**
      * Instantiated variables are marked as flexible. Free variables are marked as rigid (regardless of their prior rigidity).
      */
    case object Mixed extends InstantiateMode

  }

  /**
    * Instantiate one of the variables in the scheme, adding new quantifiers as needed.
    */
  def partiallyInstantiate(sc: Scheme, quantifier: Type.Var, value: Type)(implicit flix: Flix): Scheme = sc match {
    case Scheme(quantifiers, constraints, base) =>
      if (!quantifiers.contains(quantifier)) {
        throw InternalCompilerException("Quantifier not in scheme.")
      }
      val subst = Substitution.singleton(quantifier, value)
      val newConstraints = constraints.map(subst.apply)
      val newBase = subst(base)
      generalize(newConstraints, newBase)
  }

  /**
    * Instantiates the given type scheme `sc` by replacing all quantified variables with fresh type variables.
    *
    * The `mode` control the rigidity of quantified and free variables.
    */
  def instantiate(sc: Scheme, mode: InstantiateMode)(implicit flix: Flix): (List[TypedAst.TypeConstraint], Type) = {
    // Compute the base type.
    val baseType = sc.base

    //
    // Compute the fresh variables taking the instantiation mode into account.
    //
    val freshVars = sc.quantifiers.foldLeft(Map.empty[Int, Type.Var]) {
      case (macc, tvar) =>
        // Determine the rigidity of the fresh type variable.
        val rigidity = mode match {
          case InstantiateMode.Flexible => Rigidity.Flexible
          case InstantiateMode.Rigid => Rigidity.Rigid
          case InstantiateMode.Mixed => Rigidity.Flexible
        }
        macc + (tvar.id -> Type.freshVar(tvar.kind, rigidity))
    }

    /**
      * Replaces every variable occurrence in the given type using `freeVars`. Updates the rigidity.
      */
    def visitTvar(t: Type.Var): Type.Var = t match {
      case Type.Var(x, k, rigidity, _) =>
        freshVars.get(x) match {
          case None =>
            // Determine the rigidity of the free type variable.
            val newRigidity = mode match {
              case InstantiateMode.Flexible => rigidity
              case InstantiateMode.Rigid => Rigidity.Rigid
              case InstantiateMode.Mixed => Rigidity.Rigid
            }
            Type.Var(x, k, newRigidity)
          case Some(tvar) => tvar
        }
    }

    val newBase = baseType.map(visitTvar)

    val newConstrs = sc.constraints.map {
      case TypedAst.TypeConstraint(sym, tpe0) =>
        val tpe = tpe0.map(visitTvar)
        TypedAst.TypeConstraint(sym, tpe)
    }

    (newConstrs, newBase)
  }

  /**
    * Generalizes the given type `tpe0` with respect to the empty type environment.
    */
  def generalize(tconstrs: List[TypedAst.TypeConstraint], tpe0: Type): Scheme = {
    val quantifiers = tpe0.typeVars
    Scheme(quantifiers.toList, tconstrs, tpe0)
  }

  /**
    * Returns `true` if the given schemes are equivalent.
    */
  // TODO can optimize?
  def equal(sc1: Scheme, sc2: Scheme, instances: MultiMap[Symbol.ClassSym, ResolvedAst.Instance])(implicit flix: Flix): Boolean = {
    lessThanEqual(sc1, sc2, instances) && lessThanEqual(sc2, sc1, instances)
  }

  /**
    * Returns `true` if the given scheme `sc1` is smaller or equal to the given scheme `sc2`.
    */
  def lessThanEqual(sc1: Scheme, sc2: Scheme, instances: MultiMap[Symbol.ClassSym, ResolvedAst.Instance])(implicit flix: Flix): Boolean = {


    /**
      * Checks that `tconstr` is entailed by `tconstrs`.
      */
    def checkEntailment(tconstrs: List[TypedAst.TypeConstraint], tconstr: TypedAst.TypeConstraint): Result[Unit, UnificationError] = {
      if (ContextReduction.entail(instances, tconstrs, tconstr)) {
        ().toOk
      } else {
        UnificationError.UnfulfilledConstraint(tconstr).toErr
      }
    }

    ///
    /// Special Case: If `sc1` and `sc2` are syntactically the same then `sc1` must be less than or equal to `sc2`.
    ///
    if (sc1 == sc2) {
      return true
    }

    //
    // General Case: Compute if `sc1` <= `sc2`.
    //

    // Instantiate every variable in `sc1` as flexible and make every free variable rigid.
    val (tconstrs1, tpe1) = instantiate(sc1, InstantiateMode.Mixed)

    // Instantiate every variable in `sc2` as rigid and make every free variable rigid.
    val (tconstrs2, tpe2) = instantiate(sc2, InstantiateMode.Rigid)

    // Attempt to unify the two instantiated types.
    val result = for {
      subst <- Unification.unifyTypes(tpe1, tpe2)
      newTconstrs1 = tconstrs1.map(subst.apply)
      newTconstrs2 = tconstrs2.map(subst.apply)
      _ <- Result.sequence(newTconstrs1.map(checkEntailment(newTconstrs2, _)))
    } yield ()

    result match {
      case Result.Ok(_) => true
      case Result.Err(_) => false
    }
  }

}

/**
  * Representation of polytypes.
  */
case class Scheme(quantifiers: List[Type.Var], constraints: List[TypedAst.TypeConstraint], base: Type) {

  /**
    * Returns a human readable representation of the polytype.
    */
  override def toString: String = {
    FormatScheme.formatScheme(this)(Audience.Internal)
  }

}
