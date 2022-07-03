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
import ca.uwaterloo.flix.language.fmt.{Audience, FormatScheme}
import ca.uwaterloo.flix.language.phase.unification.{ClassEnvironment, Substitution, Unification, UnificationError}
import ca.uwaterloo.flix.util.Validation.ToSuccess
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}

object Scheme {

  /**
    * Instantiate one of the variables in the scheme, adding new quantifiers as needed.
    */
  def partiallyInstantiate(sc: Scheme, quantifier: Symbol.KindedTypeVarSym, value: Type)(implicit flix: Flix): Scheme = sc match {
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
    */
  def instantiate(sc: Scheme)(implicit flix: Flix): (List[Ast.TypeConstraint], Type) = {
    // Compute the base type.
    val baseType = sc.base

    //
    // Compute the fresh variables taking the instantiation mode into account.
    //
    val freshVars = sc.quantifiers.foldLeft(Map.empty[Int, Type.KindedVar]) {
      case (macc, tvar) =>
        // Determine the rigidity of the fresh type variable.
        macc + (tvar.id -> Type.freshVar(tvar.kind, tvar.loc, tvar.isRegion, Ast.VarText.Absent))
    }

    /**
      * Replaces every variable occurrence in the given type using `freeVars`. Updates the rigidity.
      */
    def visitTvar(t: Type.KindedVar): Type.KindedVar = t match {
      case Type.KindedVar(sym, loc) =>
        freshVars.get(sym.id) match {
          case None =>
            // Determine the rigidity of the free type variable.
            Type.KindedVar(sym, loc)
          case Some(tvar) => tvar
        }
    }

    val newBase = baseType.map(visitTvar)

    val newConstrs = sc.constraints.map {
      case Ast.TypeConstraint(head, tpe0, loc) =>
        val tpe = tpe0.map(visitTvar)
        Ast.TypeConstraint(head, tpe, loc)
    }

    (newConstrs, newBase)
  }

  /**
    * Generalizes the given type `tpe0` with respect to the empty type environment.
    */
  def generalize(tconstrs: List[Ast.TypeConstraint], tpe0: Type): Scheme = {
    val quantifiers = tpe0.typeVars ++ tconstrs.flatMap(tconstr => tconstr.arg.typeVars)
    Scheme(quantifiers.toList.map(_.sym), tconstrs, tpe0)
  }

  /**
    * Returns `true` if the given schemes are equivalent.
    */
  // TODO can optimize?
  def equal(sc1: Scheme, sc2: Scheme, classEnv: Map[Symbol.ClassSym, Ast.ClassContext])(implicit flix: Flix): Boolean = {
    lessThanEqual(sc1, sc2, classEnv) && lessThanEqual(sc2, sc1, classEnv)
  }

  /**
    * Returns `true` if the given scheme `sc1` is smaller or equal to the given scheme `sc2`.
    */
  def lessThanEqual(sc1: Scheme, sc2: Scheme, classEnv: Map[Symbol.ClassSym, Ast.ClassContext])(implicit flix: Flix): Boolean = {
    checkLessThanEqual(sc1, sc2, classEnv) match {
      case Validation.Success(_) => true
      case Validation.Failure(_) => false
    }
  }

  /**
    * Returns `Success` if the given scheme `sc1` is smaller or equal to the given scheme `sc2`.
    */
  def checkLessThanEqual(sc1: Scheme, sc2: Scheme, classEnv: Map[Symbol.ClassSym, Ast.ClassContext])(implicit flix: Flix): Validation[Substitution, UnificationError] = {

    ///
    /// Special Case: If `sc1` and `sc2` are syntactically the same then `sc1` must be less than or equal to `sc2`.
    ///
    if (sc1 == sc2) {
      return Substitution.empty.toSuccess
    }

    //
    // General Case: Compute if `sc1` <= `sc2`.
    //

    // Mark every free variable in `sc1` as rigid.
    val renv1 = RigidityEnv(sc1.base.typeVars.map(_.sym) -- sc1.quantifiers)

    // Mark every free and bound variable in `sc2` as rigid.
    val renv2 = RigidityEnv(sc2.base.typeVars.map(_.sym))

    val renv = renv1 ++ renv2

    // Attempt to unify the two instantiated types.
    for {
      subst <- Unification.unifyTypes(sc1.base, sc2.base, renv).toValidation
      newTconstrs1 <- ClassEnvironment.reduce(sc1.constraints.map(subst.apply), classEnv)
      newTconstrs2 <- ClassEnvironment.reduce(sc2.constraints.map(subst.apply), classEnv)
      _ <- Validation.sequence(newTconstrs1.map(ClassEnvironment.entail(newTconstrs2, _, classEnv)))
    } yield subst
  }

}

/**
  * Representation of polytypes.
  */
case class Scheme(quantifiers: List[Symbol.KindedTypeVarSym], constraints: List[Ast.TypeConstraint], base: Type) {

  /**
    * Returns a human readable representation of the polytype.
    */
  override def toString: String = {
    FormatScheme.formatScheme(this)(Audience.Internal)
  }

}
