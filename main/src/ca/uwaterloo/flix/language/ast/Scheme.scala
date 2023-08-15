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
import ca.uwaterloo.flix.language.fmt.{FormatOptions, FormatScheme}
import ca.uwaterloo.flix.language.phase.unification.{ClassEnvironment, EqualityEnvironment, Substitution, Unification, UnificationError}
import ca.uwaterloo.flix.util.Validation.{ToSuccess, flatMapN, mapN}
import ca.uwaterloo.flix.util.collection.ListMap
import ca.uwaterloo.flix.util.{InternalCompilerException, Result, Validation}

object Scheme {

  /**
    * Instantiate one of the variables in the scheme, adding new quantifiers as needed.
    */
  def partiallyInstantiate(sc: Scheme, quantifier: Symbol.KindedTypeVarSym, value: Type, loc: SourceLocation)(implicit flix: Flix): Scheme = sc match {
    case Scheme(quantifiers, tconstrs, econstrs, base) =>
      if (!quantifiers.contains(quantifier)) {
        throw InternalCompilerException("Quantifier not in scheme.", loc)
      }
      val subst = Substitution.singleton(quantifier, value)
      val newTconstrs = tconstrs.map(subst.apply)
      val newEconstrs = econstrs.map(subst.apply)
      val newBase = subst(base)
      generalize(newTconstrs, newEconstrs, newBase, RigidityEnv.empty)
  }

  /**
    * Instantiates the given type scheme `sc` by replacing all quantified variables with fresh type variables.
    */
  def instantiate(sc: Scheme, loc: SourceLocation)(implicit flix: Flix): (List[Ast.TypeConstraint], Type) = {
    // Compute the base type.
    val baseType = sc.base

    //
    // Compute the fresh variables taking the instantiation mode into account.
    //
    val freshVars = sc.quantifiers.foldLeft(Map.empty[Int, Type.Var]) {
      case (macc, tvar) =>
        // Determine the rigidity of the fresh type variable.
        macc + (tvar.id -> Type.freshVar(tvar.kind, loc, tvar.isRegion, Ast.VarText.Absent))
    }

    /**
      * Replaces every variable occurrence in the given type using `freeVars`.
      *
      * Replaces all source locations by `loc`.
      */
    def visitType(t: Type): Type = t match {
      case Type.Var(sym, _) => freshVars.getOrElse(sym.id, t)
      case Type.Cst(tc, _) => Type.Cst(tc, loc)
      case Type.Apply(tpe1, tpe2, _) => Type.Apply(visitType(tpe1), visitType(tpe2), loc)
      case Type.Alias(sym, args, tpe, _) => Type.Alias(sym, args.map(visitType), visitType(tpe), loc)
      case Type.AssocType(sym, args, kind, _) => Type.AssocType(sym, args.map(visitType), kind, loc)
    }

    val newBase = visitType(baseType)

    val newConstrs = sc.tconstrs.map {
      case Ast.TypeConstraint(head, tpe0, loc) =>
        val tpe = tpe0.map(visitType)
        Ast.TypeConstraint(head, tpe, loc)
    }

    (newConstrs, newBase)
  }

  /**
    * Generalizes the given type `tpe0` with respect to the empty type environment.
    */
  def generalize(tconstrs: List[Ast.TypeConstraint], econstrs: List[Ast.BroadEqualityConstraint], tpe0: Type, renv: RigidityEnv): Scheme = {
    val tvars = tpe0.typeVars ++ tconstrs.flatMap(tconstr => tconstr.arg.typeVars) ++ econstrs.flatMap(econstr => econstr.tpe1.typeVars ++ econstr.tpe2.typeVars)
    val quantifiers = renv.getFlexibleVarsOf(tvars.toList)
    Scheme(quantifiers.map(_.sym), tconstrs, econstrs, tpe0)
  }

  /**
    * Returns `true` if the given schemes are equivalent.
    */
  // TODO can optimize?
  def equal(sc1: Scheme, sc2: Scheme, classEnv: Map[Symbol.ClassSym, Ast.ClassContext], eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Boolean = {
    lessThanEqual(sc1, sc2, classEnv, eqEnv) && lessThanEqual(sc2, sc1, classEnv, eqEnv)
  }

  /**
    * Returns `true` if the given scheme `sc1` is smaller or equal to the given scheme `sc2`.
    */
  def lessThanEqual(sc1: Scheme, sc2: Scheme, classEnv: Map[Symbol.ClassSym, Ast.ClassContext], eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Boolean = {
    checkLessThanEqual(sc1, sc2, classEnv, eqEnv) match {
      case Validation.Success(_) => true
      case _failure => false
    }
  }

  /**
    * Returns `Success` if the given scheme `sc1` is smaller or equal to the given scheme `sc2`.
    */
  def checkLessThanEqual(sc1: Scheme, sc2: Scheme, classEnv: Map[Symbol.ClassSym, Ast.ClassContext], eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Validation[Substitution, UnificationError] = {

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
    flatMapN(Unification.unifyTypes(sc1.base, sc2.base, renv, LevelEnv.Top).toValidation) {
      case (subst, econstrs) => // TODO ASSOC-TYPES consider econstrs
        val newTconstrs1Val = ClassEnvironment.reduce(sc1.tconstrs.map(subst.apply), classEnv)
        val newTconstrs2Val = ClassEnvironment.reduce(sc2.tconstrs.map(subst.apply), classEnv)
        flatMapN(newTconstrs1Val, newTconstrs2Val) {
          case (newTconstrs1, newTconstrs2) =>
            flatMapN(Validation.sequence(newTconstrs1.map(ClassEnvironment.entail(newTconstrs2, _, classEnv)))) {
              case _ =>
                val newEconstrs1 = sc1.econstrs.map(subst.apply) // TODO ASSOC-TYPES reduce
                val newEconstrs2 = sc2.econstrs.map(subst.apply).map(EqualityEnvironment.narrow) // TODO ASSOC-TYPES reduce, unsafe narrowing here
                // ensure the eqenv entails the constraints and build up the substitution
                val substVal = Validation.fold(newEconstrs1, Substitution.empty) {
                  case (subst, econstr) => EqualityEnvironment.entail(newEconstrs2, subst(econstr), renv, eqEnv).map(_ @@ subst)
                }
                mapN(substVal) {
                  // combine the econstr substitution with the base type substitution
                  case s => s @@ subst
                }
            }

        }
    }
  }

}

/**
  * Representation of polytypes.
  */
case class Scheme(quantifiers: List[Symbol.KindedTypeVarSym], tconstrs: List[Ast.TypeConstraint], econstrs: List[Ast.BroadEqualityConstraint], base: Type) {

  /**
    * Returns a human readable representation of the polytype.
    */
  override def toString: String = {
    FormatScheme.formatSchemeWithOptions(this, FormatOptions.Internal)
  }

}
