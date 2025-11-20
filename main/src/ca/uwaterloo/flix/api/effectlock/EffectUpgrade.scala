/*
 * Copyright 2025 Jakob Schneider Villumsen
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
package ca.uwaterloo.flix.api.effectlock

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.shared.Scope
import ca.uwaterloo.flix.language.ast.{RigidityEnv, Scheme, Symbol, Type}
import ca.uwaterloo.flix.language.phase.typer.ConstraintSolver2
import ca.uwaterloo.flix.language.phase.unification.EqualityEnv

import scala.collection.immutable.SortedSet
import scala.collection.mutable

object EffectUpgrade {

  /**
    * Returns true if `sc1` is unifiable with `sc2` or if `sc1` is a monomorphic downgrade of `sc2`.
    */
  def isSafe(sc01: Scheme, sc02: Scheme)(implicit flix: Flix): Boolean = {
    val sc1 = alpha(sc01)
    val sc2 = alpha(sc02)
    isGeneralizable(sc1, sc2) || isSubset(sc1, sc2)
  }

  /**
    * Generalize-rule
    *
    * 𝜎1 ⊑ 𝜏2
    * -------
    * 𝜎1 ⪯ 𝜏2
    *
    */
  private def isGeneralizable(sc01: Scheme, sc02: Scheme)(implicit flix: Flix): Boolean = {
    implicit val eqEnv: EqualityEnv = EqualityEnv.empty
    val renv = RigidityEnv.apply(SortedSet.from(sc02.quantifiers))
    val unification = ConstraintSolver2.fullyUnify(sc01.base, sc02.base, Scope.Top, renv)

    unification match {
      case Some(subst) =>
        subst(sc01.base) == sc02.base

      case None =>
        false
    }
  }

  private def isSubset(sc01: Scheme, sc02: Scheme)(implicit flix: Flix): Boolean = {
    ???
  }

  private def alpha(sc0: Scheme): Scheme = {
    val seen = mutable.Map.empty[Symbol.KindedTypeVarSym, Symbol.KindedTypeVarSym]

    def visit(tpe0: Type): Type = tpe0 match {
      case Type.Var(sym, loc) => seen.get(sym) match {
        case Some(subst) => Type.Var(subst, loc)
        case None =>
          val subst = new Symbol.KindedTypeVarSym(seen.size, sym.text, sym.kind, sym.isSlack, sym.scope, sym.loc)
          seen += sym -> subst
          Type.Var(subst, loc)
      }

      case Type.Cst(_, _) =>
        tpe0

      case Type.Apply(tpe1, tpe2, loc) =>
        Type.Apply(visit(tpe1), visit(tpe2), loc)

      case Type.Alias(symUse, args, tpe, loc) =>
        Type.Alias(symUse, args.map(visit), visit(tpe), loc)

      case Type.AssocType(symUse, arg, kind, loc) =>
        Type.AssocType(symUse, visit(arg), kind, loc)

      case Type.JvmToType(tpe, loc) =>
        Type.JvmToType(visit(tpe), loc)

      case Type.JvmToEff(tpe, loc) =>
        Type.JvmToEff(visit(tpe), loc)

      case Type.UnresolvedJvmType(_, _) =>
        tpe0
    }

    val base = visit(sc0.base)
    val tconstrs = sc0.tconstrs.map(tc => tc.copy(arg = visit(tc.arg)))
    val econstrs = sc0.econstrs.map(ec => ec.copy(tpe1 = visit(ec.tpe1), tpe2 = visit(ec.tpe2)))
    val qs = sc0.quantifiers.map(q => seen.getOrElse(q, q))
    Scheme(qs, tconstrs, econstrs, base)
  }

}
