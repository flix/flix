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
import ca.uwaterloo.flix.language.ast.shared.Scope
import ca.uwaterloo.flix.language.ast.{Kind, Name, RigidityEnv, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.phase.typer.ConstraintSolver.ResolutionResult
import ca.uwaterloo.flix.language.phase.typer.TypeConstraint.{Equality, Provenance}
import ca.uwaterloo.flix.language.phase.unification.Substitution
import ca.uwaterloo.flix.util.InternalCompilerException

object RecordConstraintSolver {

  /**
    * Unifies the two given record types.
    */
  def solve(tpe1: Type, tpe2: Type, prov: Provenance, renv: RigidityEnv)(implicit scope: Scope, flix: Flix): ResolutionResult = (tpe1, tpe2) match {

    // ----------
    // ρ ~ ρ => ∅
    case (t1, t2) if t1 == t2 =>
      ResolutionResult.elimination

    //    α ∉ fv(ρ)
    // ----------------
    // α ~ ρ  =>  {α ↦ ρ}
    case (Type.Var(sym, _), tpe) if !tpe.typeVars.exists(_.sym == sym) && renv.isFlexible(sym) =>
      ResolutionResult.newSubst(Substitution.singleton(sym, tpe))

    //    α ∉ fv(ρ)
    // ----------------
    //  ρ ~ α  =>  {α ↦ ρ}
    case (tpe, Type.Var(sym, _)) if !tpe.typeVars.exists(_.sym == sym) && renv.isFlexible(sym) =>
      ResolutionResult.newSubst(Substitution.singleton(sym, tpe))

    // If labels match, then we compare the label types and rest of the record.
    //
    // -------------------------------------------------------------
    // ( ℓ : τ₁  | ρ₁ ) ~ ( ℓ : τ₂  | ρ₂ )  =>  { τ₁ ~ τ₂, ρ₁ ~ ρ₂ }
    case (Type.Apply(Type.Apply(Type.Cst(TypeConstructor.RecordRowExtend(label1), _), t1, _), rest1, _), Type.Apply(Type.Apply(Type.Cst(TypeConstructor.RecordRowExtend(label2), _), t2, _), rest2, _)) if label1 == label2 =>
      ResolutionResult.constraints(List(Equality(t1, t2, prov), Equality(rest1, rest2, prov)), progress = true)

    // If labels do not match, then we pivot the right record to make them match.
    //
    //        ρ₂ ~~{ℓ : τ₁}~~> { ℓ : τ₃ | ρ₃ } ; S
    // -------------------------------------------------
    // { ℓ : τ₁ | ρ₁ } ~ ρ₂  => { τ₁ ~ τ₃, ρ₁ ~ ρ₃ } ; S
    case (r1@Type.Apply(Type.Apply(Type.Cst(TypeConstructor.RecordRowExtend(label), _), t1, _), _, _), r2) =>
      pivot(r2, label, t1, r1.typeVars.map(_.sym), renv) match {
        case Some((newRow, subst)) =>
          ResolutionResult(subst, List(Equality(r1, newRow, prov)), progress = true)

        case None =>
          ResolutionResult.constraints(List(Equality(tpe1, tpe2, prov)), progress = false)
      }

    // If nothing matches, we give up and return the constraints as we got them.
    case _ => ResolutionResult.constraints(List(Equality(tpe1, tpe2, prov)), progress = false)
  }

  /**
    * Rearranges the row so that the given label is at the front.
    *
    * Returns None if no such pivot is possible.
    */
  private def pivot(row: Type, hdLabel: Name.Label, hdTpe: Type, tvars: Set[Symbol.KindedTypeVarSym], renv: RigidityEnv)(implicit scope: Scope, flix: Flix): Option[(Type, Substitution)] = row match {

    // If head labels match, then there is nothing to do. We return the same record.
    //
    // -------------------------------------------
    // { ℓ : τ₁ | ρ } ~~{ℓ : τ₂}~~> { ℓ : τ₁ | ρ }
    case r@Type.Apply(Type.Apply(Type.Cst(TypeConstructor.RecordRowExtend(label), _), _, _), _, _) if label == hdLabel =>
      Some((r, Substitution.empty))

    // If head labels do not match, we need to recurse and bring the selected label to the front.
    //
    //       ℓ₁ ≠ ℓ₂    ρ₁ ~~{ℓ₂ : τ₂}~~> { ℓ₂ : τ₃  | ρ₃ }
    // ---------------------------------------------------------
    // { ℓ₁ : τ₁ | ρ₁ } ~~{ℓ₂ : τ₂}~~> { ℓ₂ : τ₃, ℓ₁ : τ₁ | ρ₃ }
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.RecordRowExtend(label), _), tpe, _), rest, loc) =>
      pivot(rest, hdLabel, hdTpe, tvars, renv).map {
        case (Type.Apply(newHead, rest, _), subst) =>
          // The new row from the recursive call has the selected label at the head.
          // Now we just swap the new row's head with ours, to keep the selected label on top.
          val newRow = Type.Apply(newHead, Type.Apply(Type.Apply(Type.Cst(TypeConstructor.RecordRowExtend(label), loc), tpe, loc), rest, loc), loc)
          (newRow, subst)

        case _ => throw InternalCompilerException("unexpected non-record", loc)
      }

    // If we have a variable, then we can map it to a fresh record type with the selected label at the head.
    //
    //     β fresh, α ∉ fv(ρ)
    // ----------------------------------------------------
    //  α ~~{ℓ : τ}~~> { ℓ : τ | β } ; {α ↦ { ℓ : τ | β }}
    case Type.Var(sym, loc) if !tvars.contains(sym) && renv.isFlexible(sym) =>
      val tvar = Type.freshVar(Kind.RecordRow, loc)
      val newRow = Type.mkRecordRowExtend(hdLabel, hdTpe, tvar, loc)
      val subst = Substitution.singleton(sym, newRow)
      Some((newRow, subst))

    // If no rule matches, then we cannot pivot this record type.
    case _ => None
  }
}
