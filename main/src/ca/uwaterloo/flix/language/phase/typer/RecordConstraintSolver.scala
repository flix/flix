package ca.uwaterloo.flix.language.phase.typer

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.shared.Scope
import ca.uwaterloo.flix.language.ast.{Kind, Name, RigidityEnv, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.phase.typer.ConstraintSolver.ResolutionResult
import ca.uwaterloo.flix.language.phase.typer.TypeConstraint.{Equality, Provenance}
import ca.uwaterloo.flix.language.phase.unification.Substitution
import ca.uwaterloo.flix.util.InternalCompilerException

object RecordConstraintSolver {

  def solve(tpe1: Type, tpe2: Type, prov: Provenance, renv: RigidityEnv)(implicit scope: Scope, flix: Flix): ResolutionResult = (tpe1, tpe2) match {

    //
    // ------------
    // {} ~ {} => ∅
    case (Type.Cst(Type.RecordRowEmpty, _), Type.Cst(Type.RecordRowEmpty, _)) =>
      ResolutionResult.elimination

    //    α ∉ fv(ρ)
    // ----------------
    // α ~ ρ => {α ↦ ρ}
    case (Type.Var(sym, _), tpe) if (!tpe.typeVars.exists(_.sym == sym) && renv.isFlexible(sym)) =>
      ResolutionResult.newSubst(Substitution.singleton(sym, tpe))

    // ρ₂ ~~{ℓ : τ₁}~~> { ℓ : τ₃ | ρ₃ }    τ₁ ~ τ₃ => C    ρ₁ ~ ρ₃ => C'
    // -----------------------------------------------------------------
    //              { ℓ : τ₁ | ρ₁ } ~ ρ₂ => C ∪ C'
    case (r1@Type.Apply(Type.Apply(Type.Cst(TypeConstructor.RecordRowExtend(label), _), t1, _), _, _), r2) =>
      pivot(r2, label, t1, r1.typeVars.map(_.sym), renv) match {
        case Some((newRow, subst)) =>
          ResolutionResult(subst, List(Equality(r1, newRow, prov)), progress = true)

        case None =>
          ResolutionResult.constraints(List(Equality(tpe1, tpe2, prov)), progress = false)
      }

    case _ => ResolutionResult.constraints(List(Equality(tpe1, tpe2, prov)), progress = false)
  }

  /**
    * Rearranges the row so that the given label is at the front.
    */
  def pivot(row: Type, hdLabel: Name.Label, hdTpe: Type, tvars: Set[Symbol.KindedTypeVarSym], renv: RigidityEnv)(implicit scope: Scope, flix: Flix): Option[(Type, Substitution)] = row match {
    case Type.RecordRowEmpty => None

    case r@Type.Apply(Type.Apply(Type.Cst(TypeConstructor.RecordRowExtend(label), _), _, _), _, _) if label == hdLabel =>
      Some((r, Substitution.empty))

    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.RecordRowExtend(label), _), tpe, _), rest, loc) =>
      pivot(rest, hdLabel, hdTpe, tvars, renv).map {
        case (Type.Apply(newHead, rest, _), subst) =>
          val newRow = Type.Apply(newHead, Type.Apply(Type.Apply(Type.Cst(TypeConstructor.RecordRowExtend(label), loc), tpe, loc), rest, loc), loc)
          (newRow, subst)

        case _ => throw InternalCompilerException("unexpected non-record", loc)
      }

    case Type.Var(sym, loc) if !tvars.contains(sym) && renv.isFlexible(sym) =>
      val tvar = Type.freshVar(Kind.RecordRow, loc)
      val newRow = Type.mkRecordRowExtend(hdLabel, hdTpe, tvar, loc)
      val subst = Substitution.singleton(sym, newRow)
      Some((newRow, subst))

    case _ => None
  }
}
