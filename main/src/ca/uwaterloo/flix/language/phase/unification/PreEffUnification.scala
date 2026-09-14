/*
 *  Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.language.ast.shared.RegionScope
import ca.uwaterloo.flix.language.ast.{Kind, RigidityEnv, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.phase.typer.TypeConstraint

import scala.annotation.tailrec

/**
  * A syntactic pre-solver for effect equations.
  *
  * Eliminates atomic equations by pure variable elimination, without the set algebra
  * machinery of [[EffUnification3]].
  *
  * For example, given the equation system:
  *
  *   - `ef1 ~ ef2`
  *   - `ef2 ~ IO`
  *   - `ef3 ~ Pure`
  *
  * every equation is atomic, so the system is solved exactly by the substitution
  * `{ef1 ↦ IO, ef2 ↦ IO, ef3 ↦ Pure}` ([[PreSolveResult.Solved]]). If the system also
  * contained `ef4 ~ ef1 ∪ Crash` then that equation is not atomic, so the whole system
  * is deferred to the full solver ([[PreSolveResult.Opaque]]).
  */
object PreEffUnification {

  /** The result of [[preSolve]]. */
  sealed trait PreSolveResult

  object PreSolveResult {
    /** Every equation was atomic and the system was solved exactly by `subst`. */
    case class Solved(subst: Substitution) extends PreSolveResult

    /** The system is not fully atomic; defer it entirely to the full solver. */
    case object Opaque extends PreSolveResult
  }

  /**
    * Eliminates the atomic equations of the given system syntactically, without the set
    * algebra machinery.
    *
    * An equation is atomic if it relates two atoms (variables, `Pure`, effect applications, or
    * regions) where at least one side is a flexible variable (or the sides are identical).
    * Such equations are solvable by pure variable elimination: atoms need no normalization,
    * and the variable orientation mirrors `Equation.mk` (a variable on the right-hand side
    * is bound first) and the elimination order of `SetUnification` (in equation order).
    *
    * If every equation is atomic the system is solved exactly ([[PreSolveResult.Solved]]).
    * If any equation is non-atomic, an atomic conflict, or relates rigid variables, the whole
    * system is deferred to the full solver ([[PreSolveResult.Opaque]]), which must report the
    * conflict or solve it with subeffecting.
    */
  def preSolve(eqs: List[TypeConstraint.Equality])(implicit scope: RegionScope, renv: RigidityEnv): PreSolveResult = {
    var m = Map.empty[Symbol.KindedTypeVarSym, Type]

    // The equations that remain to be processed.
    var queue = eqs
    while (queue.nonEmpty) {
      val eq = queue.head
      queue = queue.tail
      if (isAtom(eq.tpe1) && isAtom(eq.tpe2)) {
        val t1 = resolve(eq.tpe1, m)
        val t2 = resolve(eq.tpe2, m)
        if (sameAtom(t1, t2)) {
          () // The equation is trivially satisfied.
        } else {
          (t1, t2) match {
            case (_, Type.Var(sym, _)) if renv.isFlexible(sym) =>
              m = m.updated(sym, t1)
            case (Type.Var(sym, _), _) if renv.isFlexible(sym) =>
              m = m.updated(sym, t2)
            case _ =>
              // Atomic conflict or rigid variables: defer the whole system to the full solver.
              return PreSolveResult.Opaque
          }
        }
      } else {
        // Non-atomic equation: defer the whole system to the full solver.
        return PreSolveResult.Opaque
      }
    }
    // Every equation was atomic and eliminated; the system is solved exactly.
    PreSolveResult.Solved(mkSubstitution(m))
  }

  /**
    * Returns a [[Substitution]] from the binding map `m` with every range path-compressed,
    * so the substitution needs no repeated application.
    *
    * Performance: Ranges are resolved at insertion time, so an entry is stale only if its
    * range variable was bound later. Skips the rebuild unless some entry needs compression.
    */
  private def mkSubstitution(m: Map[Symbol.KindedTypeVarSym, Type]): Substitution = {
    val needsCompression = m.exists {
      case (_, Type.Var(sym, _)) => m.contains(sym)
      case _ => false
    }
    val compressed = if (needsCompression) m.map { case (k, v) => k -> resolve(v, m) } else m
    Substitution(compressed)
  }

  /**
    * Returns `true` if `t` is an atom: a variable, `Pure`, an effect application, or a region.
    */
  private def isAtom(t: Type): Boolean = t match {
    case Type.Var(_, _) => true
    case Type.Cst(TypeConstructor.Pure, _) => true
    case Type.Cst(TypeConstructor.Effect(_, _), _) => true
    case Type.Cst(TypeConstructor.Region(_), _) => true
    case app@Type.Apply(_, _, _) if app.kind == Kind.Eff => app.baseType match {
      case Type.Cst(TypeConstructor.Effect(_, _), _) => true
      case _ => false
    }
    case _ => false
  }

  /**
    * Returns `true` if `t1` and `t2` are the same atom, ignoring source locations.
    *
    * Assumes that `t1` and `t2` satisfy [[isAtom]].
    */
  private def sameAtom(t1: Type, t2: Type): Boolean = t1 == t2

  /**
    * Follows variable bindings in `m` to the representative of `t`.
    *
    * Terminates since the bindings in `m` are acyclic.
    */
  @tailrec
  private def resolve(t: Type, m: Map[Symbol.KindedTypeVarSym, Type]): Type = t match {
    case Type.Var(sym, _) => m.get(sym) match {
      case Some(t2) => resolve(t2, m)
      case None => t
    }
    case _ => t
  }

}
