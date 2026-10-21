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
  *
  * Atomic subeffect constraints are handled the same way: `IO ⊑ ef5` is solved by `ef5 ↦ IO`
  * (the minimal solution), `ef6 ⊑ IO` by `ef6 ↦ ef6 ∩ IO`, and `Pure ⊑ ef7` is dropped.
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
    * Eliminates the atomic equations and subeffect constraints of the given system syntactically,
    * without the set algebra machinery.
    *
    * An equation is atomic if it relates two atoms (variables, `Pure`, effect applications, or
    * regions) where at least one side is a flexible variable (or the sides are identical).
    * Such equations are solvable by pure variable elimination: atoms need no normalization,
    * and the variable orientation mirrors `Equation.mk` (a variable on the right-hand side
    * is bound first) and the elimination order of `SetUnification` (in equation order).
    *
    * A subeffect constraint `s ⊑ t` is atomic under the same conditions. It is dropped if it holds
    * trivially (`s` is `Pure` or the sides are identical). If its upper bound `t` is a flexible
    * variable it is solved under the strict reading `t ~ s`, which picks the minimal solution
    * exactly as the strict phase of the full solver would. Otherwise, e.g. `x ⊑ IO` with `x` a
    * flexible variable, it is solved under the lax reading by `x ↦ x ∩ IO`, which is what SVE
    * would compute. The equations are processed before the subeffect constraints, and the lax ones
    * last, so that a subeffect constraint whose sides are determined by the other constraints is
    * recognized as trivial rather than used to bind a variable.
    *
    * If every constraint is atomic the system is solved exactly ([[PreSolveResult.Solved]]).
    * If any constraint is non-atomic, an atomic conflict, or relates rigid variables, the whole
    * system is deferred to the full solver ([[PreSolveResult.Opaque]]), which must report the
    * conflict or solve it with subeffecting. In particular an atomic subeffect constraint between two
    * constants, such as `IO ⊑ Crash`, is never rejected here: the full solver decides.
    */
  def preSolve(eqs: List[TypeConstraint.Equality], subs: List[TypeConstraint.SubEff])(implicit scope: RegionScope, renv: RigidityEnv): PreSolveResult = {
    var m = Map.empty[Symbol.KindedTypeVarSym, Type]

    /**
      * Binds a flexible variable among the atoms `t1` and `t2` to the other atom.
      *
      * Returns `false` if neither is a flexible variable, in which case the system must be deferred.
      */
    def bind(t1: Type, t2: Type): Boolean = (t1, t2) match {
      case (_, Type.Var(sym, _)) if renv.isFlexible(sym) =>
        m = m.updated(sym, t1)
        true
      case (Type.Var(sym, _), _) if renv.isFlexible(sym) =>
        m = m.updated(sym, t2)
        true
      case _ =>
        // Atomic conflict or rigid variables: defer the whole system to the full solver.
        false
    }

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
        } else if (!bind(t1, t2)) {
          return PreSolveResult.Opaque
        }
      } else {
        // Non-atomic equation: defer the whole system to the full solver.
        return PreSolveResult.Opaque
      }
    }

    // The subeffect constraints whose upper bound is not a flexible variable, e.g. `x ⊑ IO`. They are
    // processed last, once every atomic binding is known, since their solution `x ↦ x ∩ IO` is not
    // an atom and must not be followed by `resolve`.
    var laxSubs: List[TypeConstraint.SubEff] = Nil

    // The subeffect constraints that remain to be processed.
    var subQueue = subs
    while (subQueue.nonEmpty) {
      val sub = subQueue.head
      subQueue = subQueue.tail
      if (isAtom(sub.eff1) && isAtom(sub.eff2)) {
        val s = resolve(sub.eff1, m)
        val t = resolve(sub.eff2, m)
        if (sameAtom(s, t) || isPure(s)) {
          () // The subeffect constraint is trivially satisfied.
        } else {
          t match {
            case Type.Var(sym, _) if renv.isFlexible(sym) =>
              // Strict reading `t ~ s`: the upper bound is bound to the lower bound.
              m = m.updated(sym, s)
            case _ =>
              laxSubs = sub :: laxSubs
          }
        }
      } else {
        // Non-atomic subeffect constraint: defer the whole system to the full solver.
        return PreSolveResult.Opaque
      }
    }

    // The upper bounds collected for each still-unbound flexible variable, e.g. `x ⊑ IO, x ⊑ ef`.
    var bounds = Map.empty[Symbol.KindedTypeVarSym, List[Type]]
    for (sub <- laxSubs) {
      val s = resolve(sub.eff1, m)
      val t = resolve(sub.eff2, m)
      if (sameAtom(s, t) || isPure(s)) {
        () // The subeffect constraint is trivially satisfied.
      } else {
        s match {
          case Type.Var(sym, _) if renv.isFlexible(sym) =>
            // Lax reading `s ∩ ¬t ~ Pure`, solved by `s ↦ s ∩ t`. The bound is recorded and applied below.
            bounds = bounds.updated(sym, t :: bounds.getOrElse(sym, Nil))
          case _ =>
            // Two constants, or a rigid variable, in a non-trivial subeffect constraint: the full solver decides.
            return PreSolveResult.Opaque
        }
      }
    }

    // Every constraint was atomic and eliminated; the system is solved exactly.
    PreSolveResult.Solved(mkSubstitution(m, bounds))
  }

  /**
    * Returns `true` if `t` is the `Pure` effect.
    */
  private def isPure(t: Type): Boolean = t match {
    case Type.Cst(TypeConstructor.Pure, _) => true
    case _ => false
  }

  /**
    * Returns a [[Substitution]] from the binding map `m` with every range path-compressed,
    * so the substitution needs no repeated application.
    *
    * `bounds` maps a flexible variable `x` that is unbound in `m` to its upper bounds `t1, .., tn`;
    * the substitution binds `x ↦ x ∩ t1 ∩ .. ∩ tn`, and a range of `m` that is such an `x` is
    * replaced by that intersection as well.
    *
    * Performance: Ranges are resolved at insertion time, so an entry is stale only if its
    * range variable was bound later. Skips the rebuild unless some entry needs compression.
    */
  private def mkSubstitution(m: Map[Symbol.KindedTypeVarSym, Type], bounds: Map[Symbol.KindedTypeVarSym, List[Type]]): Substitution = {
    val needsCompression = m.exists {
      case (_, Type.Var(sym, _)) => m.contains(sym) || bounds.contains(sym)
      case _ => false
    }
    val boundRanges = bounds.map {
      case (sym, ts) =>
        val x = Type.Var(sym, sym.loc)
        sym -> Type.mkIntersection(x :: ts, sym.loc)
    }
    val compressed = if (needsCompression) m.map {
      case (k, v) =>
        val r = resolve(v, m)
        r match {
          case Type.Var(sym, _) => k -> boundRanges.getOrElse(sym, r)
          case _ => k -> r
        }
    } else m
    Substitution(compressed ++ boundRanges)
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
