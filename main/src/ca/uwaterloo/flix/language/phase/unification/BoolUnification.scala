/*
 *  Copyright 2020 Magnus Madsen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.Scheme.InstantiateMode
import ca.uwaterloo.flix.language.ast.{Rigidity, Scheme, Type, TypeConstructor}
import ca.uwaterloo.flix.util.Result
import ca.uwaterloo.flix.util.Result.{Err, Ok}

import scala.annotation.tailrec

object BoolUnification {

  /**
    * Returns the most general unifier of the two given effects `eff1` and `eff2`.
    */
  def unifyEffects(eff1: Type, eff2: Type)(implicit flix: Flix): Result[Substitution, UnificationError] = {
    ///
    /// Return immediately if effects are disabled.
    ///
    if (flix.options.xnoeffects)
      return Ok(Substitution.empty)

    ///
    /// Perform aggressive matching to optimize for common cases.
    ///
    if (eff1 eq eff2) {
      return Ok(Substitution.empty)
    }

    eff1 match {
      case x: Type.Var if eff2 eq Type.Pure =>
        return Ok(Substitution.singleton(x, Type.Pure))
      case x: Type.Var if eff2 eq Type.Impure =>
        return Ok(Substitution.singleton(x, Type.Impure))
      case _ => // nop
    }

    eff2 match {
      case y: Type.Var if eff1 eq Type.Pure =>
        return Ok(Substitution.singleton(y, Type.Pure))
      case y: Type.Var if eff1 eq Type.Impure =>
        return Ok(Substitution.singleton(y, Type.Impure))
      case _ => // nop
    }

    ///
    /// Run the expensive boolean unification algorithm.
    ///
    booleanUnification(eff1, eff2)
  }

  /**
    * Returns the most general unifier of the two given effects `eff1` and `eff2`.
    */
  private def booleanUnification(eff1: Type, eff2: Type)(implicit flix: Flix): Result[Substitution, UnificationError] = {
    // The boolean expression we want to show is 0.
    val query = mkEq(eff1, eff2)

    // The free and flexible type (effect) variables in the query.
    val freeVars = query.typeVars.toList.filter(_.rigidity == Rigidity.Flexible)

    // Eliminate all variables.
    try {
      val subst = successiveVariableElimination(query, freeVars)

      // TODO: Debugging
      //    if (!subst.isEmpty) {
      //      val s = subst.toString
      //      val len = s.length
      //      if (len > 50) {
      //        println(s.substring(0, Math.min(len, 300)))
      //        println()
      //      }
      //    }

      Ok(subst)
    } catch {
      case BooleanUnificationException => Err(UnificationError.MismatchedEffects(eff1, eff2))
    }
  }

  /**
    * Performs success variable elimination on the given boolean expression `f`.
    */
  private def successiveVariableElimination(f: Type, fvs: List[Type.Var])(implicit flix: Flix): Substitution = fvs match {
    case Nil =>
      // Determine if f is unsatisfiable when all (rigid) variables are made flexible.
      val q = Scheme.instantiate(Scheme(f.typeVars.toList, f), InstantiateMode.Flexible)
      if (!satisfiable(q))
        Substitution.empty
      else
        throw BooleanUnificationException

    case x :: xs =>
      val t0 = Substitution.singleton(x, Type.Impure)(f) // impure == false
      val t1 = Substitution.singleton(x, Type.Pure)(f) // pure   == true
      val se = successiveVariableElimination(mkAnd(t0, t1), xs)
      val st = Substitution.singleton(x, mkOr(se(t0), mkAnd(Type.freshTypeVar(), mkNot(se(t1)))))
      st ++ se
  }

  /**
    * An exception thrown to indicate that boolean unification failed.
    */
  private case object BooleanUnificationException extends RuntimeException

  /**
    * Returns `true` if the given boolean formula `f` is satisfiable.
    */
  private def satisfiable(f: Type)(implicit flix: Flix): Boolean = f match {
    case Type.Pure => true
    case Type.Impure => false
    case _ =>
      val q = mkEq(f, Type.Pure)
      try {
        successiveVariableElimination(q, q.typeVars.toList)
        true
      } catch {
        case BooleanUnificationException => false
      }
  }


  /**
    * To unify two effects p and q it suffices to unify t = (p ∧ ¬q) ∨ (¬p ∧ q) and check t = 0.
    */
  private def mkEq(p: Type, q: Type): Type = mkOr(mkAnd(p, mkNot(q)), mkAnd(mkNot(p), q))

  /**
    * Returns the negation of the effect `eff0`.
    */
  // NB: The order of cases has been determined by code coverage analysis.
  def mkNot(eff0: Type): Type = eff0 match {
    case Type.Pure =>
      Type.Impure

    case Type.Impure =>
      Type.Pure

    case NOT(x) =>
      x

    // ¬(¬x ∨ y) => x ∧ ¬y
    case OR(NOT(x), y) =>
      mkAnd(x, mkNot(y))

    // ¬(x ∨ ¬y) => ¬x ∧ y
    case OR(x, NOT(y)) =>
      mkAnd(mkNot(x), y)

    case _ => Type.Apply(Type.Cst(TypeConstructor.Not), eff0)
  }

  /**
    * Returns the conjunction of the two effects `eff1` and `eff2`.
    */
  // NB: The order of cases has been determined by code coverage analysis.
  @tailrec
  def mkAnd(eff1: Type, eff2: Type): Type = (eff1, eff2) match {
    // T ∧ x => x
    case (Type.Pure, _) =>
      eff2

    // x ∧ T => x
    case (_, Type.Pure) =>
      eff1

    // F ∧ x => F
    case (Type.Impure, _) =>
      Type.Impure

    // x ∧ F => F
    case (_, Type.Impure) =>
      Type.Impure

    // ¬x ∧ (x ∨ y) => ¬x ∧ y
    case (NOT(x1), OR(x2, y)) if x1 == x2 =>
      mkAnd(mkNot(x1), y)

    // x ∧ ¬x => F
    case (x1, NOT(x2)) if x1 == x2 =>
      Type.Impure

    // ¬x ∧ x => F
    case (NOT(x1), x2) if x1 == x2 =>
      Type.Impure

    // x ∧ (x ∧ y) => (x ∧ y)
    case (x1, AND(x2, y)) if x1 == x2 =>
      mkAnd(x1, y)

    // x ∧ (y ∧ x) => (x ∧ y)
    case (x1, AND(y, x2)) if x1 == x2 =>
      mkAnd(x1, y)

    // (x ∧ y) ∧ x) => (x ∧ y)
    case (AND(x1, y), x2) if x1 == x2 =>
      mkAnd(x1, y)

    // (x ∧ y) ∧ y) => (x ∧ y)
    case (AND(x, y1), y2) if y1 == y2 =>
      mkAnd(x, y1)

    // x ∧ (x ∨ y) => x
    case (x1, OR(x2, _)) if x1 == x2 =>
      x1

    // (x ∨ y) ∧ x => x
    case (OR(x1, _), x2) if x1 == x2 =>
      x1

    // x ∧ (y ∧ ¬x) => F
    case (x1, AND(_, NOT(x2))) if x1 == x2 =>
      Type.Impure

    // (¬x ∧ y) ∧ x => F
    case (AND(NOT(x1), _), x2) if x1 == x2 =>
      Type.Impure

    // x ∧ ¬(x ∨ y) => F
    case (x1, NOT(OR(x2, _))) if x1 == x2 =>
      Type.Impure

    // ¬(x ∨ y) ∧ x => F
    case (NOT(OR(x1, _)), x2) if x1 == x2 =>
      Type.Impure

    // x ∧ (¬x ∧ y) => F
    case (x1, AND(NOT(x2), _)) if x1 == x2 =>
      Type.Impure

    // (¬x ∧ y) ∧ x => F
    case (AND(NOT(x1), _), x2) if x1 == x2 =>
      Type.Impure

    // x ∧ x => x
    case _ if eff1 == eff2 => eff1

    case _ =>
      //      val s = s"And($eff1, $eff2)"
      //      val len = s.length
      //      if (true) {
      //        println(s.substring(0, Math.min(len, 300)))
      //      }

      Type.Apply(Type.Apply(Type.Cst(TypeConstructor.And), eff1), eff2)
  }

  /**
    * Returns the disjunction of the two effects `eff1` and `eff2`.
    */
  // NB: The order of cases has been determined by code coverage analysis.
  @tailrec
  def mkOr(eff1: Type, eff2: Type): Type = (eff1, eff2) match {
    // T ∨ x => T
    case (Type.Pure, _) =>
      Type.Pure

    // F ∨ y => y
    case (Type.Impure, _) =>
      eff2

    // x ∨ T => T
    case (_, Type.Pure) =>
      Type.Pure

    // x ∨ F => x
    case (_, Type.Impure) =>
      eff1

    // x ∨ (y ∨ x) => x ∨ y
    case (x1, OR(y, x2)) if x1 == x2 =>
      mkOr(x1, y)

    // (x ∨ y) ∨ x => x ∨ y
    case (OR(x1, y), x2) if x1 == x2 =>
      mkOr(x1, y)

    // ¬x ∨ x => T
    case (NOT(x), y) if x == y =>
      Type.Pure

    // x ∨ ¬x => T
    case (x, NOT(y)) if x == y =>
      Type.Pure

    // (¬x ∨ y) ∨ x) => T
    case (OR(NOT(x), _), y) if x == y =>
      Type.Pure

    // x ∨ (¬x ∨ y) => T
    case (x, OR(NOT(y), _)) if x == y =>
      Type.Pure

    // x ∨ (y ∧ x) => x
    case (x1, AND(_, x2)) if x1 == x2 => x1

    // (y ∧ x) ∨ x => x
    case (AND(_, x1), x2) if x1 == x2 => x1

    // x ∨ x => x
    case _ if eff1 == eff2 =>
      eff1

    case _ =>

      //              val s = s"Or($eff1, $eff2)"
      //              val len = s.length
      //              if (len > 30) {
      //                println(s.substring(0, Math.min(len, 300)))
      //              }

      Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Or), eff1), eff2)
  }

  private object NOT {
    @inline
    def unapply(eff: Type): Option[Type] = eff match {
      case Type.Apply(Type.Cst(TypeConstructor.Not), x) => Some(x)
      case _ => None
    }
  }

  private object AND {
    @inline
    def unapply(eff: Type): Option[(Type, Type)] = eff match {
      case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.And), x), y) => Some((x, y))
      case _ => None
    }
  }

  private object OR {
    @inline
    def unapply(eff: Type): Option[(Type, Type)] = eff match {
      case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Or), x), y) => Some((x, y))
      case _ => None
    }
  }

}
