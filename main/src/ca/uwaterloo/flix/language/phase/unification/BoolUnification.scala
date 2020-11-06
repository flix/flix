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
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.util.Result
import ca.uwaterloo.flix.util.Result.{Err, Ok}

import scala.annotation.tailrec

object BoolUnification {

  /**
    * Returns the most general unifier of the two given Boolean formulas `tpe1` and `tpe2`.
    */
  def unify(tpe1: Type, tpe2: Type)(implicit flix: Flix): Result[Substitution, UnificationError] = {
    ///
    /// Return immediately if Boolean unification is disabled.
    ///
    if (flix.options.xnoboolunification)
      return Ok(Substitution.empty)

    ///
    /// Perform aggressive matching to optimize for common cases.
    ///
    if (tpe1 eq tpe2) {
      return Ok(Substitution.empty)
    }

    tpe1 match {
      case x: Type.Var if x.rigidity eq Rigidity.Flexible =>
        if (tpe2 eq Type.True)
          return Ok(Substitution.singleton(x, Type.True))
        if (tpe2 eq Type.False)
          return Ok(Substitution.singleton(x, Type.False))
      case _ => // nop
    }

    tpe2 match {
      case y: Type.Var if y.rigidity eq Rigidity.Flexible =>
        if (tpe1 eq Type.True)
          return Ok(Substitution.singleton(y, Type.True))
        if (tpe1 eq Type.False)
          return Ok(Substitution.singleton(y, Type.False))
      case _ => // nop
    }

    ///
    /// Run the expensive boolean unification algorithm.
    ///
    booleanUnification(tpe1, tpe2)
  }

  /**
    * Returns the most general unifier of the two given Boolean formulas `tpe1` and `tpe2`.
    */
  private def booleanUnification(tpe1: Type, tpe2: Type)(implicit flix: Flix): Result[Substitution, UnificationError] = {
    // The boolean expression we want to show is 0.
    val query = mkEq(tpe1, tpe2)

    // The free and flexible type variables in the query.
    val freeVars = query.typeVars.toList.filter(_.rigidity == Rigidity.Flexible)

    // Eliminate all variables.
    try {
      val subst = successiveVariableElimination(query, freeVars)

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
      case BooleanUnificationException => Err(UnificationError.MismatchedBools(tpe1, tpe2))
    }
  }

  /**
    * Performs success variable elimination on the given boolean expression `f`.
    */
  private def successiveVariableElimination(f: Type, fvs: List[Type.Var])(implicit flix: Flix): Substitution = fvs match {
    case Nil =>
      // Determine if f is unsatisfiable when all (rigid) variables are made flexible.
      val (_, q) = Scheme.instantiate(Scheme(f.typeVars.toList, List.empty, f), InstantiateMode.Flexible)
      if (!satisfiable(q))
        Substitution.empty
      else
        throw BooleanUnificationException

    case x :: xs =>
      val t0 = Substitution.singleton(x, Type.False)(f)
      val t1 = Substitution.singleton(x, Type.True)(f)
      val se = successiveVariableElimination(mkAnd(t0, t1), xs)
      val st = Substitution.singleton(x, mkOr(se(t0), mkAnd(Type.freshVar(Kind.Bool), mkNot(se(t1)))))
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
    case Type.True => true
    case Type.False => false
    case _ =>
      val q = mkEq(f, Type.True)
      try {
        successiveVariableElimination(q, q.typeVars.toList)
        true
      } catch {
        case BooleanUnificationException => false
      }
  }


  /**
    * To unify two Boolean formulas p and q it suffices to unify t = (p ∧ ¬q) ∨ (¬p ∧ q) and check t = 0.
    */
  private def mkEq(p: Type, q: Type): Type = mkOr(mkAnd(p, mkNot(q)), mkAnd(mkNot(p), q))

  /**
    * Returns the negation of the Boolean formula `tpe0`.
    */
  // NB: The order of cases has been determined by code coverage analysis.
  def mkNot(tpe0: Type): Type = tpe0 match {
    case Type.True =>
      Type.False

    case Type.False =>
      Type.True

    case NOT(x) =>
      x

    // ¬(¬x ∨ y) => x ∧ ¬y
    case OR(NOT(x), y) =>
      mkAnd(x, mkNot(y))

    // ¬(x ∨ ¬y) => ¬x ∧ y
    case OR(x, NOT(y)) =>
      mkAnd(mkNot(x), y)

    case _ => Type.Apply(Type.Not, tpe0)
  }

  /**
    * Returns the conjunction of the two Boolean formulas `tpe1` and `tpe2`.
    */
  // NB: The order of cases has been determined by code coverage analysis.
  @tailrec
  def mkAnd(tpe1: Type, tpe2: Type): Type = (tpe1, tpe2) match {
    // T ∧ x => x
    case (Type.True, _) =>
      tpe2

    // x ∧ T => x
    case (_, Type.True) =>
      tpe1

    // F ∧ x => F
    case (Type.False, _) =>
      Type.False

    // x ∧ F => F
    case (_, Type.False) =>
      Type.False

    // ¬x ∧ (x ∨ y) => ¬x ∧ y
    case (NOT(x1), OR(x2, y)) if x1 == x2 =>
      mkAnd(mkNot(x1), y)

    // x ∧ ¬x => F
    case (x1, NOT(x2)) if x1 == x2 =>
      Type.False

    // ¬x ∧ x => F
    case (NOT(x1), x2) if x1 == x2 =>
      Type.False

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
      Type.False

    // (¬x ∧ y) ∧ x => F
    case (AND(NOT(x1), _), x2) if x1 == x2 =>
      Type.False

    // x ∧ ¬(x ∨ y) => F
    case (x1, NOT(OR(x2, _))) if x1 == x2 =>
      Type.False

    // ¬(x ∨ y) ∧ x => F
    case (NOT(OR(x1, _)), x2) if x1 == x2 =>
      Type.False

    // x ∧ (¬x ∧ y) => F
    case (x1, AND(NOT(x2), _)) if x1 == x2 =>
      Type.False

    // (¬x ∧ y) ∧ x => F
    case (AND(NOT(x1), _), x2) if x1 == x2 =>
      Type.False

    // x ∧ x => x
    case _ if tpe1 == tpe2 => tpe1

    case _ =>
      //      val s = s"And($eff1, $eff2)"
      //      val len = s.length
      //      if (true) {
      //        println(s.substring(0, Math.min(len, 300)))
      //      }

      Type.Apply(Type.Apply(Type.And, tpe1), tpe2)
  }

  /**
    * Returns the disjunction of the two Boolean formulas `tpe1` and `tpe2`.
    */
  // NB: The order of cases has been determined by code coverage analysis.
  @tailrec
  def mkOr(tpe1: Type, tpe2: Type): Type = (tpe1, tpe2) match {
    // T ∨ x => T
    case (Type.True, _) =>
      Type.True

    // F ∨ y => y
    case (Type.False, _) =>
      tpe2

    // x ∨ T => T
    case (_, Type.True) =>
      Type.True

    // x ∨ F => x
    case (_, Type.False) =>
      tpe1

    // x ∨ (y ∨ x) => x ∨ y
    case (x1, OR(y, x2)) if x1 == x2 =>
      mkOr(x1, y)

    // (x ∨ y) ∨ x => x ∨ y
    case (OR(x1, y), x2) if x1 == x2 =>
      mkOr(x1, y)

    // ¬x ∨ x => T
    case (NOT(x), y) if x == y =>
      Type.True

    // x ∨ ¬x => T
    case (x, NOT(y)) if x == y =>
      Type.True

    // (¬x ∨ y) ∨ x) => T
    case (OR(NOT(x), _), y) if x == y =>
      Type.True

    // x ∨ (¬x ∨ y) => T
    case (x, OR(NOT(y), _)) if x == y =>
      Type.True

    // x ∨ (y ∧ x) => x
    case (x1, AND(_, x2)) if x1 == x2 => x1

    // (y ∧ x) ∨ x => x
    case (AND(_, x1), x2) if x1 == x2 => x1

    // x ∨ x => x
    case _ if tpe1 == tpe2 =>
      tpe1

    case _ =>

      //              val s = s"Or($eff1, $eff2)"
      //              val len = s.length
      //              if (len > 30) {
      //                println(s.substring(0, Math.min(len, 300)))
      //              }

      Type.Apply(Type.Apply(Type.Or, tpe1), tpe2)
  }

  private object NOT {
    @inline
    def unapply(tpe: Type): Option[Type] = tpe match {
      case Type.Apply(Type.Cst(TypeConstructor.Not, _), x) => Some(x)
      case _ => None
    }
  }

  private object AND {
    @inline
    def unapply(tpe: Type): Option[(Type, Type)] = tpe match {
      case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.And, _), x), y) => Some((x, y))
      case _ => None
    }
  }

  private object OR {
    @inline
    def unapply(tpe: Type): Option[(Type, Type)] = tpe match {
      case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Or, _), x), y) => Some((x, y))
      case _ => None
    }
  }

}
