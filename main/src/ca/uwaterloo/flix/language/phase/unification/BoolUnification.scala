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
import ca.uwaterloo.flix.language.ast.Type.{Bool, eraseAliases}
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.{InternalCompilerException, Result}

import scala.annotation.tailrec

object BoolUnification {

  /**
    * Returns the most general unifier of the two given Boolean formulas `tpe1` and `tpe2`.
    */
  def unify(tpe1: Type, tpe2: Type, renv: Rigidity.Env)(implicit flix: Flix): Result[Substitution, UnificationError] = {
    ///
    /// Perform aggressive matching to optimize for common cases.
    ///
    if (tpe1 eq tpe2) {
      return Ok(Substitution.empty)
    }

    tpe1 match {
      case x: Type.KindedVar if !renv.contains(x.sym) =>
//      case x: Type.KindedVar if x.sym.rigidity eq Rigidity.Flexible =>
        if (tpe2 eq Type.True)
          return Ok(Substitution.singleton(x.sym, Type.True))
        if (tpe2 eq Type.False)
          return Ok(Substitution.singleton(x.sym, Type.False))

      case _: Type.UnkindedVar => throw InternalCompilerException("Unexpected unkinded type variable")
      case _ => // nop
    }

    tpe2 match {
      case y: Type.KindedVar if !renv.contains(y.sym) =>
//      case y: Type.KindedVar if y.sym.rigidity eq Rigidity.Flexible =>
        if (tpe1 eq Type.True)
          return Ok(Substitution.singleton(y.sym, Type.True))
        if (tpe1 eq Type.False)
          return Ok(Substitution.singleton(y.sym, Type.False))

      case _: Type.UnkindedVar => throw InternalCompilerException("Unexpected unkinded type variable")
      case _ => // nop
    }

    ///
    /// Run the expensive boolean unification algorithm.
    ///
    booleanUnification(eraseAliases(tpe1), eraseAliases(tpe2), renv)
  }

  /**
    * Returns the most general unifier of the two given Boolean formulas `tpe1` and `tpe2`.
    */
  private def booleanUnification(tpe1: Type, tpe2: Type, renv: Rigidity.Env)(implicit flix: Flix): Result[Substitution, UnificationError] = {
    // The boolean expression we want to show is 0.
    val query = mkEq(tpe1, tpe2)

    // Compute the variables in the query.
    val typeVars = query.typeVars.toList

    // Compute the flexible variables.
    val flexibleTypeVars = typeVars.filterNot(tvar => renv.contains(tvar.sym))
//    val flexibleTypeVars = typeVars.filter(_.sym.rigidity == Rigidity.Flexible)

    // Determine the order in which to eliminate the variables.
    val freeVars = computeVariableOrder(flexibleTypeVars)

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
    * A heuristic used to determine the order in which to eliminate variable.
    *
    * Semantically the order of variables is immaterial. Changing the order may
    * yield different unifiers, but they are all equivalent. However, changing
    * the can lead to significant speed-ups / slow-downs.
    *
    * We make the following observation:
    *
    * We want to have synthetic variables (i.e. fresh variables introduced during
    * type inference) expressed in terms of real variables (i.e. variables that
    * actually occur in the source code). We can ensure this by eliminating the
    * synthetic variables first.
    */
  private def computeVariableOrder(l: List[Type.KindedVar]): List[Type.KindedVar] = {
    val realVars = l.filter(_.sym.isReal)
    val synthVars = l.filterNot(_.sym.isReal)
    synthVars ::: realVars
  }

  /**
    * Performs success variable elimination on the given boolean expression `f`.
    */
  private def successiveVariableElimination(f: Type, fvs: List[Type.KindedVar])(implicit flix: Flix): Substitution = fvs match {
    case Nil =>
      // Determine if f is unsatisfiable when all (rigid) variables are made flexible.
      val (_, q) = Scheme.instantiate(Scheme(f.typeVars.toList.map(_.sym), List.empty, f), InstantiateMode.Flexible)
      if (!satisfiable(q))
        Substitution.empty
      else
        throw BooleanUnificationException

    case x :: xs =>
      val t0 = Substitution.singleton(x.sym, Type.False)(f)
      val t1 = Substitution.singleton(x.sym, Type.True)(f)
      val se = successiveVariableElimination(mkAnd(t0, t1), xs)

      val f1 = BoolTable.minimizeType(mkOr(se(t0), mkAnd(x, mkNot(se(t1)))))
      val st = Substitution.singleton(x.sym, f1)
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
      // Make all variables flexible.
      val f1 = f.map(tvar => tvar.withRigidity(Rigidity.Flexible))
      val q = mkEq(f1, Type.True)
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

    case _ => Type.Apply(Type.Not, tpe0, tpe0.loc)
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

      Type.Apply(Type.Apply(Type.And, tpe1, tpe1.loc), tpe2, tpe1.loc)
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

      Type.Apply(Type.Apply(Type.Or, tpe1, tpe1.loc), tpe2, tpe1.loc)
  }

  private object NOT {
    @inline
    def unapply(tpe: Type): Option[Type] = tpe match {
      case Type.Apply(Type.Cst(TypeConstructor.Not, _), x, _) => Some(x)
      case _ => None
    }
  }

  private object AND {
    @inline
    def unapply(tpe: Type): Option[(Type, Type)] = tpe match {
      case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.And, _), x, _), y, _) => Some((x, y))
      case _ => None
    }
  }

  private object OR {
    @inline
    def unapply(tpe: Type): Option[(Type, Type)] = tpe match {
      case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Or, _), x, _), y, _) => Some((x, y))
      case _ => None
    }
  }

}
