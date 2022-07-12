/*
 *  Copyright 2022 Matthew Lutze
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
import ca.uwaterloo.flix.language.ast.Type.eraseAliases
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.{InternalCompilerException, Result}

import scala.annotation.tailrec
import scala.collection.mutable

object SetUnification {

  /**
    * Returns the most general unifier of the two given set formulas `tpe1` and `tpe2`.
    */
  def unify(tpe1: Type, tpe2: Type, renv: RigidityEnv)(implicit flix: Flix): Result[Substitution, UnificationError] = {
    ///
    /// Perform aggressive matching to optimize for common cases.
    ///
    if (tpe1 eq tpe2) {
      return Ok(Substitution.empty)
    }

    tpe1 match {
      case x: Type.KindedVar if renv.isFlexible(x.sym) =>
        if (tpe2 eq Type.All)
          return Ok(Substitution.singleton(x.sym, Type.All))
        if (tpe2 eq Type.Empty)
          return Ok(Substitution.singleton(x.sym, Type.Empty))

      case _: Type.UnkindedVar => throw InternalCompilerException("Unexpected unkinded type variable")
      case _ => // nop
    }

    tpe2 match {
      case y: Type.KindedVar if renv.isFlexible(y.sym) =>
        if (tpe1 eq Type.All)
          return Ok(Substitution.singleton(y.sym, Type.All))
        if (tpe1 eq Type.Empty)
          return Ok(Substitution.singleton(y.sym, Type.Empty))

      case _: Type.UnkindedVar => throw InternalCompilerException("Unexpected unkinded type variable")
      case _ => // nop
    }

    ///
    /// Run the expensive boolean unification algorithm.
    ///
    booleanUnification(simplify(eraseAliases(tpe1)), simplify(eraseAliases(tpe2)), renv)
  }

  /**
    * Simplifies the type, removing trivial redundancies and the Difference construct.
    */
  private def simplify(tpe: Type): Type = tpe match {
    case COMPLEMENT(tpe1) => mkComplement(tpe1)
    case UNION(tpe1, tpe2) => mkUnion(tpe1, tpe2)
    case INTERSECTION(tpe1, tpe2) => mkIntersection(tpe1, tpe2)
    case DIFFERENCE(tpe1, tpe2) => mkDifference(tpe1, tpe2)
    case _: Type.Var => tpe
    case _: Type.Cst => tpe
    case Type.Apply(tpe1, tpe2, loc) => Type.Apply(simplify(tpe1), simplify(tpe2), loc)

    case _: Type.Alias => throw InternalCompilerException("Unexpected type alias.")
    case _: Type.Ascribe => throw InternalCompilerException("Unexpected unkinded type.")
    case _: Type.ReadWrite => throw InternalCompilerException("Unexpected unkinded type.")
    case _: Type.UnkindedArrow => throw InternalCompilerException("Unexpected unkinded type.")
  }

  /**
    * Returns the most general unifier of the two given set formulas `tpe1` and `tpe2`.
    */
  private def booleanUnification(tpe1: Type, tpe2: Type, renv: RigidityEnv)(implicit flix: Flix): Result[Substitution, UnificationError] = {
    // The boolean expression we want to show is 0.
    val query = mkEq(tpe1, tpe2)

    // Compute the variables in the query.
    val typeVars = query.typeVars.toList

    // Compute the flexible variables.
    val flexibleTypeVars = renv.getFlexibleVarsOf(typeVars)

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
      case SetUnificationException => Err(UnificationError.MismatchedBools(tpe1, tpe2)) // TODO make setty
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
    * Performs successive variable elimination on the given set expression `f`.
    */
  private def successiveVariableElimination(f: Type, fvs: List[Type.KindedVar])(implicit flix: Flix): Substitution = fvs match {
    case Nil =>
      // Determine if f is unsatisfiable when all (rigid) variables and constants are made flexible.
      if (!satisfiable(deconst(f)))
        Substitution.empty
      else
        throw SetUnificationException

    case x :: xs =>
      val t0 = Substitution.singleton(x.sym, Type.Empty)(f)
      val t1 = Substitution.singleton(x.sym, Type.All)(f)
      val se = successiveVariableElimination(mkIntersection(t0, t1), xs)

      val f1 = BoolTable.minimizeType(mkUnion(se(t0), mkIntersection(x, mkComplement(se(t1)))))
      val st = Substitution.singleton(x.sym, f1)
      st ++ se
  }

  /**
    * Transforms effect constants in the type into variables.
    */
  private def deconst(t0: Type)(implicit flix: Flix): Type = {
    val eenv = mutable.Map.empty[Symbol.EffectSym, Type]

    def visit(t: Type): Type = t match {
      case Type.Cst(TypeConstructor.Effect(sym), loc) =>
        eenv.getOrElseUpdate(sym, Type.freshVar(Kind.Effect, loc, text = Ast.VarText.SourceText(sym.name)))

      case COMPLEMENT(tpe) => mkComplement(visit(tpe))
      case INTERSECTION(tpe1, tpe2) => mkIntersection(visit(tpe1), visit(tpe2))
      case UNION(tpe1, tpe2) => mkUnion(visit(tpe1), visit(tpe2))

      case Type.Apply(tpe1, tpe2, loc) => Type.Apply(visit(tpe1), visit(tpe2), loc)

      case tpe: Type.Cst => tpe
      case tpe: Type.KindedVar => tpe
      case tpe: Type.UnkindedVar => tpe

      case _: Type.Alias => throw InternalCompilerException("Unexpected type alias.")
      case _: Type.UnkindedArrow => throw InternalCompilerException("Unexpected unkinded type.")
      case _: Type.ReadWrite => throw InternalCompilerException("Unexpected unkinded type.")
      case _: Type.Ascribe => throw InternalCompilerException("Unexpected unkinded type.")
    }

    visit(t0)
  }

  /**
    * An exception thrown to indicate that boolean unification failed.
    */
  private case object SetUnificationException extends RuntimeException

  /**
    * Returns `true` if the given boolean formula `f` is satisfiable.
    */
  private def satisfiable(f: Type)(implicit flix: Flix): Boolean = f match {
    case Type.All => true
    case Type.Empty => false
    case _ =>
      val q = mkEq(f, Type.All)
      try {
        successiveVariableElimination(q, q.typeVars.toList)
        true
      } catch {
        case SetUnificationException => false
      }
  }


  /**
    * To unify two set formulas p and q it suffices to unify t = (p ∧ ¬q) ∨ (¬p ∧ q) and check t = 0.
    */
  private def mkEq(p: Type, q: Type): Type = mkUnion(mkIntersection(p, mkComplement(q)), mkIntersection(mkComplement(p), q))

  /**
    * Returns the negation of the set formula `tpe0`.
    */
  // NB: The order of cases has been determined by code coverage analysis.
  def mkComplement(tpe0: Type): Type = tpe0 match {
    case Type.All =>
      Type.Empty

    case Type.Empty =>
      Type.All

    case COMPLEMENT(x) =>
      x

    // ¬(¬x ∨ y) => x ∧ ¬y
    case UNION(COMPLEMENT(x), y) =>
      mkIntersection(x, mkComplement(y))

    // ¬(x ∨ ¬y) => ¬x ∧ y
    case UNION(x, COMPLEMENT(y)) =>
      mkIntersection(mkComplement(x), y)

    case _ => Type.Apply(Type.Cst(TypeConstructor.Complement, tpe0.loc), tpe0, tpe0.loc)
  }

  /**
    * Returns the conjunction of the two set formulas `tpe1` and `tpe2`.
    */
  // NB: The order of cases has been determined by code coverage analysis.
  @tailrec
  def mkIntersection(tpe1: Type, tpe2: Type): Type = (tpe1, tpe2) match {
    // T ∧ x => x
    case (Type.All, _) =>
      tpe2

    // x ∧ T => x
    case (_, Type.All) =>
      tpe1

    // F ∧ x => F
    case (Type.Empty, _) =>
      Type.Empty

    // x ∧ F => F
    case (_, Type.Empty) =>
      Type.Empty

    // C ∧ D => F
    case (CONSTANT(x1), CONSTANT(x2)) if x1 != x2 =>
      Type.Empty

    // C ∧ ¬D => C
    case (CONSTANT(x1), COMPLEMENT(CONSTANT(x2))) if x1 != x2 =>
      x1

    // ¬C ∧ D => D
    case (COMPLEMENT(CONSTANT(x1)), CONSTANT(x2)) if x1 != x2 =>
      x2

    // ¬x ∧ (x ∨ y) => ¬x ∧ y
    case (COMPLEMENT(x1), UNION(x2, y)) if x1 == x2 =>
      mkIntersection(mkComplement(x1), y)

    // x ∧ ¬x => F
    case (x1, COMPLEMENT(x2)) if x1 == x2 =>
      Type.Empty

    // ¬x ∧ x => F
    case (COMPLEMENT(x1), x2) if x1 == x2 =>
      Type.Empty

    // x ∧ (x ∧ y) => (x ∧ y)
    case (x1, INTERSECTION(x2, y)) if x1 == x2 =>
      mkIntersection(x1, y)

    // x ∧ (y ∧ x) => (x ∧ y)
    case (x1, INTERSECTION(y, x2)) if x1 == x2 =>
      mkIntersection(x1, y)

    // (x ∧ y) ∧ x) => (x ∧ y)
    case (INTERSECTION(x1, y), x2) if x1 == x2 =>
      mkIntersection(x1, y)

    // (x ∧ y) ∧ y) => (x ∧ y)
    case (INTERSECTION(x, y1), y2) if y1 == y2 =>
      mkIntersection(x, y1)

    // x ∧ (x ∨ y) => x
    case (x1, UNION(x2, _)) if x1 == x2 =>
      x1

    // (x ∨ y) ∧ x => x
    case (UNION(x1, _), x2) if x1 == x2 =>
      x1

    // x ∧ (y ∧ ¬x) => F
    case (x1, INTERSECTION(_, COMPLEMENT(x2))) if x1 == x2 =>
      Type.Empty

    // (¬x ∧ y) ∧ x => F
    case (INTERSECTION(COMPLEMENT(x1), _), x2) if x1 == x2 =>
      Type.Empty

    // x ∧ ¬(x ∨ y) => F
    case (x1, COMPLEMENT(UNION(x2, _))) if x1 == x2 =>
      Type.Empty

    // ¬(x ∨ y) ∧ x => F
    case (COMPLEMENT(UNION(x1, _)), x2) if x1 == x2 =>
      Type.Empty

    // x ∧ (¬x ∧ y) => F
    case (x1, INTERSECTION(COMPLEMENT(x2), _)) if x1 == x2 =>
      Type.Empty

    // (¬x ∧ y) ∧ x => F
    case (INTERSECTION(COMPLEMENT(x1), _), x2) if x1 == x2 =>
      Type.Empty

    // x ∧ x => x
    case _ if tpe1 == tpe2 => tpe1

    case _ =>
      //      val s = s"And($eff1, $eff2)"
      //      val len = s.length
      //      if (true) {
      //        println(s.substring(0, Math.min(len, 300)))
      //      }

      Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Intersection, tpe1.loc), tpe1, tpe1.loc), tpe2, tpe1.loc)
  }

  /**
    * Returns the disjunction of the two set formulas `tpe1` and `tpe2`.
    */
  // NB: The order of cases has been determined by code coverage analysis.
  @tailrec
  def mkUnion(tpe1: Type, tpe2: Type): Type = (tpe1, tpe2) match {
    // T ∨ x => T
    case (Type.All, _) =>
      Type.All

    // F ∨ y => y
    case (Type.Empty, _) =>
      tpe2

    // x ∨ T => T
    case (_, Type.All) =>
      Type.All

    // x ∨ F => x
    case (_, Type.Empty) =>
      tpe1

    // x ∨ (y ∨ x) => x ∨ y
    case (x1, UNION(y, x2)) if x1 == x2 =>
      mkUnion(x1, y)

    // (x ∨ y) ∨ x => x ∨ y
    case (UNION(x1, y), x2) if x1 == x2 =>
      mkUnion(x1, y)

    // ¬x ∨ x => T
    case (COMPLEMENT(x), y) if x == y =>
      Type.All

    // x ∨ ¬x => T
    case (x, COMPLEMENT(y)) if x == y =>
      Type.All

    // (¬x ∨ y) ∨ x) => T
    case (UNION(COMPLEMENT(x), _), y) if x == y =>
      Type.All

    // x ∨ (¬x ∨ y) => T
    case (x, UNION(COMPLEMENT(y), _)) if x == y =>
      Type.All

    // x ∨ (y ∧ x) => x
    case (x1, INTERSECTION(_, x2)) if x1 == x2 => x1

    // (y ∧ x) ∨ x => x
    case (INTERSECTION(_, x1), x2) if x1 == x2 => x1

    // x ∨ x => x
    case _ if tpe1 == tpe2 =>
      tpe1

    case _ =>

      //              val s = s"Or($eff1, $eff2)"
      //              val len = s.length
      //              if (len > 30) {
      //                println(s.substring(0, Math.min(len, 300)))
      //              }

      Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Union, tpe1.loc), tpe1, tpe1.loc), tpe2, tpe1.loc)
  }

  /**
    * Returns the difference of the given types.
    */
  def mkDifference(tpe1: Type, tpe2: Type): Type = mkIntersection(tpe1, mkComplement(tpe2))

  private object COMPLEMENT {
    @inline
    def unapply(tpe: Type): Option[Type] = tpe match {
      case Type.Apply(Type.Cst(TypeConstructor.Complement, _), x, _) => Some(x)
      case _ => None
    }
  }

  private object INTERSECTION {
    @inline
    def unapply(tpe: Type): Option[(Type, Type)] = tpe match {
      case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Intersection, _), x, _), y, _) => Some((x, y))
      case _ => None
    }
  }

  private object UNION {
    @inline
    def unapply(tpe: Type): Option[(Type, Type)] = tpe match {
      case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Union, _), x, _), y, _) => Some((x, y))
      case _ => None
    }
  }

  private object DIFFERENCE {
    @inline
    def unapply(tpe: Type): Option[(Type, Type)] = tpe match {
      case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Difference, _), x, _), y, _) => Some((x, y))
      case _ => None
    }
  }

  private object CONSTANT {
    @inline
    def unapply(tpe: Type): Option[Type] = tpe match {
      case Type.Cst(TypeConstructor.Effect(sym), _) => Some(tpe)
      case _ => None
    }
  }

}
