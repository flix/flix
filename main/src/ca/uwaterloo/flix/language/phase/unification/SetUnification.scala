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
      case x: Type.Var if renv.isFlexible(x.sym) =>
        if (tpe2 eq Type.All)
          return Ok(Substitution.singleton(x.sym, Type.All))
        if (tpe2 eq Type.Empty)
          return Ok(Substitution.singleton(x.sym, Type.Empty))

      case _ => // nop
    }

    tpe2 match {
      case y: Type.Var if renv.isFlexible(y.sym) =>
        if (tpe1 eq Type.All)
          return Ok(Substitution.singleton(y.sym, Type.All))
        if (tpe1 eq Type.Empty)
          return Ok(Substitution.singleton(y.sym, Type.Empty))

      case _ => // nop
    }

    ///
    /// Run the expensive boolean unification algorithm.
    ///
    booleanUnification(eraseIo(eraseAliases(tpe1)), eraseIo(eraseAliases(tpe2)), renv)
  }

  /**
    * Erases IO effects from the type, mapping them to Empty.
    *
    * Expects a type without type aliases.
    */
  private def eraseIo(t: Type): Type = t match {
    case Type.Cst(TypeConstructor.Effect(sym), _) if sym.namespace == Nil && sym.name == "IO" => Type.Empty
    case tpe: Type.Cst => tpe
    case tpe: Type.Var => tpe
    case Type.Apply(tpe1, tpe2, loc) => Type.Apply(eraseIo(tpe1), eraseIo(tpe2), loc)

    case _: Type.Alias => throw InternalCompilerException("unexpected type alias", t.loc)
    case _: Type.AssocType => throw InternalCompilerException("unexpected associated type", t.loc) // TODO ASSOC-TYPE ???
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
  private def computeVariableOrder(l: List[Type.Var]): List[Type.Var] = {
    val realVars = l.filter(_.sym.isReal)
    val synthVars = l.filterNot(_.sym.isReal)
    synthVars ::: realVars
  }

  /**
    * Performs successive variable elimination on the given set expression `f`.
    */
  private def successiveVariableElimination(f: Type, fvs: List[Type.Var])(implicit flix: Flix): Substitution = fvs match {
    case Nil =>
      // Determine if f is necessarily empty when all (rigid) variables and constants are made flexible.
      if (isEmpty(dnf(f)))
        Substitution.empty
      else
        throw SetUnificationException

    case x :: xs =>
      val t0 = Substitution.singleton(x.sym, Type.Empty)(f)
      val t1 = Substitution.singleton(x.sym, Type.All)(f)
      val se = successiveVariableElimination(mkIntersection(t0, t1), xs)

      val f1 = TypeMinimization.minimizeType(mkUnion(se(t0), mkIntersection(x, mkComplement(se(t1)))))
      val st = Substitution.singleton(x.sym, f1)
      st ++ se
  }

  /**
    * An exception thrown to indicate that boolean unification failed.
    */
  private case object SetUnificationException extends RuntimeException

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
    case (x1@CONSTANT(_), COMPLEMENT(x2@CONSTANT(_))) if x1 != x2 =>
      x1

    // ¬C ∧ D => D
    case (COMPLEMENT(x1@CONSTANT(_)), x2@CONSTANT(_)) if x1 != x2 =>
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

  private object CONSTANT {
    @inline
    def unapply(tpe: Type): Option[Symbol.EffectSym] = tpe match {
      case Type.Cst(TypeConstructor.Effect(sym), _) => Some(sym)
      case _ => None
    }
  }

  private object VAR {
    @inline
    def unapply(tpe: Type): Option[Symbol.KindedTypeVarSym] = tpe match {
      case Type.Var(sym, _) => Some(sym)
      case _ => None
    }
  }

  /**
    * An atom is a constant or a variable.
    */
  private sealed trait Atom

  private object Atom {
    case class Var(sym: Symbol.KindedTypeVarSym) extends Atom

    case class Eff(sym: Symbol.EffectSym) extends Atom
  }

  /**
    * A literal is a negated or un-negated atom.
    */
  private sealed trait Literal

  private object Literal {
    case class Positive(atom: Atom) extends Literal

    case class Negative(atom: Atom) extends Literal
  }

  /**
    * A DNF intersection is a set of literals.
    */
  private type Intersection = Set[Literal]

  /**
    * A DNF formula is either:
    * - a union of intersections of literals.
    * - the universal set
    */
  private sealed trait Dnf

  private object Dnf {
    case class Union(inters: Set[Intersection]) extends Dnf // TODO should be a list to avoid extra comparisons?

    /**
      * The bottom value is an empty union.
      */
    val Empty = Union(Set())

    /**
      * The top value is an empty intersection.
      */
    val All = Union(Set(Set()))
  }

  /**
    * An NNF formula is a formula where all negations are on atoms.
    */
  private sealed trait Nnf

  private object Nnf {
    case class Union(tpe1: Nnf, tpe2: Nnf) extends Nnf

    case class Intersection(tpe1: Nnf, tpe2: Nnf) extends Nnf

    case class Singleton(tpe: Literal) extends Nnf

    case object Empty extends Nnf

    case object All extends Nnf
  }

  /**
    * Converts the given type to DNF
    */
  private def dnf(t: Type): Dnf = {
    val n = nnf(t)
    val d = nnfToDnf(n)
    d
  }

  /**
    * Converts the given type to NNF.
    */
  private def nnf(t: Type): Nnf = t match {
    case CONSTANT(sym) => Nnf.Singleton(Literal.Positive(Atom.Eff(sym)))
    case VAR(sym) => Nnf.Singleton(Literal.Positive(Atom.Var(sym)))
    case COMPLEMENT(tpe) => nnfNot(tpe)
    case UNION(tpe1, tpe2) => Nnf.Union(nnf(tpe1), nnf(tpe2))
    case INTERSECTION(tpe1, tpe2) => Nnf.Intersection(nnf(tpe1), nnf(tpe2))
    case Type.Empty => Nnf.Empty
    case Type.All => Nnf.All
    case _ => throw InternalCompilerException(s"unexpected type: $t", t.loc)
  }

  /**
    * Converts the complement of the given type to NNF.
    */
  private def nnfNot(t: Type): Nnf = t match {
    case CONSTANT(sym) => Nnf.Singleton(Literal.Negative(Atom.Eff(sym)))
    case VAR(sym) => Nnf.Singleton(Literal.Negative(Atom.Var(sym)))
    case COMPLEMENT(tpe) => nnf(tpe)
    case UNION(tpe1, tpe2) => Nnf.Intersection(
      nnf(mkComplement(tpe1)),
      nnf(mkComplement(tpe2))
    )
    case INTERSECTION(tpe1, tpe2) => Nnf.Union(
      nnf(mkComplement(tpe1)),
      nnf(mkComplement(tpe2))
    )
    case _ => throw InternalCompilerException(s"unexpected type: $t", t.loc)
  }

  /**
    * Converts the given type from NNF to DNF.
    */
  private def nnfToDnf(t: Nnf): Dnf = t match {
    case Nnf.Union(tpe1, tpe2) => union(nnfToDnf(tpe1), nnfToDnf(tpe2))
    case Nnf.Intersection(tpe1, tpe2) => intersect(nnfToDnf(tpe1), nnfToDnf(tpe2))
    case Nnf.Singleton(tpe) => Dnf.Union(Set(Set(tpe)))
    case Nnf.Empty => Dnf.Empty
    case Nnf.All => Dnf.All
  }

  /**
    * Calculates the intersection of two DNF sets.
    */
  private def intersect(t1: Dnf, t2: Dnf): Dnf = (t1, t2) match {
    case (Dnf.Union(inters1), Dnf.Union(inters2)) =>
      val inters = for {
        inter1 <- inters1
        inter2 <- inters2
      } yield inter1 ++ inter2
      Dnf.Union(inters)
  }

  /**
    * Calculates the union of two DNF sets.
    */
  private def union(t1: Dnf, t2: Dnf): Dnf = (t1, t2) match {
    case (Dnf.Union(inters1), Dnf.Union(inters2)) => Dnf.Union(inters1 ++ inters2)
  }

  /**
    * Returns true if the given DNF set represents an empty set.
    */
  private def isEmpty(t1: Dnf): Boolean = t1 match {
    case Dnf.Union(inters) => inters.forall(isEmptyIntersection)
  }

  /*complement *
    * Returns true if `t1` represents an empty intersection of effects.
    */
  private def isEmptyIntersection(t1: Intersection): Boolean = {
    val pos = t1.collect {
      case Literal.Positive(atom) => atom
    }
    val neg = t1.collect {
      case Literal.Negative(atom) => atom
    }

    // an intersection is empty if any of the following is true:

    // 1. It contains two different positive constants
    val diffConst = pos.collect {
      case Atom.Eff(sym) => sym
    }.size > 1

    // 2. It contains an atom in both the positive and negative sets
    val negation = (pos & neg).nonEmpty

    diffConst || negation
  }
}
