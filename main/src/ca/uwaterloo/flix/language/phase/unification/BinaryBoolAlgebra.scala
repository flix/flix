/*
 * Copyright 2022 Matthew Lutze
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
package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.util.InternalCompilerException
import ca.uwaterloo.flix.util.collection.Bimap

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet
import scala.util.chaining.scalaUtilChainingOps

sealed trait BinaryBoolAlgebra

// MATT docs
object BinaryBoolAlgebra {
  case object True extends BinaryBoolAlgebra

  case object False extends BinaryBoolAlgebra

  case class And(f1: BinaryBoolAlgebra, f2: BinaryBoolAlgebra) extends BinaryBoolAlgebra

  case class Or(f1: BinaryBoolAlgebra, f2: BinaryBoolAlgebra) extends BinaryBoolAlgebra

  case class Not(f1: BinaryBoolAlgebra) extends BinaryBoolAlgebra

  case class Var(id: Int) extends BinaryBoolAlgebra

  implicit val AsBoolAlgTrait: BoolAlgTrait[BinaryBoolAlgebra] = new BoolAlgTrait[BinaryBoolAlgebra] {
    @tailrec
    override def mkAnd(alg1: BinaryBoolAlgebra, alg2: BinaryBoolAlgebra): BinaryBoolAlgebra = (alg1, alg2) match {
      // T ∧ x => x
      case (BinaryBoolAlgebra.True, _) =>
        alg2

      // x ∧ T => x
      case (_, BinaryBoolAlgebra.True) =>
        alg1

      // F ∧ x => F
      case (BinaryBoolAlgebra.False, _) =>
        BinaryBoolAlgebra.False

      // x ∧ F => F
      case (_, BinaryBoolAlgebra.False) =>
        BinaryBoolAlgebra.False

      // ¬x ∧ (x ∨ y) => ¬x ∧ y
      case (BinaryBoolAlgebra.Not(x1), BinaryBoolAlgebra.Or(x2, y)) if x1 == x2 =>
        mkAnd(mkNot(x1), y)

      // x ∧ ¬x => F
      case (x1, BinaryBoolAlgebra.Not(x2)) if x1 == x2 =>
        BinaryBoolAlgebra.False

      // ¬x ∧ x => F
      case (BinaryBoolAlgebra.Not(x1), x2) if x1 == x2 =>
        BinaryBoolAlgebra.False

      // x ∧ (x ∧ y) => (x ∧ y)
      case (x1, BinaryBoolAlgebra.And(x2, y)) if x1 == x2 =>
        mkAnd(x1, y)

      // x ∧ (y ∧ x) => (x ∧ y)
      case (x1, BinaryBoolAlgebra.And(y, x2)) if x1 == x2 =>
        mkAnd(x1, y)

      // (x ∧ y) ∧ x) => (x ∧ y)
      case (BinaryBoolAlgebra.And(x1, y), x2) if x1 == x2 =>
        mkAnd(x1, y)

      // (x ∧ y) ∧ y) => (x ∧ y)
      case (BinaryBoolAlgebra.And(x, y1), y2) if y1 == y2 =>
        mkAnd(x, y1)

      // x ∧ (x ∨ y) => x
      case (x1, BinaryBoolAlgebra.Or(x2, _)) if x1 == x2 =>
        x1

      // (x ∨ y) ∧ x => x
      case (BinaryBoolAlgebra.Or(x1, _), x2) if x1 == x2 =>
        x1

      // x ∧ (y ∧ ¬x) => F
      case (x1, BinaryBoolAlgebra.And(_, BinaryBoolAlgebra.Not(x2))) if x1 == x2 =>
        BinaryBoolAlgebra.False

      // (¬x ∧ y) ∧ x => F
      case (BinaryBoolAlgebra.And(BinaryBoolAlgebra.Not(x1), _), x2) if x1 == x2 =>
        BinaryBoolAlgebra.False

      // x ∧ ¬(x ∨ y) => F
      case (x1, BinaryBoolAlgebra.Not(BinaryBoolAlgebra.Or(x2, _))) if x1 == x2 =>
        BinaryBoolAlgebra.False

      // ¬(x ∨ y) ∧ x => F
      case (BinaryBoolAlgebra.Not(BinaryBoolAlgebra.Or(x1, _)), x2) if x1 == x2 =>
        BinaryBoolAlgebra.False

      // x ∧ (¬x ∧ y) => F
      case (x1, BinaryBoolAlgebra.And(BinaryBoolAlgebra.Not(x2), _)) if x1 == x2 =>
        BinaryBoolAlgebra.False

      // (¬x ∧ y) ∧ x => F
      case (BinaryBoolAlgebra.And(BinaryBoolAlgebra.Not(x1), _), x2) if x1 == x2 =>
        BinaryBoolAlgebra.False

      // x ∧ x => x
      case _ if alg1 == alg2 => alg1

      case _ =>
        //      val s = s"And($eff1, $eff2)"
        //      val len = s.length
        //      if (true) {
        //        println(s.substring(0, Math.min(len, 300)))
        //      }

        BinaryBoolAlgebra.And(alg1, alg2)
    }

    @tailrec
    override def mkOr(tpe1: BinaryBoolAlgebra, tpe2: BinaryBoolAlgebra): BinaryBoolAlgebra = (tpe1, tpe2) match {
      // T ∨ x => T
      case (BinaryBoolAlgebra.True, _) =>
        BinaryBoolAlgebra.True

      // F ∨ y => y
      case (BinaryBoolAlgebra.False, _) =>
        tpe2

      // x ∨ T => T
      case (_, BinaryBoolAlgebra.True) =>
        BinaryBoolAlgebra.True

      // x ∨ F => x
      case (_, BinaryBoolAlgebra.False) =>
        tpe1

      // x ∨ (y ∨ x) => x ∨ y
      case (x1, BinaryBoolAlgebra.Or(y, x2)) if x1 == x2 =>
        mkOr(x1, y)

      // (x ∨ y) ∨ x => x ∨ y
      case (BinaryBoolAlgebra.Or(x1, y), x2) if x1 == x2 =>
        mkOr(x1, y)

      // ¬x ∨ x => T
      case (BinaryBoolAlgebra.Not(x), y) if x == y =>
        BinaryBoolAlgebra.True

      // x ∨ ¬x => T
      case (x, BinaryBoolAlgebra.Not(y)) if x == y =>
        BinaryBoolAlgebra.True

      // (¬x ∨ y) ∨ x) => T
      case (BinaryBoolAlgebra.Or(BinaryBoolAlgebra.Not(x), _), y) if x == y =>
        BinaryBoolAlgebra.True

      // x ∨ (¬x ∨ y) => T
      case (x, BinaryBoolAlgebra.Or(BinaryBoolAlgebra.Not(y), _)) if x == y =>
        BinaryBoolAlgebra.True

      // x ∨ (y ∧ x) => x
      case (x1, BinaryBoolAlgebra.And(_, x2)) if x1 == x2 => x1

      // (y ∧ x) ∨ x => x
      case (BinaryBoolAlgebra.And(_, x1), x2) if x1 == x2 => x1

      // x ∨ x => x
      case _ if tpe1 == tpe2 =>
        tpe1

      case _ =>

        //              val s = s"Or($eff1, $eff2)"
        //              val len = s.length
        //              if (len > 30) {
        //                println(s.substring(0, Math.min(len, 300)))
        //              }

        BinaryBoolAlgebra.Or(tpe1, tpe2)
    }

    override def mkNot(f0: BinaryBoolAlgebra): BinaryBoolAlgebra = f0 match {
      case BinaryBoolAlgebra.True =>
        BinaryBoolAlgebra.False

      case BinaryBoolAlgebra.False =>
        BinaryBoolAlgebra.True

      case BinaryBoolAlgebra.Not(x) =>
        x

      // ¬(¬x ∨ y) => x ∧ ¬y
      case BinaryBoolAlgebra.Or(BinaryBoolAlgebra.Not(x), y) =>
        mkAnd(x, mkNot(y))

      // ¬(x ∨ ¬y) => ¬x ∧ y
      case BinaryBoolAlgebra.Or(x, BinaryBoolAlgebra.Not(y)) =>
        mkAnd(mkNot(x), y)

      case _ => BinaryBoolAlgebra.Not(f0)
    }

    override def map(g: Int => BinaryBoolAlgebra, f: BinaryBoolAlgebra): BinaryBoolAlgebra = f match {
      case True => True
      case False => False
      case And(f1, f2) => mkAnd(map(g, f1), map(g, f2))
      case Or(f1, f2) => mkOr(map(g, f1), map(g, f2))
      case Not(f1) => mkNot(map(g, f1))
      case Var(sym) => g(sym)
    }

    override def mkTrue: BinaryBoolAlgebra = True

    override def mkFalse: BinaryBoolAlgebra = False

    override def mkVar(id: Int): BinaryBoolAlgebra = Var(id)

    override def getEnv(fs: List[Type]): Bimap[Symbol.KindedTypeVarSym, Int] = {
      // Compute the variables in `tpe`.
      val tvars = fs.flatMap(_.typeVars).map(_.sym).to(SortedSet)

      // Construct a bi-directional map from type variables to indices.
      // The idea is that the first variable becomes x0, the next x1, and so forth.
      tvars.zipWithIndex.foldLeft(Bimap.empty[Symbol.KindedTypeVarSym, Int]) {
        case (macc, (sym, x)) => macc + (sym -> x)
      }
    }

    override def toType(f: BinaryBoolAlgebra, env: Bimap[Symbol.KindedTypeVarSym, Int]): Type = f match {
      case True => Type.True
      case False => Type.False
      case And(f1, f2) => Type.mkApply(Type.And, List(toType(f1, env), toType(f2, env)), SourceLocation.Unknown)
      case Or(f1, f2) => Type.mkApply(Type.Or, List(toType(f1, env), toType(f2, env)), SourceLocation.Unknown)
      case Not(f1) => Type.Apply(Type.Not, toType(f1, env), SourceLocation.Unknown)
      case Var(id) => env.getBackward(id) match {
        case Some(sym) => Type.Var(sym, SourceLocation.Unknown)
        case None => throw InternalCompilerException(s"unexpected unknown ID: $id")
      }
    }

    override def freeVars(f: BinaryBoolAlgebra): SortedSet[Int] = f match {
      case True => SortedSet.empty
      case False => SortedSet.empty
      case And(f1, f2) => freeVars(f1) ++ freeVars(f2)
      case Or(f1, f2) => freeVars(f1) ++ freeVars(f2)
      case Not(f1) => freeVars(f1)
      case Var(id) => SortedSet(id)
    }

    override def minimize(f: BinaryBoolAlgebra): BinaryBoolAlgebra = {
      def toBoolAlgebra(f: BinaryBoolAlgebra): ExplicitFormula = f match {
        case True => ExplicitFormula.True
        case False => ExplicitFormula.False
        case And(f1, f2) => ExplicitFormula.And(toBoolAlgebra(f1), toBoolAlgebra(f2))
        case Or(f1, f2) => ExplicitFormula.Or(toBoolAlgebra(f1), toBoolAlgebra(f2))
        case Not(f1) => ExplicitFormula.Not(toBoolAlgebra(f1))
        case Var(id) => ExplicitFormula.Var(id)
      }

      def fromBoolAlgebra(f: ExplicitFormula): BinaryBoolAlgebra = f match {
        case ExplicitFormula.True => True
        case ExplicitFormula.False => False
        case ExplicitFormula.Var(x) => Var(x)
        case ExplicitFormula.Not(f) => Not(fromBoolAlgebra(f))
        case ExplicitFormula.And(f1, f2) => And(fromBoolAlgebra(f1), fromBoolAlgebra(f2))
        case ExplicitFormula.Or(f1, f2) => Or(fromBoolAlgebra(f1), fromBoolAlgebra(f2))
      }

      f pipe
        toBoolAlgebra pipe
        BoolAlgebraTable.minimizeFormula pipe
        fromBoolAlgebra
    }
  }
}
