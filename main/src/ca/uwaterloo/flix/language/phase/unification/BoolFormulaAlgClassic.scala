/*
 * Copyright 2022 Magnus Madsen
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

/**
  * An implementation of the [[BoolAlg]] interface for [[BoolFormula]].
  */
class BoolFormulaAlgClassic extends BoolFormulaAlg {

  override def mkNot(f: BoolFormula): BoolFormula = f match {
    case BoolFormula.True =>
      BoolFormula.False

    case BoolFormula.False =>
      BoolFormula.True

    case BoolFormula.Not(x) =>
      x

    // ¬(¬x ∨ y) => x ∧ ¬y
    case BoolFormula.Or(BoolFormula.Not(x), y) =>
      mkAnd(x, mkNot(y))

    // ¬(x ∨ ¬y) => ¬x ∧ y
    case BoolFormula.Or(x, BoolFormula.Not(y)) =>
      mkAnd(mkNot(x), y)

    case _ => BoolFormula.Not(f)
  }

  override def mkAnd(f1: BoolFormula, f2: BoolFormula): BoolFormula = (f1, f2) match {
    // T ∧ x => x
    case (BoolFormula.True, _) =>
      f2

    // x ∧ T => x
    case (_, BoolFormula.True) =>
      f1

    // F ∧ x => F
    case (BoolFormula.False, _) =>
      BoolFormula.False

    // x ∧ F => F
    case (_, BoolFormula.False) =>
      BoolFormula.False

    // ¬x ∧ (x ∨ y) => ¬x ∧ y
    case (BoolFormula.Not(x1), BoolFormula.Or(x2, y)) if x1 == x2 =>
      mkAnd(mkNot(x1), y)

    // x ∧ ¬x => F
    case (x1, BoolFormula.Not(x2)) if x1 == x2 =>
      BoolFormula.False

    // ¬x ∧ x => F
    case (BoolFormula.Not(x1), x2) if x1 == x2 =>
      BoolFormula.False

    // x ∧ (x ∧ y) => (x ∧ y)
    case (x1, BoolFormula.And(x2, y)) if x1 == x2 =>
      mkAnd(x1, y)

    // x ∧ (y ∧ x) => (x ∧ y)
    case (x1, BoolFormula.And(y, x2)) if x1 == x2 =>
      mkAnd(x1, y)

    // (x ∧ y) ∧ x) => (x ∧ y)
    case (BoolFormula.And(x1, y), x2) if x1 == x2 =>
      mkAnd(x1, y)

    // (x ∧ y) ∧ y) => (x ∧ y)
    case (BoolFormula.And(x, y1), y2) if y1 == y2 =>
      mkAnd(x, y1)

    // x ∧ (x ∨ y) => x
    case (x1, BoolFormula.Or(x2, _)) if x1 == x2 =>
      x1

    // (x ∨ y) ∧ x => x
    case (BoolFormula.Or(x1, _), x2) if x1 == x2 =>
      x1

    // x ∧ (y ∧ ¬x) => F
    case (x1, BoolFormula.And(_, BoolFormula.Not(x2))) if x1 == x2 =>
      BoolFormula.False

    // (¬x ∧ y) ∧ x => F
    case (BoolFormula.And(BoolFormula.Not(x1), _), x2) if x1 == x2 =>
      BoolFormula.False

    // x ∧ ¬(x ∨ y) => F
    case (x1, BoolFormula.Not(BoolFormula.Or(x2, _))) if x1 == x2 =>
      BoolFormula.False

    // ¬(x ∨ y) ∧ x => F
    case (BoolFormula.Not(BoolFormula.Or(x1, _)), x2) if x1 == x2 =>
      BoolFormula.False

    // x ∧ (¬x ∧ y) => F
    case (x1, BoolFormula.And(BoolFormula.Not(x2), _)) if x1 == x2 =>
      BoolFormula.False

    // (¬x ∧ y) ∧ x => F
    case (BoolFormula.And(BoolFormula.Not(x1), _), x2) if x1 == x2 =>
      BoolFormula.False

    // x ∧ x => x
    case _ if f1 == f2 => f1

    case _ =>
      //      val s = s"And($eff1, $eff2)"
      //      val len = s.length
      //      if (true) {
      //        println(s.substring(0, Math.min(len, 300)))
      //      }

      BoolFormula.And(f1, f2)
  }

  override def mkOr(f1: BoolFormula, f2: BoolFormula): BoolFormula = (f1, f2) match {
    // T ∨ x => T
    case (BoolFormula.True, _) =>
      BoolFormula.True

    // F ∨ y => y
    case (BoolFormula.False, _) =>
      f2

    // x ∨ T => T
    case (_, BoolFormula.True) =>
      BoolFormula.True

    // x ∨ F => x
    case (_, BoolFormula.False) =>
      f1

    // x ∨ (y ∨ x) => x ∨ y
    case (x1, BoolFormula.Or(y, x2)) if x1 == x2 =>
      mkOr(x1, y)

    // (x ∨ y) ∨ x => x ∨ y
    case (BoolFormula.Or(x1, y), x2) if x1 == x2 =>
      mkOr(x1, y)

    // ¬x ∨ x => T
    case (BoolFormula.Not(x), y) if x == y =>
      BoolFormula.True

    // x ∨ ¬x => T
    case (x, BoolFormula.Not(y)) if x == y =>
      BoolFormula.True

    // (¬x ∨ y) ∨ x) => T
    case (BoolFormula.Or(BoolFormula.Not(x), _), y) if x == y =>
      BoolFormula.True

    // x ∨ (¬x ∨ y) => T
    case (x, BoolFormula.Or(BoolFormula.Not(y), _)) if x == y =>
      BoolFormula.True

    // x ∨ (y ∧ x) => x
    case (x1, BoolFormula.And(_, x2)) if x1 == x2 => x1

    // (y ∧ x) ∨ x => x
    case (BoolFormula.And(_, x1), x2) if x1 == x2 => x1

    // x ∨ x => x
    case _ if f1 == f2 =>
      f1

    case _ =>

      //              val s = s"Or($eff1, $eff2)"
      //              val len = s.length
      //              if (len > 30) {
      //                println(s.substring(0, Math.min(len, 300)))
      //              }

      BoolFormula.Or(f1, f2)
  }

  // No minimization via tabling.
  override def minimize(f: BoolFormula): BoolFormula = f
}
