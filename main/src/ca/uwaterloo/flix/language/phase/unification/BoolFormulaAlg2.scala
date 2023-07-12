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

import ca.uwaterloo.flix.language.phase.unification.BoolFormula._
import ca.uwaterloo.flix.util.collection.Bimap

/**
  * An implementation of the [[BoolAlg]] interface for [[BoolFormula]].
  */
class BoolFormulaAlg2 extends BoolAlg2[BoolFormula, Int] {

  override def isTrue(f: BoolFormula): Boolean = f == BoolFormula.True

  override def isFalse(f: BoolFormula): Boolean = f == BoolFormula.False

  override def isVar(f: BoolFormula): Boolean = f match {
    case Var(_) => true
    case _ => false
  }


  override def mkTrue: BoolFormula = True

  override def mkFalse: BoolFormula = False

  override def mkVar(id: Int): BoolFormula = Var(id)

  override def mkNot(f: BoolFormula): BoolFormula = f match {
    case BoolFormula.True =>
      BoolFormula.False

    case BoolFormula.False =>
      BoolFormula.True

    case BoolFormula.Not(x) =>
      x

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

    // x ∧ ¬x => F
    case (x1, BoolFormula.Not(x2)) if x1 == x2 =>
      BoolFormula.False

    // ¬x ∧ x => F
    case (BoolFormula.Not(x1), x2) if x1 == x2 =>
      BoolFormula.False

    // (x ∧ y) ∧ x) => (x ∧ y)
    case (BoolFormula.And(x1, y), x2) if x1 == x2 =>
      mkAnd(x1, y)

    // (x ∧ y) ∧ y) => (x ∧ y)
    case (BoolFormula.And(x, y1), y2) if y1 == y2 =>
      mkAnd(x, y1)

    // x ∧ x => x
    case _ if f1 == f2 => f1

    case _ => BoolFormula.And(f1, f2)
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

    // ¬x ∨ x => T
    case (BoolFormula.Not(x), y) if x == y =>
      BoolFormula.True

    // x ∨ ¬x => T
    case (x, BoolFormula.Not(y)) if x == y =>
      BoolFormula.True

    case _ => BoolFormula.Or(f1, f2)
  }

  /**
    * Converts the given formula f into another Boolean formula.
    */
  override def convert[G, W](f: BoolFormula, env: Bimap[Int, W])(implicit otherAlg: BoolAlg2[G, W]): G = f match {
    case True => otherAlg.mkTrue
    case False => otherAlg.mkFalse
    case And(f1, f2) => otherAlg.mkAnd(convert(f1, env), convert(f2, env))
    case Or(f1, f2) => otherAlg.mkOr(convert(f1, env), convert(f2, env))
    case Not(f1) => otherAlg.mkNot(convert(f1, env))
    case Var(id) => otherAlg.mkVar(env.getForward(id).get)
  }
}
