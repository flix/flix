/*
 * Copyright 2023 Matthew Lutze
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

import scala.collection.immutable.SortedSet

object BoolFormulaSat extends Sat[BoolFormula, Int] {
  /**
    * Returns true if the given Boolean formula is satisfiable.
    */
  override def satisfiable(f: BoolFormula)(implicit alg: BoolAlg2[BoolFormula, Int]): Boolean = f match {
    case BoolFormula.True => true
    case BoolFormula.False => false
    case BoolFormula.Var(_) => true
    case _ => evaluateAll(f, freeVars(f).toList, List.empty)
  }

  override def freeVars(f: BoolFormula)(implicit alg: BoolAlg2[BoolFormula, Int]): SortedSet[Int] = f.freeVars

  override def map(f: BoolFormula)(fn: Int => BoolFormula)(implicit alg: BoolAlg2[BoolFormula, Int]): BoolFormula = f match {
    case BoolFormula.True => alg.mkTrue
    case BoolFormula.False => alg.mkFalse
    case BoolFormula.And(f1, f2) => alg.mkAnd(map(f1)(fn), map(f2)(fn))
    case BoolFormula.Or(f1, f2) => alg.mkOr(map(f1)(fn), map(f2)(fn))
    case BoolFormula.Not(f1) => alg.mkNot(map(f1)(fn))
    case BoolFormula.Var(sym) => fn(sym)
  }

  /**
    * Enumerates all assignments to `f` and checks if one of them is satisfiable.
    */
  private def evaluateAll(f: BoolFormula, l: List[Int], env: List[Int]): Boolean = l match {
    case Nil =>
      // All variables are bound. Compute the truth value.
      evaluate(f, env)
    case x :: xs =>
      // Recurse on two cases: x = false and x = true.
      evaluateAll(f, xs, env) || evaluateAll(f, xs, x :: env)
  }

  /**
    * Computes the truth value of the formula `f` assuming the variables in `trueVars`
    * are true and the rest are false.
    */
  private def evaluate(f: BoolFormula, trueVars: List[Int]): Boolean = f match {
    case BoolFormula.True => true
    case BoolFormula.False => false
    case BoolFormula.Var(x) => trueVars.contains(x)
    case BoolFormula.Not(f1) => !evaluate(f1, trueVars)
    case BoolFormula.Or(f1, f2) => evaluate(f1, trueVars) || evaluate(f2, trueVars)
    case BoolFormula.And(f1, f2) => evaluate(f1, trueVars) && evaluate(f2, trueVars)
  }
}
