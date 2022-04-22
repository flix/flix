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

import scala.collection.immutable.SortedSet

/**
  * A common super-type for Boolean formulas.
  */
sealed trait BoolFormula {

  /**
    * Returns the free variables in `this` formula.
    */
  final def freeVars: SortedSet[Int] = this match {
    case BoolFormula.True => SortedSet.empty
    case BoolFormula.False => SortedSet.empty
    case BoolFormula.Var(x) => SortedSet(x)
    case BoolFormula.Neg(f) => f.freeVars
    case BoolFormula.Conj(f1, f2) => f1.freeVars ++ f2.freeVars
    case BoolFormula.Disj(f1, f2) => f1.freeVars ++ f2.freeVars
  }

  /**
    * Returns the size of `this` formulas.
    *
    * The size is the number of conjunctions and disjunctions.
    */
  final def size: Int = this match {
    case BoolFormula.True => 0
    case BoolFormula.False => 0
    case BoolFormula.Var(_) => 0
    case BoolFormula.Neg(t) => t.size
    case BoolFormula.Conj(t1, t2) => t1.size + t2.size + 1
    case BoolFormula.Disj(t1, t2) => t1.size + t2.size + 1
  }

  /**
    * Returns a human-readable fully parenthesized string representation of `this` term.
    */
  override def toString: String = this match {
    case BoolFormula.True => "true"
    case BoolFormula.False => "false"
    case BoolFormula.Var(x) => s"x$x"
    case BoolFormula.Neg(f) => f match {
      case BoolFormula.Var(x) => s"!x$x"
      case _ => s"!($f)"
    }
    case BoolFormula.Conj(f1, f2) => s"(and $f1 $f2)"
    case BoolFormula.Disj(f1, f2) => s"(or $f1 $f2)"
  }

}

object BoolFormula {

  /**
    * Represents the constant True.
    */
  case object True extends BoolFormula

  /**
    * Represents the constant False.
    */
  case object False extends BoolFormula

  /**
    * Represents a variable.
    *
    * Variables are numbered by integers.
    */
  case class Var(x: Int) extends BoolFormula

  /**
    * Represents the negation of the formula `f`.
    */
  case class Neg(f: BoolFormula) extends BoolFormula

  /**
    * Represents the conjunction (logical and) of `f1` and `f2`.
    */
  case class Conj(f1: BoolFormula, f2: BoolFormula) extends BoolFormula

  /**
    * Represents the disjunction (logical or) of `f1` and `f2`.
    */
  case class Disj(f1: BoolFormula, f2: BoolFormula) extends BoolFormula

}
