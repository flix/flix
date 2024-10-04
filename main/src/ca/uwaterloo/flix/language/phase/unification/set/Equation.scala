/*
 * Copyright 2024 Jonathan Lindegaard Starup
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

package ca.uwaterloo.flix.language.phase.unification.set

import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.language.phase.unification.set.SetFormula.*

import scala.annotation.nowarn

/**
  * Represents an equality equation between the formulas `f1` and `f2` (`f1 ~ f2`).
  *
  * Equations can be marked as [[Equation.Status.Unsolvable]] during unification, in which case its
  * written `f1 !~ f2`.
  *
  * The `@nowarn` annotation is required for Scala 3 compatibility, since the derived `copy` method
  * is private in Scala 3 due to the private constructor. In Scala 2 the `copy` method is still
  * public. However, we do not use the `copy` method anywhere for [[Equation]], so this is fine.
  */
@nowarn
case class Equation private(f1: SetFormula, f2: SetFormula, status: Equation.Status, loc: SourceLocation) {

  /** Returns the sum of the sizes of the formulas in `this`. */
  final def size: Int = f1.size + f2.size

  /** Returns a human-readable string of `this`. */
  override final def toString: String = s"$f1 ~ $f2"

  /** Returns a copy of `this` with the new `status` */
  final def copyWithStatus(status: Equation.Status): Equation = {
    if (status == this.status) this
    else Equation(f1, f2, status, loc)
  }

  /** Returns a copy of `this` with status [[Equation.Status.Unsolvable]]. */
  @inline
  final def toUnsolvable: Equation = copyWithStatus(Equation.Status.Unsolvable)

  /** Returns a copy of `this` with status [[Equation.Status.Timeout]]. */
  @inline
  final def toTimeout(msg: String): Equation = copyWithStatus(Equation.Status.Timeout(msg))

}

object Equation {

  /**
    * Returns an equality equation between the formulas `f1` and `f2` (`f1 ~ f2`).
    *
    * The smart constructor performs normalization:
    *   - Move single [[Univ]], [[Empty]], [[ElemSet]], and [[Cst]] to the right (in that priority).
    *   - Move single [[Var]] to the left.
    *
    * Examples:
    *   -    `mk(univ, x7) = x7 ~ univ`
    *   - `mk(x3 ∩ x7, x4) = x4 ~ c3 ∩ x7`
    *   - `mk(c2, x3 ∩ x7) = x3 ∩ x7 ~ c2`
    *   -    `mk(univ, c2) = c2 ~ univ`
    */
  def mk(f1: SetFormula, f2: SetFormula, loc: SourceLocation, status: Status = Status.Unknown): Equation = (f1, f2) match {
    case (Univ, _) => Equation(f2, f1, status, loc)
    case (Empty, _) => Equation(f2, f1, status, loc)
    case (ElemSet(_), _) => Equation(f2, f1, status, loc)
    case (Cst(_), _) => Equation(f2, f1, status, loc)
    case (_, Var(_)) => Equation(f2, f1, status, loc)
    case _ => Equation(f1, f2, status, loc)
  }

  sealed trait Status

  object Status {

    /** Equation might be solvable or unsolvable. */
    case object Unknown extends Status

    /** Equation is unsolvable. */
    case object Unsolvable extends Status

    /** The equation is unsolvable due to a timeout or a size threshold. */
    case class Timeout(msg: String) extends Status

  }

}


