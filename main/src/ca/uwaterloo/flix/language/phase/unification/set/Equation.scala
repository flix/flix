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
import ca.uwaterloo.flix.language.phase.unification.set.Equation.Status
import ca.uwaterloo.flix.language.phase.unification.set.SetFormula.*

import scala.annotation.nowarn

/**
  * Represents an equality equation between the formulas `f1` and `f2` (`f1 ~ f2`).
  *
  * Equations are created with [[Equation.Status.Pending]] but can be marked as
  * [[Equation.Status.Unsolvable]] (`f1 ~unsolvable f2`) or
  * [[Equation.Status.Timeout]] (`f1 ~timeout f2`) during unification.
  * An unspecified non-pending equation is written `f1 !~ f2`.
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
  override final def toString: String = status match {
    case Status.Pending => s"$f1 ~ $f2"
    case Status.Unsolvable => s"$f1 ~unsolvable $f2"
    case Status.Timeout(_) => s"$f1 ~timeout $f2"
  }

  /** Returns a copy of `this` with the new `status` */
  final def copyWithStatus(status: Equation.Status): Equation = {
    if (status == this.status) this
    else Equation(f1, f2, status, loc)
  }

  /** Returns a copy of `this` with status [[Equation.Status.Unsolvable]]. */
  final def toUnsolvable: Equation = copyWithStatus(Equation.Status.Unsolvable)

  /** Returns a copy of `this` with status [[Equation.Status.Timeout]]. */
  final def toTimeout(msg: String): Equation = copyWithStatus(Equation.Status.Timeout(msg))

  /** Returns `true` if this equation is [[Equation.Status.Pending]]. */
  final def isPending: Boolean = status match {
    case Status.Pending => true
    case Status.Unsolvable => false
    case Status.Timeout(_) => false
  }

}

object Equation {

  /**
    * Returns an equality equation between the formulas `f1` and `f2` with [[Status.Pending]] as
    * the default status (`f1 ~ f2`).
    */
  def mk(f1: SetFormula, f2: SetFormula, loc: SourceLocation, status: Status = Status.Pending): Equation = {
    // This reordering is not part of the interface and should not be relied on.
    (f1, f2) match {
      case (_, Var(_)) => Equation(f2, f1, status, loc)
      case _ => Equation(f1, f2, status, loc)
    }
  }

  sealed trait Status

  object Status {

    /** Equation might be solvable or unsolvable (`f1 ~ f2`). */
    case object Pending extends Status

    /** Equation is unsolvable (`f1 ~unsolvable f2`). */
    case object Unsolvable extends Status

    /** The equation is unsolvable due to a timeout or a size threshold (`f1 ~timeout f2`). */
    case class Timeout(msg: String) extends Status

  }

}


