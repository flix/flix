/*
 * Copyright 2024 Andreas Stenbæk Larsen
 * Copyright 2025 Jakob Schneider Villumsen
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
package ca.uwaterloo.flix.tools.pkg

sealed trait Trust {

  /**
    * Returns the least upper bound of `this` and `other` according to the following lattice:
    * {{{
    *     Unrestricted
    *          |
    *        Plain
    *          |
    *       Paranoid
    * }}}
    */
  def lub(other: Trust): Trust = (this, other) match {
    case (Trust.Paranoid, _) => other
    case (Trust.Plain, Trust.Unrestricted) => Trust.Unrestricted
    case (Trust.Plain, _) => Trust.Plain
    case (Trust.Unrestricted, _) => Trust.Unrestricted
  }

  /**
    * Returns the greatest lower bound of `this` and `other` according to the following lattice:
    * {{{
    *     Unrestricted
    *          |
    *        Plain
    *          |
    *       Paranoid
    * }}}
    */
  def glb(other: Trust): Trust = if (this.lessThanEq(other)) this else other

  /**
    * Returns `true` if `this` is less than or equal to `other` in the lattice
    * {{{
    *     Unrestricted
    *          |
    *        Plain
    *          |
    *       Paranoid
    * }}}
    */
  def lessThanEq(other: Trust): Boolean = this.lub(other) == other

  /**
    * Returns `true` iff `!this.lessThanOrEq(other)` holds.
    *
    * @see [[lessThanEq]]
    */
  def greaterThan(other: Trust): Boolean = !this.lessThanEq(other)

}

/**
  * Trust for dependencies.
  */
object Trust {

  /**
    * Returns the greatest lower bound of `ts`.
    */
  def glb(ts: List[Trust]): Trust = ts match {
    case Nil => throw new IllegalArgumentException("unexpected empty list")
    case x :: xs => xs.foldLeft(x)((acc, y) => acc.glb(y))
  }

  /**
    * May use unchecked casts and Java interop.
    */
  case object Unrestricted extends Trust {
    override def toString: String = "unrestricted"
  }

  /**
    * Plain Flix must not have any unsafe features.
    *   1. No unsafe casts
    *   1. No Java interop
    */
  case object Plain extends Trust {
    override def toString: String = "plain"
  }

  /**
    * Has all the same restrictions as [[Plain]] in addition to the following:
    *   1. No `IO` effect
    */
  case object Paranoid extends Trust {
    override def toString: String = "paranoid"
  }

  def fromString(s: String): Option[Trust] = s match {
    case "paranoid" => Some(Paranoid)
    case "plain" => Some(Plain)
    case "unrestricted" => Some(Unrestricted)
    case _ => None
  }
}
