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
    * Combines `this` and `other` according to the following lattice:
    * {{{
    *        Plain
    *          |
    *   TrustJavaClass
    *          |
    *     Unrestricted
    * }}}
    */
  def combine(other: Trust): Trust = (this, other) match {
    // TODO: Add tests
    case (Trust.Plain, Trust.Plain) => Trust.Plain
    case (Trust.Plain, Trust.TrustJavaClass) => Trust.Plain
    case (Trust.Plain, Trust.Unrestricted) => Trust.Plain
    case (Trust.TrustJavaClass, Trust.Plain) => Trust.Plain
    case (Trust.TrustJavaClass, Trust.TrustJavaClass) => Trust.TrustJavaClass
    case (Trust.TrustJavaClass, Trust.Unrestricted) => Trust.TrustJavaClass
    case (Trust.Unrestricted, Trust.Plain) => Trust.Plain
    case (Trust.Unrestricted, Trust.TrustJavaClass) => Trust.TrustJavaClass
    case (Trust.Unrestricted, Trust.Unrestricted) => Trust.Unrestricted
  }

  /**
    * Returns `true` if `this` is greater than or equal to `other` in the lattice
    * {{{
    *        Plain
    *          |
    *   TrustJavaClass
    *          |
    *     Unrestricted
    * }}}
    */
  def greaterThanEq(other: Trust): Boolean = (this, other) match {
    // TODO: Add tests
    case (Trust.Plain, Trust.Plain) => true
    case (Trust.Plain, Trust.TrustJavaClass) => true
    case (Trust.Plain, Trust.Unrestricted) => true
    case (Trust.TrustJavaClass, Trust.Plain) => false
    case (Trust.TrustJavaClass, Trust.TrustJavaClass) => true
    case (Trust.TrustJavaClass, Trust.Unrestricted) => true
    case (Trust.Unrestricted, Trust.Plain) => false
    case (Trust.Unrestricted, Trust.TrustJavaClass) => false
    case (Trust.Unrestricted, Trust.Unrestricted) => true
  }

  /**
    * Returns `true` iff `!this.greaterThanEq(other)` holds.
    *
    * @see [[greaterThanEq]]
    */
  def lessThan(other: Trust): Boolean = {
    // TODO: Add tests
    !this.greaterThanEq(other)
  }
}

/**
  * Trust for dependencies.
  */
object Trust {

  /**
    * Plain Flix must not have any unsafe features.
    *   1. No unsafe casts
    *   1. No Java interop
    */
  case object Plain extends Trust {
    override def toString: String = "plain"
  }

  /**
    * `Trust-javaclass` may not have unchecked casts but perform Java interop.
    */
  case object TrustJavaClass extends Trust {
    override def toString: String = "trust-javaclass"
  }

  /**
    * May use unchecked casts and Java interop.
    */
  case object Unrestricted extends Trust {
    override def toString: String = "unrestricted"
  }

  def fromString(s: String): Option[Trust] = s match {
    case "plain" => Some(Plain)
    case "trust-javaclass" => Some(TrustJavaClass)
    case "unrestricted" => Some(Unrestricted)
    case _ => None
  }
}
