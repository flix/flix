/*
 * Copyright 2024 Magnus Madsen
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
package ca.uwaterloo.flix.language.ast.shared

/**
  * A common super-type for security contexts.
  */
sealed trait SecurityContext {

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
  def lub(other: SecurityContext): SecurityContext = (this, other) match {
    case (SecurityContext.Paranoid, _) => other
    case (SecurityContext.Plain, SecurityContext.Unrestricted) => SecurityContext.Unrestricted
    case (SecurityContext.Plain, _) => SecurityContext.Plain
    case (SecurityContext.Unrestricted, _) => SecurityContext.Unrestricted
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
  def glb(other: SecurityContext): SecurityContext = if (this.lessThanEq(other)) this else other

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
  def lessThanEq(other: SecurityContext): Boolean = this.lub(other) == other

  /**
    * Returns `true` iff `!this.lessThanOrEq(other)` holds.
    *
    * @see [[lessThanEq]]
    */
  def greaterThan(other: SecurityContext): Boolean = !this.lessThanEq(other)

}

object SecurityContext {

  /** The default [[SecurityContext]]. */
  val Default: SecurityContext = Plain

  /**
    * Returns the greatest lower bound of `sctxs`.
    */
  def glb(sctxs: List[SecurityContext]): SecurityContext = sctxs match {
    case Nil => throw new IllegalArgumentException("unexpected empty list")
    case x :: xs => xs.foldLeft(x)((acc, y) => acc.glb(y))
  }

  /**
    * A security context where everything is permitted.
    */
  case object Unrestricted extends SecurityContext {
    override def toString: String = "unrestricted"
  }

  /**
    * A security context where no unsafe features are permitted:
    *   1. No unsafe casts
    *   1. No Java interop
    */
  case object Plain extends SecurityContext {
    override def toString: String = "plain"
  }

  /**
    * A security context where the same restrictions as [[Plain]] apply
    * and also prohibits the use of the IO effect.
    */
  case object Paranoid extends SecurityContext {
    override def toString: String = "paranoid"
  }

  /** Parses `s` into a [[SecurityContext]] */
  def fromString(s: String): Option[SecurityContext] = s match {
    case "unrestricted" => Some(Unrestricted)
    case "plain" => Some(Plain)
    case "paranoid" => Some(Paranoid)
    case _ => None
  }
}
