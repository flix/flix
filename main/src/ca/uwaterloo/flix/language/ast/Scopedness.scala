/*
 * Copyright 2021 Matthew Lutze
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
package ca.uwaterloo.flix.language.ast

/**
  * Indicates whether a variable is scoped.
  * A scoped value must not escape its scope.
  */
sealed trait Scopedness {

  /**
    * Unscoped may be considered a subtype of Scoped.
    */
  def <=(other: Scopedness): Boolean = (this, other) match {
    case (Scopedness.Scoped, Scopedness.Unscoped) => false
    case _ => true
  }

  /**
    * The minimum of two Scopednesses according to the subtyping relation.
    */
  def min(other: Scopedness): Scopedness = if (this <= other) this else other

  /**
    * The minimum of two Scopednesses according to the subtyping relation.
    */
  def max(other: Scopedness): Scopedness = if (this <= other) other else this
}

object Scopedness {
  case object Scoped extends Scopedness

  case object Unscoped extends Scopedness
}
