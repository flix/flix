/*
 * Copyright 2024 Holger Dal Mogensen
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

import ca.uwaterloo.flix.language.ast.Ast.Modifier

/**
  * Companion object of [[Modifiers]].
  */
object Modifiers {
  /**
    * The empty sequence of modifiers.
    */
  val Empty: Modifiers = Modifiers(Nil)
}

/**
  * A sequence of modifiers.
  */
case class Modifiers(mod: List[Modifier]) {

  /**
    * Returns a new modifier sequence with `pub` added.
    */
  def asPublic: Modifiers = if (isPublic) this else Modifiers(Modifier.Public :: mod)

  /**
    * Returns `true` if these modifiers contain the lawful modifier.
    */
  def isLawful: Boolean = mod contains Modifier.Lawful

  /**
    * Returns `true` if these modifiers contain the mutable modifier.
    */
  def isMutable: Boolean = mod contains Modifier.Mutable

  /**
    * Returns `true` if these modifiers contain the override modifier.
    */
  def isOverride: Boolean = mod contains Modifier.Override

  /**
    * Returns `true` if these modifiers contain the public modifier.
    */
  def isPublic: Boolean = mod contains Modifier.Public

  /**
    * Returns `true` if these modifiers contain the sealed modifier.
    */
  def isSealed: Boolean = mod contains Modifier.Sealed

  /**
    * Returns `true` if these modifiers contain the synthetic modifier.
    */
  def isSynthetic: Boolean = mod contains Modifier.Synthetic

  /**
    * Returns a string representation that hides the internals.
    */
  override def toString: String = "Modifiers(...)"

}
