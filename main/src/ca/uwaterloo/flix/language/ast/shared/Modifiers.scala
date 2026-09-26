/*
 * Copyright 2024 Holger Dal Mogensen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast.shared

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
    * Returns `true` if these modifiers contain the mutable modifier.
    */
  def isMutable: Boolean = mod contains Modifier.Mutable

  /**
    * Returns `true` if these modifiers contain the public modifier.
    */
  def isPublic: Boolean = mod contains Modifier.Public

  /**
    * Returns `true` if these modifiers contain the redefinition modifier.
    */
  def isRedef: Boolean = mod contains Modifier.Redef

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
