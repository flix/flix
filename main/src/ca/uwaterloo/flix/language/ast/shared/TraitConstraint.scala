/*
 * Copyright 2024 Holger Dal Mogensen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast.shared

import ca.uwaterloo.flix.language.ast.shared.SymUse.TraitSymUse
import ca.uwaterloo.flix.language.ast.{SourceLocation, Type}

import java.util.Objects

/**
  * Represents that the type `arg` must belong to trait `sym`.
  */
case class TraitConstraint(symUse: TraitSymUse, arg: Type, loc: SourceLocation) {
  override def equals(o: Any): Boolean = o match {
    case that: TraitConstraint =>
      this.symUse.sym == that.symUse.sym && this.arg == that.arg
    case _ => false
  }

  override def hashCode(): Int = Objects.hash(symUse.sym, arg)
}
