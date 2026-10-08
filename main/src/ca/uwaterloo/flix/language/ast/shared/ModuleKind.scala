/*
 * Copyright 2025 Chenhao Gao
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast.shared

/**
  * Represents the kind of a module.
  */
sealed trait ModuleKind

object ModuleKind {
  /**
    * A module associated with an effect, enum, struct, or trait.
    */
  case object Companion extends ModuleKind

  /**
    * A standalone module.
    */
  case object Standalone extends ModuleKind
}
