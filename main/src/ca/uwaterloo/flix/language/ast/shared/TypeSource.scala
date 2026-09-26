/*
 * Copyright 2024 Holger Dal Mogensen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast.shared

/**
  * Enum representing whether a type is ascribed or inferred.
  */
sealed trait TypeSource

object TypeSource {
  /**
    * The type is ascribed (present in the source code).
    */
  case object Ascribed extends TypeSource

  /**
    * The type is inferred (absent in the source code).
    */
  case object Inferred extends TypeSource
}
