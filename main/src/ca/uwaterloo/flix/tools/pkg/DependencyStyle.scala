/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.tools.pkg

/** How a [[Dependency.FlixDependency]] is written in `flix.toml`. */
sealed trait DependencyStyle

object DependencyStyle {

  /** As its version only, e.g. `"github:flix/museum" = "1.4.0"`. */
  case object VersionOnly extends DependencyStyle

  /** As a table, e.g. `"github:flix/museum" = { version = "1.4.0", mount = "Museum" }`. */
  case object Table extends DependencyStyle

}
