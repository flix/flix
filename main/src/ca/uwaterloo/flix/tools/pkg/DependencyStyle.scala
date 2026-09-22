/*
 * Copyright 2026 Magnus Madsen
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

/** How a [[Dependency.FlixDependency]] is written in `flix.toml`. */
sealed trait DependencyStyle

object DependencyStyle {

  /** As its version only, e.g. `"github:flix/museum" = "1.4.0"`. */
  case object VersionOnly extends DependencyStyle

  /** As a table, e.g. `"github:flix/museum" = { version = "1.4.0", mount = "Museum" }`. */
  case object Table extends DependencyStyle

}
