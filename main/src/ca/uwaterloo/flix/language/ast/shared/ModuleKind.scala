/*
 * Copyright 2025 Chenhao Gao
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
