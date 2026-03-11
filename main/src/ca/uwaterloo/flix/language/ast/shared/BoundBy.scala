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

/**
  * Represents the way a variable is bound.
  */
sealed trait BoundBy

object BoundBy {

  /**
    * Represents a variable that is bound by a formal parameter.
    */
  case object FormalParam extends BoundBy

  /**
    * Represents a variable that is bound by a let-binding.
    */
  case object Let extends BoundBy

  /**
    * Represents a variable that is bound by a pattern.
    */
  case object Pattern extends BoundBy

  /**
    * Represents a variable that is bound by a select.
    */
  case object SelectRule extends BoundBy

  /**
    * Represents a variable that is bound by a catch rule.
    */
  case object CatchRule extends BoundBy

  /**
    * Represents a variable that is bound by a constraint.
    */
  case object Constraint extends BoundBy

  /**
    * Represents a variable that is bound by a local def.
    */
  case object LocalDef extends BoundBy
}
