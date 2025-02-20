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

sealed trait TraitUsageKind

/**
  * Represents the kind of trait use.
  * Used to indicate the context of an undefined trait
  */
object TraitUsageKind {
  /**
    * Represents a trait use in a call
    * e.g. let res = E...
    */
  case object Call extends TraitUsageKind

  /**
    * Represents a trait use in a constraint
    * e.g. def f(a: t) : Unit with E...
    * e.g. trait Foo[t] with E...
    */
  case object Constraint extends TraitUsageKind

  /**
    * Represents a trait use in a derivation
    * e.g. enum Color with E...
    */
  case object Derivation extends TraitUsageKind

  /**
    * Represents a trait use in an instance declaration
    * e.g. instance E...
    */
  case object Implementation extends TraitUsageKind
}
