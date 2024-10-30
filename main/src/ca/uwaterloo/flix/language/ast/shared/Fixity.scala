/*
 * Copyright 2024 Magnus Madsen
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
  * A common super-type for the fixity of an atom.
  */
sealed trait Fixity

object Fixity {

  /**
    * The atom is loose (it does not have to be fully materialized before it can be used).
    */
  case object Loose extends Fixity

  /**
    * The atom is fixed (it must be fully materialized before it can be used).
    */
  case object Fixed extends Fixity

}
