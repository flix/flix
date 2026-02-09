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
package ca.uwaterloo.flix.language.ast.shared

/**
  * Represents a symbol use that may or may not have been resolved during name resolution.
  *
  * - [[SymOrNot.Found]] indicates that the symbol was successfully resolved.
  * - [[SymOrNot.NotFound]] indicates that name resolution failed (the error is reported separately).
  */
sealed trait SymOrNot[+T]

object SymOrNot {

  /** The symbol was successfully resolved to `t`. */
  case class Found[T](t: T) extends SymOrNot[T]

  /** The symbol could not be resolved. */
  case object NotFound extends SymOrNot[Nothing]

}
