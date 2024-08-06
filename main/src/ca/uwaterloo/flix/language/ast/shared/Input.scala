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

import java.nio.file.Path

/**
  * A common super-type for inputs.
  */
sealed trait Input {

  /**
   * Returns the security context associated with the input.
   */
  def security: SecurityContext = SecurityContext.AllPermissions

}

object Input {

  /**
    * A source that is backed by an internal resource.
    *
    * A source is stable if it cannot change after being loaded (e.g. the standard library, etc).
    */
  case class Text(name: String, text: String, stable: Boolean) extends Input {
    override def hashCode(): Int = name.hashCode

    override def equals(obj: Any): Boolean = obj match {
      case that: Text => this.name == that.name
      case _ => false
    }
  }

  /**
    * A source that is backed by a regular file.
    */
  case class TxtFile(path: Path) extends Input

  /**
    * A source that is backed by flix package file.
    */
  case class PkgFile(path: Path) extends Input

  /**
   * Represents an input from an unknown source.
   */
   case object Unknown extends Input

}
