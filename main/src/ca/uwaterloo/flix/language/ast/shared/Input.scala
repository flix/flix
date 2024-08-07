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
    * Returns `true` if the input is stable (i.e. cannot be changed once loaded).
    */
  def isStable: Boolean = this match {
    case Input.Text(_, _, stable) => stable
    case Input.StandardLibrary(_, _) => true
    case Input.TxtFile(_) => false
    case Input.PkgFile(_) => false
    case Input.Unknown => false
  }

  /**
    * Returns the security context associated with the input.
    */
  def security: SecurityContext = SecurityContext.AllPermissions

}

object Input {

  /**
    * Represents an input that originates from a virtual path.
    */
  case class Text(name: String, text: String, stable: Boolean) extends Input {
    override def hashCode(): Int = name.hashCode

    override def equals(obj: Any): Boolean = obj match {
      case that: Text => this.name == that.name
      case _ => false
    }
  }

  /**
    * Represent an input that originates from the built-in Standard Library.
    *
    * @param virtualPath the virtual path of the source code.
    * @param text        the source code text.
    *
    */
  case class StandardLibrary(virtualPath: String, text: String) extends Input

  /**
    * Represents an input originates from the filesystem.
    */
  case class TxtFile(path: Path) extends Input

  /**
    * Represents an input, which is a package, on the filesystem.
    */
  case class PkgFile(path: Path) extends Input

  /**
    * Represents an input from an unknown source.
    */
  case object Unknown extends Input

}
