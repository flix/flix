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
  def security: SecurityContext = this match {
    case Input.Text(_, _, sctx) => sctx
    case Input.TxtFile(_, sctx) => sctx
    case Input.PkgFile(_, sctx) => sctx
    case Input.FileInPackage(_, _, _, sctx) => sctx
    case Input.Unknown => SecurityContext.AllPermissions
  }

}

object Input {

  /**
    * Represents an input that originates from a virtual path.
    */
  case class Text(name: String, text: String, sctx: SecurityContext) extends Input {
    override def hashCode(): Int = name.hashCode

    override def equals(obj: Any): Boolean = obj match {
      case that: Text => this.name == that.name
      case _ => false
    }

    override def toString: String = name
  }

  /**
    * Represents an input that originates from the filesystem.
    */
  case class TxtFile(path: Path, sctx: SecurityContext) extends Input {
    override def hashCode(): Int = path.hashCode()

    override def equals(obj: Any): Boolean = obj match {
      case that: TxtFile => this.path == that.path
      case _ => false
    }
  }

  /**
    * Represents an input, which is a package, on the filesystem.
    */
  case class PkgFile(packagePath: Path, sctx: SecurityContext) extends Input {
    override def hashCode(): Int = packagePath.hashCode()

    override def equals(obj: Any): Boolean = obj match {
      case that: PkgFile => this.packagePath == that.packagePath
      case _ => false
    }
  }

  /**
    * Represents an input that originates from inside a package.
    */
  case class FileInPackage(packagePath: Path, virtualPath: String, text: String, sctx: SecurityContext) extends Input {
    override def hashCode(): Int = packagePath.hashCode()

    override def equals(obj: Any): Boolean = obj match {
      case that: FileInPackage => this.packagePath == that.packagePath
      case _ => false
    }
  }

  /**
    * Represents an input from an unknown source.
    */
  case object Unknown extends Input

}
