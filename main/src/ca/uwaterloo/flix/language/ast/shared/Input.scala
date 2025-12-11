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

import java.net.URI
import java.nio.file.Path

/**
  * A common super-type for inputs.
  */
sealed trait Input {

  /**
    * Returns the security context associated with the input.
    */
  def security: SecurityContext = this match {
    case Input.VirtualFile(_, _, sctx) => sctx
    case Input.VirtualUri(_, _, sctx) => sctx
    case Input.RealFile(_, sctx) => sctx
    case Input.PkgFile(_, sctx) => sctx
    case Input.FileInPackage(_, _, _, sctx) => sctx
    case Input.Unknown => SecurityContext.Unrestricted
  }

}

object Input {

  /**
    * Represents an input that points to the file system and which must exist.
    */
  case class RealFile(realPath: Path, sctx: SecurityContext) extends Input

  /**
    * Represents an input with the source code text `src` located at `virtualPath` -- a path that may not actually exist.
    */
  case class VirtualFile(virtualPath: Path, src: String, sctx: SecurityContext) extends Input {
    override def hashCode(): Int = virtualPath.hashCode

    override def equals(obj: Any): Boolean = obj match {
      case that: VirtualFile => this.virtualPath == that.virtualPath
      case _ => false
    }

    override def toString: String = virtualPath.toString
  }

  /**
    * Represents an input with the source code text `src` located at `virtualUri` -- a URI that may not actually exist.
    */
  case class VirtualUri(virtualUri: URI, src: String, sctx: SecurityContext) extends Input {
    override def hashCode(): Int = virtualUri.hashCode

    override def equals(obj: Any): Boolean = obj match {
      case that: VirtualUri => this.virtualUri == that.virtualUri
      case _ => false
    }

    override def toString: String = virtualUri.toString
  }

  /**
    * Represents an input, which is a package, on the filesystem.
    */
  case class PkgFile(packagePath: Path, sctx: SecurityContext) extends Input

  /**
    * Represents an input that originates from inside a package.
    */
  case class FileInPackage(packagePath: Path, virtualPath: String, text: String, sctx: SecurityContext) extends Input

  /**
    * Represents an input from an unknown source.
    */
  case object Unknown extends Input

}
