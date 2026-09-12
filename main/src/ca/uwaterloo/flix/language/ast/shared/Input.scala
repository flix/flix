/*
 * Copyright 2024 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
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
    case Input.RealFile(_, sctx) => sctx
    case Input.VirtualFile(_, _, sctx) => sctx
    case Input.VirtualUri(_, _, sctx) => sctx
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
