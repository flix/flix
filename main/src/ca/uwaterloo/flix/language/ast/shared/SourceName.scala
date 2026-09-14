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

import java.net.URI
import java.nio.file.{FileSystemNotFoundException, Path}

/**
  * The name of a source. Two sources with the same name are the same source.
  *
  * The name is what the compiler keys on: the registered sources, the change set, and the
  * dependency graph. Its string form is what error messages print and what the language
  * server hands back to the client.
  */
sealed trait SourceName {

  /**
    * Returns the name as a path, if it denotes one.
    *
    * A path name is returned as is. A `file:` URI is converted to a path. Any other URI, and
    * any entry of a package, has no path.
    */
  def toPath: Option[Path] = this match {
    case SourceName.PathName(path) => Some(path)
    case SourceName.UriName(uri) => try {
      Some(Path.of(uri))
    } catch {
      case _: IllegalArgumentException => None
      case _: FileSystemNotFoundException => None
    }
    case SourceName.PackageEntry(_, _) => None
  }

  /**
    * Returns the name as a string.
    *
    * For a URI this is the string the URI was parsed from, so a name that came from a language
    * client round-trips unchanged.
    */
  override def toString: String = this match {
    case SourceName.PathName(path) => path.toString
    case SourceName.UriName(uri) => uri.toString
    case SourceName.PackageEntry(pkg, entry) => pkg.getFileName.toString + ":" + entry
  }

}

object SourceName {

  /**
    * A name that is a path. The path need not exist on disk: a fragment typed into the REPL and a
    * file of the bundled library have path names too.
    */
  case class PathName(path: Path) extends SourceName

  /**
    * A name that is a URI, as used by language clients.
    */
  case class UriName(uri: URI) extends SourceName

  /**
    * The name of the file `entry` inside the Flix package at `pkg`.
    */
  case class PackageEntry(pkg: Path, entry: String) extends SourceName

}
