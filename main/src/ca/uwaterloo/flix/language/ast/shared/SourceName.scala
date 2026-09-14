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
import java.nio.file.Path

/**
  * The name of a source. Two sources with the same name are the same source.
  *
  * The name is what the compiler keys on: the registered sources, the change set, and the
  * dependency graph. Its string form is what error messages print and what the language
  * server hands back to the client.
  */
sealed trait SourceName {

  // Every case caches its hash: names are hashed on hot paths, e.g. once per dependency edge and
  // as part of every source location, and a product hash over a path would be computed each time.

  /**
    * Returns the name as a path, if it denotes one.
    *
    * A path name is returned as is. An entry of a package is its path relative to the package
    * root. A URI name never denotes a path: a language server registers a document that is a file
    * under its path, so a URI name only ever names a document that is not a file, such as an
    * editor's `untitled:` buffer.
    */
  def toPath: Option[Path] = this match {
    case SourceName.PathName(path) => Some(path)
    case SourceName.UriName(_) => None
    case SourceName.PackageEntry(_, entry) => Some(Path.of(entry))
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
  case class PathName(path: Path) extends SourceName {
    override val hashCode: Int = path.hashCode()
  }

  /**
    * A name that is a URI: a document of a language client that is not a file, such as an editor's
    * `untitled:` buffer. A file open in an editor is named by its path.
    */
  case class UriName(uri: URI) extends SourceName {
    override val hashCode: Int = uri.hashCode()
  }

  /**
    * The name of the file `entry` inside the Flix package at `pkg`.
    */
  case class PackageEntry(pkg: Path, entry: String) extends SourceName {
    override val hashCode: Int = 31 * pkg.hashCode() + entry.hashCode
  }

}
