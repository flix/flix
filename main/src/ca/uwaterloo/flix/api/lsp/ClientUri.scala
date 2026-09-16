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
package ca.uwaterloo.flix.api.lsp

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.language.ast.shared.{SecurityContext, SourceName}
import ca.uwaterloo.flix.util.InternalCompilerException

import java.net.URI
import java.nio.file.{FileSystemNotFoundException, Path}
import java.util.concurrent.ConcurrentHashMap

/**
  * The conversion between the URIs a language client uses and the names the compiler uses.
  *
  * This is the only place that turns a client's URI into a source name, or a source name back
  * into a URI. A `file:` URI names a path, so that a document open in an editor and the same file
  * read from disk are one source. Any other URI, such as an editor's `untitled:` buffer, is kept
  * as a URI.
  *
  * The client's own spelling of every name it has used is remembered, so that what goes back to
  * the client is exactly what came from it, whatever the platform makes of a path. A URI is
  * synthesized only for a source the client never named, such as a file of the library.
  */
object ClientUri {

  /**
    * The client's spelling of each name it has used, most recent first.
    */
  private val spellings: ConcurrentHashMap[SourceName, String] = new ConcurrentHashMap()

  /**
    * Returns the source name the client URI `uri` denotes.
    *
    * A `file:` URI names a path when the platform can convert it, and stays a URI name when it
    * cannot, e.g. a UNC name on a host without UNC. Any other URI, such as an editor's
    * `untitled:` buffer, is a URI name.
    */
  def toSourceName(uri: URI): SourceName = {
    val name = parse(uri)
    spellings.put(name, uri.toString)
    name
  }

  /**
    * Returns the path the client URI `uri` denotes, or `None` if it denotes no path.
    *
    * Used for a directory a client names, such as a workspace root. A source is named with
    * [[toSourceName]] instead, which keeps a URI that denotes no path as a name of its own.
    */
  def toPath(uri: URI): Option[Path] = try {
    Some(Path.of(uri).normalize())
  } catch {
    case _: IllegalArgumentException => None
    case _: FileSystemNotFoundException => None
  }

  /**
    * Returns the string the client uses for the source named `name`: its own spelling if it has
    * ever used one, otherwise a `file:` URI for an absolute path, the path itself for a relative
    * path, which only the compiler's own sources have, and the text a URI name was parsed from.
    */
  def fromSourceName(name: SourceName): String = {
    val spelling = spellings.get(name)
    if (spelling != null) {
      spelling
    } else {
      synthesize(name)
    }
  }

  /**
    * Returns the string the client uses for the source of `loc`.
    */
  def fromLocation(loc: SourceLocation): String = fromSourceName(loc.source.sourceName)

  /**
    * Adds the source `text` to `flix` under `name`, which a client has produced through
    * [[toSourceName]] and is therefore a path or a URI.
    */
  def addSource(flix: Flix, name: SourceName, text: String): Unit = name match {
    case SourceName.PathName(path) => flix.addSource(path, text, SecurityContext.Unrestricted)
    case SourceName.UriName(uri) => flix.addSource(uri, text, SecurityContext.Unrestricted)
    case SourceName.PackageEntry(_, _) => throw InternalCompilerException(s"Unexpected package entry '$name' from a client.", SourceLocation.Unknown)
  }

  /**
    * Removes the source named `name`, which a client has produced through [[toSourceName]], from `flix`.
    */
  def remSource(flix: Flix, name: SourceName): Unit = name match {
    case SourceName.PathName(path) => flix.remSource(path)
    case SourceName.UriName(uri) => flix.remSource(uri)
    case SourceName.PackageEntry(_, _) => throw InternalCompilerException(s"Unexpected package entry '$name' from a client.", SourceLocation.Unknown)
  }

  /**
    * Parses the client URI `uri` into a source name, without remembering it.
    */
  private def parse(uri: URI): SourceName = {
    val scheme = uri.getScheme
    if (scheme != null && scheme.equalsIgnoreCase("file")) {
      try {
        SourceName.PathName(Path.of(uri).normalize())
      } catch {
        case _: IllegalArgumentException => SourceName.UriName(uri)
      }
    } else {
      SourceName.UriName(uri)
    }
  }

  /**
    * Returns a string for a name the client has never spelled.
    */
  private def synthesize(name: SourceName): String = name match {
    case SourceName.PathName(path) => if (path.isAbsolute) path.toUri.toString else path.toString
    case SourceName.UriName(uri) => uri.toString
    case SourceName.PackageEntry(_, _) => name.toString
  }

}
