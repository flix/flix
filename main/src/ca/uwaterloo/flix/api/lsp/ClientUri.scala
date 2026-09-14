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

import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.language.ast.shared.SourceName

import java.net.{URI, URISyntaxException}
import java.nio.file.{InvalidPathException, Path}
import java.util.concurrent.ConcurrentHashMap

/**
  * The conversion between the URIs a language client uses and the names the compiler uses.
  *
  * This is the only place that turns a client's URI into a source name, or a source name back
  * into a URI. A `file:` URI names a path, so that a document open in an editor and the same file
  * read from disk are one source. A string without a scheme is a path too. Any other scheme, such
  * as an editor's `untitled:` buffer, is kept as a URI.
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
    * Returns the source name the client string `uri` denotes, or `None` if `uri` is malformed.
    */
  def toSourceName(uri: String): Option[SourceName] = {
    val name = parse(uri)
    name.foreach(n => spellings.put(n, uri))
    name
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
    * Parses the client string `uri` into a source name, without remembering it.
    */
  private def parse(uri: String): Option[SourceName] = {
    val parsed = try {
      Some(new URI(uri))
    } catch {
      case _: URISyntaxException => None
    }
    parsed.flatMap { u =>
      val scheme = u.getScheme
      if (scheme == null) {
        try {
          Some(SourceName.PathName(Path.of(uri)))
        } catch {
          case _: InvalidPathException => None
        }
      } else if (scheme.equalsIgnoreCase("file")) {
        try {
          Some(SourceName.PathName(Path.of(u).normalize()))
        } catch {
          case _: IllegalArgumentException => None
        }
      } else {
        Some(SourceName.UriName(u))
      }
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
