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

import ca.uwaterloo.flix.language.ast.shared.SourceName

import java.net.{URI, URISyntaxException}
import java.nio.file.{InvalidPathException, Path}
import scala.collection.mutable

/**
  * The conversion between the URIs a language client uses and the names the compiler uses.
  *
  * Not yet used to register or match sources: the language servers still work on the client's
  * strings. It runs in the shadow of every registration and every request instead, to check on
  * real client input that the conversion round-trips before the servers are switched over to it.
  * See [[shadow]].
  */
object ClientUri {

  /**
    * Returns the source name the client string `uri` denotes, or `None` if `uri` is malformed.
    *
    * A `file:` URI names a path, so that a document open in an editor and the same file added
    * from disk are the same source. A string without a scheme is a path too. Any other scheme,
    * such as an editor's `untitled:` buffer, is kept as a URI.
    */
  def toSourceName(uri: String): Option[SourceName] = {
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
          Some(SourceName.PathName(Path.of(u)))
        } catch {
          case _: IllegalArgumentException => None
        }
      } else {
        Some(SourceName.UriName(u))
      }
    }
  }

  /**
    * Returns the string the client uses for the source named `name`.
    *
    * An absolute path is sent as a `file:` URI. A relative path, which only the compiler's own
    * sources have, is sent as is. A URI name is sent as the text it was parsed from, so a client's
    * own URI round-trips unchanged.
    */
  def fromSourceName(name: SourceName): String = name match {
    case SourceName.PathName(path) => if (path.isAbsolute) path.toUri.toString else path.toString
    case SourceName.UriName(uri) => uri.toString
    case SourceName.PackageEntry(_, _) => name.toString
  }

  /**
    * The distinct client strings that did not survive the round trip through [[toSourceName]] and
    * [[fromSourceName]], each with what the round trip produced, or `None` if the string did not
    * parse. In the order they were first seen.
    */
  private val mismatches: mutable.LinkedHashMap[String, Option[String]] = mutable.LinkedHashMap.empty

  /**
    * Returns the mismatches recorded by [[shadow]] so far.
    */
  def shadowMismatches: List[(String, Option[String])] = synchronized {
    mismatches.toList
  }

  /**
    * Runs the client string `uri` through the round trip and, if it does not come back unchanged,
    * records it once on stderr and in [[shadowMismatches]].
    *
    * Never throws and never changes what the caller does with `uri`.
    */
  def shadow(uri: String): Unit = {
    val roundTrip = toSourceName(uri).map(fromSourceName)
    if (!roundTrip.contains(uri)) {
      val fresh = synchronized {
        if (mismatches.contains(uri)) {
          false
        } else {
          mismatches.put(uri, roundTrip)
          true
        }
      }
      if (fresh) {
        roundTrip match {
          case None => System.err.println(s"[flix-lsp] shadow: the URI '$uri' does not parse as a source name.")
          case Some(other) => System.err.println(s"[flix-lsp] shadow: the URI '$uri' round-trips to '$other'.")
        }
      }
    }
  }

}
