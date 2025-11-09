/*
 * Copyright 2015-2016 Magnus Madsen
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

package ca.uwaterloo.flix.language

import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.language.ast.shared.Source
import ca.uwaterloo.flix.util.Formatter

import scala.collection.mutable

/**
  * A common super-type for compilation messages.
  */
trait CompilationMessage {

  /**
    * Returns the kind of error message, e.g. "Syntax Error" or "Type Error".
    */
  def kind: CompilationMessageKind

  /**
    * Returns the input source of the error message.
    */
  def source: Source = loc.source

  /**
    * Returns the primary source location of the error.
    */
  def loc: SourceLocation

  /**
    * Returns additional locations associated with the error.
    */
  def locs: List[SourceLocation] = Nil

  /**
    * Returns a short description of the error message.
    */
  def summary: String

  /**
    * Returns the error message.
    *
    * You probably want to use [[messageWithLoc]] instead.
    */
  def message(formatter: Formatter): String

  /**
    * Returns a formatted string with helpful suggestions.
    */
  def explain(formatter: Formatter): Option[String] = None

  /**
    * Returns the error message formatted with source location.
    */
  def messageWithLoc(formatter: Formatter): String = {
    formatter.line(kind.toString, source.name) + System.lineSeparator() + message(formatter)
  }

  /**
    * Returns the given message `m` but with a URL linking to the source code of the error on GitHub.
    */
  protected def messageWithLink(m: String)(implicit f: sourcecode.FullName, l: sourcecode.Line): String = {
    // Assumes that flix.dev is configured with:
    //   location ~ ^/go/(.*)$ {
    //     return 301 https://github.com/flix/flix/edit/master/main/src/ca/uwaterloo/flix/$1;
    //   }
    val base = "https://flix.dev/go"

    // We convert the Java name:
    //   "ca.uwaterloo.flix.language.errors.ResolutionError.UndefinedName.message"
    // to:
    //   "language/errors/ResolutionError.scala"
    val file = f.value.split('.').drop(3).dropRight(2).mkString("/") + ".scala"
    val line = l.value
    val url = s"$base/$file#L$line"

    s"""$m
       |~ Want to help improve this error message? Create a PR on GitHub:
       |  $url
       |""".stripMargin
  }

}

object CompilationMessage {

  /**
    * Filters compilation messages to prevent cascading errors.
    *
    * When an error occurs in an early phase (e.g., lexer), it often causes spurious errors
    * in later phases (e.g., parser, typer) at the same or overlapping source locations.
    * This method returns only the earliest-phase error for each source location, filtering
    * out later errors that are likely consequences of the earlier one.
    *
    * The filtering works as follows:
    * 1. Messages are processed in phase order (lexer → parser → weeder → ... → safety)
    * 2. For each phase, we include messages whose locations don't overlap with
    *    messages already included from earlier phases
    * 3. A location A "overlaps" with location B if A is contained within B
    * 4. Messages from the same phase never shadow each other, even if they overlap
    *
    * @param l the list of compilation messages to filter
    * @return the filtered list containing only the earliest relevant error for each location
    */
  def filterShadowedMessages(l: List[CompilationMessage]): List[CompilationMessage] = {
    // Accumulator for messages that pass the filter
    val result = mutable.ArrayBuffer.empty[CompilationMessage]

    // Group messages by their error kind for efficient lookup
    val msgByKind = l.groupBy(_.kind)

    // Start with the first phase in the compilation pipeline
    var current: Option[CompilationMessageKind] = Some(CompilationMessageKind.LexerError)

    // Process each phase in order
    while (current.isDefined) {
      val c = current.get

      // Messages to include from this phase.
      val acc = mutable.ArrayBuffer.empty[CompilationMessage]

      // Check if there are any messages for this phase
      msgByKind.get(c) match {
        case None => // No messages for this phase, continue to next
        case Some(msgs) =>
          // For each message in this phase
          for (msg <- msgs) {
            // Only include if no earlier message contains this location
            if (!result.exists(m => m.loc.contains(msg.loc))) {
              acc += msg
            }
          }
      }

      // Add messages from this phase.
      result ++= acc

      // Move to the next phase in the pipeline
      current = c.next
    }
    result.toList
  }

}
