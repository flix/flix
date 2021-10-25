/*
 * Copyright 2021 Matthew Lutze
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
package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.{Kind, SourceLocation}
import ca.uwaterloo.flix.language.debug.FormatKind.formatKind
import ca.uwaterloo.flix.util.Formatter

/**
  * A common super-type for kind errors.
  */
sealed trait KindError extends CompilationMessage {
  val kind: String = "Kind Error"
}

object KindError {
  /**
    * An error describing mismatched inferred kinds.
    *
    * @param k1  the first kind.
    * @param k2  the second kind.
    * @param loc the location where the error occurred.
    */
  case class MismatchedKinds(k1: Kind, k2: Kind, loc: SourceLocation) extends KindError {
    override def summary: String = s"Mismatched kinds: '${formatKind(k1)}' and '${formatKind(k2)}''"

    override def message(implicit formatter: Formatter): String = {
      s"""${formatter.line(kind, source.format)}
         |>> This type variable was used as both kind 'red(formatKind(k1))' and kind 'red(formatKind(k2))'.
         |
         |${formatter.code(loc, "mismatched kind.")}
         |
         |Kind One: ${formatter.cyan(formatKind(k1))}
         |Kind Two: ${formatter.magenta(formatKind(k2))}
         |""".stripMargin
    }

    /**
      * Returns a formatted string with helpful suggestions.
      */
    override def explain(implicit formatter: Formatter): Option[String] = None
  }

  /**
    * An error describing a kind that doesn't match the expected kind.
    *
    * @param expectedKind the expected kind.
    * @param actualKind   the actual kind.
    * @param loc          the location where the error occurred.
    */
  case class UnexpectedKind(expectedKind: Kind, actualKind: Kind, loc: SourceLocation) extends KindError {
    override def summary: String = s"Kind ${formatKind(expectedKind)} was expected, but found ${formatKind(actualKind)}."

    override def message(implicit formatter: Formatter): String = {
      s"""${formatter.line(kind, source.format)}
         |>> Expected kind '${formatter.red(formatKind(expectedKind))}' here, but kind 'red(formatKind(actualKind))' is used.
         |
         |${formatter.code(loc, "unexpected kind.")}
         |
         |Expected kind: ${formatter.cyan(formatKind(expectedKind))}
         |Actual kind:   ${formatter.magenta(formatKind(actualKind))}
         |""".stripMargin
    }

    /**
      * Returns a formatted string with helpful suggestions.
      */
    override def explain(implicit formatter: Formatter): Option[String] = None
  }
}
