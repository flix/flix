/*
 *  Copyright 2016 Magnus Madsen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package ca.uwaterloo.flix.util

import ca.uwaterloo.flix.language.ast.SourceLocation

/**
  * Inspired by Dotty.
  *
  * https://github.com/lampepfl/dotty/blob/master/src/dotty/tools/dotc/printing/Highlighting.scala
  */
object Highlight {

  // TODO: Deprecated. Slow migrate dependencies to use FormattedMessage.

  implicit class Helper(val sc: StringContext) extends AnyVal {
    def hl(args: Any*): String = {
      val strings = sc.parts.iterator
      val expressions = args.iterator
      val buf = new StringBuffer(strings.next)
      while (strings.hasNext) {
        buf append expressions.next
        buf append strings.next
      }
      buf.toString
    }
  }

  abstract class Highlight(color: String) {
    def text: String

    override def toString: String = color + text + Console.RESET
  }

  case class Code(loc: SourceLocation, msg: String) {
    private val beginLine = loc.beginLine
    private val beginCol = loc.beginCol
    private val endLine = loc.endLine
    private val endCol = loc.endCol
    private val lineAt = loc.lineAt

    /**
      * Returns this line of code with the source location underlined.
      */
    override def toString: String = if (beginLine == endLine) underline else leftline

    /**
      * Highlights this source location with red arrows under the text.
      */
    private def underline: String = {
      val lineNo = beginLine.toString + " | "
      val line1 = lineNo + lineAt(beginLine) + "\n"
      val line2 = " " * (beginCol + lineNo.length - 1) + Red("^" * (endCol - beginCol)) + "\n"
      val line3 = " " * (beginCol + lineNo.length - 1) + msg
      line1 + line2 + line3
    }

    /**
      * Highlights this source location with red arrows left of the text.
      */
    private def leftline: String = {
      val sb = new StringBuilder()
      for (lineNo <- beginLine to endLine) {
        val currentLine = lineAt(lineNo)
        sb.
          append(lineNo).append(" |").
          append(Red(">") + " ").
          append(currentLine).
          append("\n")
      }
      sb.append("\n")
      sb.append(msg)
      sb.toString()
    }
  }

  case class Red(text: String) extends Highlight(Console.RED)

  case class Blue(text: String) extends Highlight(Console.BLUE)

  case class Cyan(text: String) extends Highlight(Console.CYAN)

  case class Black(text: String) extends Highlight(Console.BLACK)

  case class Green(text: String) extends Highlight(Console.GREEN)

  case class White(text: String) extends Highlight(Console.WHITE)

  case class Yellow(text: String) extends Highlight(Console.YELLOW)

  case class Magenta(text: String) extends Highlight(Console.MAGENTA)

  case class Bold(text: String) extends Highlight(Console.BOLD)

  case class Underline(text: String) extends Highlight(Console.UNDERLINED)

}
