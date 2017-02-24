/*
 * Copyright 2017 Magnus Madsen
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

package ca.uwaterloo.flix.util.terminal

import ca.uwaterloo.flix.language.ast.SourceLocation

sealed trait Color {
  /**
    * Formats `this` text according to the given color context.
    */
  def fmt(implicit ctx: ColorContext): String = this match {
    case Color.Txt(s) => s
    case Color.Black(s) => ctx.fmtBlack(s.fmt)
    case Color.Blue(s) => ctx.fmtBlue(s.fmt)
    case Color.Cyan(s) => ctx.fmtCyan(s.fmt)
    case Color.Green(s) => ctx.fmtGreen(s.fmt)
    case Color.Magenta(s) => ctx.fmtMagenta(s.fmt)
    case Color.Red(s) => ctx.fmtRed(s.fmt)
    case Color.Yellow(s) => ctx.fmtYellow(s.fmt)
    case Color.White(s) => ctx.fmtWhite(s.fmt)
    case Color.Bold(s) => ctx.fmtBold(s.fmt)
    case Color.Underline(s) => ctx.fmtUnderline(s.fmt)
    case c: Color.Code => c.fmt
  }
}

object Color {

  case class Txt(text: String) extends Color

  case class Black(text: Color) extends Color

  case class Blue(text: Color) extends Color

  case class Cyan(text: Color) extends Color

  case class Green(text: Color) extends Color

  case class Magenta(text: Color) extends Color

  case class Red(text: Color) extends Color

  case class Yellow(text: Color) extends Color

  case class White(text: Color) extends Color

  case class Bold(text: Color) extends Color

  case class Underline(text: Color) extends Color

  case class Code(loc: SourceLocation, msg: String) extends Color {
    private val beginLine = loc.beginLine
    private val beginCol = loc.beginCol
    private val endLine = loc.endLine
    private val endCol = loc.endCol
    private val lineAt = loc.lineAt

    /**
      * Returns this line of code with the source location underlined.
      */
    def fmt: String = if (beginLine == endLine) underline else leftline

    /**
      * Highlights this source location with red arrows under the text.
      */
    private def underline: String = {
      import ColorString._

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
      import ColorString._

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

}