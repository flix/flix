/*
 *  Copyright 2017 Magnus Madsen
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

package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.language.ast.{SourceInput, SourceLocation}
import ca.uwaterloo.flix.language.errors.Token.{Blue, Red}

class FormattedMessage() {

  sealed trait Line {
    def fmt(implicit ctx: ColorContext): String = this match {
      case Line.TextLine(tokens) => tokens.map(_.fmt).mkString("") + "\n"
    }
  }

  object Line {

    case class TextLine(x: List[Token]) extends Line

  }

  val lines = scala.collection.mutable.ListBuffer.empty[Line]
  var currentLine = List.empty[Token]

  def text(t: Token): FormattedMessage = {
    currentLine = t :: currentLine
    this
  }

  def text(s: Int): FormattedMessage = {
    currentLine = Token.Txt(s.toString) :: currentLine
    this
  }

  def text(s: String): FormattedMessage = {
    currentLine = Token.Txt(s) :: currentLine
    this
  }

  def quote(t: Token): FormattedMessage = {
    currentLine = Token.Quote(t) :: currentLine
    this
  }

  def header(kind: String, source: SourceInput): FormattedMessage = {
    text(Blue(s"-- $kind -------------------------------------------------- ${source.format}")).newLine().newLine()
    this
  }

  def highlight(loc: SourceLocation, msg: Token*): FormattedMessage = {

    val beginLine = loc.beginLine
    val beginCol = loc.beginCol
    val endLine = loc.endLine
    val endCol = loc.endCol
    val lineAt = loc.lineAt

    def underline(): Unit = {
      val lineNo = beginLine.toString + " | "

      text(lineNo).text(lineAt(beginLine)).newLine().
        text(" " * (beginCol + lineNo.length - 1)).text(Token.Red("^" * (endCol - beginCol))).newLine().
        text(" " * (beginCol + lineNo.length - 1))
      for (t <- msg) {
        text(t)
      }
      newLine()
    }

    def leftline(): Unit = {
      for (lineNo <- beginLine to endLine) {
        val currentLine = lineAt(lineNo)
        text(lineNo).text(" |").
          text(Red(">")).text(" ").
          text(currentLine).
          newLine()
      }
      newLine()
      for (t <- msg) {
        text(t)
      }
      newLine()
    }

    if (beginLine == endLine) underline() else leftline()

    this
  }

  def newLine(): FormattedMessage = {
    lines += Line.TextLine(currentLine.reverse)
    currentLine = Nil
    this
  }

  def use(f: FormattedMessage => Unit): FormattedMessage = {
    f(this)
    this
  }

  def fmt(implicit ctx: ColorContext): String = {
    lines.map(_.fmt).mkString("")
  }

}
