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

package ca.uwaterloo.flix.util.vt

import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.util.vt.VirtualString._

class VirtualTerminal() {

  /**
    * The current lines.
    */
  private var buffer: List[VirtualString] = Nil

  /**
    * The current indentation level.
    */
  private var indentation: Int = 0

  def <<(i: Int): VirtualTerminal = <<(Text(i.toString))

  def <<(s: String): VirtualTerminal = <<(Text(s.toString))

  def <<(s: VirtualString): VirtualTerminal = s match {
    case VirtualString.NewLine => newLine()
    case VirtualString.Code(loc, msg) => {
      highlight(loc, Text(msg))
      this
    }
    case _ => text(s)
  }

  def <<(vt: VirtualTerminal): VirtualTerminal = {
    // TODO: check order
    buffer = vt.buffer ::: this.buffer
    this
  }

  // TODO: Remove
  def text(t: VirtualString): VirtualTerminal = {
    buffer = t :: buffer
    this
  }

  // TODO: Remove
  def text(s: String): VirtualTerminal = text(VirtualString.Text(s))

  // TODO: Remove
  private def newLine(): VirtualTerminal = {
    val id: List[VirtualString] = (0 until indentation).map(x => Text(" ")).toList
    buffer = id ::: NewLine :: buffer
    this
  }

  /**
    * Returns the buffer of the virtual terminal as a string.
    */
  def fmt(implicit ctx: TerminalContext): String = {
    buffer.reverse.map {
      case Indent => indent(); ""
      case Dedent => dedent(); ""
      case NewLine => "\n" + "  " * indentation
      case x => fmt(x)
    }.mkString("")
  }

  /**
    * Formats `this` text according to the given terminal context.
    */
  private def fmt(c: VirtualString)(implicit ctx: TerminalContext): String = c match {
    case VirtualString.NewLine => "\n"
    case VirtualString.Text(s) => s
    case VirtualString.Black(s) => ctx.emitBlack(s)
    case VirtualString.Blue(s) => ctx.emitBlue(s)
    case VirtualString.Cyan(s) => ctx.emitCyan(s)
    case VirtualString.Green(s) => ctx.emitGreen(s)
    case VirtualString.Magenta(s) => ctx.emitMagenta(s)
    case VirtualString.Red(s) => ctx.emitRed(s)
    case VirtualString.Yellow(s) => ctx.emitYellow(s)
    case VirtualString.White(s) => ctx.emitWhite(s)
    case VirtualString.Bold(s) => ctx.emitBold(s)
    case VirtualString.Underline(s) => ctx.emitUnderline(s)
    case VirtualString.Line(l, r) => ctx.emitBlue(s"-- $l -------------------------------------------------- $r\n")
    case _ => ""
  }

  /////////////////////////////////////////////////////////////////////////////
  /// Indentation                                                           ///
  /////////////////////////////////////////////////////////////////////////////
  /**
    * Increases indentation by one level.
    */
  private def indent(): VirtualTerminal = {
    indentation = indentation + 1
    this
  }

  /**
    * Decreases indentation by one level.
    */
  private def dedent(): VirtualTerminal = {
    indentation = indentation - 1
    this
  }

  /////////////////////////////////////////////////////////////////////////////
  /// Highlights Source Code                                                ///
  /////////////////////////////////////////////////////////////////////////////
  // TODO: Need array of msg?
  /**
    * Highlights the given source location `loc` with the given message `msg`.
    */
  private def highlight(loc: SourceLocation, msg: VirtualString*): Unit = {
    val beginLine = loc.beginLine
    val beginCol = loc.beginCol
    val endLine = loc.endLine
    val endCol = loc.endCol
    val lineAt = loc.lineAt

    def underline(): Unit = {
      val lineNo = beginLine.toString + " | "

      this << lineNo << lineAt(beginLine) << NewLine
      this << " " * (beginCol + lineNo.length - 1) << Red("^" * (endCol - beginCol)) << NewLine
      this << " " * (beginCol + lineNo.length - 1)
      for (m <- msg) {
        this << m
      }
      this << NewLine
    }

    def leftline(): Unit = {
      for (lineNo <- beginLine to endLine) {
        val currentLine = lineAt(lineNo)
        this << lineNo << " |" << Red(">") << " " << currentLine << NewLine
      }
      this << NewLine
      for (m <- msg) {
        this << m
      }
      this << NewLine
    }

    if (beginLine == endLine)
      underline()
    else
      leftline()
  }

}
