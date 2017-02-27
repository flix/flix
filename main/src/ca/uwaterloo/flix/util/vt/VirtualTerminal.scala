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

  // TODO: Rename VirtualString to VirtualToken?

  // TODO: Add more primitive types
  def <<(i: Int): VirtualTerminal = <<(Text(i.toString))

  def <<(s: String): VirtualTerminal = <<(Text(s.toString))


  def <<(s: VirtualString): VirtualTerminal = s match {
    case VirtualString.NewLine => newLine()
    case VirtualString.Code(loc, msg) => {
      highlight(loc, Text(msg))
      this
    }
    case _ => {
      buffer = s :: buffer
      this
    }
  }

  /**
    * Appends the content of the given virtual terminal `vt` to this terminal.
    */
  def <<(vt: VirtualTerminal): VirtualTerminal = {
    buffer = vt.buffer ::: this.buffer
    this
  }

  // TODO: Remove
  def text(s: String): VirtualTerminal = {
    buffer = Text(s) :: buffer
    this
  }

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
    val sb = new StringBuilder

    // TODO: SORT
    for (t <- buffer.reverse) {
      t match {
        // Control Characters
        case NewLine => sb.append("\n" + "  " * indentation)
        case Indent => indent()
        case Dedent => dedent()

        // Colors
        case Text(s) => sb.append(s)
        case Black(s) => sb.append(ctx.emitBlack(s))
        case Blue(s) => sb.append(ctx.emitBlue(s))
        case Cyan(s) => sb.append(ctx.emitCyan(s))
        case Green(s) => sb.append(ctx.emitGreen(s))
        case Magenta(s) => sb.append(ctx.emitMagenta(s))
        case Red(s) => sb.append(ctx.emitRed(s))
        case Yellow(s) => sb.append(ctx.emitYellow(s))
        case White(s) => sb.append(ctx.emitWhite(s))

        // Formatting
        case Bold(s) => sb.append(ctx.emitBold(s))
        case Underline(s) => sb.append(ctx.emitUnderline(s))

        // Macros
        case Line(l, r) => sb.append(ctx.emitBlue(s"-- $l -------------------------------------------------- $r\n"))
      }
    }

    sb.toString()
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
  private def highlight(loc: SourceLocation, msg: VirtualString): Unit = {
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
      this << msg
      this << NewLine
    }

    def leftline(): Unit = {
      for (lineNo <- beginLine to endLine) {
        val currentLine = lineAt(lineNo)
        this << lineNo << " |" << Red(">") << " " << currentLine << NewLine
      }
      this << NewLine
      this << msg
      this << NewLine
    }

    if (beginLine == endLine)
      underline()
    else
      leftline()
  }

}
