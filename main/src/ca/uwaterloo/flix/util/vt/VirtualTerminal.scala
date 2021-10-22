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
   * Appends the given int to this terminal.
   */
  def <<(i: Int): VirtualTerminal = <<(text(i.toString))

  /**
   * Appends the given string to this terminal.
   */
  def <<(s: String): VirtualTerminal = <<(text(s.toString))

  /**
    * Appends the given string to this terminal.
    */
  def text(s: String): VirtualTerminal = {
    buffer = text(s) :: buffer
    this
  }

  /**
    * Appends the given virtual string to this terminal.
    */
  def <<(s: VirtualString): VirtualTerminal = s match {
    case VirtualString.code(loc, msg) => highlight(loc, text(msg)); this
    case _ => buffer = s :: buffer; this
  }

  /**
    * Appends the content of the given virtual terminal `vt` to this terminal.
    */
  def <<(vt: VirtualTerminal): VirtualTerminal = {
    buffer = vt.buffer ::: this.buffer
    this
  }

  /**
    * Returns the buffer of the virtual terminal as a string.
    */
  def fmt(implicit ctx: TerminalContext): String = {
    val sb = new StringBuilder
    var indentation: Int = 0
    for (t <- buffer.reverse) {
      t match {
        // Control Characters
        case NewLine => sb.append("\n" + "  " * indentation)
        case Indent => indentation = indentation + 1
        case Dedent => indentation = indentation - 1

        // Colors
        case text(s) => sb.append(s)
        case black(s) => sb.append(ctx.emitBlack(s))
        case blue(s) => sb.append(ctx.emitBlue(s))
        case cyan(s) => sb.append(ctx.emitCyan(s))
        case green(s) => sb.append(ctx.emitGreen(s))
        case magenta(s) => sb.append(ctx.emitMagenta(s))
        case red(s) => sb.append(ctx.emitRed(s))
        case yellow(s) => sb.append(ctx.emitYellow(s))
        case white(s) => sb.append(ctx.emitWhite(s))

        // Formatting
        case bold(s) => sb.append(ctx.emitBold(s))
        case underline(s) => sb.append(ctx.emitUnderline(s))

        // Macros
        case line(l, r) => sb.append(ctx.emitBlue(s"-- $l -------------------------------------------------- $r\n"))
        case code(l, m) => // NB: Already de-sugared.
      }
    }
    sb.toString()
  }

  /////////////////////////////////////////////////////////////////////////////
  /// Highlights Source Code                                                ///
  /////////////////////////////////////////////////////////////////////////////
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
      this << " " * (beginCol + lineNo.length - 1) << red("^" * (endCol - beginCol)) << NewLine
      this << " " * (beginCol + lineNo.length - 1)
      this << msg
      this << NewLine
    }

    def leftline(): Unit = {
      for (lineNo <- beginLine to endLine) {
        val currentLine = lineAt(lineNo)
        this << lineNo << " |" << red(">") << " " << currentLine << NewLine
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
