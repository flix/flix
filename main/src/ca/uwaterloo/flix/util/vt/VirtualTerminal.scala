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

import java.math.BigInteger

import ca.uwaterloo.flix.language.ast.{SourceInput, SourceLocation}
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

  /**
    * Increases indentation by one level.
    */
  def indent(): VirtualTerminal = {
    indentation = indentation + 1
    this
  }

  /**
    * Decreases indentation by one level.
    */
  def dedent(): VirtualTerminal = {
    indentation = indentation - 1
    this
  }


  def <<(i: Int): VirtualTerminal = <<(Text(i.toString))

  def <<(s: String): VirtualTerminal = <<(Text(s.toString))

  def <<(s: VirtualString): VirtualTerminal = s match {
    case VirtualString.NewLine => newLine()
    case VirtualString.Code(loc, msg) => {
      highlight(loc, msg)
      this
    }
    case VirtualString.RichCode(loc, msg) => {
      highlight(loc, msg)
      this
    }
    case _ => text(s)
  }

  def <<(vt: VirtualTerminal): VirtualTerminal = {
    // TODO: check order
    buffer = vt.buffer ::: this.buffer
    this
  }

  def text(t: VirtualString): VirtualTerminal = {
    buffer = t :: buffer
    this
  }

  // TODO: Remove
  def text(b: BigInteger): VirtualTerminal = text(b.toString)

  def text(s: String): VirtualTerminal = text(VirtualString.Text(s))


  // TODO: Cleanup
  def newLine(): VirtualTerminal = {
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
      case RichCode(_, _) => ""
      case x => x.fmt
    }.mkString("")
  }

  /////////////////////////////////////////////////////////////////////////////
  /// Highlights Source Code                                                ///
  /////////////////////////////////////////////////////////////////////////////
  private def highlight(loc: SourceLocation, msg: VirtualString*): Unit = {
    val beginLine = loc.beginLine
    val beginCol = loc.beginCol
    val endLine = loc.endLine
    val endCol = loc.endCol
    val lineAt = loc.lineAt

    def underline(): Unit = {
      val lineNo = beginLine.toString + " | "

      text(lineNo).text(lineAt(beginLine)).newLine().
        text(" " * (beginCol + lineNo.length - 1)).text(VirtualString.Red("^" * (endCol - beginCol))).newLine().
        text(" " * (beginCol + lineNo.length - 1))
      for (t <- msg) {
        <<(t)
      }
      newLine()
    }

    def leftline(): Unit = {
      for (lineNo <- beginLine to endLine) {
        val currentLine = lineAt(lineNo)
        text(lineNo.toString).text(" |").
          text(Red(">")).text(" ").
          text(currentLine).
          newLine()
      }
      newLine()
      for (t <- msg) {
        <<(t)
      }
      newLine()
    }

    if (beginLine == endLine)
      underline()
    else
      leftline()
  }

}
