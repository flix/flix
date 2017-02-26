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

import scala.collection.mutable.ListBuffer

class VirtualTerminal() {

  /**
    * The current lines.
    */
  private val lines: ListBuffer[List[VirtualString]] = ListBuffer.empty

  /**
    * The tokens in the current line (in reverse order).
    */
  private var currentLine: List[VirtualString] = List.empty

  /**
    * The current indentation level.
    */
  private var currentIndent: Int = 0

  /**
    * Increases indentation by one level.
    */
  def indent(): VirtualTerminal = {
    currentIndent = currentIndent + 1
    this
  }

  /**
    * Decreases indentation by one level.
    */
  def dedent(): VirtualTerminal = {
    currentIndent = currentIndent - 1
    this
  }

  /**
    * Appends the given string `s` without creating a new line break.
    */
  def print(s: String): VirtualTerminal = {
    text(s)
    this
  }

  /**
    * Appends the given string `s` followed by a line break.
    */
  def println(s: String): VirtualTerminal = {
    text(s)
    newLine()
    this
  }

  def <<(s: String): VirtualTerminal = print(s)

  def <<(s: VirtualString): VirtualTerminal = {
    currentLine = s :: currentLine
    this
  }

  def text(t: VirtualString): VirtualTerminal = {
    currentLine = t :: currentLine
    this
  }

  def text(s: Int): VirtualTerminal = {
    currentLine = VirtualString.Text(s.toString) :: currentLine
    this
  }

  def text(s: Double): VirtualTerminal = {
    currentLine = VirtualString.Text(s.toString) :: currentLine
    this
  }

  def text(s: Float): VirtualTerminal = {
    currentLine = VirtualString.Text(s.toString) :: currentLine
    this
  }

  def text(s: BigInteger): VirtualTerminal = {
    currentLine = VirtualString.Text(s.toString) :: currentLine
    this
  }

  def text(s: String): VirtualTerminal = {
    currentLine = VirtualString.Text(s) :: currentLine
    this
  }

  def quote(t: VirtualString): VirtualTerminal = {
    currentLine = VirtualString.Quote(t) :: currentLine
    this
  }

  // TODO: Move to other package and implement other methods, including << and so on.


  def header(kind: String, source: SourceInput): VirtualTerminal = {
    text(Blue(s"-- $kind -------------------------------------------------- ${source.format}")).newLine().newLine()
    this
  }


  def newLine(): VirtualTerminal = {
    lines += currentLine.reverse
    currentLine = Nil

    for (i <- 0 until currentIndent) {
      currentLine = VirtualString.Text("  ") :: currentLine
    }

    this
  }

  def use(f: VirtualTerminal => Unit): VirtualTerminal = {
    f(this)
    this
  }

  def fmt(implicit ctx: TerminalContext): String = {
    lines.map(line => line.map(_.fmt).mkString("")).mkString("\n")
  }

  def space(): VirtualTerminal = {
    text(" ")
    this
  }

  /////////////////////////////////////////////////////////////////////////////
  /// Highlights Source Code                                                ///
  /////////////////////////////////////////////////////////////////////////////
  def highlight(loc: SourceLocation, msg: VirtualString*): VirtualTerminal = {
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

  /////////////////////////////////////////////////////////////////////////////
  /// Color Operations                                                      ///
  /////////////////////////////////////////////////////////////////////////////

  /**
    * Appends the result of calling the given object `o`'s `toString` method in black text.
    */
  def black(o: AnyRef): VirtualTerminal = {
    text(Black(o.toString))
    this
  }

  /**
    * Appends the result of calling the given object `o`'s `toString` method in blue text.
    */
  def blue(o: AnyRef): VirtualTerminal = {
    text(Blue(o.toString))
    this
  }

  /**
    * Appends the result of calling the given object `o`'s `toString` method in cyan text.
    */
  def cyan(o: AnyRef): VirtualTerminal = {
    text(Cyan(o.toString))
    this
  }

  /**
    * Appends the result of calling the given object `o`'s `toString` method in green text.
    */
  def green(o: AnyRef): VirtualTerminal = {
    text(Green(o.toString))
    this
  }

  /**
    * Appends the result of calling the given object `o`'s `toString` method in magenta text.
    */
  def magenta(o: AnyRef): VirtualTerminal = {
    text(Magenta(o.toString))
    this
  }

  /**
    * Appends the result of calling the given object `o`'s `toString` method in red text.
    */
  def red(o: AnyRef): VirtualTerminal = {
    text(Red(o.toString))
    this
  }

  /**
    * Appends the result of calling the given object `o`'s `toString` method in yellow text.
    */
  def yellow(o: AnyRef): VirtualTerminal = {
    text(Yellow(o.toString))
    this
  }

  /**
    * Appends the result of calling the given object `o`'s `toString` method in white text.
    */
  def white(o: AnyRef): VirtualTerminal = {
    text(Yellow(o.toString))
    this
  }

  /**
    * Appends the result of calling the given object `o`'s `toString` method in bold text.
    */
  def bold(o: AnyRef): VirtualTerminal = {
    text(Bold(o.toString))
    this
  }

  /**
    * Appends the result of calling the given object `o`'s `toString` method in underlined text.
    */
  def underline(s: AnyRef): VirtualTerminal = {
    text(Underline(s.toString))
    this
  }

}
