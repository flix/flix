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

package ca.uwaterloo.flix.util

/**
  * A terminal context abstract the operations supported by a specific terminal.
  */
sealed trait OutputContext {

  def emitBlack(s: String): String

  def emitBlue(s: String): String

  def emitCyan(s: String): String

  def emitGreen(s: String): String

  def emitMagenta(s: String): String

  def emitRed(s: String): String

  def emitYellow(s: String): String

  def emitWhite(s: String): String

  def emitBold(s: String): String

  def emitUnderline(s: String): String

}

object OutputContext {

  /**
    * A basic terminal context.
    */
  object RawOutput extends OutputContext {

    def emitBlack(s: String): String = s

    def emitBlue(s: String): String = s

    def emitCyan(s: String): String = s

    def emitGreen(s: String): String = s

    def emitMagenta(s: String): String = s

    def emitRed(s: String): String = s

    def emitYellow(s: String): String = s

    def emitWhite(s: String): String = s

    def emitBold(s: String): String = s

    def emitUnderline(s: String): String = s
  }

  /**
    * A terminal context compatible with an ANSI terminal.
    */
  object AnsiTerminalOutput extends OutputContext {

    def emitBlack(s: String): String =
      s.replace(Format.BlackTag.open, Console.BLACK)
        .replace(Format.BlackTag.close, Console.RESET)

    def emitBlue(s: String): String =
      s.replace(Format.BlueTag.open, Console.BLUE)
        .replace(Format.BlueTag.close, Console.RESET)

    def emitCyan(s: String): String =
      s.replace(Format.CyanTag.open, Console.CYAN)
        .replace(Format.CyanTag.close, Console.RESET)

    def emitGreen(s: String): String =
      s.replace(Format.GreenTag.open, Console.GREEN)
        .replace(Format.GreenTag.close, Console.RESET)

    def emitMagenta(s: String): String =
      s.replace(Format.MagentaTag.open, Console.MAGENTA)
        .replace(Format.MagentaTag.close, Console.RESET)

    def emitRed(s: String): String =
      s.replace(Format.RedTag.open, Console.RED)
        .replace(Format.RedTag.close, Console.RESET)

    def emitYellow(s: String): String =
      s.replace(Format.YellowTag.open, Console.YELLOW)
        .replace(Format.YellowTag.close, Console.RESET)

    def emitWhite(s: String): String =
      s.replace(Format.WhiteTag.open, Console.WHITE)
        .replace(Format.WhiteTag.close, Console.RESET)

    def emitBold(s: String): String =
      s.replace(Format.BoldTag.open, Console.BOLD)
        .replace(Format.BoldTag.close, Console.RESET)

    def emitUnderline(s: String): String =
      s.replace(Format.UnderlineTag.open, Console.UNDERLINED)
        .replace(Format.UnderlineTag.close, Console.RESET)
  }

  /**
    * Returns `true` if the terminal appears to support at least 256 colors.
    */
  def hasColorSupport: Boolean = isAnsiTerminal || isTrueColorTerminal || isWindowsTerminal

  /**
    * Returns `true` if the terminal appears to be an ANSI terminal.
    */
  private def isAnsiTerminal: Boolean = {
    val term = System.getenv("TERM")
    term != null && (
      term.contains("256") ||
        term.contains("ansi") ||
        term.contains("xterm") ||
        term.contains("screen"))
  }

  /**
    * Returns `true` if the terminal appears to support 24bit colors.
    */
  private def isTrueColorTerminal: Boolean = {
    val colorTerm = System.getenv("COLORTERM")
    colorTerm != null && colorTerm.contains("truecolor")
  }

  /**
    * Returns `true` if the terminal appears to be a Windows Terminal.
    */
  private def isWindowsTerminal: Boolean = {
    val wtSession = System.getenv("WT_SESSION")
    wtSession != null
  }

  def render(s: String)(implicit outputContext: OutputContext): String =
    outputContext.emitRed(
      outputContext.emitCyan(
        outputContext.emitBlack(
          outputContext.emitBlue(
            outputContext.emitBold(
              outputContext.emitGreen(
                outputContext.emitMagenta(
                  outputContext.emitUnderline(
                    outputContext.emitWhite(
                      outputContext.emitYellow(s))))))))))

}
