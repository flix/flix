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

/**
  * A terminal context abstract the operations supported by a specific virtual terminal.
  */
sealed trait TerminalContext {

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

object TerminalContext {

  /**
    * A basic terminal context.
    */
  object NoTerminal extends TerminalContext {

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
  object AnsiTerminal extends TerminalContext {

    def emitBlack(s: String): String = Console.BLACK + s + Console.RESET

    def emitBlue(s: String): String = Console.BLUE + s + Console.RESET

    def emitCyan(s: String): String = Console.CYAN + s + Console.RESET

    def emitGreen(s: String): String = Console.GREEN + s + Console.RESET

    def emitMagenta(s: String): String = Console.MAGENTA + s + Console.RESET

    def emitRed(s: String): String = Console.RED + s + Console.RESET

    def emitYellow(s: String): String = Console.YELLOW + s + Console.RESET

    def emitWhite(s: String): String = Console.WHITE + s + Console.RESET

    def emitBold(s: String): String = Console.BOLD + s + Console.RESET

    def emitUnderline(s: String): String = Console.UNDERLINED + s + Console.RESET
  }

  /**
    * A terminal context compatible with HTML output.
    */
  object HtmlTerminal extends TerminalContext {

    def emitBlack(s: String): String = s"""<span class="black">$s</span>"""

    def emitBlue(s: String): String = s"""<span class="blue">$s</span>"""

    def emitCyan(s: String): String = s"""<span class="cyan">$s</span>"""

    def emitGreen(s: String): String = s"""<span class="green">$s</span>"""

    def emitMagenta(s: String): String = s"""<span class="magenta">$s</span>"""

    def emitRed(s: String): String = s"""<span class="red">$s</span>"""

    def emitYellow(s: String): String = s"""<span class="yellow">$s</span>"""

    def emitWhite(s: String): String = s"""<span class="white">$s</span>"""

    def emitBold(s: String): String = s"""<span class="bold">$s</span>"""

    def emitUnderline(s: String): String = s"""<span class="underline">$s</span>"""

  }

}
