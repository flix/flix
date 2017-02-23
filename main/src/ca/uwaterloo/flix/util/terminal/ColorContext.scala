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

/**
  * A color context abstracts the colors supported by some output mechanism.
  */
sealed trait ColorContext {

  def fmtBlack(s: String): String

  def fmtBlue(s: String): String

  def fmtCyan(s: String): String

  def fmtGreen(s: String): String

  def fmtMagenta(s: String): String

  def fmtRed(s: String): String

  def fmtYellow(s: String): String

  def fmtWhite(s: String): String

  def fmtBold(s: String): String

  def fmtUnderline(s: String): String
}

object ColorContext {

  /**
    * A color context that prints no colors or other highlighting.
    */
  object NoColor extends ColorContext {

    def fmtBlack(s: String): String = s

    def fmtBlue(s: String): String = s

    def fmtCyan(s: String): String = s

    def fmtGreen(s: String): String = s

    def fmtMagenta(s: String): String = s

    def fmtRed(s: String): String = s

    def fmtYellow(s: String): String = s

    def fmtWhite(s: String): String = s

    def fmtBold(s: String): String = s

    def fmtUnderline(s: String): String = s
  }

  /**
    * A color context that prints using ANSI colors and highlighting.
    */
  object AnsiColor extends ColorContext {

    def fmtBlack(s: String): String = Console.BLACK + s + Console.RESET

    def fmtBlue(s: String): String = Console.BLUE + s + Console.RESET

    def fmtCyan(s: String): String = Console.CYAN + s + Console.RESET

    def fmtGreen(s: String): String = Console.GREEN + s + Console.RESET

    def fmtMagenta(s: String): String = Console.MAGENTA + s + Console.RESET

    def fmtRed(s: String): String = Console.RED + s + Console.RESET

    def fmtYellow(s: String): String = Console.YELLOW + s + Console.RESET

    def fmtWhite(s: String): String = Console.WHITE + s + Console.RESET

    def fmtBold(s: String): String = Console.BOLD + s + Console.RESET

    def fmtUnderline(s: String): String = Console.UNDERLINED + s + Console.RESET

  }

  object HtmlColor extends ColorContext {

    def fmtBlack(s: String): String = s"""<span class="black">$s</span>"""

    def fmtBlue(s: String): String = s"""<span class="blue">$s</span>"""

    def fmtCyan(s: String): String = s"""<span class="cyan">$s</span>"""

    def fmtGreen(s: String): String = s"""<span class="green">$s</span>"""

    def fmtMagenta(s: String): String = s"""<span class="magenta">$s</span>"""

    def fmtRed(s: String): String = s"""<span class="red">$s</span>"""

    def fmtYellow(s: String): String = s"""<span class="yellow">$s</span>"""

    def fmtWhite(s: String): String = s"""<span class="white">$s</span>"""

    def fmtBold(s: String): String = s"""<span class="bold">$s</span>"""

    def fmtUnderline(s: String): String = s"""<span class="underline">$s</span>"""

  }

}
