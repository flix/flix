/*
 * Copyright 2026 Magnus Madsen
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
package ca.uwaterloo.flix.tools.compilertop

/**
  * ANSI escape codes and styling primitives shared by the compiler-top TUI.
  * Pure: no state, no terminal probing — just strings.
  */
object Ansi {

  /** ANSI escape character. */
  val ESC: Char = 27.toChar

  /** ANSI sequence that homes the cursor and clears the screen. */
  val ClearScreen: String = s"$ESC[H$ESC[2J"

  /**
    * DEC mode 2026 — Begin Synchronized Update. Terminals that support it
    * (iTerm2, kitty, alacritty, wezterm, modern xterm, recent VTE) buffer
    * the bytes between BSU and ESU and swap atomically, eliminating the
    * flash from clear-and-redraw. Terminals that don't recognize the
    * sequence silently ignore it.
    */
  val BeginSync: String = s"$ESC[?2026h"

  /** DEC mode 2026 — End Synchronized Update; pair of [[BeginSync]]. */
  val EndSync: String = s"$ESC[?2026l"

  // Color codes.

  /** ANSI reset (clears bold, dim, color, etc.). */
  val Reset: String = s"$ESC[0m"
  /** ANSI bold. */
  val BoldCode: String = s"$ESC[1m"
  /** ANSI dim/faint. */
  val DimCode: String = s"$ESC[2m"
  /** ANSI underline on (paired with [[NoUnderlineCode]] — surgical, leaves bold/color intact). */
  val UnderlineCode: String = s"$ESC[4m"
  /** ANSI underline off; cancels [[UnderlineCode]] without disturbing bold/color. */
  val NoUnderlineCode: String = s"$ESC[24m"
  /** ANSI foreground red. */
  val Red: String = s"$ESC[31m"
  /** ANSI foreground green. */
  val Green: String = s"$ESC[32m"
  /** ANSI foreground yellow. */
  val Yellow: String = s"$ESC[33m"
  /** ANSI foreground blue. */
  val Blue: String = s"$ESC[34m"
  /** ANSI foreground cyan. */
  val Cyan: String = s"$ESC[36m"
  /** ANSI foreground bright black (gray). */
  val Gray: String = s"$ESC[90m"

  // Wrappers — each takes a string and returns it surrounded by codes.

  /** Wraps `s` in the ANSI color code `c` and a reset suffix. */
  def color(s: String, c: String): String = s"$c$s$Reset"
  /** Wraps `s` in ANSI bold codes. */
  def bold(s: String): String = s"$BoldCode$s$Reset"
  /** Wraps `s` in ANSI dim/faint codes. */
  def dim(s: String): String = s"$DimCode$s$Reset"
  /** Wraps `s` in ANSI red codes. */
  def red(s: String): String = color(s, Red)
  /** Wraps `s` in ANSI yellow codes. */
  def yellow(s: String): String = color(s, Yellow)
  /** Wraps `s` in ANSI green codes. */
  def green(s: String): String = color(s, Green)
  /** Wraps `s` in ANSI blue codes. */
  def blue(s: String): String = color(s, Blue)
  /** Wraps `s` in ANSI cyan codes. */
  def cyan(s: String): String = color(s, Cyan)

  /**
    * Renders a column header with one keystroke letter underlined. Active
    * sort columns are bold yellow; the rest are bold cyan. The label string
    * is taken as-is (already padded by the caller) and the keystroke
    * position is given as a 0-based index into it. Visible width is
    * unchanged — only ANSI codes are inserted around the keystroke char.
    */
  def keyHeader(paddedLabel: String, keyIdx: Int, active: Boolean): String = {
    val c = if (active) Yellow else Cyan
    val before = paddedLabel.take(keyIdx)
    val key = paddedLabel.slice(keyIdx, keyIdx + 1)
    val after = paddedLabel.drop(keyIdx + 1)
    s"$BoldCode$c$before$UnderlineCode$key$NoUnderlineCode$after$Reset"
  }

  /** Renders a non-sort column header (no underlined keystroke) in bold cyan. */
  def plainHeader(paddedLabel: String): String =
    s"$BoldCode$Cyan$paddedLabel$Reset"
}
