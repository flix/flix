/*
 * Copyright 2021 Magnus Madsen
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
package ca.uwaterloo.flix.util

import ca.uwaterloo.flix.api.Flix

import java.util.concurrent.atomic.AtomicInteger

class ProgressBar(flix: Flix) {
  /**
    * The width of the progress bar in visible characters.
    */
  private val Width = 80

  /**
    * The characters in the spinner.
    */
  private val SpinnerChars = Array("|", "/", "-", "\\")

  /**
    * An internal counter used to print the spinner.
    *
    * Monotonically increasing.
    */
  private val spinnerTick = new AtomicInteger(0)

  /**
    * Updates the progress with the given message `msg` in the given `phase`.
    */
  def observe(phase: String, msg: String): Unit = {
    print(phase, msg)
  }

  /**
    * Indicates that no further events will be observed.
    *
    * Used to properly reset the current line.
    */
  def complete(): Unit = {
    System.out.print(" " * Width + "\r")
    System.out.flush()
  }

  /**
    * Prints the given string `msg` from the given `phase` to the terminal.
    *
    * This function flushes the output and should not be called too often.
    */
  private def print(phase: String, msg: String): Unit = {
    // Compute the next character in the spinner.
    val index = spinnerTick.getAndIncrement() % SpinnerChars.length
    val spinner = SpinnerChars(index)

    // Compute the total amount of memory in use.
    val usedMemoryInBytes = Runtime.getRuntime.totalMemory()
    val usedMemoryInMegaBytes = (usedMemoryInBytes / (1024L * 1024L)).toInt
    val memoryPadded = f"$usedMemoryInMegaBytes%4dM"
    val memPart = usedMemoryInMegaBytes match {
      case x if x <= 1_000 => memoryPadded
      case x if x <= 4_000 => flix.getFormatter.yellow(memoryPadded)
      case _ => flix.getFormatter.red(memoryPadded)
    }

    // We abbreviate phase and msg if they are too long to fit within `Width`.
    val p = abbreviate(phase, 20)
    val prefix = s" [${flix.getFormatter.green(spinner)}] [$memPart] [${flix.getFormatter.blue(p)}] "
    val m = abbreviate(msg, Width - visibleLength(prefix) - 1)
    val s = prefix + m + " "

    // Pad the string with spaces up to `Width` so that it fully overwrites the previous line.
    //
    // NB: The padding must be computed from the *visible* length of the string,
    // i.e. excluding ANSI escape codes. Otherwise the padded string may be
    // visibly shorter than the previous line, leaving stray characters behind.
    val padding = " " * math.max(0, Width - visibleLength(s))

    // Print the string followed by carriage return.
    // NB: We do *NOT* print a newline because then
    // we would not be able to overwrite the current
    // line in the iteration.
    System.out.print(s + padding + "\r")

    // Flush to ensure that the string is printed.
    System.out.flush()
  }

  /**
    * Returns `s` if it less than or equal to `l` chars.
    *
    * Otherwise returns a prefix of `s` with ...
    */
  private def abbreviate(s: String, l: Int): String =
    if (s.length <= l)
      s
    else
      s.substring(0, l - 3) + "..."

  /**
    * Returns the number of visible characters in `s`, i.e. the length of `s` excluding ANSI escape codes.
    */
  private def visibleLength(s: String): Int =
    s.replaceAll("\u001b\\[[0-9;]*m", "").length

}
