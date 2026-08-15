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

    // Compute the amount of heap memory in use and its percentage of the maximum heap size.
    val runtime = Runtime.getRuntime
    val usedMemoryInBytes = runtime.totalMemory() - runtime.freeMemory()
    val maxMemoryInBytes = runtime.maxMemory()
    val usedMemoryInMegaBytes = (usedMemoryInBytes / (1024L * 1024L)).toInt
    // We cap the percentage at 99 to keep the field a fixed width.
    val usedMemoryInPercent = math.min(99, ((100L * usedMemoryInBytes) / maxMemoryInBytes).toInt)
    val memoryPadded = f"$usedMemoryInMegaBytes%4dM $usedMemoryInPercent%2d%%"
    val memPart = usedMemoryInPercent match {
      case x if x < 70 => memoryPadded
      case x if x < 90 => flix.getFormatter.yellow(memoryPadded)
      case _ => flix.getFormatter.red(memoryPadded)
    }

    // We abbreviate phase and msg if they are too long to fit within `Width`.
    // The fixed parts (spinner, memory, brackets, and spaces) take up 21 chars.
    val p = abbreviate(phase, 20)
    val m = abbreviate(msg, Width - (20 + 21))
    val s = s" [${flix.getFormatter.green(spinner)}] [$memPart] [${flix.getFormatter.blue(p)}] $m "

    // Clear the current line.
    // NB: We clear the line with spaces (rather than padding the string) because
    // the string may contain ANSI escape codes which do not take up any width.
    System.out.print(" " * Width + "\r")

    // Print the string followed by carriage return.
    // NB: We do *NOT* print a newline because then
    // we would not be able to overwrite the current
    // line in the iteration.
    System.out.print(s + "\r")

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

}
