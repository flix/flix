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

import ca.uwaterloo.flix.api.{CompilerConstants, Flix}

import java.util.concurrent.atomic.AtomicInteger

class ProgressBar(flix: Flix) {
  /**
    * The width of the progress bar in visible characters.
    */
  private val Width = 80

  /**
    * The characters in the spinner.
    */
  private val SpinnerChars = Array("⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏")

  /**
    * The width of the phase name column in visible characters.
    *
    * Set by the longest phase names: "Dependencies" and "EffectBinder".
    */
  private val PhaseWidth = 12

  /**
    * An internal counter used to print the spinner.
    *
    * Monotonically increasing.
    */
  private val spinnerTick = new AtomicInteger(0)

  /**
    * The time (in nanoseconds) at which the first phase of the current compilation was observed.
    */
  private var startNanos: Long = System.nanoTime()

  /**
    * Updates the progress to the given `phase`.
    */
  def observe(phase: String): Unit = {
    print(phase)
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
    * Prints the progress line for the given `phase` to the terminal.
    *
    * This function flushes the output and should not be called too often.
    */
  private def print(phase: String): Unit = {
    val fmt = flix.getFormatter

    // Compute the next character in the spinner.
    val index = spinnerTick.getAndIncrement() % SpinnerChars.length
    val spinner = SpinnerChars(index)

    // Compute the amount of heap memory in use and color it by its percentage of the maximum heap size.
    val runtime = Runtime.getRuntime
    val usedMemoryInBytes = runtime.totalMemory() - runtime.freeMemory()
    val maxMemoryInBytes = runtime.maxMemory()
    val usedMemoryInMegaBytes = (usedMemoryInBytes / (1024L * 1024L)).toInt
    val usedMemoryInPercent = ((100L * usedMemoryInBytes) / maxMemoryInBytes).toInt
    val memoryPadded = f"$usedMemoryInMegaBytes%4dM"
    val memPart = usedMemoryInPercent match {
      case x if x < 70 => memoryPadded
      case x if x < 90 => fmt.yellow(memoryPadded)
      case _ => fmt.red(memoryPadded)
    }

    // The phase name is abbreviated and padded to `PhaseWidth` so the columns to its right do not jitter.
    val phasePadded = abbreviate(phase, PhaseWidth).padTo(PhaseWidth, ' ')

    // The phase progress bar has one cell per phase: the frontend cells (blue), a divider, and the backend cells (magenta).
    // The finished phases are recorded in `phaseTimers`, so the current phase is number `phaseTimers.size + 1`.
    // We cap at `TotalPhases` in case some phase is not instrumented.
    val current = (flix.phaseTimers.size + 1).min(CompilerConstants.TotalPhases)
    val frontendDone = current.min(CompilerConstants.FrontendPhaseCount)
    val backendDone = current - frontendDone
    val frontendBar = fmt.blue("█" * frontendDone) + "░" * (CompilerConstants.FrontendPhaseCount - frontendDone)
    val backendBar = fmt.magenta("█" * backendDone) + "░" * (CompilerConstants.BackendPhaseCount - backendDone)
    val bar = s"$frontendBar│$backendBar"
    val count = f"$current%2d/${CompilerConstants.TotalPhases}%2d"

    // Compute the time elapsed since the first phase of the current compilation, in tenths of a second.
    // `phaseTimers` is reset when a compilation starts, so an empty list marks its first phase.
    // NB: We format the tenths ourselves (rather than with `%.1f`) so the decimal separator is locale-independent.
    if (flix.phaseTimers.isEmpty) startNanos = System.nanoTime()
    val elapsedTenths = (System.nanoTime() - startNanos) / 100_000_000L
    val elapsed = f"${elapsedTenths / 10}%3d.${elapsedTenths % 10}%ds"

    val s = s" [${fmt.green(spinner)}] [$memPart] [${fmt.blue(phasePadded)}] [$bar] $count $elapsed"

    // The visible width of the line, i.e. excluding ANSI escape codes: the spinner, memory, phase, bar, count,
    // and elapsed fields together with their brackets and spaces. Typically 76 chars, which fits within `Width`.
    val visibleWidth = 5 + (memoryPadded.length + 3) + (PhaseWidth + 3) + (CompilerConstants.TotalPhases + 4) + (count.length + 1) + elapsed.length

    // Print the line, padded with spaces to `Width`, followed by a carriage return.
    // NB: We pad the line (rather than first clearing it) so that each frame overwrites the previous
    // one in a single write. Clearing first would briefly leave the line blank, which flickers, since
    // `System.out` is auto-flushed and hence every `print` is a separate write to the terminal.
    // NB: We do *NOT* print a newline because then we would not be able to overwrite the current line.
    System.out.print(s + " " * (Width - visibleWidth) + "\r")

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
