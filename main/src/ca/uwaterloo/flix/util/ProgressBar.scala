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

import java.util.concurrent.atomic.AtomicInteger

class ProgressBar {
  /** The characters in the spinner. */
  private val SpinnerChars = Array("|", "/", "-", "\\")

  /** The sample rate. */
  private val SampleRate: Int = 100

  /**
    * An internal counter used to print the spinner.
    *
    * Monotonically increasing.
    */
  private val spinnerTick = new AtomicInteger(0)

  /**
    * An internal counter used for sampling.
    *
    * Monotonically increasing.
    */
  private val sampleTick = new AtomicInteger(0)

  /** A Boolean that represents whether the terminal is believed to support color. */
  private val supportsColors: Boolean = Formatter.hasColorSupport

  /**
    * Updates the progress with the given message `msg` in the given `phase`.
    *
    * If sample is `true` then
    */
  def observe(phase: String, msg: String, sample: Boolean): Unit = {
    // Always print if `sample` is `false`.
    if (!sample) {
      print(phase, msg)
    } else {
      // Print if `sample` is `true` and we have passed `SampleRate` ticks.
      if (sampleTick.getAndIncrement() % SampleRate == 0) {
        print(phase, msg)
      }
    }
  }

  /**
    * Indicates that no further events will be observed.
    *
    * Used to properly reset the current line.
    */
  def complete(): Unit = {
    System.out.print(" " * 80 + s"\r")
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

    // We abbreviate phase and msg if they are too long to fit.
    val p = abbreviate(phase, 20)
    val m = abbreviate(msg, 80 - (20 + 10))
    val s = s" [${colorGreen(spinner)}] [${colorBlue(p)}] $m"

    // Print the string followed by carriage return.
    // NB: We do *NOT* print a newline because then
    // we would not be able to overwrite the current
    // line in the iteration.
    System.out.print(s.padTo(80, ' ') + "\r")

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

  /** Colors the given string `s` green (if supported). */
  private def colorGreen(s: String): String =
    if (supportsColors)
      Console.GREEN + s + Console.RESET
    else
      s

  /** Colors the given string `s` blue (if supported). */
  private def colorBlue(s: String): String =
    if (supportsColors)
      Console.BLUE + s + Console.RESET
    else
      s

}
