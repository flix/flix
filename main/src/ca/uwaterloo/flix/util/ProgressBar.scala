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
  /**
    * The characters in the spinner.
    */
  private val SpinnerChars = Array("|", "/", "-", "\\")

  /**
    * The sample rate.
    */
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
  private def print(phase: String, msg: String): Unit = synchronized {
    // Compute the next character in the spinner.
    val index = spinnerTick.getAndIncrement() % SpinnerChars.length
    val spinner = SpinnerChars(index)

    // Build the string to print and pad it.
    val s = s"$spinner [$phase] $msg"
    val r = s + " " * Math.max(80 - s.length, 0)

    // Print the string followed by carriage return.
    // NB: We do *NOT* print a newline because then
    // we would not be able to overwrite the current
    // line in the iteration.
    System.out.print(s"$r\r")

    // Flush to ensure that the string is printed.
    System.out.flush()
  }

}
