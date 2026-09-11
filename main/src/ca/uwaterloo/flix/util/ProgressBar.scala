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

import java.util.concurrent.locks.ReentrantLock
import java.util.concurrent.{Executors, ScheduledExecutorService, TimeUnit}

class ProgressBar(flix: Flix) {
  /**
    * The width of the progress bar in visible characters.
    */
  private val Width = 80

  /**
    * The width of the phase name column in visible characters.
    *
    * Set by the longest phase names: "Dependencies" and "EffectBinder".
    */
  private val PhaseWidth = 12

  /**
    * The characters in the spinner.
    */
  private val SpinnerChars = Array("⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏")

  /**
    * The delay between frames. At 80 ms the animation runs at 12.5 frames per second.
    */
  private val FrameDelayMillis = 80L

  /**
    * The duration of one complete phase-name pulse.
    */
  private val PulsePeriodMillis = 1500L

  /**
    * The interval between heap-memory samples. Memory changes less frequently than animation
    * frames so the displayed value remains readable.
    */
  private val MemorySampleIntervalMillis = 750L

  /**
    * An immutable snapshot passed from the compiler thread to the animation thread.
    *
    * @param phase                  the name of the current compiler phase.
    * @param current                the one-based index of the current compiler phase.
    * @param phaseStartMillis       the monotonic time at which the current phase started.
    * @param compilationStartMillis the monotonic time at which the compilation started.
    */
  private case class State(phase: String, current: Int, phaseStartMillis: Long, compilationStartMillis: Long)

  /**
    * The executor responsible for refreshing the current progress line.
    * Only accessed by the compiler thread.
    */
  private var animator: ScheduledExecutorService = null

  /**
    * The latest phase snapshot published by the compiler thread and read by the animation thread.
    */
  @volatile private var state: State = null

  /**
    * The time (in milliseconds) at which the first phase of the current compilation was observed.
    */
  private var startMillis: Long = nowMillis()

  /**
    * Guards terminal writes against line clearing and protects the render-thread-owned fields
    * `spinnerTick`, `cachedMemory`, and `memorySampleMillis`.
    */
  private val renderLock = new ReentrantLock()

  /**
    * The index of the spinner character to print in the next frame.
    */
  private var spinnerTick: Int = 0

  /**
    * The most recently sampled heap-memory display and its percentage of the maximum heap.
    */
  private var cachedMemory: (String, Int) = ("   0M", 0)

  /**
    * The time at which `cachedMemory` was last updated, or zero if no sample is cached.
    */
  private var memorySampleMillis: Long = 0L

  /**
    * Starts the animation thread if progress is enabled and the animator is not already running.
    */
  def start(): Unit = {
    if (flix.options.progress && animator == null) {
      animator = Executors.newSingleThreadScheduledExecutor((r: Runnable) => {
        val thread = new Thread(r, "flix-progress-bar")
        thread.setDaemon(true)
        thread
      })
      animator.scheduleAtFixedRate(() => renderFrame(), FrameDelayMillis, FrameDelayMillis, TimeUnit.MILLISECONDS)
    }
  }

  /**
    * Updates the progress to the given `phase`.
    */
  def observe(phase: String): Unit = {
    if (animator != null) {
      val now = nowMillis()
      if (flix.phaseTimers.isEmpty) startMillis = now
      val current = (flix.phaseTimers.size + 1).min(CompilerConstants.TotalPhases)
      state = State(phase, current, now, startMillis)
    }
  }

  /**
    * Indicates that no further events will be observed.
    *
    * Used to properly reset the current line.
    */
  def complete(): Unit = {
    state = null
    val executor = animator
    animator = null
    if (executor != null) executor.shutdownNow()

    if (executor != null) {
      // Wait for an in-flight terminal write.
      renderLock.lock()
      try {
        memorySampleMillis = 0L
        System.out.print(" " * Width + "\r")
        System.out.flush()
      } finally {
        renderLock.unlock()
      }
    }
  }

  /**
    * Renders the latest state.
    */
  private def renderFrame(): Unit = {
    renderLock.lock()
    try {
      val snapshot = state
      if (snapshot != null) print(snapshot)
    } finally {
      renderLock.unlock()
    }
  }

  /**
    * Prints the progress line represented by `snapshot` to the terminal.
    *
    * This function flushes the output and should not be called too often.
    *
    * Locking: Must only be called while holding `renderLock`.
    */
  private def print(snapshot: State): Unit = {
    val fmt = flix.getFormatter

    // Compute the next character in the spinner.
    val spinner = SpinnerChars(spinnerTick)
    spinnerTick = (spinnerTick + 1) % SpinnerChars.length

    // Sample heap memory less frequently than animation frames so the value remains readable.
    val now = nowMillis()
    if (memorySampleMillis == 0L || now - memorySampleMillis >= MemorySampleIntervalMillis) {
      val runtime = Runtime.getRuntime
      val usedMemoryInBytes = runtime.totalMemory() - runtime.freeMemory()
      val maxMemoryInBytes = runtime.maxMemory()
      val usedMemoryInMegaBytes = (usedMemoryInBytes / (1024L * 1024L)).toInt
      val usedMemoryInPercent = ((100L * usedMemoryInBytes) / maxMemoryInBytes).toInt
      cachedMemory = (f"$usedMemoryInMegaBytes%4dM", usedMemoryInPercent)
      memorySampleMillis = now
    }
    val (memoryPadded, usedMemoryInPercent) = cachedMemory
    val memPart = usedMemoryInPercent match {
      case x if x < 70 => memoryPadded
      case x if x < 90 => fmt.yellow(memoryPadded)
      case _ => fmt.red(memoryPadded)
    }

    // The phase name is abbreviated and padded to `PhaseWidth` so the columns to its right do not jitter.
    val phasePadded = abbreviate(snapshot.phase, PhaseWidth).padTo(PhaseWidth, ' ')
    val pulseProgress = ((nowMillis() - snapshot.phaseStartMillis) % PulsePeriodMillis).toDouble / PulsePeriodMillis
    val phasePart = pulsePhase(phasePadded, pulseProgress, fmt)

    // The phase progress bar has one cell per phase: the frontend cells (blue), a divider, and the backend cells (magenta).
    // The finished phases are recorded in `phaseTimers`, so the current phase is number `phaseTimers.size + 1`.
    // We cap at `TotalPhases` in case some phase is not instrumented.
    val frontendDone = snapshot.current.min(CompilerConstants.FrontendPhaseCount)
    val backendDone = snapshot.current - frontendDone
    val frontendBar = fmt.blue("█" * frontendDone) + "░" * (CompilerConstants.FrontendPhaseCount - frontendDone)
    val backendBar = fmt.magenta("█" * backendDone) + "░" * (CompilerConstants.BackendPhaseCount - backendDone)
    val bar = s"$frontendBar│$backendBar"
    val count = f"${snapshot.current}%2d/${CompilerConstants.TotalPhases}%2d"

    // Compute the time elapsed since the first phase of the current compilation, in tenths of a second.
    // `phaseTimers` is reset when a compilation starts, so an empty list marks its first phase.
    // NB: We format the tenths ourselves (rather than with `%.1f`) so the decimal separator is locale-independent.
    val elapsedTenths = (nowMillis() - snapshot.compilationStartMillis) / 100L
    val elapsed = f"${elapsedTenths / 10}%3d.${elapsedTenths % 10}%ds"

    val s = s" [${fmt.green(spinner)}] [$memPart] [$phasePart] [$bar] $count $elapsed"

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
    * Colors each character of `phase` independently, producing a soft highlight that travels
    * from left to right. The highlight begins and ends outside the text so consecutive pulses
    * join without a visible jump.
    */
  private def pulsePhase(phase: String, progress: Double, fmt: Formatter): String = {
    val margin = 3.0
    val center = progress * (phase.length + 2.0 * margin) - margin
    phase.zipWithIndex.map { case (char, index) =>
      val distance = index - center
      val intensity = Math.exp(-(distance * distance) / 4.0)
      fmt.fgColor(
        interpolate(68, 120, intensity),
        interpolate(147, 210, intensity),
        interpolate(200, 255, intensity),
        char.toString
      )
    }.mkString
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
    * Linearly interpolates between `from` and `to` by `amount`.
    */
  private def interpolate(from: Int, to: Int, amount: Double): Int =
    (from + (to - from) * amount).round.toInt

  /**
    * Returns monotonic time in milliseconds.
    */
  private def nowMillis(): Long = TimeUnit.NANOSECONDS.toMillis(System.nanoTime())

}
