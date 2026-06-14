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

import ca.uwaterloo.flix.tools.compilertop.Ansi.*

/**
  * Threshold-driven coloring of formatted fields for the compiler-top TUI.
  * The `style*` functions decide *when* to color; [[Ansi]] supplies the
  * escape codes.
  */
object Styling {

  // -- Warning thresholds --------------------------------------------------
  //
  // Per-column tier cutoffs consumed by the `style*` functions below. Naming:
  // `<Column><Tier>Threshold[<Unit>]`. Sorted alphabetically — keep new
  // entries in order. Adjusting a number here is a UX call; please don't
  // bury it inside one of the styling functions.

  private val HeapRedThresholdRatio:           Double = 0.9
  private val HeapYellowThresholdRatio:        Double = 0.7
  private val HotnessRedThresholdMsPerLine:    Double = 25.0
  private val HotnessYellowThresholdMsPerLine: Double = 15.0
  private val ModuleHotnessRedThresholdMsPerLine:    Double = 4.0
  private val ModuleHotnessYellowThresholdMsPerLine: Double = 2.0
  private val PctCpuRedThreshold:              Double = 5.0
  private val PctCpuYellowThreshold:           Double = 1.0
  private val PctWallRedThreshold:             Double = 15.0
  private val PctWallYellowThreshold:          Double = 5.0
  private val TimeRedThresholdMs:              Long   = 1000L
  private val TimeYellowThresholdMs:           Long   = 50L
  private val TrackedGreenThresholdPct:        Double = 90.0
  private val TrackedYellowThresholdPct:       Double = 75.0

  // -- Conditional styling -------------------------------------------------

  /** Colors a formatted time field by absolute magnitude. */
  def styleTime(formatted: String, nanos: Long): String = {
    val ms = nanos / 1_000_000L
    if (ms >= TimeRedThresholdMs) bold(red(formatted))
    else if (ms >= TimeYellowThresholdMs) yellow(formatted)
    else formatted
  }

  /**
    * Colors the sym name based on its hotness (ms-per-source-line) — surfaces
    * small defs that consume time disproportionate to their body size. Defs
    * with no real source span (`lines <= 0`) are left unstyled because the
    * denominator is meaningless; [[hotnessMsPerLine]] returns 0 for them so
    * they fall below the yellow threshold and pass through unchanged.
    *
    * Bold is deliberately NOT used here — the renderer reserves it as the
    * "currently being compiled" signal on def-row first cells, so bold
    * composes naturally with red/yellow when a hot def is also active.
    */
  def styleSym(name: String, nanos: Long, lines: Int): String = {
    val msPerLine = Formatting.hotnessMsPerLine(nanos, lines)
    if (msPerLine >= HotnessRedThresholdMsPerLine) red(name)
    else if (msPerLine >= HotnessYellowThresholdMsPerLine) yellow(name)
    else name
  }

  /**
    * Module-row analogue of [[styleSym]]: colors a module name by its
    * aggregate hotness (summed ms / summed source lines). Uses the same
    * absolute two-tier red/yellow scheme but with much lower cutoffs —
    * averaging a module's cost over all its lines dilutes the per-line
    * figure, so it never approaches the per-def thresholds. Calibrated so
    * the densest modules light up while the bulk stay plain.
    */
  def styleModule(name: String, nanos: Long, lines: Int): String = {
    val msPerLine = Formatting.hotnessMsPerLine(nanos, lines)
    if (msPerLine >= ModuleHotnessRedThresholdMsPerLine) red(name)
    else if (msPerLine >= ModuleHotnessYellowThresholdMsPerLine) yellow(name)
    else name
  }

  /** %cpu = totalNanos / (elapsed × threads). One def's slice of total compute. */
  def stylePctCpu(formatted: String, pct: Double): String = {
    if (pct >= PctCpuRedThreshold) bold(red(formatted))
    else if (pct >= PctCpuYellowThreshold) yellow(formatted)
    else formatted
  }

  /** %wall = totalNanos / elapsed. Upper bound on wall-clock savings if removed. */
  def stylePctWall(formatted: String, pct: Double): String = {
    if (pct >= PctWallRedThreshold) bold(red(formatted))
    else if (pct >= PctWallYellowThreshold) yellow(formatted)
    else formatted
  }

  /** Colors the active/parallelism field by occupancy: full=green bold, ≥50%=green, idle=gray, else yellow. */
  def styleThreads(active: Int, par: Int): String = {
    val s = f"$active%2d/$par%-2d"
    if (par <= 0) color(s, Gray)
    else if (active == par) bold(green(s))
    else if (active.toDouble / par >= 0.5) green(s)
    else if (active == 0) color(s, Gray)
    else yellow(s)
  }

  /** Colors a formatted heap field by used/max ratio. */
  def styleHeap(formatted: String, ratio: Double): String = {
    if (ratio >= HeapRedThresholdRatio) bold(red(formatted))
    else if (ratio >= HeapYellowThresholdRatio) yellow(formatted)
    else green(formatted)
  }

  /**
    * Colors the dashboard `tracked` percentage. Inverted from the other tiers
    * because *high* coverage is the good outcome: green at/above the green
    * cutoff, yellow in the middle band, red below.
    */
  def styleTracked(formatted: String, pct: Double): String = {
    if (pct >= TrackedGreenThresholdPct) green(formatted)
    else if (pct >= TrackedYellowThresholdPct) yellow(formatted)
    else red(formatted)
  }
}
