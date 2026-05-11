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

import ca.uwaterloo.flix.tools.compilertop.Model.PhaseFilter

/**
  * Column layout for the per-frame table render. Computed from the current
  * terminal width: when the terminal is narrow, the optional LOC / counts
  * / cns / phase columns are dropped in that order (lowest-signal first).
  * When the terminal is wide, the surplus is distributed between the
  * DefnSym and location columns proportionally to their default widths.
  *
  * @param symWidth    width of the DefnSym column.
  * @param locWidth    width of the location column.
  * @param showLOC     whether to render the LOC column.
  * @param showCounts  whether to render the mono / opt / cls per-phase count columns (backend filter only).
  * @param showCns     whether to render the cns (constraints) column (frontend filter only).
  * @param showPhase   whether to render the dominant-phase column.
  * @param totalWidth  total rendered width of the row, less leading/trailing pad.
  */
final case class Layout(symWidth: Int, locWidth: Int, showLOC: Boolean, showCounts: Boolean, showCns: Boolean, showPhase: Boolean, totalWidth: Int)

object Layout {

  /** Default width of the DefnSym column when the terminal is wide enough. */
  private val DefaultSymWidth: Int = 24
  /** Default width of the location column when the terminal is wide enough. */
  private val DefaultLocWidth: Int = 28
  /** Floor on the DefnSym column width when the terminal is narrow. */
  private val MinSymWidth: Int = 16
  /** Floor on the location column width when the terminal is narrow. */
  private val MinLocWidth: Int = 20

  /** Fixed-width contribution of `time + %cpu + %wall` columns and their separators. */
  private val FixedTailWidth: Int = 9 + 1 + 6 + 1 + 6 // time(9) + %cpu(6) + %wall(6) with two separators between

  /** Width contribution of the optional LOC column (separator + width). */
  private val LocColWidth: Int = 1 + 4
  /** Width contribution of the optional mono / opt / cls per-phase count columns (3 × separator + width). */
  private val CountsColWidth: Int = 3 * (1 + 4)
  /** Width contribution of the optional cns column (separator + 5-char numeric field). */
  private val CnsColWidth: Int = 1 + 5
  /** Width contribution of the optional tvars column (separator + 4-char numeric field). */
  private val TvarsColWidth: Int = 1 + 4
  /** Width contribution of the optional evars column (separator + 4-char numeric field). */
  private val EvarsColWidth: Int = 1 + 4
  /**
    * Combined width contribution of all three frontend-only columns
    * (`cns`, `tv`, `ev`) including separators.
    */
  private val FrontendColsWidth: Int = CnsColWidth + TvarsColWidth + EvarsColWidth
  /** Width contribution of the optional dominant-phase column (separator + width). */
  private val PhaseColWidth: Int = 1 + 10

  /**
    * Picks a [[Layout]] for the given terminal width by trying tiers in
    * descending feature order. The optional count-style columns are
    * filter-gated to the views where their values are meaningful:
    *   - `mono` / `opt` / `cls` only under [[PhaseFilter.Backend]] (the
    *     phases that populate them only fire in the backend pipeline).
    *   - `cns` only under [[PhaseFilter.Frontend]] (constraint generation
    *     is purely a Typer concern).
    * Outside their respective filters, the columns are hidden and the
    * reclaimed width expands the sym / location text columns instead.
    */
  def compute(cols: Int, activeFilter: PhaseFilter): Layout = {
    // Width contribution of the "fixed" half: separator before time + tail.
    val tail = 1 + FixedTailWidth
    // Width of just the text section (sym + 1 + location) at default sizes.
    def textWidth(symW: Int, locW: Int): Int = symW + 1 + locW

    val countsAllowed = activeFilter == PhaseFilter.Backend
    val countsW = if (countsAllowed) CountsColWidth else 0
    val cnsAllowed = activeFilter == PhaseFilter.Frontend
    val cnsW = if (cnsAllowed) FrontendColsWidth else 0
    // Combined extra contribution of all filter-specific count columns. At
    // most one filter's columns are active for any given filter, so this is
    // either CountsColWidth, CnsColWidth, or zero.
    val extraColsW = countsW + cnsW

    // Try tiers in descending feature order.
    val full = textWidth(DefaultSymWidth, DefaultLocWidth) + LocColWidth + extraColsW + PhaseColWidth + tail
    if (cols >= full) {
      // Surplus → expand sym (~46%) and location (~54%) proportionally.
      val extra = cols - full
      val extraSym = extra * 46 / 100
      val extraLoc = extra - extraSym
      val symW = DefaultSymWidth + extraSym
      val locW = DefaultLocWidth + extraLoc
      return Layout(symW, locW, showLOC = true, showCounts = countsAllowed, showCns = cnsAllowed, showPhase = true,
        totalWidth = textWidth(symW, locW) + LocColWidth + extraColsW + PhaseColWidth + tail)
    }

    // Tier: drop phase.
    val noPhase = textWidth(DefaultSymWidth, DefaultLocWidth) + LocColWidth + extraColsW + tail
    if (cols >= noPhase)
      return Layout(DefaultSymWidth, DefaultLocWidth, showLOC = true, showCounts = countsAllowed, showCns = cnsAllowed, showPhase = false, noPhase)

    // Tier: drop phase + filter-specific counts (mono/opt/cls or cns).
    val noPhaseNoExtras = textWidth(DefaultSymWidth, DefaultLocWidth) + LocColWidth + tail
    if (cols >= noPhaseNoExtras)
      return Layout(DefaultSymWidth, DefaultLocWidth, showLOC = true, showCounts = false, showCns = false, showPhase = false, noPhaseNoExtras)

    // Tier: drop phase + extras + LOC.
    val noOptional = textWidth(DefaultSymWidth, DefaultLocWidth) + tail
    if (cols >= noOptional)
      return Layout(DefaultSymWidth, DefaultLocWidth, showLOC = false, showCounts = false, showCns = false, showPhase = false, noOptional)

    // Even minimum tier doesn't fit; shrink sym/locWidth to floors.
    val available = (cols - tail).max(MinSymWidth + 1 + MinLocWidth)
    val symW = MinSymWidth.max(available * 46 / 100)
    val locW = MinLocWidth.max(available - 1 - symW)
    Layout(symW, locW, showLOC = false, showCounts = false, showCns = false, showPhase = false,
      textWidth(symW, locW) + tail)
  }
}
