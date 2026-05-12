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
  * Column layout for the per-frame table render. Computed from the current
  * terminal width: when the terminal is narrow, the optional LOC / counts
  * / cns / phase columns are dropped in that order (lowest-signal first).
  * When the terminal is wide, the surplus is distributed between the
  * DefnSym and location columns proportionally to their default widths.
  *
  * @param symWidth    width of the DefnSym column.
  * @param locWidth    width of the location column.
  * @param showLOC     whether to render the LOC column.
  * @param showCounts  whether to render the mono / opt / cls per-phase count columns.
  * @param showCns     whether to render the cns / tv / ev columns.
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
  /** Width contribution of the optional mono / opt / inl / cls / size per-phase count columns (4 × 4-char + 1 × 5-char, with separators). */
  private val CountsColWidth: Int = 4 * (1 + 4) + (1 + 5)
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
    * descending feature order. The caller gates the optional count-style
    * columns by passing `showCounts` (mono / opt / inl / cls / size) and
    * `showCns` (cns / tv / ev) — Layout itself doesn't know which UI mode
    * they correspond to. When a flag is false the column is hidden and the
    * reclaimed width expands the sym / location text columns instead.
    *
    * At most one of `showCounts` and `showCns` is true under the current
    * UI gating, but the algorithm accepts any combination — both false
    * means neither column group renders; both true would render both.
    */
  def compute(cols: Int, showCounts: Boolean, showCns: Boolean): Layout = {
    // Width contribution of the "fixed" half: separator before time + tail.
    val tail = 1 + FixedTailWidth
    // Width of just the text section (sym + 1 + location) at default sizes.
    def textWidth(symW: Int, locW: Int): Int = symW + 1 + locW

    val countsW = if (showCounts) CountsColWidth else 0
    val cnsW = if (showCns) FrontendColsWidth else 0
    // Combined extra contribution of the optional count columns.
    val extraColsW = countsW + cnsW

    /**
      * Returns a [[Layout]] for `tierFixed` chars of non-text columns
      * (LOC / counts / phase / tail) plus a sym + loc text section that
      * expands to fill any surplus over [[DefaultSymWidth]] /
      * [[DefaultLocWidth]]. The resulting `totalWidth` equals `cols`
      * exactly, so the divider and rows fill the terminal regardless of
      * which tier we landed in.
      */
    def fillTier(tierFixed: Int, showLOC: Boolean, showCountsOut: Boolean, showCnsOut: Boolean, showPhase: Boolean): Layout = {
      val baseText = textWidth(DefaultSymWidth, DefaultLocWidth)
      val extra = (cols - (baseText + tierFixed)).max(0)
      val extraSym = extra * 46 / 100
      val extraLoc = extra - extraSym
      val symW = DefaultSymWidth + extraSym
      val locW = DefaultLocWidth + extraLoc
      Layout(symW, locW, showLOC = showLOC, showCounts = showCountsOut, showCns = showCnsOut, showPhase = showPhase,
        totalWidth = textWidth(symW, locW) + tierFixed)
    }

    // Try tiers in descending feature order. Each tier expands sym/loc
    // into whatever width is left over after its fixed columns.
    val full = textWidth(DefaultSymWidth, DefaultLocWidth) + LocColWidth + extraColsW + PhaseColWidth + tail
    if (cols >= full)
      return fillTier(LocColWidth + extraColsW + PhaseColWidth + tail, showLOC = true, showCountsOut = showCounts, showCnsOut = showCns, showPhase = true)

    // Tier: drop phase.
    val noPhase = textWidth(DefaultSymWidth, DefaultLocWidth) + LocColWidth + extraColsW + tail
    if (cols >= noPhase)
      return fillTier(LocColWidth + extraColsW + tail, showLOC = true, showCountsOut = showCounts, showCnsOut = showCns, showPhase = false)

    // Tier: drop phase + the optional counts (mono/opt/inl/cls/size or cns/tv/ev).
    val noPhaseNoExtras = textWidth(DefaultSymWidth, DefaultLocWidth) + LocColWidth + tail
    if (cols >= noPhaseNoExtras)
      return fillTier(LocColWidth + tail, showLOC = true, showCountsOut = false, showCnsOut = false, showPhase = false)

    // Tier: drop phase + extras + LOC.
    val noOptional = textWidth(DefaultSymWidth, DefaultLocWidth) + tail
    if (cols >= noOptional)
      return fillTier(tail, showLOC = false, showCountsOut = false, showCnsOut = false, showPhase = false)

    // Even minimum tier doesn't fit; shrink sym/locWidth to floors.
    val available = (cols - tail).max(MinSymWidth + 1 + MinLocWidth)
    val symW = MinSymWidth.max(available * 46 / 100)
    val locW = MinLocWidth.max(available - 1 - symW)
    Layout(symW, locW, showLOC = false, showCounts = false, showCns = false, showPhase = false,
      textWidth(symW, locW) + tail)
  }
}
