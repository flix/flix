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
  * terminal width: when the terminal is narrow, the optional lines / counts
  * / cns / phase columns are dropped in that order (lowest-signal first).
  * When the terminal is wide, the surplus expands the merged name column
  * directly. The name column holds the def name + its source location in
  * parens on def rows, and the module name on module rows.
  *
  * @param nameWidth   width of the merged name column (def + `(file:line)` on def rows, module name on module rows).
  * @param showLines   whether to render the lines column.
  * @param showCounts  whether to render the mono / inl / cls per-phase count columns.
  * @param showCns     whether to render the cns / tv / ev / tpv columns.
  * @param showPhase   whether to render the dominant-phase column.
  * @param totalWidth  total rendered width of the row, less leading/trailing pad.
  */
final case class Layout(nameWidth: Int, showLines: Boolean, showCounts: Boolean, showCns: Boolean, showPhase: Boolean, totalWidth: Int)

object Layout {

  /**
    * Default width of the merged name column. Sized to comfortably fit the
    * common case of `Module.def (File.flix:NNN)` without truncation; longer
    * names trim the location from the front first, then the sym from the
    * back. Smaller than the previous `sym (24) + 1 + loc (28) = 53` budget
    * because most defs use a fraction of that — the freed width lets
    * optional columns (phase / counts / lines) survive at narrower terminals.
    */
  private val DefaultNameWidth: Int = 44
  /** Floor on the merged name column width when the terminal is narrow. */
  private val MinNameWidth: Int = 30

  /** Fixed-width contribution of `alloc + time + %cpu + %wall` columns and their separators. */
  private val FixedTailWidth: Int = 5 + 1 + 9 + 1 + 6 + 1 + 6 // alloc(5) + time(9) + %cpu(6) + %wall(6) with three separators between

  /** Width contribution of the optional lines column (separator + width). */
  private val LinesColWidth: Int = 1 + 5
  /** Width contribution of the optional mono / inl / cls / size per-phase count columns (3 × 4-char + 1 × 5-char, with separators). */
  private val CountsColWidth: Int = 3 * (1 + 4) + (1 + 5)
  /** Width contribution of the optional cns column (separator + 5-char numeric field). */
  private val CnsColWidth: Int = 1 + 5
  /** Width contribution of the optional tvars column (separator + 4-char numeric field). */
  private val TvarsColWidth: Int = 1 + 4
  /** Width contribution of the optional evars column (separator + 4-char numeric field). */
  private val EvarsColWidth: Int = 1 + 4
  /** Width contribution of the optional type-substitution-per-variable density column (`tpv`): separator + 4-char numeric field; the ratio is a small integer. */
  private val SubstColWidth: Int = 1 + 4
  /**
    * Combined width contribution of all four frontend-only columns
    * (`cns`, `tv`, `ev`, `tpv`) including separators.
    */
  private val FrontendColsWidth: Int = CnsColWidth + TvarsColWidth + EvarsColWidth + SubstColWidth
  /** Width contribution of the optional dominant-phase column (separator + width). */
  private val PhaseColWidth: Int = 1 + 10

  /**
    * Picks a [[Layout]] for the given terminal width by trying tiers in
    * descending feature order. The caller gates the optional count-style
    * columns by passing `showCounts` (mono / inl / cls / size) and
    * `showCns` (cns / tv / ev / tpv) — Layout itself doesn't know which UI mode
    * they correspond to. When a flag is false the column is hidden and the
    * reclaimed width expands the name column instead.
    *
    * At most one of `showCounts` and `showCns` is true under the current
    * UI gating, but the algorithm accepts any combination — both false
    * means neither column group renders; both true would render both.
    */
  def compute(cols: Int, showCounts: Boolean, showCns: Boolean): Layout = {
    // Width contribution of the "fixed" half: separator before time + tail.
    val tail = 1 + FixedTailWidth

    val countsW = if (showCounts) CountsColWidth else 0
    val cnsW = if (showCns) FrontendColsWidth else 0
    // Combined extra contribution of the optional count columns.
    val extraColsW = countsW + cnsW

    /**
      * Returns a [[Layout]] for `tierFixed` chars of non-text columns
      * (lines / counts / phase / tail) plus a name column that expands to
      * fill any surplus over [[DefaultNameWidth]]. The resulting
      * `totalWidth` equals `cols` exactly, so the divider and rows fill
      * the terminal regardless of which tier we landed in.
      */
    def fillTier(tierFixed: Int, showLines: Boolean, showCountsOut: Boolean, showCnsOut: Boolean, showPhase: Boolean): Layout = {
      val extra = (cols - (DefaultNameWidth + tierFixed)).max(0)
      val nameW = DefaultNameWidth + extra
      Layout(nameW, showLines = showLines, showCounts = showCountsOut, showCns = showCnsOut, showPhase = showPhase,
        totalWidth = nameW + tierFixed)
    }

    // Try tiers in descending feature order. Each tier expands name into
    // whatever width is left over after its fixed columns.
    val full = DefaultNameWidth + LinesColWidth + extraColsW + PhaseColWidth + tail
    if (cols >= full)
      return fillTier(LinesColWidth + extraColsW + PhaseColWidth + tail, showLines = true, showCountsOut = showCounts, showCnsOut = showCns, showPhase = true)

    // Tier: drop phase.
    val noPhase = DefaultNameWidth + LinesColWidth + extraColsW + tail
    if (cols >= noPhase)
      return fillTier(LinesColWidth + extraColsW + tail, showLines = true, showCountsOut = showCounts, showCnsOut = showCns, showPhase = false)

    // Tier: drop phase + the optional counts (mono/inl/cls/size or cns/tv/ev/tpv).
    val noPhaseNoExtras = DefaultNameWidth + LinesColWidth + tail
    if (cols >= noPhaseNoExtras)
      return fillTier(LinesColWidth + tail, showLines = true, showCountsOut = false, showCnsOut = false, showPhase = false)

    // Tier: drop phase + extras + lines.
    val noOptional = DefaultNameWidth + tail
    if (cols >= noOptional)
      return fillTier(tail, showLines = false, showCountsOut = false, showCnsOut = false, showPhase = false)

    // Even minimum tier doesn't fit; shrink name to its floor.
    val nameW = (cols - tail).max(MinNameWidth)
    Layout(nameW, showLines = false, showCounts = false, showCns = false, showPhase = false,
      totalWidth = nameW + tail)
  }
}
