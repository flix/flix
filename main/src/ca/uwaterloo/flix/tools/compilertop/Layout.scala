/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.tools.compilertop

/**
  * Column layout for the per-frame table render. Computed from the current
  * terminal width: the merged name column absorbs whatever the fixed and
  * optional columns leave over, compressing down to [[Layout.MinNameWidth]]
  * before any optional column is dropped. Only when even that floor cannot
  * be kept are the optional phase / counts (or cns) / lines columns dropped,
  * in that order. When the terminal is wide, the surplus expands the name
  * column directly. The name column holds the def name + its source location
  * in parens on def rows, and the module name on module rows.
  *
  * @param nameWidth   width of the merged name column (def + `(file:line)` on def rows, module name on module rows).
  * @param showLines   whether to render the lines column.
  * @param showCounts  whether to render the mono / inl / cls per-phase count columns.
  * @param showCns     whether to render the cns / tv / ev columns.
  * @param showPhase   whether to render the dominant-phase column.
  * @param totalWidth  total rendered width of the row, less leading/trailing pad.
  */
final case class Layout(nameWidth: Int, showLines: Boolean, showCounts: Boolean, showCns: Boolean, showPhase: Boolean, totalWidth: Int)

object Layout {

  /**
    * Floor on the merged name column width. Wide enough that a typical
    * `Module.def (…flix:NNN)` keeps the whole sym plus a useful tail of its
    * location (see the truncation ladder in the renderer's def row).
    *
    * Optional columns are dropped only once the name column would otherwise
    * fall below this floor: a narrower name beats a missing column, since the
    * user toggles into the frontend / backend views precisely to see their
    * extra columns. At the 100-column fallback terminal width the backend
    * view — the widest optional set — fits every column with the name at
    * exactly this floor.
    */
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
  /**
    * Combined width contribution of all three frontend-only columns
    * (`cns`, `tv`, `ev`) including separators.
    */
  private val FrontendColsWidth: Int = CnsColWidth + TvarsColWidth + EvarsColWidth
  /** Width contribution of the optional dominant-phase column (separator + width). */
  private val PhaseColWidth: Int = 1 + 10

  /**
    * Picks a [[Layout]] for the given terminal width by trying tiers in
    * descending feature order. A tier is taken as soon as its fixed columns
    * leave at least [[MinNameWidth]] for the name column, which then takes
    * every remaining column — so `totalWidth` equals `cols` exactly and the
    * divider and rows fill the terminal regardless of which tier we land in.
    *
    * The caller gates the optional count-style columns by passing
    * `showCounts` (mono / inl / cls / size) and `showCns` (cns / tv / ev) —
    * Layout itself doesn't know which UI mode they correspond to. When a
    * flag is false the column is hidden and the reclaimed width expands the
    * name column instead.
    *
    * At most one of `showCounts` and `showCns` is true under the current
    * UI gating, but the algorithm accepts any combination — both false
    * means neither column group renders; both true would render both.
    */
  def compute(cols: Int, showCounts: Boolean, showCns: Boolean): Layout = {
    // Width contribution of the "fixed" half: separator before alloc + tail.
    val tail = 1 + FixedTailWidth

    val countsW = if (showCounts) CountsColWidth else 0
    val cnsW = if (showCns) FrontendColsWidth else 0
    // Combined extra contribution of the optional count columns.
    val extraColsW = countsW + cnsW

    /** True if `tierFixed` chars of non-name columns leave at least [[MinNameWidth]] for the name column. */
    def fits(tierFixed: Int): Boolean = cols - tierFixed >= MinNameWidth

    /** Returns a [[Layout]] for `tierFixed` chars of non-name columns; the name column takes the rest. */
    def fillTier(tierFixed: Int, showLines: Boolean, showCountsOut: Boolean, showCnsOut: Boolean, showPhase: Boolean): Layout =
      Layout(cols - tierFixed, showLines = showLines, showCounts = showCountsOut, showCns = showCnsOut, showPhase = showPhase,
        totalWidth = cols)

    // Try tiers in descending feature order. The name column compresses toward
    // its floor before any optional column is dropped.
    val full = LinesColWidth + extraColsW + PhaseColWidth + tail
    if (fits(full))
      return fillTier(full, showLines = true, showCountsOut = showCounts, showCnsOut = showCns, showPhase = true)

    // Tier: drop phase.
    val noPhase = LinesColWidth + extraColsW + tail
    if (fits(noPhase))
      return fillTier(noPhase, showLines = true, showCountsOut = showCounts, showCnsOut = showCns, showPhase = false)

    // Tier: drop phase + the optional counts (mono/inl/cls/size or cns/tv/ev).
    val noPhaseNoExtras = LinesColWidth + tail
    if (fits(noPhaseNoExtras))
      return fillTier(noPhaseNoExtras, showLines = true, showCountsOut = false, showCnsOut = false, showPhase = false)

    // Tier: drop phase + extras + lines.
    if (fits(tail))
      return fillTier(tail, showLines = false, showCountsOut = false, showCnsOut = false, showPhase = false)

    // Even the minimum tier doesn't fit; hold the name at its floor and let the row overflow.
    Layout(MinNameWidth, showLines = false, showCounts = false, showCns = false, showPhase = false,
      totalWidth = MinNameWidth + tail)
  }
}
