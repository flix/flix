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

import ca.uwaterloo.flix.language.ast.SourceLocation

/**
  * Pure text/numeric helpers used by the compiler-top TUI. No ANSI styling,
  * no terminal IO — just formatting primitives.
  */
object Formatting {

  // -- Numeric helpers ----------------------------------------------------

  /** Returns `(usedMb, maxMb)` from the JVM runtime. */
  def heapUsage(): (Long, Long) = {
    val rt = Runtime.getRuntime
    val used = rt.totalMemory() - rt.freeMemory()
    val max = rt.maxMemory()
    (used / (1024L * 1024L), max / (1024L * 1024L))
  }

  /**
    * Returns ms-per-source-line for the given `nanos / lines` pair, or 0
    * when `lines <= 0` (synthetic defs with no real source span — the
    * denominator would be meaningless). Single source of truth for the
    * "hotness" metric used by both the hotness sort key and the threshold-
    * based row coloring.
    */
  def hotnessMsPerLine(nanos: Long, lines: Int): Double =
    if (lines <= 0) 0.0 else (nanos / 1_000_000L).toDouble / lines

  /**
    * Floor on `reduces` below which the `rpv` density score is suppressed.
    * Like the `MinSkewableNanos` floor from earlier cohort experiments, this
    * keeps tiny wrapper defs (a couple-line combinator doing a few hundred
    * reductions over very few variables) out of the ranking — a raw ratio
    * would otherwise reward those small denominators rather than the defs
    * doing genuinely disproportionate type-reduction work.
    */
  val MinDensityReduces: Long = 3000L

  /**
    * The `rpv` ("reductions per variable") density score: deterministic
    * constraint-solver `reduces` divided by the def's `tvars + evars` (the
    * static type/effect variable counts). Surfaces defs whose bodies drive
    * far more type reduction than their variable count implies — orthogonal
    * to `cns` / `tv` / `ev` and to wall time. `None` (rendered `-`, sorted to
    * the bottom) when `reduces` is below [[MinDensityReduces]].
    *
    * Single source of truth for both the `rpv` sort key and its rendered cell
    * so the displayed and sorted values can't drift.
    */
  def densityScore(reduces: Long, tvars: Long, evars: Long): Option[Double] =
    if (reduces < MinDensityReduces) None
    else Some(reduces.toDouble / (tvars + evars + 1L).toDouble)

  /** Renders [[densityScore]] as a right-padded integer, or `-` when unscored. */
  def formatDensity(reduces: Long, tvars: Long, evars: Long): String =
    densityScore(reduces, tvars, evars).map(d => f"$d%.0f").getOrElse("-")

  // -- Formatting helpers -------------------------------------------------

  /** Renders a [[SourceLocation]] as `file:line`, or `?` if synthetic. */
  def formatLocation(loc: SourceLocation): String =
    if (!loc.isReal) "?" else s"${loc.source.name}:${loc.startLine}"

  /** Returns the inclusive line span of `loc`, or 0 if synthetic. */
  def lineCount(loc: SourceLocation): Int =
    if (!loc.isReal) 0 else (loc.endLine - loc.startLine + 1).max(0)

  /** Formats a nanosecond duration as `Nms` or `N.Ns` (≥10s). */
  def formatMillis(nanos: Long): String = {
    val ms = nanos / 1_000_000L
    if (ms >= 10_000L) f"${ms / 1000.0}%.1fs"
    else f"${ms}ms"
  }

  /**
    * Formats a byte count compactly into at most 5 characters: `NB`, `NKB`,
    * `NNNKB`, `NMB`, `NNNMB`, `NGB`, `NNNGB`, or `NTB`. Always integer —
    * sacrifices precision below 10 of each unit so the digit width stays
    * uniform across rows. A mixed integer / one-decimal scheme made `9.0MB`
    * read visually wider than `14MB` despite being the smaller value.
    *
    * Units roll over at 1000 (not 1024) so the formatted string never exceeds
    * 5 characters. E.g. a rounded `1023KB` becomes `1MB` rather than the
    * 6-char `1023KB` it would be under exact-power-of-two rollover — which
    * would overflow the 5-char column it sits in and wrap the row.
    */
  def formatBytes(bytes: Long): String = {
    val Kib = 1024L
    val Mib = Kib * Kib
    val Gib = Kib * Mib
    val Tib = Kib * Gib
    val UnitMax = 1000L
    if (bytes < Kib) s"${bytes}B"
    else {
      val kb = math.round(bytes.toDouble / Kib)
      if (kb < UnitMax) s"${kb}KB"
      else {
        val mb = math.round(bytes.toDouble / Mib)
        if (mb < UnitMax) s"${mb}MB"
        else {
          val gb = math.round(bytes.toDouble / Gib)
          if (gb < UnitMax) s"${gb}GB"
          else s"${math.round(bytes.toDouble / Tib)}TB"
        }
      }
    }
  }

  /** Returns `s` truncated to `width` characters, with a trailing ellipsis when shortened. */
  def truncate(s: String, width: Int): String =
    if (s.length <= width) s else s.take(width - 1) + "…"

  /**
    * Returns `s` truncated to `width` characters, with a *leading* ellipsis
    * when shortened. Mirror of [[truncate]] for fields where the tail carries
    * the signal — e.g. a source location like `…flix:128` reads better than
    * `Foo.fl…` when the budget can't fit the whole `Foo.flix:128`.
    */
  def truncateFront(s: String, width: Int): String =
    if (s.length <= width) s else "…" + s.takeRight(width - 1)

  /** Left-pads `s` with spaces to width `w`. */
  def lpad(s: String, w: Int): String =
    if (s.length >= w) s else " " * (w - s.length) + s

  /** Right-pads `s` with spaces to width `w`. */
  def rpad(s: String, w: Int): String =
    if (s.length >= w) s else s + " " * (w - s.length)
}
