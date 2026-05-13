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
    * Returns ms-per-source-line for the given `nanos / locLines` pair, or 0
    * when `locLines <= 0` (synthetic defs with no real source span — the
    * denominator would be meaningless). Single source of truth for the
    * "hotness" metric used by both the hotness sort key and the threshold-
    * based row coloring.
    */
  def hotnessMsPerLine(nanos: Long, locLines: Int): Double =
    if (locLines <= 0) 0.0 else (nanos / 1_000_000L).toDouble / locLines

  // -- Formatting helpers -------------------------------------------------

  /** Renders a [[SourceLocation]] as `file:line`, or `?` if synthetic. */
  def formatLocation(loc: SourceLocation): String =
    if (!loc.isReal) "?" else s"${loc.source.name}:${loc.startLine}"

  /** Returns the inclusive line span of `loc`, or 0 if synthetic. */
  def locLineCount(loc: SourceLocation): Int =
    if (!loc.isReal) 0 else (loc.endLine - loc.startLine + 1).max(0)

  /** Formats a nanosecond duration as `Nms` or `N.Ns` (≥10s). */
  def formatMillis(nanos: Long): String = {
    val ms = nanos / 1_000_000L
    if (ms >= 10_000L) f"${ms / 1000.0}%.1fs"
    else f"${ms}ms"
  }

  /**
    * Formats a byte count compactly into at most 5 characters: `NB`, `N.NKB`,
    * `NNNKB`, `N.NMB`, or `NNNMB`. Uses one decimal place under 10 of the
    * higher unit so small values keep precision while large ones stay narrow.
    */
  def formatBytes(bytes: Long): String = {
    val Kib = 1024.0
    val Mib = 1024.0 * 1024.0
    if (bytes < 1024L) s"${bytes}B"
    else if (bytes < 1024L * 1024L) {
      val kb = bytes / Kib
      if (kb < 10.0) f"$kb%.1fKB" else f"${kb.round}%dKB"
    } else {
      val mb = bytes / Mib
      if (mb < 10.0) f"$mb%.1fMB" else f"${mb.round}%dMB"
    }
  }

  /** Returns `s` truncated to `width` characters, with a trailing ellipsis when shortened. */
  def truncate(s: String, width: Int): String =
    if (s.length <= width) s else s.take(width - 1) + "…"

  /** Left-pads `s` with spaces to width `w`. */
  def lpad(s: String, w: Int): String =
    if (s.length >= w) s else " " * (w - s.length) + s

  /** Right-pads `s` with spaces to width `w`. */
  def rpad(s: String, w: Int): String =
    if (s.length >= w) s else s + " " * (w - s.length)
}
