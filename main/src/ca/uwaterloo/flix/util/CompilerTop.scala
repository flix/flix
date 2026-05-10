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
package ca.uwaterloo.flix.util

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.util.CompilerProfiler.DefnStats
import org.jline.terminal.{Attributes, Terminal, TerminalBuilder}

import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.collection.mutable

object CompilerTop {

  /**
    * Restricts which phases' time the dashboard accounts for. Toggled
    * interactively via the input thread (`f` / `b` / `a`). The split tracks
    * `Flix.check` (frontend) vs the phases that follow in `Flix.compile`'s
    * `codeGen` (backend) — see [[FrontendPhases]].
    */
  sealed trait PhaseFilter
  object PhaseFilter {
    case object All extends PhaseFilter
    case object Frontend extends PhaseFilter
    case object Backend extends PhaseFilter
  }

  /**
    * Phase-name strings that count as "frontend" — i.e. phases that run
    * inside `Flix.check`. Strings must match the literals each phase passes
    * to `flix.phase("…")` / `flix.phaseNew("…")`. Verified against the
    * `run` methods under `main/src/ca/uwaterloo/flix/language/phase`.
    *
    * Phases not in this set (and not the unknown-phase fallback `"?"`) are
    * treated as backend.
    */
  private val FrontendPhases: Set[String] = Set(
    "Reader", "Lexer", "Parser2", "Weeder2", "Desugar", "Namer", "Resolver",
    "Kinder", "Deriver", "Typer", "EntryPoints", "Instances", "PredDeps",
    "Stratifier", "PatMatch", "Redundancy", "Safety", "Terminator", "Dependencies",
  )

  /** True if `phase` should be accounted for under the current `f`. */
  private def matchesFilter(phase: String, f: PhaseFilter): Boolean = f match {
    case PhaseFilter.All      => true
    case PhaseFilter.Frontend => FrontendPhases.contains(phase)
    case PhaseFilter.Backend  => phase != "?" && !FrontendPhases.contains(phase)
  }

  /**
    * Selects the descending sort key applied to the def and module tables.
    * Each option surfaces a different kind of suspect, so the same def can
    * top one ranking and not another.
    */
  sealed trait Sort
  object Sort {
    /** Slowest defs first. The default. */
    case object Time extends Sort
    /** Hottest-per-line defs first — small defs that burn disproportionate time. */
    case object Hotness extends Sort
    /** Most monomorphic instances first — surfaces generic-explosion hotspots. */
    case object Mono extends Sort
    /** Most optimizer fixed-point re-visits first — surfaces inliner / occurrence-analyzer thrashing. */
    case object Opt extends Sort
    /** Most class files emitted first — surfaces JVM fan-out from polymorphism / closures. */
    case object Cls extends Sort
    /** Most type constraints first — surfaces type-checking-heavy defs. */
    case object Cns extends Sort
  }

  /** How often the screen refreshes, in milliseconds (5 FPS). */
  private val RefreshIntervalMs: Long = 200L

  /** Poll interval for the input thread (ms); doubles as how fast `running=false` is observed. */
  private val InputPollMs: Long = 100L

  /** Total number of phases the compiler runs. Bump when `Flix.check` / `Flix.codeGen` adds or removes a `phase` / `phaseNew` call. */
  private val TotalPhases: Int = 32

  /** Longest phase-name length, used to pad the phase column. Currently set by `"Dependencies"` / `"EffectBinder"`. */
  private val MaxPhaseLen: Int = 12

  /** Width (in characters) of the phase-progress bar. */
  private val BarWidth: Int = 12

  /** 1-indexed column where the `progress` label starts on the dashboard. */
  private lazy val ProgressStartCol: Int = 5 + MaxPhaseLen

  /** 1-indexed column where the `threads` label starts on the dashboard. */
  private lazy val ThreadsStartCol: Int = ProgressStartCol + 20 + BarWidth

  /** Width of the threads-sparkline. */
  private val SparkWidth: Int = 12

  /** Block characters used for the sparkline, low → high. */
  private val SparkChars: Array[Char] = "▁▂▃▄▅▆▇█".toArray

  /** ANSI escape character. */
  private val ESC: Char = 27.toChar

  /** ANSI sequence that homes the cursor and clears the screen. */
  private val ClearScreen: String = s"$ESC[H$ESC[2J"

  /**
    * DEC mode 2026 — Begin Synchronized Update. Terminals that support it
    * (iTerm2, kitty, alacritty, wezterm, modern xterm, recent VTE) buffer
    * the bytes between BSU and ESU and swap atomically, eliminating the
    * flash from clear-and-redraw. Terminals that don't recognize the
    * sequence silently ignore it.
    */
  private val BeginSync: String = s"$ESC[?2026h"

  /** DEC mode 2026 — End Synchronized Update; pair of [[BeginSync]]. */
  private val EndSync: String = s"$ESC[?2026l"

  // Color codes.

  /** ANSI reset (clears bold, dim, color, etc.). */
  private val Reset: String = s"$ESC[0m"
  /** ANSI bold. */
  private val BoldCode: String = s"$ESC[1m"
  /** ANSI dim/faint. */
  private val DimCode: String = s"$ESC[2m"
  /** ANSI underline on (paired with [[NoUnderlineCode]] — surgical, leaves bold/color intact). */
  private val UnderlineCode: String = s"$ESC[4m"
  /** ANSI underline off; cancels [[UnderlineCode]] without disturbing bold/color. */
  private val NoUnderlineCode: String = s"$ESC[24m"
  /** ANSI foreground red. */
  private val Red: String = s"$ESC[31m"
  /** ANSI foreground green. */
  private val Green: String = s"$ESC[32m"
  /** ANSI foreground yellow. */
  private val Yellow: String = s"$ESC[33m"
  /** ANSI foreground blue. */
  private val Blue: String = s"$ESC[34m"
  /** ANSI foreground cyan. */
  private val Cyan: String = s"$ESC[36m"
  /** ANSI foreground bright black (gray). */
  private val Gray: String = s"$ESC[90m"

  // Wrappers — each takes a string and returns it surrounded by codes.

  /** Wraps `s` in the ANSI color code `c` and a reset suffix. */
  private def color(s: String, c: String): String = s"$c$s$Reset"
  /** Wraps `s` in ANSI bold codes. */
  private def bold(s: String): String = s"$BoldCode$s$Reset"
  /** Wraps `s` in ANSI dim/faint codes. */
  private def dim(s: String): String = s"$DimCode$s$Reset"
  /** Wraps `s` in ANSI red codes. */
  private def red(s: String): String = color(s, Red)
  /** Wraps `s` in ANSI yellow codes. */
  private def yellow(s: String): String = color(s, Yellow)
  /** Wraps `s` in ANSI green codes. */
  private def green(s: String): String = color(s, Green)
  /** Wraps `s` in ANSI blue codes. */
  private def blue(s: String): String = color(s, Blue)
  /** Wraps `s` in ANSI cyan codes. */
  private def cyan(s: String): String = color(s, Cyan)

  /**
    * Renders a column header with one keystroke letter underlined. Active
    * sort columns are bold yellow; the rest are bold cyan. The label string
    * is taken as-is (already padded by the caller) and the keystroke
    * position is given as a 0-based index into it. Visible width is
    * unchanged — only ANSI codes are inserted around the keystroke char.
    */
  private def keyHeader(paddedLabel: String, keyIdx: Int, active: Boolean): String = {
    val c = if (active) Yellow else Cyan
    val before = paddedLabel.take(keyIdx)
    val key = paddedLabel.slice(keyIdx, keyIdx + 1)
    val after = paddedLabel.drop(keyIdx + 1)
    s"$BoldCode$c$before$UnderlineCode$key$NoUnderlineCode$after$Reset"
  }

  /** Renders a non-sort column header (no underlined keystroke) in bold cyan. */
  private def plainHeader(paddedLabel: String): String =
    s"$BoldCode$Cyan$paddedLabel$Reset"

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
  private val PctCpuRedThreshold:              Double = 5.0
  private val PctCpuYellowThreshold:           Double = 1.0
  private val PctWallRedThreshold:             Double = 15.0
  private val PctWallYellowThreshold:          Double = 5.0
  private val TimeRedThresholdMs:              Long   = 1000L
  private val TimeYellowThresholdMs:           Long   = 50L

  // -- Conditional styling -------------------------------------------------

  /** Colors a formatted time field by absolute magnitude. */
  private def styleTime(formatted: String, nanos: Long): String = {
    val ms = nanos / 1_000_000L
    if (ms >= TimeRedThresholdMs) bold(red(formatted))
    else if (ms >= TimeYellowThresholdMs) yellow(formatted)
    else formatted
  }

  /**
    * Colors the sym name based on `time / locLines` — a "hotness per line"
    * signal that surfaces small defs which consume time disproportionate
    * to their body size. Defs with no real source span (`locLines <= 0`,
    * e.g. lifted closures) are left unstyled because the denominator is
    * meaningless.
    */
  private def styleSym(name: String, nanos: Long, locLines: Int): String = {
    if (locLines <= 0) return name
    val msPerLine = (nanos / 1_000_000L).toDouble / locLines
    if (msPerLine >= HotnessRedThresholdMsPerLine) bold(red(name))
    else if (msPerLine >= HotnessYellowThresholdMsPerLine) yellow(name)
    else name
  }

  /** %cpu = totalNanos / (elapsed × threads). One def's slice of total compute. */
  private def stylePctCpu(formatted: String, pct: Double): String = {
    if (pct >= PctCpuRedThreshold) bold(red(formatted))
    else if (pct >= PctCpuYellowThreshold) yellow(formatted)
    else formatted
  }

  /** %wall = totalNanos / elapsed. Upper bound on wall-clock savings if removed. */
  private def stylePctWall(formatted: String, pct: Double): String = {
    if (pct >= PctWallRedThreshold) bold(red(formatted))
    else if (pct >= PctWallYellowThreshold) yellow(formatted)
    else formatted
  }

  /** Colors the active/parallelism field by occupancy: full=green bold, ≥50%=green, idle=gray, else yellow. */
  private def styleThreads(active: Int, par: Int): String = {
    val s = f"$active%2d/$par%-2d"
    if (par <= 0) color(s, Gray)
    else if (active == par) bold(green(s))
    else if (active.toDouble / par >= 0.5) green(s)
    else if (active == 0) color(s, Gray)
    else yellow(s)
  }

  /** Colors a formatted heap field by used/max ratio. */
  private def styleHeap(formatted: String, ratio: Double): String = {
    if (ratio >= HeapRedThresholdRatio) bold(red(formatted))
    else if (ratio >= HeapYellowThresholdRatio) yellow(formatted)
    else green(formatted)
  }


  // -- Numeric helpers ----------------------------------------------------

  /** Returns `(usedMb, maxMb)` from the JVM runtime. */
  private def heapUsage(): (Long, Long) = {
    val rt = Runtime.getRuntime
    val used = rt.totalMemory() - rt.freeMemory()
    val max = rt.maxMemory()
    (used / (1024L * 1024L), max / (1024L * 1024L))
  }

  /**
    * A row in the per-module aggregate table.
    *
    * @param module         dot-joined namespace (or `(root)` when empty).
    * @param totalNanos     summed wall-clock time across the module's defs.
    * @param totalCallCount summed `track` call counts across the module's defs.
    * @param totalLocLines  summed source-line counts across the module's defs.
    * @param byPhase        phase → summed nanoseconds across the module's defs.
    * @param byPhaseCount   phase → summed track-call counts across the module's defs.
    * @param totalCns       summed Typer constraint counts across the module's defs.
    */
  private final case class ModuleStats(module: String, totalNanos: Long, totalCallCount: Long, totalLocLines: Int, byPhase: Map[String, Long], byPhaseCount: Map[String, Long], totalCns: Long) {
    /** Returns the phase that consumed the most time in this module, or None if empty. */
    def dominantPhase: Option[String] =
      if (byPhase.isEmpty) None else Some(byPhase.maxBy(_._2)._1)
  }

  /** Groups `snap` by namespace, sums each metric, and returns rows sorted by `srt` descending. */
  private def aggregateByModule(snap: Vector[DefnStats], srt: Sort): Vector[ModuleStats] = {
    val groups = snap.groupBy { s =>
      val ns = s.sym.namespace
      if (ns.isEmpty) "(root)" else ns.mkString(".")
    }
    groups.iterator.map { case (mod, defs) =>
      val totalNanos = defs.iterator.map(_.totalNanos).sum
      val totalCallCount = defs.iterator.map(_.callCount.toLong).sum
      val totalLocLines = defs.iterator.map(s => locLineCount(s.loc)).sum
      val byPhase = defs.foldLeft(Map.empty[String, Long]) { (acc, d) =>
        d.byPhase.foldLeft(acc) { case (m, (phase, n)) =>
          m.updated(phase, m.getOrElse(phase, 0L) + n)
        }
      }
      val byPhaseCount = defs.foldLeft(Map.empty[String, Long]) { (acc, d) =>
        d.byPhaseCount.foldLeft(acc) { case (m, (phase, n)) =>
          m.updated(phase, m.getOrElse(phase, 0L) + n)
        }
      }
      val totalCns = defs.iterator.map(_.cns).sum
      ModuleStats(mod, totalNanos, totalCallCount, totalLocLines, byPhase, byPhaseCount, totalCns)
    }.toVector.sortBy(m => -moduleSortKey(m, srt))
  }

  /** Phases whose `track` count maps to the `mono` column — number of monomorphic instances created. */
  private val MonoCountPhases: Set[String] = Set("Monomorpher")
  /** Phases whose `track` count maps to the `opt` column — optimizer fixed-point re-visits. */
  private val OptCountPhases: Set[String] = Set("Optimizer", "LambdaDrop")
  /** Phases whose `track` count maps to the `cls` column — class files emitted post-specialization. */
  private val ClsCountPhases: Set[String] = Set("JvmBackend")

  /** Sums `byPhaseCount` entries for the given phase set. */
  private def sumPhaseCounts(byPhaseCount: Map[String, Long], phases: Set[String]): Long =
    byPhaseCount.iterator.collect { case (p, n) if phases.contains(p) => n }.sum

  /**
    * Returns the descending sort key for a def under the given sort mode.
    * `Hotness` (time / LOC) is 0 for synthetic defs with no real source
    * span (`locLines <= 0`) so they sink to the bottom rather than dominating.
    */
  private def defSortKey(s: DefnStats, srt: Sort): Double = srt match {
    case Sort.Time    => s.totalNanos.toDouble
    case Sort.Hotness =>
      val lines = locLineCount(s.loc)
      if (lines <= 0) 0.0 else s.totalNanos.toDouble / lines
    case Sort.Mono => sumPhaseCounts(s.byPhaseCount, MonoCountPhases).toDouble
    case Sort.Opt  => sumPhaseCounts(s.byPhaseCount, OptCountPhases).toDouble
    case Sort.Cls  => sumPhaseCounts(s.byPhaseCount, ClsCountPhases).toDouble
    case Sort.Cns  => s.cns.toDouble
  }

  /** Module-level analogue of [[defSortKey]], applied after summing across each module's defs. */
  private def moduleSortKey(m: ModuleStats, srt: Sort): Double = srt match {
    case Sort.Time    => m.totalNanos.toDouble
    case Sort.Hotness => if (m.totalLocLines <= 0) 0.0 else m.totalNanos.toDouble / m.totalLocLines
    case Sort.Mono => sumPhaseCounts(m.byPhaseCount, MonoCountPhases).toDouble
    case Sort.Opt  => sumPhaseCounts(m.byPhaseCount, OptCountPhases).toDouble
    case Sort.Cls  => sumPhaseCounts(m.byPhaseCount, ClsCountPhases).toDouble
    case Sort.Cns  => m.totalCns.toDouble
  }

  /** Fallback terminal height when JLine cannot determine the real one. */
  private val DefaultRows: Int = 24

  /** Fallback terminal width when JLine cannot determine the real one. */
  private val DefaultCols: Int = 100

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
  private final case class Layout(symWidth: Int, locWidth: Int, showLOC: Boolean, showCounts: Boolean, showCns: Boolean, showPhase: Boolean, totalWidth: Int)

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
  private def computeLayout(cols: Int, activeFilter: PhaseFilter): Layout = {
    // Width contribution of the "fixed" half: separator before time + tail.
    val tail = 1 + FixedTailWidth
    // Width of just the text section (sym + 1 + location) at default sizes.
    def textWidth(symW: Int, locW: Int): Int = symW + 1 + locW

    val countsAllowed = activeFilter == PhaseFilter.Backend
    val countsW = if (countsAllowed) CountsColWidth else 0
    val cnsAllowed = activeFilter == PhaseFilter.Frontend
    val cnsW = if (cnsAllowed) CnsColWidth else 0
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

  /**
    * Fixed-overhead rows: blank + 2 dashboard/stats lines + blank + 2 def-chrome +
    * 1 blank-before-modules + 2 module-chrome + 1 cursor-parking row.
    */
  private val ChromeRows: Int = 10

  /** Reserved breathing room above and below the rendered view. */
  private val RowMargin: Int = 2

  /** Floor on the total data-row budget. */
  private val MinDataRows: Int = 8

  /** Floor on the def-table row count. */
  private val MinDefN: Int = 5
  /** Floor on the module-table row count. */
  private val MinModuleN: Int = 3

  // -- Formatting helpers -------------------------------------------------

  /** Renders a [[SourceLocation]] as `file:line`, or `?` if synthetic. */
  private def formatLocation(loc: SourceLocation): String =
    if (!loc.isReal) "?" else s"${loc.source.name}:${loc.startLine}"

  /** Returns the inclusive line span of `loc`, or 0 if synthetic. */
  private def locLineCount(loc: SourceLocation): Int =
    if (!loc.isReal) 0 else (loc.endLine - loc.startLine + 1).max(0)

  /** Formats a nanosecond duration as `Nms` or `N.Ns` (≥10s). */
  private def formatMillis(nanos: Long): String = {
    val ms = nanos / 1_000_000L
    if (ms >= 10_000L) f"${ms / 1000.0}%.1fs"
    else f"${ms}ms"
  }

  /** Returns `s` truncated to `width` characters, with a trailing ellipsis when shortened. */
  private def truncate(s: String, width: Int): String =
    if (s.length <= width) s else s.take(width - 1) + "…"

  /** Left-pads `s` with spaces to width `w`. */
  private def lpad(s: String, w: Int): String =
    if (s.length >= w) s else " " * (w - s.length) + s

  /** Right-pads `s` with spaces to width `w`. */
  private def rpad(s: String, w: Int): String =
    if (s.length >= w) s else s + " " * (w - s.length)
}

/**
  * A live, top(1)-style TUI showing which `DefnSym`s the compiler has spent
  * the most wall-clock time on so far, with a per-phase breakdown, call
  * count, active-threads sparkline, threadpool occupancy, and heap usage.
  *
  * Reads from [[Flix.getProfiler]] and [[Flix.getCurrentPhaseName]] every
  * [[CompilerTop.RefreshIntervalMs]] milliseconds and re-renders the screen
  * using ANSI escape codes.
  */
final class CompilerTop(flix: Flix, profiler: CompilerProfiler) {

  import CompilerTop.*

  /** True while the renderer thread should keep looping. */
  private val running = new AtomicBoolean(false)

  /**
    * Flips to true when [[stop]] is first called. Drives the dashboard's
    * "done — press q to quit" mode and guards [[stop]] against re-entry from
    * both the error path in `Flix.check` and the success path in `Flix.compile`.
    */
  private val completed = new AtomicBoolean(false)

  /**
    * Elapsed nanos at the moment compilation finished. Captured by [[stop]]
    * before flipping [[completed]] so the dashboard can freeze the field
    * instead of advancing it during the wait-for-quit phase.
    */
  @volatile private var frozenElapsedNanos: Long = 0L

  /** Active-thread count captured at the same instant as [[frozenElapsedNanos]]. */
  @volatile private var frozenActiveThreads: Int = 0

  /** `(usedMb, maxMb)` heap snapshot captured at the same instant as [[frozenElapsedNanos]]. */
  @volatile private var frozenHeap: (Long, Long) = (0L, 0L)

  /**
    * Active filter for the def / module tables. Written by the input thread,
    * read by the renderer thread. Default is [[PhaseFilter.All]] (no filtering).
    */
  private val filter = new AtomicReference[PhaseFilter](PhaseFilter.All)

  /**
    * Active sort for the def / module tables. Written by the input thread,
    * read by the renderer thread. Default is [[Sort.Time]].
    */
  private val sort = new AtomicReference[Sort](Sort.Time)

  /**
    * Counted down by the input thread when the user presses `q` (only after
    * [[completed]] is set). [[stop]] awaits it before tearing the renderer
    * down so the user can keep toggling the filter post-compile.
    */
  private val quitLatch = new CountDownLatch(1)

  /**
    * Terminal attributes captured before entering raw mode in [[start]].
    * Restored by [[stop]] so the user's shell isn't left with line buffering
    * and echo disabled if anything goes wrong further down the teardown.
    */
  private var savedAttrs: Attributes = _

  /** The renderer thread, or `null` before [[start]] / after [[stop]]. */
  private var thread: Thread = _

  /** The input thread that reads keypresses (filter toggles + quit), or `null` before [[start]] / after [[stop]]. */
  private var inputThread: Thread = _

  /** Wall-clock start time, used as the denominator for `%wall` and `%cpu`. */
  private val startNanos: Long = System.nanoTime()

  /** A JLine terminal handle used to query screen size. May be null if JLine fails. */
  private val terminal: Terminal = {
    java.util.logging.Logger.getLogger("org.jline").setLevel(java.util.logging.Level.OFF)
    try TerminalBuilder.builder().system(true).build()
    catch { case _: Throwable => null }
  }

  /** Rolling history of active-thread counts feeding the dashboard sparkline. Touched only by the renderer thread. */
  private val threadsHistory = mutable.Queue.empty[Double]

  /** Returns the terminal height in rows, or [[DefaultRows]] if unavailable. */
  private def terminalRows(): Int = {
    if (terminal == null) return DefaultRows
    val h = terminal.getHeight
    if (h > 0) h else DefaultRows
  }

  /** Returns the terminal width in columns, or [[DefaultCols]] if unavailable. */
  private def terminalCols(): Int = {
    if (terminal == null) return DefaultCols
    val w = terminal.getWidth
    if (w > 0) w else DefaultCols
  }

  /**
    * Starts the renderer + input threads on daemon threads. Enters raw mode
    * once at startup so the input thread can read individual keystrokes
    * during compilation as well as after it finishes.
    */
  def start(): Unit = {
    if (!running.compareAndSet(false, true)) return

    // Enter raw mode for the lifetime of the TUI so keypresses (f/b/a/q)
    // are delivered immediately rather than line-buffered.
    if (terminal != null) {
      try savedAttrs = terminal.enterRawMode()
      catch { case _: Throwable => savedAttrs = null }
    }

    thread = new Thread(() => loop(), "flix-top-renderer")
    thread.setDaemon(true)
    thread.start()

    inputThread = new Thread(() => inputLoop(), "flix-top-input")
    inputThread.setDaemon(true)
    inputThread.start()
  }

  /**
    * Input-thread body: reads keypresses with a [[InputPollMs]] timeout so
    * shutdown is responsive to `running.get() == false`.
    *
    *   - `f` / `F` → filter to frontend phases
    *   - `b` / `B` → filter to backend phases
    *   - `a` / `A` → reset to all phases
    *   - `q` / `Q` / Ctrl-C / EOF → if compilation has completed, signal
    *     [[stop]] to finish teardown; otherwise ignored (pressing `q`
    *     mid-compile must not abort the build).
    *   - Any other key is ignored.
    */
  private def inputLoop(): Unit = {
    if (terminal == null) return
    val r = terminal.reader()
    while (running.get()) {
      val c =
        try r.read(InputPollMs)
        catch { case _: InterruptedException => return; case _: Throwable => -1 }
      c match {
        case -2 => // timeout — loop and re-check `running`
        case -1 | 3 | 'q' | 'Q' =>
          if (completed.get()) {
            quitLatch.countDown()
            return
          }
        case 'f' | 'F' => filter.set(PhaseFilter.Frontend)
        case 'b' | 'B' => filter.set(PhaseFilter.Backend)
        case 'a' | 'A' => filter.set(PhaseFilter.All)
        case 't' | 'T' => sort.set(Sort.Time)
        case 'h' | 'H' => sort.set(Sort.Hotness)
        case 'm' | 'M' => sort.set(Sort.Mono)
        case 'o' | 'O' => sort.set(Sort.Opt)
        case 'c' | 'C' => sort.set(Sort.Cls)
        case 'n' | 'N' => sort.set(Sort.Cns)
        case _         => // ignored
      }
    }
  }

  /**
    * Marks compilation done, blocks until the user presses `q` (or hits EOF /
    * Ctrl-C), then tears the renderer down and clears the screen so whatever
    * Main prints next (errors, runtime output) starts on a clean slate.
    *
    * Idempotent: the second caller — both error and success paths in `Flix`
    * may fire — returns immediately rather than blocking again.
    */
  def stop(): Unit = {
    // Snapshot live values BEFORE flipping `completed`. The renderer reads
    // `completed` first and uses the snapshot when it sees true; the volatile
    // writes here happen-before the AtomicBoolean store via JMM.
    frozenElapsedNanos = System.nanoTime() - startNanos
    frozenActiveThreads = if (flix.threadPool == null) 0 else flix.threadPool.getActiveThreadCount
    frozenHeap = heapUsage()

    if (!completed.compareAndSet(false, true)) return

    // Block until the input thread observes `q`. The renderer keeps drawing
    // the "press q to quit" hint and the user can keep toggling f/b/a in
    // the meantime.
    if (terminal != null) {
      try quitLatch.await()
      catch { case _: InterruptedException => () }
    }

    if (!running.compareAndSet(true, false)) return
    if (thread != null) thread.join()
    if (inputThread != null) inputThread.join()
    if (terminal != null && savedAttrs != null) {
      try terminal.setAttributes(savedAttrs) catch { case _: Throwable => () }
    }
    System.out.print(ClearScreen)
    System.out.flush()
    if (terminal != null) try terminal.close() catch { case _: Throwable => () }
  }

  /** Renderer thread body: render, sleep, repeat until [[running]] is false. */
  private def loop(): Unit = {
    while (running.get()) {
      render()
      try Thread.sleep(RefreshIntervalMs)
      catch { case _: InterruptedException => return }
    }
  }

  /** Builds and prints one full frame of the TUI. */
  private def render(): Unit = {
    val isDone = completed.get()
    val now = System.nanoTime()
    val pool = flix.threadPool
    val parallelism = if (pool == null) flix.options.threads.max(1) else pool.getParallelism.max(1)
    // Once compilation is done, freeze elapsed + active threads at their
    // snapshot values so the dashboard reflects the state at completion
    // rather than ticking forward during the wait-for-quit phase.
    val elapsed = if (isDone) frozenElapsedNanos else now - startNanos
    val activeThreads = if (isDone) frozenActiveThreads else (if (pool == null) 0 else pool.getActiveThreadCount)

    // Budget rows for the two tables based on the current terminal height,
    // reserving an extra `RowMargin` rows so the view never quite touches
    // the top or bottom edge of the terminal.
    val dataRows = (terminalRows() - ChromeRows - RowMargin).max(MinDataRows)
    val moduleN = (dataRows / 3).max(MinModuleN)
    val defN = (dataRows - moduleN).max(MinDefN)

    val activeFilter = filter.get()
    val activeSort = sort.get()

    // Compute the column layout based on the current terminal width,
    // reserving 2 columns for the 1-space left and right table padding.
    // The filter is part of the input because the mono / opt / cls columns
    // only render under the backend view.
    val layout = computeLayout((terminalCols() - 2).max(1), activeFilter)

    val snap = applyFilter(profiler.snapshot(), activeFilter).sortBy(s => -defSortKey(s, activeSort))
    val visible = snap.take(defN)

    // Active-threads sparkline: history of thread-pool occupancy. Stops
    // updating once compilation is done so the bar reflects the work, not
    // a long tail of zero-occupancy samples while waiting for `q`.
    if (!isDone) {
      threadsHistory.enqueue(activeThreads.toDouble)
      while (threadsHistory.size > SparkWidth) threadsHistory.dequeue()
    }

    val modules = aggregateByModule(snap, activeSort).take(moduleN)

    val sb = new StringBuilder
    sb.append(BeginSync)
    sb.append(ClearScreen)
    sb.append('\n')

    renderDashboard(sb, activeThreads, parallelism, activeFilter)
    renderStats(sb, elapsed)
    sb.append('\n')
    renderTableHeader(sb, layout, activeSort)
    renderRows(sb, visible, elapsed, parallelism, layout)
    renderModuleTable(sb, modules, elapsed, parallelism, layout, activeSort)

    sb.append(EndSync)
    System.out.print(sb)
    System.out.flush()

  }

  /**
    * Top dashboard line: current phase + progress bar + active-threads bar.
    * Both bars sit beside each other so the eye picks them up as a pair.
    */
  private def renderDashboard(sb: StringBuilder, activeThreads: Int, parallelism: Int, activeFilter: PhaseFilter): Unit = {
    val isDone = completed.get()
    val phase = if (isDone) "done" else flix.getCurrentPhaseName.getOrElse("starting")
    // Once compilation has finished, force the bar to 100% rather than relying
    // on `phaseTimers.size + 1` (which is +1 ahead of reality during execution
    // and would still under-fill if any phase is uninstrumented).
    val done = if (isDone) TotalPhases else (flix.phaseTimers.size + 1).min(TotalPhases)
    val filled = (BarWidth.toLong * done / TotalPhases).toInt

    sb.append("  ")
    sb.append(bold(rpad(phase, MaxPhaseLen)))
    sb.append(dim("  progress "))
    sb.append(dim("["))
    // Blue while compilation is in progress; green once finished so the bar
    // doubles as a "done" indicator.
    val filledBar = "█" * filled
    sb.append(if (isDone) green(filledBar) else blue(filledBar))
    sb.append(dim("░" * (BarWidth - filled)))
    sb.append(dim("] "))
    sb.append(f"$done%2d/$TotalPhases%2d")
    sb.append(dim("   threads "))
    // With parallelism=1 ForkJoin runs work inline on the submitter, leaving
    // `getActiveThreadCount` at 0 most of the time — sparkline goes flat,
    // field reads `0/1` continuously. Show a label instead.
    if (parallelism > 1) {
      sb.append(dim(cyan(renderSparkline(parallelism.toDouble))))
      sb.append(' ')
      sb.append(styleThreads(activeThreads, parallelism))
    } else {
      sb.append(dim("single-threaded"))
    }
    sb.append("   ")
    sb.append(renderFilterLegend(activeFilter))
    if (isDone) sb.append(dim("   press q to quit"))
    sb.append('\n')
  }

  /**
    * Renders the `[f|b|a]` legend with the active letter highlighted in
    * bold cyan when the filter is non-default. Always visible so the user
    * sees the available toggles whether the filter is active or not.
    */
  private def renderFilterLegend(active: PhaseFilter): String = {
    val sb = new StringBuilder
    sb.append(dim("["))
    sb.append(if (active == PhaseFilter.All) bold(cyan("a")) else dim("a"))
    sb.append(dim("|"))
    sb.append(if (active == PhaseFilter.Frontend) bold(cyan("f")) else dim("f"))
    sb.append(dim("|"))
    sb.append(if (active == PhaseFilter.Backend) bold(cyan("b")) else dim("b"))
    sb.append(dim("]"))
    sb.toString
  }

  /**
    * Projects each [[DefnStats]] through the active filter: keep only the
    * `byPhase` and `byPhaseCount` entries whose phase matches, and recompute
    * `totalNanos` and `callCount` as the sum over the kept phases. Module
    * aggregation re-rolls from these filtered defs, so module rows track
    * the filter automatically.
    *
    * `loc` isn't per-phase and passes through unchanged.
    */
  private def applyFilter(snap: Vector[DefnStats], f: PhaseFilter): Vector[DefnStats] = {
    if (f == PhaseFilter.All) return snap
    snap.map { s =>
      val keptPhases = s.byPhase.filter { case (p, _) => matchesFilter(p, f) }
      val keptCounts = s.byPhaseCount.filter { case (p, _) => matchesFilter(p, f) }
      val keptTotal = keptPhases.values.sum
      // Sum of per-phase counts cannot overflow Int in any realistic build.
      val keptCount = keptCounts.values.sum.toInt
      s.copy(totalNanos = keptTotal, callCount = keptCount, byPhase = keptPhases, byPhaseCount = keptCounts)
    }
  }

  /**
    * Stats line: elapsed (right-aligned under `progress`) and heap
    * (right-aligned under `threads`).
    */
  private def renderStats(sb: StringBuilder, elapsed: Long): Unit = {
    val (heapUsedMb, heapMaxMb) = if (completed.get()) frozenHeap else heapUsage()
    val heapUsedField = f"$heapUsedMb%4d MB"
    val heapMaxField = f"$heapMaxMb%4d MB"
    val heapRatio = if (heapMaxMb <= 0) 0.0 else heapUsedMb.toDouble / heapMaxMb
    val elapsedField = formatMillis(elapsed)

    // "elapsed" (7 chars) right-aligned with "progress" (8 chars) → start at ProgressStartCol + 1.
    val elapsedStartCol = ProgressStartCol + 1
    // "heap" (4 chars) right-aligned with "threads" (7 chars) → start at ThreadsStartCol + 3.
    val heapStartCol = ThreadsStartCol + 3

    sb.append("  ")
    sb.append(" " * (elapsedStartCol - 3).max(0))
    sb.append(dim("elapsed "))
    sb.append(elapsedField)

    val colAfterElapsed = elapsedStartCol + "elapsed ".length + elapsedField.length
    sb.append(" " * (heapStartCol - colAfterElapsed).max(1))

    sb.append(dim("heap "))
    sb.append(styleHeap(heapUsedField, heapRatio))
    sb.append(dim(" / "))
    sb.append(heapMaxField)
    sb.append('\n')
  }

  /** Renders the def-table column header and the divider underneath it. */
  private def renderTableHeader(sb: StringBuilder, layout: Layout, activeSort: Sort): Unit = {
    sb.append(' ')
    sb.append(buildHeader(layout, activeSort))
    sb.append(' ')
    sb.append('\n')
    sb.append(' ')
    sb.append(dim("─" * layout.totalWidth))
    sb.append(' ')
    sb.append('\n')
  }

  /**
    * Builds the def-table header. Each sortable column underlines its
    * keystroke letter (`m̲ono`, `o̲pt`, `c̲ls`, `t̲ime`) and the Def header
    * carries a tiny `(h̲ot)` annotation marking the hotness sort key.
    * The active column is rendered bold yellow; the rest bold cyan.
    */
  private def buildHeader(layout: Layout, activeSort: Sort): String = {
    val sb = new StringBuilder

    // First column: "Def (hot)" — the 'h' (index 5) is the hotness keystroke.
    val defPadded = rpad("Def (hot)", layout.symWidth)
    sb.append(keyHeader(defPadded, 5, activeSort == Sort.Hotness))

    sb.append(' ')
    sb.append(plainHeader(rpad("location", layout.locWidth)))

    if (layout.showLOC) {
      sb.append(' '); sb.append(plainHeader(lpad("LOC", 4)))
    }
    if (layout.showCounts) {
      val monoP = lpad("mono", 4); sb.append(' '); sb.append(keyHeader(monoP, monoP.indexOf('m'), activeSort == Sort.Mono))
      val optP  = lpad("opt", 4);  sb.append(' '); sb.append(keyHeader(optP,  optP.indexOf('o'),  activeSort == Sort.Opt))
      val clsP  = lpad("cls", 4);  sb.append(' '); sb.append(keyHeader(clsP,  clsP.indexOf('c'),  activeSort == Sort.Cls))
    }
    if (layout.showCns) {
      val cnsP = lpad("cns", 5)
      sb.append(' '); sb.append(keyHeader(cnsP, cnsP.indexOf('n'), activeSort == Sort.Cns))
    }
    if (layout.showPhase) {
      sb.append(' '); sb.append(plainHeader(rpad("phase", 10)))
    }
    val timeP = lpad("time", 9); sb.append(' '); sb.append(keyHeader(timeP, timeP.indexOf('t'), activeSort == Sort.Time))
    sb.append(' '); sb.append(plainHeader(lpad("%cpu", 6)))
    sb.append(' '); sb.append(plainHeader(lpad("%wall", 6)))
    sb.toString
  }

  /** Renders the def-table body (one row per [[DefnStats]]), or a centered placeholder if `visible` is empty. */
  private def renderRows(sb: StringBuilder, visible: Vector[DefnStats], elapsed: Long, parallelism: Int, layout: Layout): Unit = {
    if (visible.isEmpty) {
      val msg = "(no timings yet)"
      val pad = (((layout.totalWidth + 2) - msg.length) / 2).max(0)
      sb.append('\n')
      sb.append(" " * pad)
      sb.append(dim(msg))
      sb.append('\n')
      return
    }
    val safeElapsed = elapsed.max(1L).toDouble
    for (s <- visible) {
      val pctWall = 100.0 * s.totalNanos / safeElapsed
      val pctCpu = pctWall / parallelism
      val locStr = formatLocation(s.loc)
      val locLines = locLineCount(s.loc)
      val phase = s.dominantPhase.getOrElse("?")

      val nameMax = layout.symWidth - 1
      val nameText = truncate(s.sym.name, nameMax)
      val locField = rpad(truncate(locStr, layout.locWidth), layout.locWidth)
      val marker = if (s.isActive) yellow("*") else " "

      sb.append(' ')
      sb.append(styleSym(nameText, s.totalNanos, locLines))
      sb.append(marker)
      sb.append(" " * (nameMax - nameText.length))
      sb.append(' ')
      sb.append(dim(locField))
      appendNumericFields(sb, locLines, s.byPhaseCount, s.cns, phase, s.totalNanos, pctCpu, pctWall, layout, aggregate = false)
      sb.append(' ')
      sb.append('\n')
    }
  }

  /** Renders the per-module aggregate table below the def table; no-op if `modules` is empty. */
  private def renderModuleTable(sb: StringBuilder, modules: Vector[ModuleStats], elapsed: Long, parallelism: Int, layout: Layout, activeSort: Sort): Unit = {
    if (modules.isEmpty) return

    sb.append('\n')
    val modWidth = layout.symWidth + 1 + layout.locWidth
    sb.append(' ')
    sb.append(buildModuleHeader(layout, activeSort))
    sb.append(' ')
    sb.append('\n')
    sb.append(' ')
    sb.append(dim("─" * layout.totalWidth))
    sb.append(' ')
    sb.append('\n')

    val safeElapsed = elapsed.max(1L).toDouble
    for (m <- modules) {
      val pctWall = 100.0 * m.totalNanos / safeElapsed
      val pctCpu = pctWall / parallelism
      val phase = m.dominantPhase.getOrElse("?")

      val modField = rpad(truncate(m.module, modWidth), modWidth)

      sb.append(' ')
      sb.append(modField)
      appendNumericFields(sb, m.totalLocLines, m.byPhaseCount, m.totalCns, phase, m.totalNanos, pctCpu, pctWall, layout, aggregate = true)
      sb.append(' ')
      sb.append('\n')
    }
  }

  /**
    * Module-table header: single wide first column, then the same numeric
    * columns as the def table. Mirrors [[buildHeader]]'s underline / active
    * styling so the same sort keys are discoverable in both tables.
    */
  private def buildModuleHeader(layout: Layout, activeSort: Sort): String = {
    val sb = new StringBuilder
    val modWidth = layout.symWidth + 1 + layout.locWidth

    // First column: "Module (hot)" — the 'h' (index 8) is the hotness keystroke.
    val modPadded = rpad("Module (hot)", modWidth)
    sb.append(keyHeader(modPadded, 8, activeSort == Sort.Hotness))

    if (layout.showLOC) {
      sb.append(' '); sb.append(plainHeader(lpad("LOC", 4)))
    }
    if (layout.showCounts) {
      val monoP = lpad("mono", 4); sb.append(' '); sb.append(keyHeader(monoP, monoP.indexOf('m'), activeSort == Sort.Mono))
      val optP  = lpad("opt", 4);  sb.append(' '); sb.append(keyHeader(optP,  optP.indexOf('o'),  activeSort == Sort.Opt))
      val clsP  = lpad("cls", 4);  sb.append(' '); sb.append(keyHeader(clsP,  clsP.indexOf('c'),  activeSort == Sort.Cls))
    }
    if (layout.showCns) {
      val cnsP = lpad("cns", 5)
      sb.append(' '); sb.append(keyHeader(cnsP, cnsP.indexOf('n'), activeSort == Sort.Cns))
    }
    if (layout.showPhase) {
      sb.append(' '); sb.append(plainHeader(rpad("phase", 10)))
    }
    val timeP = lpad("time", 9); sb.append(' '); sb.append(keyHeader(timeP, timeP.indexOf('t'), activeSort == Sort.Time))
    sb.append(' '); sb.append(plainHeader(lpad("%cpu", 6)))
    sb.append(' '); sb.append(plainHeader(lpad("%wall", 6)))
    sb.toString
  }

  /**
    * Appends the trailing numeric columns (LOC, mono / opt / cls, cns,
    * phase, time, %cpu, %wall) to a row, honoring the layout's visibility
    * flags. Always emits a leading separator before each column it writes.
    *
    * `aggregate` skips all warning colors on `time` / `%cpu` / `%wall`. The
    * `style*` thresholds are calibrated for individual defs; at module
    * scale, sums-across-defs trip the thresholds unconditionally and the
    * colors become noise. Counts (mono / opt / cls / cns) render plain in
    * both tables.
    */
  private def appendNumericFields(sb: StringBuilder, locLines: Int, byPhaseCount: Map[String, Long], cns: Long, phase: String,
                                   nanos: Long, pctCpu: Double, pctWall: Double, layout: Layout,
                                   aggregate: Boolean): Unit = {
    if (layout.showLOC) {
      sb.append(' ')
      val locStr = if (locLines > 0) locLines.toString else "-"
      sb.append(lpad(locStr, 4))
    }
    if (layout.showCounts) {
      val mono = sumPhaseCounts(byPhaseCount, MonoCountPhases)
      val opt  = sumPhaseCounts(byPhaseCount, OptCountPhases)
      val cls  = sumPhaseCounts(byPhaseCount, ClsCountPhases)
      sb.append(' '); sb.append(lpad(mono.toString, 4))
      sb.append(' '); sb.append(lpad(opt.toString, 4))
      sb.append(' '); sb.append(lpad(cls.toString, 4))
    }
    if (layout.showCns) {
      sb.append(' '); sb.append(lpad(cns.toString, 5))
    }
    if (layout.showPhase) {
      sb.append(' ')
      sb.append(rpad(truncate(phase, 10), 10))
    }
    val timeField = lpad(formatMillis(nanos), 9)
    val cpuField  = f"$pctCpu%5.1f%%"
    val wallField = f"$pctWall%5.1f%%"
    sb.append(' '); sb.append(if (aggregate) timeField else styleTime(timeField, nanos))
    sb.append(' '); sb.append(if (aggregate) cpuField else stylePctCpu(cpuField, pctCpu))
    sb.append(' '); sb.append(if (aggregate) wallField else stylePctWall(wallField, pctWall))
  }

  /**
    * Renders the threads-history sparkline at a fixed scale where
    * `█` ⇔ `maxValue` (typically the threadpool's parallelism). Earlier
    * samples appear on the left, the most recent on the right.
    */
  private def renderSparkline(maxValue: Double): String = {
    if (threadsHistory.isEmpty) return " " * SparkWidth
    val safeMax = maxValue.max(1.0)
    val chars = threadsHistory.iterator.map { v =>
      val i = math.min(SparkChars.length - 1, math.max(0, (v / safeMax * (SparkChars.length - 1)).round.toInt))
      SparkChars(i)
    }.toArray
    val pad = (SparkWidth - chars.length).max(0)
    " " * pad + new String(chars)
  }
}
