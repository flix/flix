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
import org.jline.terminal.{Terminal, TerminalBuilder}

import java.util.concurrent.atomic.AtomicBoolean
import scala.collection.mutable

object CompilerTop {

  /** How often the screen refreshes, in milliseconds. */
  private val RefreshIntervalMs: Long = 100L

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
  /** ANSI foreground red. */
  private val Red: String = s"$ESC[31m"
  /** ANSI foreground green. */
  private val Green: String = s"$ESC[32m"
  /** ANSI foreground yellow. */
  private val Yellow: String = s"$ESC[33m"
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
  /** Wraps `s` in ANSI cyan codes. */
  private def cyan(s: String): String = color(s, Cyan)

  // -- Conditional styling -------------------------------------------------

  /** Colors a formatted time field by absolute magnitude (≥1s red bold, ≥200ms yellow bold, ≥50ms yellow). */
  private def styleTime(formatted: String, nanos: Long): String = {
    val ms = nanos / 1_000_000L
    if (ms >= 1000) bold(red(formatted))
    else if (ms >= 200) bold(yellow(formatted))
    else if (ms >= 50) yellow(formatted)
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
    if (msPerLine >= 25.0) bold(red(name))
    else if (msPerLine >= 15.0) yellow(name)
    else name
  }

  /** %cpu = totalNanos / (elapsed × threads). One def's slice of total compute. */
  private def stylePctCpu(formatted: String, pct: Double): String = {
    if (pct >= 5.0) bold(red(formatted))
    else if (pct >= 1.0) yellow(formatted)
    else formatted
  }

  /** %wall = totalNanos / elapsed. Upper bound on wall-clock savings if removed. */
  private def stylePctWall(formatted: String, pct: Double): String = {
    if (pct >= 15.0) bold(red(formatted))
    else if (pct >= 5.0) yellow(formatted)
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

  /** Colors a formatted heap field by used/max ratio: ≥90% red bold, ≥70% yellow, else green. */
  private def styleHeap(formatted: String, ratio: Double): String = {
    if (ratio >= 0.9) bold(red(formatted))
    else if (ratio >= 0.7) yellow(formatted)
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
    */
  private final case class ModuleStats(module: String, totalNanos: Long, totalCallCount: Long, totalLocLines: Int, byPhase: Map[String, Long]) {
    /** Returns the phase that consumed the most time in this module, or None if empty. */
    def dominantPhase: Option[String] =
      if (byPhase.isEmpty) None else Some(byPhase.maxBy(_._2)._1)
  }

  /** Groups `snap` by namespace, sums each metric, and returns rows sorted by `totalNanos` descending. */
  private def aggregateByModule(snap: Vector[DefnStats]): Vector[ModuleStats] = {
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
      ModuleStats(mod, totalNanos, totalCallCount, totalLocLines, byPhase)
    }.toVector.sortBy(-_.totalNanos)
  }

  /** Fallback terminal height when JLine cannot determine the real one. */
  private val DefaultRows: Int = 24

  /** Fallback terminal width when JLine cannot determine the real one. */
  private val DefaultCols: Int = 100

  /**
    * Column layout for the per-frame table render. Computed from the current
    * terminal width: when the terminal is narrow, the optional LOC / n /
    * phase columns are dropped in that order (lowest-signal first). When the
    * terminal is wide, the surplus is distributed between the DefnSym and
    * location columns proportionally to their default widths.
    *
    * @param symWidth   width of the DefnSym column.
    * @param locWidth   width of the location column.
    * @param showLOC    whether to render the LOC column.
    * @param showN      whether to render the call-count (n) column.
    * @param showPhase  whether to render the dominant-phase column.
    * @param totalWidth total rendered width of the row, less leading/trailing pad.
    */
  private final case class Layout(symWidth: Int, locWidth: Int, showLOC: Boolean, showN: Boolean, showPhase: Boolean, totalWidth: Int)

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
  /** Width contribution of the optional call-count column (separator + width). */
  private val NColWidth: Int = 1 + 4
  /** Width contribution of the optional dominant-phase column (separator + width). */
  private val PhaseColWidth: Int = 1 + 10

  /** Picks a [[Layout]] for the given terminal width by trying tiers in descending feature order. */
  private def computeLayout(cols: Int): Layout = {
    // Width contribution of the "fixed" half: separator before time + tail.
    val tail = 1 + FixedTailWidth
    // Width of just the text section (sym + 1 + location) at default sizes.
    def textWidth(symW: Int, locW: Int): Int = symW + 1 + locW

    // Try tiers in descending feature order.
    val full = textWidth(DefaultSymWidth, DefaultLocWidth) + LocColWidth + NColWidth + PhaseColWidth + tail
    if (cols >= full) {
      // Surplus → expand sym (~46%) and location (~54%) proportionally.
      val extra = cols - full
      val extraSym = extra * 46 / 100
      val extraLoc = extra - extraSym
      val symW = DefaultSymWidth + extraSym
      val locW = DefaultLocWidth + extraLoc
      return Layout(symW, locW, showLOC = true, showN = true, showPhase = true,
        totalWidth = textWidth(symW, locW) + LocColWidth + NColWidth + PhaseColWidth + tail)
    }

    // Tier: drop phase.
    val noPhase = textWidth(DefaultSymWidth, DefaultLocWidth) + LocColWidth + NColWidth + tail
    if (cols >= noPhase)
      return Layout(DefaultSymWidth, DefaultLocWidth, showLOC = true, showN = true, showPhase = false, noPhase)

    // Tier: drop phase + n.
    val noPhaseNoN = textWidth(DefaultSymWidth, DefaultLocWidth) + LocColWidth + tail
    if (cols >= noPhaseNoN)
      return Layout(DefaultSymWidth, DefaultLocWidth, showLOC = true, showN = false, showPhase = false, noPhaseNoN)

    // Tier: drop phase + n + LOC.
    val noOptional = textWidth(DefaultSymWidth, DefaultLocWidth) + tail
    if (cols >= noOptional)
      return Layout(DefaultSymWidth, DefaultLocWidth, showLOC = false, showN = false, showPhase = false, noOptional)

    // Even minimum tier doesn't fit; shrink sym/locWidth to floors.
    val available = (cols - tail).max(MinSymWidth + 1 + MinLocWidth)
    val symW = MinSymWidth.max(available * 46 / 100)
    val locW = MinLocWidth.max(available - 1 - symW)
    Layout(symW, locW, showLOC = false, showN = false, showPhase = false,
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

  /** The renderer thread, or `null` before [[start]] / after [[stop]]. */
  private var thread: Thread = _

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

  /** Starts the renderer on a daemon thread. */
  def start(): Unit = {
    if (!running.compareAndSet(false, true)) return
    thread = new Thread(() => loop(), "flix-top-renderer")
    thread.setDaemon(true)
    thread.start()
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

    // Block until the user dismisses the TUI. The renderer keeps drawing the
    // "press q to quit" hint while we wait.
    if (terminal != null) {
      val attrs = terminal.enterRawMode()
      try {
        val r = terminal.reader()
        var quit = false
        while (!quit) {
          val c = r.read()
          if (c == -1 || c == 'q' || c == 'Q' || c == 3) quit = true
        }
      } finally {
        try terminal.setAttributes(attrs) catch { case _: Throwable => () }
      }
    }

    if (!running.compareAndSet(true, false)) return
    if (thread != null) thread.join()
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

    // Compute the column layout based on the current terminal width,
    // reserving 2 columns for the 1-space left and right table padding.
    val layout = computeLayout((terminalCols() - 2).max(1))

    val snap = profiler.snapshot().sortBy(-_.totalNanos)
    val visible = snap.take(defN)

    // Active-threads sparkline: history of thread-pool occupancy. Stops
    // updating once compilation is done so the bar reflects the work, not
    // a long tail of zero-occupancy samples while waiting for `q`.
    if (!isDone) {
      threadsHistory.enqueue(activeThreads.toDouble)
      while (threadsHistory.size > SparkWidth) threadsHistory.dequeue()
    }

    val modules = aggregateByModule(snap).take(moduleN)

    val sb = new StringBuilder
    sb.append(BeginSync)
    sb.append(ClearScreen)
    sb.append('\n')

    renderDashboard(sb, activeThreads, parallelism)
    renderStats(sb, elapsed)
    sb.append('\n')
    renderTableHeader(sb, layout)
    renderRows(sb, visible, elapsed, parallelism, layout)
    renderModuleTable(sb, modules, elapsed, parallelism, layout)

    sb.append(EndSync)
    System.out.print(sb)
    System.out.flush()

  }

  /**
    * Top dashboard line: current phase + progress bar + active-threads bar.
    * Both bars sit beside each other so the eye picks them up as a pair.
    */
  private def renderDashboard(sb: StringBuilder, activeThreads: Int, parallelism: Int): Unit = {
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
    sb.append(green("█" * filled))
    sb.append(dim("░" * (BarWidth - filled)))
    sb.append(dim("] "))
    sb.append(f"$done%2d/$TotalPhases%2d")
    sb.append(dim("   threads "))
    sb.append(dim(cyan(renderSparkline(parallelism.toDouble))))
    sb.append(' ')
    sb.append(styleThreads(activeThreads, parallelism))
    if (isDone) sb.append(dim("   press q to quit"))
    sb.append('\n')
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
  private def renderTableHeader(sb: StringBuilder, layout: Layout): Unit = {
    val header = buildHeader(rpad("Def", layout.symWidth), rpad("location", layout.locWidth), layout)
    sb.append(' ')
    sb.append(bold(cyan(header)))
    sb.append(' ')
    sb.append('\n')
    sb.append(' ')
    sb.append(dim("─" * header.length))
    sb.append(' ')
    sb.append('\n')
  }

  /**
    * Builds a table header (or any row's left side) given the formatted text
    * for the first two columns. Numeric columns are conditionally appended
    * per the layout's visibility flags.
    */
  private def buildHeader(firstCol: String, secondCol: String, layout: Layout): String = {
    val sb = new StringBuilder
    sb.append(firstCol)
    sb.append(' ')
    sb.append(secondCol)
    if (layout.showLOC) { sb.append(' '); sb.append(lpad("LOC", 4)) }
    if (layout.showN) { sb.append(' '); sb.append(lpad("n", 4)) }
    if (layout.showPhase) { sb.append(' '); sb.append(rpad("phase", 10)) }
    sb.append(' '); sb.append(lpad("time", 9))
    sb.append(' '); sb.append(lpad("%cpu", 6))
    sb.append(' '); sb.append(lpad("%wall", 6))
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
      appendNumericFields(sb, locLines, s.callCount.toLong, phase, s.totalNanos, pctCpu, pctWall, layout)
      sb.append(' ')
      sb.append('\n')
    }
  }

  /** Renders the per-module aggregate table below the def table; no-op if `modules` is empty. */
  private def renderModuleTable(sb: StringBuilder, modules: Vector[ModuleStats], elapsed: Long, parallelism: Int, layout: Layout): Unit = {
    if (modules.isEmpty) return

    sb.append('\n')
    val modWidth = layout.symWidth + 1 + layout.locWidth
    val header = buildModuleHeader(rpad("Module", modWidth), layout)
    sb.append(' ')
    sb.append(bold(cyan(header)))
    sb.append(' ')
    sb.append('\n')
    sb.append(' ')
    sb.append(dim("─" * header.length))
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
      appendNumericFields(sb, m.totalLocLines, m.totalCallCount, phase, m.totalNanos, pctCpu, pctWall, layout)
      sb.append(' ')
      sb.append('\n')
    }
  }

  /** Module-table header: single wide first column, then the same numeric columns as the def table. */
  private def buildModuleHeader(firstCol: String, layout: Layout): String = {
    val sb = new StringBuilder
    sb.append(firstCol)
    if (layout.showLOC) { sb.append(' '); sb.append(lpad("LOC", 4)) }
    if (layout.showN) { sb.append(' '); sb.append(lpad("n", 4)) }
    if (layout.showPhase) { sb.append(' '); sb.append(rpad("phase", 10)) }
    sb.append(' '); sb.append(lpad("time", 9))
    sb.append(' '); sb.append(lpad("%cpu", 6))
    sb.append(' '); sb.append(lpad("%wall", 6))
    sb.toString
  }

  /**
    * Appends the trailing numeric columns (LOC, n, phase, time, %cpu, %wall)
    * to a row, honoring the layout's visibility flags. Always emits a leading
    * separator before each column it writes.
    */
  private def appendNumericFields(sb: StringBuilder, locLines: Int, callCount: Long, phase: String,
                                   nanos: Long, pctCpu: Double, pctWall: Double, layout: Layout): Unit = {
    if (layout.showLOC) {
      sb.append(' ')
      sb.append(lpad(if (locLines > 0) locLines.toString else "-", 4))
    }
    if (layout.showN) {
      sb.append(' ')
      sb.append(lpad(callCount.toString, 4))
    }
    if (layout.showPhase) {
      sb.append(' ')
      sb.append(rpad(truncate(phase, 10), 10))
    }
    sb.append(' '); sb.append(styleTime(lpad(formatMillis(nanos), 9), nanos))
    sb.append(' '); sb.append(stylePctCpu(f"$pctCpu%5.1f%%", pctCpu))
    sb.append(' '); sb.append(stylePctWall(f"$pctWall%5.1f%%", pctWall))
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
