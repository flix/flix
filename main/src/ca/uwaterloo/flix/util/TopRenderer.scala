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
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol}
import org.jline.terminal.{Terminal, TerminalBuilder}

import java.lang.management.ManagementFactory
import java.util.concurrent.atomic.AtomicBoolean
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/**
  * A live, top(1)-style TUI showing which `DefnSym`s the compiler has spent
  * the most wall-clock time on so far, with per-phase breakdown, call count,
  * top-mover deltas, throughput sparkline, threadpool occupancy, heap usage,
  * GC time, and outlier highlighting.
  *
  * Reads from [[Flix.defnTimer]] and [[Flix.currentPhaseName]] every
  * [[TopRenderer.RefreshIntervalMs]] milliseconds and re-renders the screen
  * using ANSI escape codes.
  */
final class TopRenderer(flix: Flix) {

  import TopRenderer.*

  private val running = new AtomicBoolean(false)
  private var thread: Thread = _
  private val startNanos: Long = System.nanoTime()

  /** A JLine terminal handle used to query screen size. May be null if JLine fails. */
  private val terminal: Terminal = {
    java.util.logging.Logger.getLogger("org.jline").setLevel(java.util.logging.Level.OFF)
    try TerminalBuilder.builder().system(true).build()
    catch { case _: Throwable => null }
  }

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

  // State carried across frames so we can compute the throughput sparkline.
  // Touched only by the renderer thread.
  private var prevFrameNanos: Long = startNanos
  private var prevTotalCalls: Long = 0L
  private val throughputHistory = mutable.Queue.empty[Double]

  /** Starts the renderer on a daemon thread. */
  def start(): Unit = {
    if (!running.compareAndSet(false, true)) return
    thread = new Thread(() => loop(), "flix-top-renderer")
    thread.setDaemon(true)
    thread.start()
  }

  /** Stops the renderer, prints one final frame, and joins the render thread. */
  def stop(): Unit = {
    if (!running.compareAndSet(true, false)) return
    if (thread != null) thread.join()
    render()
    System.out.println()
    if (terminal != null) try terminal.close() catch { case _: Throwable => () }
  }

  private def loop(): Unit = {
    while (running.get()) {
      render()
      try Thread.sleep(RefreshIntervalMs)
      catch { case _: InterruptedException => return }
    }
  }

  private def render(): Unit = {
    val now = System.nanoTime()
    val elapsed = now - startNanos
    val frameDt = (now - prevFrameNanos).max(1L)
    val pool = flix.threadPool
    val parallelism = if (pool == null) flix.options.threads.max(1) else pool.getParallelism.max(1)

    // Budget rows for the two tables based on the current terminal height,
    // reserving an extra `RowMargin` rows so the view never quite touches
    // the top or bottom edge of the terminal.
    val dataRows = (terminalRows() - ChromeRows - RowMargin).max(MinDataRows)
    val moduleN = (dataRows / 3).max(MinModuleN).min(MaxModuleN)
    val defN = (dataRows - moduleN).max(MinDefN).min(MaxDefN)

    // Compute the column layout based on the current terminal width,
    // reserving 2 columns for the 1-space left and right table padding.
    val layout = computeLayout((terminalCols() - 2).max(1))

    val snap = flix.defnTimer.snapshot().sortBy(-_.totalNanos)
    val visible = snap.take(defN)

    // Throughput sparkline: visits-per-second since last frame.
    val totalCalls = flix.defnTimer.totalCallCount
    val callsThisFrame = (totalCalls - prevTotalCalls).max(0L)
    val throughputPerSec = callsThisFrame.toDouble * 1_000_000_000.0 / frameDt
    throughputHistory.enqueue(throughputPerSec)
    while (throughputHistory.size > SparkWidth) throughputHistory.dequeue()

    val modules = aggregateByModule(snap).take(moduleN)

    val sb = new StringBuilder
    sb.append(ClearScreen)
    sb.append('\n')

    renderHeader(sb, elapsed)
    renderProgressBar(sb)
    renderResources(sb, elapsed, throughputPerSec)
    sb.append('\n')
    renderTableHeader(sb, layout)
    renderRows(sb, visible, elapsed, parallelism, layout)
    renderModuleTable(sb, modules, elapsed, parallelism, layout)

    System.out.print(sb)
    System.out.flush()

    // Update carried state for next frame.
    prevFrameNanos = now
    prevTotalCalls = totalCalls
  }

  private def renderHeader(sb: StringBuilder, elapsed: Long): Unit = {
    val phase = flix.currentPhaseName
    val group = phaseGroup(phase)
    val pool = flix.threadPool
    val (active, par) =
      if (pool == null) (0, flix.options.threads)
      else (pool.getActiveThreadCount, pool.getParallelism)

    sb.append("  ")
    sb.append(color(rpad(phase, 14), groupColor(group)))
    sb.append(dim(" ("))
    sb.append(color(group, groupColor(group)))
    sb.append(dim(")   elapsed: "))
    sb.append(formatMillis(elapsed))
    sb.append(dim("   threads: "))
    sb.append(styleThreads(active, par))
    sb.append('\n')
  }

  private def renderProgressBar(sb: StringBuilder): Unit = {
    val phase = flix.currentPhaseName
    val total = Phases.size
    val idx = Phases.indexOf(phase)
    val done = if (idx < 0) 0 else idx + 1
    val filled = (BarWidth.toLong * done / total).toInt

    sb.append("  ")
    sb.append(dim("["))
    sb.append(dim(cyan("█" * filled)))
    sb.append(dim("░" * (BarWidth - filled)))
    sb.append(dim("] "))
    sb.append(f"$done%2d/$total%2d")
    sb.append('\n')
  }

  private def renderResources(sb: StringBuilder, elapsed: Long, throughputPerSec: Double): Unit = {
    val (heapUsedMb, heapMaxMb) = heapUsage()
    val gcMs = gcMillis()
    val gcPct = 100.0 * gcMs * 1_000_000L / elapsed.max(1L)

    val heapUsedField = f"$heapUsedMb%4d MB"
    val heapMaxField = f"$heapMaxMb%4d MB"
    val heapRatio = if (heapMaxMb <= 0) 0.0 else heapUsedMb.toDouble / heapMaxMb
    val gcField = f"$gcPct%4.1f%% (${gcMs}%5dms)"

    sb.append("  ")
    sb.append(dim("heap: "))
    sb.append(styleHeap(heapUsedField, heapRatio))
    sb.append(dim(" / "))
    sb.append(heapMaxField)
    sb.append(dim("   gc: "))
    sb.append(styleGc(gcField, gcPct))
    sb.append(dim("   throughput: "))
    sb.append(dim(cyan(renderSparkline())))
    sb.append(' ')
    sb.append(formatRate(throughputPerSec))
    sb.append('\n')
  }

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

      val symField = rpad(truncate(s.sym.name, layout.symWidth), layout.symWidth)
      val locField = rpad(truncate(locStr, layout.locWidth), layout.locWidth)

      sb.append(' ')
      sb.append(symField)
      sb.append(' ')
      sb.append(dim(locField))
      appendNumericFields(sb, locLines, s.callCount.toLong, phase, s.totalNanos, pctCpu, pctWall, layout)
      sb.append(' ')
      sb.append('\n')
    }
  }

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

  private def renderSparkline(): String = {
    if (throughputHistory.isEmpty) return " " * SparkWidth
    val max = throughputHistory.max.max(1.0)
    val chars = throughputHistory.iterator.map { v =>
      val i = math.min(SparkChars.length - 1, math.max(0, (v / max * (SparkChars.length - 1)).round.toInt))
      SparkChars(i)
    }.toArray
    // Right-align so the most recent sample is at the right edge.
    val pad = (SparkWidth - chars.length).max(0)
    " " * pad + new String(chars)
  }
}

object TopRenderer {

  /** How often the screen refreshes, in milliseconds. */
  private val RefreshIntervalMs: Long = 100L

  /** Width (in characters) of the phase-progress bar. */
  private val BarWidth: Int = 32

  /** Width of the throughput sparkline. */
  private val SparkWidth: Int = 24

  /** Block characters used for the sparkline, low → high. */
  private val SparkChars: Array[Char] = "▁▂▃▄▅▆▇█".toArray

  /** ANSI escape character. */
  private val ESC: Char = 27.toChar
  private val ClearScreen: String = s"$ESC[H$ESC[2J"

  // Color codes.
  private val Reset: String = s"$ESC[0m"
  private val BoldCode: String = s"$ESC[1m"
  private val DimCode: String = s"$ESC[2m"
  private val Red: String = s"$ESC[31m"
  private val Green: String = s"$ESC[32m"
  private val Yellow: String = s"$ESC[33m"
  private val Blue: String = s"$ESC[34m"
  private val Magenta: String = s"$ESC[35m"
  private val Cyan: String = s"$ESC[36m"
  private val Gray: String = s"$ESC[90m"

  // Wrappers — each takes a string and returns it surrounded by codes.
  private def color(s: String, c: String): String = s"$c$s$Reset"
  private def bold(s: String): String = s"$BoldCode$s$Reset"
  private def dim(s: String): String = s"$DimCode$s$Reset"
  private def red(s: String): String = color(s, Red)
  private def yellow(s: String): String = color(s, Yellow)
  private def green(s: String): String = color(s, Green)
  private def cyan(s: String): String = color(s, Cyan)

  // -- Conditional styling -------------------------------------------------

  private def styleTime(formatted: String, nanos: Long): String = {
    val ms = nanos / 1_000_000L
    if (ms >= 1000) bold(red(formatted))
    else if (ms >= 200) bold(yellow(formatted))
    else if (ms >= 50) yellow(formatted)
    else formatted
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

  private def styleThreads(active: Int, par: Int): String = {
    val s = f"$active%2d/$par%-2d"
    if (par <= 0) color(s, Gray)
    else if (active == par) bold(green(s))
    else if (active.toDouble / par >= 0.5) green(s)
    else if (active == 0) color(s, Gray)
    else yellow(s)
  }

  private def styleHeap(formatted: String, ratio: Double): String = {
    if (ratio >= 0.9) bold(red(formatted))
    else if (ratio >= 0.7) yellow(formatted)
    else green(formatted)
  }

  private def styleGc(formatted: String, pct: Double): String = {
    if (pct >= 10.0) bold(red(formatted))
    else if (pct >= 3.0) yellow(formatted)
    else green(formatted)
  }

  /**
    * The compiler phases, in the order they fire.
    *
    * Mirrors the calls to `flix.phase` / `flix.phaseNew` in
    * `Flix.check` followed by `Flix.codeGen`.
    *
    * Note: appearance here means the phase advances the progress bar; it does
    * not mean the phase feeds [[DefnTimer]]. See `DefnTimer`'s class-level
    * doc for the list of instrumented vs. uninstrumented phases.
    */
  private val Phases: Vector[String] = Vector(
    // syntax (parsing)
    "Reader", "Lexer", "Parser2", "Weeder2", "Desugar",
    // semantic (frontend)
    "Namer", "Resolver", "Kinder", "Deriver", "Typer", "EntryPoints", "Instances",
    "PredDeps", "Stratifier", "PatMatch", "Redundancy", "Safety", "Terminator", "Dependencies",
    // mid-end (lowering and optimization)
    "TreeShaker1", "Monomorpher", "LambdaDrop", "Optimizer", "Simplifier",
    "ClosureConv", "LambdaLift", "TreeShaker2", "EffectBinder", "TailPos",
    "Eraser", "Reducer",
    // backend
    "JvmBackend"
  )

  private def phaseGroup(phase: String): String = {
    val idx = Phases.indexOf(phase)
    if (idx < 0) "?"
    else if (idx <= 4) "syntax"
    else if (idx <= 18) "semantic"
    else if (idx <= 30) "midend"
    else "backend"
  }

  private def groupColor(group: String): String = group match {
    case "syntax"   => Blue
    case "semantic" => Green
    case "midend"   => Yellow
    case "backend"  => Magenta
    case _          => Gray
  }

  // -- Numeric helpers ----------------------------------------------------

  private def heapUsage(): (Long, Long) = {
    val rt = Runtime.getRuntime
    val used = rt.totalMemory() - rt.freeMemory()
    val max = rt.maxMemory()
    (used / (1024L * 1024L), max / (1024L * 1024L))
  }

  private def gcMillis(): Long =
    ManagementFactory.getGarbageCollectorMXBeans.asScala.iterator
      .map(_.getCollectionTime).filter(_ >= 0L).sum

  private final case class ModuleStats(
    module: String,
    totalNanos: Long,
    totalCallCount: Long,
    totalLocLines: Int,
    byPhase: Map[String, Long]
  ) {
    def dominantPhase: Option[String] =
      if (byPhase.isEmpty) None else Some(byPhase.maxBy(_._2)._1)
  }

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
    */
  private final case class Layout(
    symWidth: Int,
    locWidth: Int,
    showLOC: Boolean,
    showN: Boolean,
    showPhase: Boolean,
    totalWidth: Int
  )

  /** Default DefnSym/location widths and minimum bounds when the terminal shrinks. */
  private val DefaultSymWidth: Int = 24
  private val DefaultLocWidth: Int = 28
  private val MinSymWidth: Int = 16
  private val MinLocWidth: Int = 20

  /** Fixed-width contribution of `time + %cpu + %wall` columns and their separators. */
  private val FixedTailWidth: Int = 9 + 1 + 6 + 1 + 6 // time(9) + %cpu(6) + %wall(6) with two separators between

  /** Width contribution of an optional column (separator + width). */
  private val LocColWidth: Int = 1 + 4
  private val NColWidth: Int = 1 + 4
  private val PhaseColWidth: Int = 1 + 10

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
    * Fixed-overhead rows: blank + 3 header lines + blank + 2 def-chrome +
    * 1 blank-before-modules + 2 module-chrome + 1 cursor-parking row.
    */
  private val ChromeRows: Int = 11

  /** Reserved breathing room above and below the rendered view. */
  private val RowMargin: Int = 2

  /** Floor on the total data-row budget. */
  private val MinDataRows: Int = 8

  /** Bounds on per-table row counts. */
  private val MinDefN: Int = 5
  private val MaxDefN: Int = 30
  private val MinModuleN: Int = 3
  private val MaxModuleN: Int = 10

  // -- Formatting helpers -------------------------------------------------

  private def formatLocation(loc: SourceLocation): String =
    if (!loc.isReal) "?" else s"${loc.source.name}:${loc.startLine}"

  private def locLineCount(loc: SourceLocation): Int =
    if (!loc.isReal) 0 else (loc.endLine - loc.startLine + 1).max(0)

  private def formatMillis(nanos: Long): String = {
    val ms = nanos / 1_000_000L
    if (ms >= 10_000L) f"${ms / 1000.0}%.1fs"
    else f"${ms}ms"
  }

  /** Format a per-second rate of count as e.g. "1.2k/s" or "421/s". */
  private def formatRate(perSec: Double): String = {
    if (perSec >= 1000.0) f"${perSec / 1000.0}%.1fk/s"
    else f"${perSec.toInt}%d/s"
  }

  private def truncate(s: String, width: Int): String =
    if (s.length <= width) s else s.take(width - 1) + "…"

  private def lpad(s: String, w: Int): String =
    if (s.length >= w) s else " " * (w - s.length) + s

  private def rpad(s: String, w: Int): String =
    if (s.length >= w) s else s + " " * (w - s.length)
}
