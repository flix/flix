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
final class TopRenderer(flix: Flix, topN: Int = 20) {

  import TopRenderer.*

  private val running = new AtomicBoolean(false)
  private var thread: Thread = _
  private val startNanos: Long = System.nanoTime()

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

    val snap = flix.defnTimer.snapshot().sortBy(-_.totalNanos)
    val visible = snap.take(topN)

    // Throughput sparkline: visits-per-second since last frame.
    val totalCalls = flix.defnTimer.totalCallCount
    val callsThisFrame = (totalCalls - prevTotalCalls).max(0L)
    val throughputPerSec = callsThisFrame.toDouble * 1_000_000_000.0 / frameDt
    throughputHistory.enqueue(throughputPerSec)
    while (throughputHistory.size > SparkWidth) throughputHistory.dequeue()

    // Outlier threshold: mean + 2σ of total nanos across visible entries.
    val outlierThreshold = computeOutlierThreshold(visible)

    val sb = new StringBuilder
    sb.append(ClearScreen)

    renderHeader(sb, elapsed)
    renderProgressBar(sb)
    renderResources(sb, elapsed, throughputPerSec)
    sb.append('\n')
    renderTableHeader(sb)
    renderRows(sb, visible, elapsed, parallelism, outlierThreshold)
    renderLegend(sb)

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

    sb.append(dim("["))
    sb.append(cyan("█" * filled))
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

    sb.append(dim("heap: "))
    sb.append(styleHeap(heapUsedField, heapRatio))
    sb.append(dim(" / "))
    sb.append(heapMaxField)
    sb.append(dim("   gc: "))
    sb.append(styleGc(gcField, gcPct))
    sb.append(dim("   throughput: "))
    sb.append(cyan(renderSparkline()))
    sb.append(' ')
    sb.append(formatRate(throughputPerSec))
    sb.append('\n')
  }

  private def renderTableHeader(sb: StringBuilder): Unit = {
    val header = f"${"DefnSym"}%-38s ${"location"}%-22s ${"LOC"}%4s ${"n"}%4s ${"phase"}%-12s ${"time"}%9s ${"%cpu"}%6s ${"%wall"}%6s"
    sb.append(bold(cyan(header)))
    sb.append('\n')
    sb.append(dim("─" * header.length))
    sb.append('\n')
  }

  private def renderRows(sb: StringBuilder, visible: Vector[DefnStats], elapsed: Long, parallelism: Int, outlierThreshold: Long): Unit = {
    if (visible.isEmpty) {
      sb.append(dim("(no DefnSyms have been timed yet)\n"))
      return
    }
    val safeElapsed = elapsed.max(1L).toDouble
    for (s <- visible) {
      val pctWall = 100.0 * s.totalNanos / safeElapsed
      val pctCpu = pctWall / parallelism
      val locStr = formatLocation(s.loc)
      val locLines = locLineCount(s.loc)
      val phase = s.dominantPhase.getOrElse("?")
      val isOutlier = s.totalNanos >= outlierThreshold

      val symField = rpad(truncate(s.sym.toString, 38), 38)
      val locField = rpad(truncate(locStr, 22), 22)
      val locCntField = lpad(if (locLines > 0) locLines.toString else "-", 4)
      val nField = lpad(s.callCount.toString, 4)
      val phaseField = rpad(truncate(phase, 12), 12)
      val timeField = lpad(formatMillis(s.totalNanos), 9)
      val pctCpuField = f"$pctCpu%5.1f%%"
      val pctWallField = f"$pctWall%5.1f%%"

      sb.append(if (isOutlier) bold(red(symField)) else symField)
      sb.append(' ')
      sb.append(dim(locField))
      sb.append(' ')
      sb.append(locCntField)
      sb.append(' ')
      sb.append(nField)
      sb.append(' ')
      sb.append(phaseField)
      sb.append(' ')
      sb.append(styleTime(timeField, s.totalNanos))
      sb.append(' ')
      sb.append(stylePctCpu(pctCpuField, pctCpu))
      sb.append(' ')
      sb.append(stylePctWall(pctWallField, pctWall))
      sb.append('\n')
    }
  }

  private def renderLegend(sb: StringBuilder): Unit = {
    sb.append('\n')
    sb.append(dim("bold red sym = outlier (>µ+2σ)    %cpu = totalNanos / (elapsed × threads)    %wall = totalNanos / elapsed"))
    sb.append('\n')
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

  private def computeOutlierThreshold(stats: Vector[DefnStats]): Long = {
    if (stats.length < 3) return Long.MaxValue
    val xs = stats.map(_.totalNanos.toDouble).toArray
    val mean = xs.sum / xs.length
    val variance = xs.map(x => (x - mean) * (x - mean)).sum / xs.length
    val stdev = math.sqrt(variance)
    (mean + 2.0 * stdev).toLong
  }

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
