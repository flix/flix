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
import ca.uwaterloo.flix.tools.compilertop.Aggregation.*
import ca.uwaterloo.flix.tools.compilertop.Ansi.*
import ca.uwaterloo.flix.tools.compilertop.Formatting.*
import ca.uwaterloo.flix.tools.compilertop.Layout
import ca.uwaterloo.flix.tools.compilertop.Model.*
import ca.uwaterloo.flix.tools.compilertop.Profiler
import ca.uwaterloo.flix.tools.compilertop.Profiler.DefnStats
import ca.uwaterloo.flix.tools.compilertop.Styling.*
import org.jline.terminal.{Attributes, Terminal, TerminalBuilder}

import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.collection.mutable

object CompilerTop {

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


  /** Fallback terminal height when JLine cannot determine the real one. */
  private val DefaultRows: Int = 24

  /** Fallback terminal width when JLine cannot determine the real one. */
  private val DefaultCols: Int = 100


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
final class CompilerTop(flix: Flix, profiler: Profiler) {

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
        case 'v' | 'V' => sort.set(Sort.Tvars)
        case 'e' | 'E' => sort.set(Sort.Evars)
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
    val layout = Layout.compute((terminalCols() - 2).max(1), activeFilter)

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
    sb.append(filterLegendEntry("all",      active == PhaseFilter.All))
    sb.append(dim("|"))
    sb.append(filterLegendEntry("frontend", active == PhaseFilter.Frontend))
    sb.append(dim("|"))
    sb.append(filterLegendEntry("backend",  active == PhaseFilter.Backend))
    sb.append(dim("]"))
    sb.toString
  }

  /**
    * Renders one filter-legend entry: the first letter is the keystroke,
    * underlined. The whole word is bold cyan when this is the active filter,
    * dim otherwise. Underline state is toggled surgically so the inactive
    * dim styling and the active bold+cyan styling pass through unchanged on
    * the remaining letters.
    */
  private def filterLegendEntry(label: String, active: Boolean): String = {
    val key = label.head.toString
    val rest = label.tail
    if (active) s"$BoldCode$Yellow$UnderlineCode$key$NoUnderlineCode$rest$Reset"
    else s"$DimCode$UnderlineCode$key$NoUnderlineCode$rest$Reset"
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
      val tvP  = lpad("tv",  4)
      sb.append(' '); sb.append(keyHeader(tvP,  tvP.indexOf('v'),  activeSort == Sort.Tvars))
      val evP  = lpad("ev",  4)
      sb.append(' '); sb.append(keyHeader(evP,  evP.indexOf('e'),  activeSort == Sort.Evars))
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
      appendNumericFields(sb, locLines, s.byPhaseCount, s.cns, s.tvars.toLong, s.evars.toLong, phase, s.totalNanos, pctCpu, pctWall, layout, aggregate = false)
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
      appendNumericFields(sb, m.totalLocLines, m.byPhaseCount, m.totalCns, m.totalTvars, m.totalEvars, phase, m.totalNanos, pctCpu, pctWall, layout, aggregate = true)
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
      val tvP  = lpad("tv",  4)
      sb.append(' '); sb.append(keyHeader(tvP,  tvP.indexOf('v'),  activeSort == Sort.Tvars))
      val evP  = lpad("ev",  4)
      sb.append(' '); sb.append(keyHeader(evP,  evP.indexOf('e'),  activeSort == Sort.Evars))
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
  private def appendNumericFields(sb: StringBuilder, locLines: Int, byPhaseCount: Map[String, Long], cns: Long, tvars: Long, evars: Long, phase: String,
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
      sb.append(' '); sb.append(lpad(tvars.toString, 4))
      sb.append(' '); sb.append(lpad(evars.toString, 4))
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
