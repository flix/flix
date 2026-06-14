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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.tools.compilertop.Aggregation.*
import ca.uwaterloo.flix.tools.compilertop.Ansi.*
import ca.uwaterloo.flix.tools.compilertop.Formatting.*
import ca.uwaterloo.flix.tools.compilertop.Model.*
import org.jline.terminal.{Attributes, Terminal, TerminalBuilder}

import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

object CompilerTop {

  /** How often the screen refreshes, in milliseconds (5 FPS). */
  private val RefreshIntervalMs: Long = 200L

  /** Poll interval for the input thread (ms); doubles as how fast `running=false` is observed. */
  private val InputPollMs: Long = 100L

  /** Fallback terminal height when JLine cannot determine the real one. */
  private val DefaultRows: Int = 24

  /** Fallback terminal width when JLine cannot determine the real one. */
  private val DefaultCols: Int = 100


  /**
    * Fixed-overhead rows: blank + 2 dashboard/stats lines + blank +
    * 2 table-chrome (header + divider) + 1 cursor-parking row. Only one
    * table is visible at a time now, so there is no second-table chrome to
    * reserve.
    */
  private val ChromeRows: Int = 7

  /**
    * Reserved breathing room below the rendered view, on top of the
    * cursor-parking row already counted in [[ChromeRows]]. Just one cosmetic
    * blank line — the parking row alone is enough to prevent the terminal
    * from scrolling.
    */
  private val RowMargin: Int = 1

  /** Floor on the table's data-row budget. */
  private val MinTableRows: Int = 8

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
    * Active top-level view (defs table / modules table / help screen).
    * Written by the input thread on `Tab` / `?` / `Esc`, read by the
    * renderer thread. Default is [[View.Defs]]. Compilation keeps running
    * regardless of which view is showing; toggling does not interfere with
    * the build.
    */
  private val view = new AtomicReference[View](View.Defs)

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

  /** Owns the dashboard sparkline history; otherwise a pure function of [[FrameState]]. */
  private val renderer = new Renderer()

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
    *   - Tab (`9`) → flip between the defs and modules tables (from help,
    *     lands on the defs table).
    *   - `?`       → toggle the help view (column / keystroke legend).
    *   - Esc (`27`) → return to the defs view (no-op if already there).
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
        case 9 => // Tab — flip the table; from help, land on the defs table.
          view.updateAndGet(v => if (v == View.Help) View.Defs else v.toggledTable)
        case 27 => // Esc — return to the "all" view: filter All, defs view.
          filter.set(PhaseFilter.All)
          view.set(View.Defs)
        case '?' =>
          view.updateAndGet(v => if (v == View.Help) View.Defs else View.Help)
        case ch if ch > 0 =>
          // Dispatch via the Sort / PhaseFilter ADTs so the key ↔ value
          // mapping lives in one place ([[Model.Sort.fromKey]],
          // [[Model.PhaseFilter.fromKey]]) rather than being duplicated
          // across this match and the renderer's header / legend code.
          //
          // Pressing a filter key from the help view also leaves help (onto
          // the defs table) so the user can see the filter take effect —
          // otherwise the toggle happens invisibly behind the help screen.
          // From a table view the current table is preserved.
          val char = ch.toChar
          Sort.fromKey(char).foreach(sort.set)
          PhaseFilter.fromKey(char).foreach { f =>
            filter.set(f)
            view.updateAndGet(v => if (v == View.Help) View.Defs else v)
          }
        case _ => // ignored
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
    val frame = renderer.render(buildFrameState())
    System.out.print(frame)
    System.out.flush()
  }

  /**
    * Snapshots everything the renderer needs into a [[FrameState]]: live
    * vs. frozen elapsed / threads / heap, the user-toggled filter and sort,
    * the column layout from the current terminal width, and the
    * already-filtered / sorted / trimmed def + module views.
    */
  private def buildFrameState(): FrameState = {
    val isDone = completed.get()
    val now = System.nanoTime()
    val pool = flix.threadPool
    val parallelism = if (pool == null) flix.options.threads.max(1) else pool.getParallelism.max(1)
    // Once compilation is done, freeze elapsed + active threads + heap at
    // their snapshot values so the dashboard reflects the state at
    // completion rather than ticking forward during the wait-for-quit phase.
    val elapsed = if (isDone) frozenElapsedNanos else now - startNanos
    val activeThreads = if (isDone) frozenActiveThreads else (if (pool == null) 0 else pool.getActiveThreadCount)
    val heap = if (isDone) frozenHeap else heapUsage()
    val currentPhase = if (isDone) "done" else flix.getCurrentPhaseName.getOrElse("starting")
    val phaseTimersSize = flix.phaseTimers.size

    // Budget rows for the single visible table based on the current terminal
    // height, reserving an extra `RowMargin` rows so the view never quite
    // touches the top or bottom edge of the terminal. Only one table shows at
    // a time, so it gets the whole budget.
    val tableRows = (terminalRows() - ChromeRows - RowMargin).max(MinTableRows)

    val activeView = view.get()
    val activeFilter = filter.get()
    val activeSort = sort.get()

    // The mono / inl / cls columns only render under the backend view (the
    // phases that populate them only fire in the backend pipeline). The cns /
    // tv / ev columns only render under the frontend view (constraint
    // generation is purely a Typer concern). Layout itself doesn't know about
    // PhaseFilter; we hand it booleans.
    val showCounts = activeFilter == PhaseFilter.Backend
    val showCns    = activeFilter == PhaseFilter.Frontend
    // Compute the column layout based on the current terminal width,
    // reserving 2 columns for the 1-space left and right table padding.
    val layout = Layout.compute((terminalCols() - 2).max(1), showCounts, showCns)

    // Only build the rows the active view actually renders: the def list when
    // on Defs / Help, the module aggregate when on Modules. `snap` is computed
    // either way since module aggregation rolls up from it.
    val raw = profiler.snapshot()
    val snap = applyFilter(raw, activeFilter).sortBy(s => -defSortKey(s, activeSort))
    val visible = if (activeView == View.Modules) Vector.empty else snap.take(tableRows)
    val modules = if (activeView == View.Modules) aggregateByModule(snap, activeSort).take(tableRows) else Vector.empty

    // Coverage scopes to the active phase filter (via `matchesFilter`), so the
    // observed figure narrows to the same phases the visible table covers: press
    // `f`/`b` and both the rows and the coverage % follow. `parallelism` divides
    // the thread-summed per-def time into an estimated attributed wall slice.
    val coverage = computeCoverage(raw, flix.phaseTimers.toVector, activeFilter, parallelism)

    FrameState(parallelism, isDone, elapsed, activeThreads, heap, currentPhase, phaseTimersSize, activeFilter, activeSort, activeView, layout, visible, modules, coverage)
  }

}
