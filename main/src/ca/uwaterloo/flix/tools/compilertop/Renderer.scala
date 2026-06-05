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

import ca.uwaterloo.flix.tools.compilertop.Aggregation.*
import ca.uwaterloo.flix.tools.compilertop.Ansi.*
import ca.uwaterloo.flix.tools.compilertop.Formatting.*
import ca.uwaterloo.flix.tools.compilertop.Model.*
import ca.uwaterloo.flix.tools.compilertop.Profiler.DefnStats
import ca.uwaterloo.flix.tools.compilertop.Styling.*

import scala.collection.mutable

/**
  * Per-frame inputs consumed by [[Renderer.render]]. CompilerTop builds one
  * of these per tick from its terminal handle, the live profiler snapshot,
  * and the user-toggled filter / sort knobs. The renderer is a pure
  * function of this record (plus its own private sparkline history).
  *
  * @param parallelism     the threadpool's parallelism (≥ 1).
  * @param isDone          true once compilation has finished; freezes the
  *                        elapsed / threads / heap fields and switches the
  *                        progress bar to its "done" rendering.
  * @param elapsed         wall-clock nanos since the TUI started.
  * @param activeThreads   threadpool active-thread count for the dashboard.
  * @param heap            `(usedMb, maxMb)` from the JVM at frame time.
  * @param currentPhase    already-resolved phase label (e.g. `"Typer"`,
  *                        `"done"`, or `"starting"`).
  * @param phaseTimersSize raw `flix.phaseTimers.size`; the renderer caps to
  *                        [[Renderer.TotalPhases]] for the progress bar.
  * @param activeFilter    user-toggled phase filter (drives the legend).
  * @param activeSort      user-toggled sort key (drives header underlines).
  * @param activeView      user-toggled top-level view: defs table, modules
  *                        table, or help. Only the active table's rows are
  *                        populated below.
  * @param layout          column layout produced by [[Layout.compute]].
  * @param visible         def-table rows, already filtered / sorted / trimmed.
  *                        Empty when [[activeView]] is [[Model.View.Modules]].
  * @param modules         module-table rows, already aggregated / sorted /
  *                        trimmed. Empty unless [[activeView]] is
  *                        [[Model.View.Modules]].
  */
final case class FrameState(parallelism: Int,
                            isDone: Boolean,
                            elapsed: Long,
                            activeThreads: Int,
                            heap: (Long, Long),
                            currentPhase: String,
                            phaseTimersSize: Int,
                            activeFilter: PhaseFilter,
                            activeSort: Sort,
                            activeView: View,
                            layout: Layout,
                            visible: Vector[DefnStats],
                            modules: Vector[ModuleStats])

object Renderer {

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

  /**
    * Numeric fields shared between def and module table rows. Built once
    * per row by [[Row.cells]] from a [[DefnStats]] or [[ModuleStats]] plus
    * the per-frame `safeElapsed` and `parallelism`. Lets
    * [[Renderer.appendNumericFields]] take a single record instead of a
    * dozen positional parameters.
    *
    * @param aggregate `true` for module rows — disables the per-field
    *                  warning colors, since aggregating across defs almost
    *                  always trips the thresholds. False for def rows.
    */
  private final case class RowCells(lines: Int,
                                    byPhaseCount: Map[String, Long],
                                    inlined: Long,
                                    classBytes: Long,
                                    cns: Long,
                                    tvars: Long,
                                    evars: Long,
                                    dominantPhase: String,
                                    allocBytes: Long,
                                    nanos: Long,
                                    pctCpu: Double,
                                    pctWall: Double,
                                    aggregate: Boolean)

  /**
    * A renderable table row. The numeric trailing columns are uniform
    * across def and module rows ([[RowCells]] / [[Renderer.appendNumericFields]]);
    * only the contents of the leading [[Layout.nameWidth]]-wide cell
    * differs — def rows pack `sym + (file:line)` into it (bolded when the
    * sym is currently being compiled), module rows fill it with just the
    * module name.
    */
  private sealed trait Row {
    /** The trailing numeric columns for this row, ready for [[Renderer.appendNumericFields]]. */
    def cells: RowCells
    /** Appends this row's leading (type-specific) cell to `sb`. */
    def appendFirstCell(sb: StringBuilder, layout: Layout): Unit
  }

  /** Def-table row: hotness-colored sym (bold if currently being compiled), parenthesized location — all in one merged cell. */
  private final case class DefnRow(s: DefnStats, safeElapsed: Double, parallelism: Int) extends Row {
    private val lines = lineCount(s.loc)

    /** Numeric cells for this def, with `aggregate = false` so warning colors apply. */
    val cells: RowCells = {
      val pctWall = 100.0 * s.totalNanos / safeElapsed
      RowCells(
        lines         = lines,
        byPhaseCount  = s.byPhaseCount,
        inlined       = s.inlined,
        classBytes    = s.classBytes,
        cns           = s.cns,
        tvars         = s.tvars,
        evars         = s.evars,
        dominantPhase = s.dominantPhase.getOrElse("?"),
        allocBytes    = s.allocBytes,
        nanos         = s.totalNanos,
        pctCpu        = pctWall / parallelism,
        pctWall       = pctWall,
        aggregate     = false,
      )
    }

    /**
      * Appends a single merged cell: hotness-colored sym (bolded if the def
      * is currently being compiled), then a dim parenthesized `(file:line)`.
      * If the cell can't fit the full pair, the location is truncated from
      * the front first (`…flix:128`); only when even a minimal `…:N`
      * location won't leave room for the sym does the sym itself get
      * truncated. Synthetic defs (no real source location) render bare — no
      * parens — since `(?)` is just noise.
      *
      * Bold composes with the hotness color so a currently-running def that
      * is also very hot renders as bold red, amplifying the signal instead
      * of fighting it.
      *
      * Decision (what to display) is separated from emission (how to lay it
      * out): the truncation ladder picks a `(nameVisible, locVisible)` pair,
      * and a single emit path then formats and pads to width. Padding is
      * always derived from visible widths, so cells fill exactly even when
      * the location is shorter than the minimum-tail budget.
      */
    def appendFirstCell(sb: StringBuilder, layout: Layout): Unit = {
      val width     = layout.nameWidth
      val name      = s.sym.name
      val ChromeLen = 3  // " (" + ")"
      val MinLocLen = 5  // minimum useful tail of a location, e.g. "…:128"

      val (nameVisible, locVisible) =
        if (!s.loc.isReal) (truncate(name, width), "")
        else {
          val locStr = formatLocation(s.loc)
          val ideal  = name.length + ChromeLen + locStr.length
          if (ideal <= width) (name, locStr)
          else if (name.length + ChromeLen + MinLocLen <= width)
            (name, truncateFront(locStr, width - name.length - ChromeLen))
          else
            (truncate(name, width - ChromeLen - MinLocLen), truncateFront(locStr, MinLocLen))
        }

      val suffix = if (locVisible.isEmpty) "" else s" ($locVisible)"
      val styled = styleSym(nameVisible, s.totalNanos, lines)
      sb.append(if (s.isActive) bold(styled) else styled)
      if (suffix.nonEmpty) sb.append(dim(suffix))
      sb.append(" " * (width - nameVisible.length - suffix.length))
    }
  }

  /** Module-table row: one wide name field spanning sym + location widths. */
  private final case class ModuleRow(m: ModuleStats, safeElapsed: Double, parallelism: Int) extends Row {
    /** Numeric cells for this module, with `aggregate = true` so per-def warning colors are skipped. */
    val cells: RowCells = {
      val pctWall = 100.0 * m.totalNanos / safeElapsed
      RowCells(
        lines         = m.totalLines,
        byPhaseCount  = m.byPhaseCount,
        inlined       = m.totalInlined,
        classBytes    = m.totalClassBytes,
        cns           = m.totalCns,
        tvars         = m.totalTvars,
        evars         = m.totalEvars,
        dominantPhase = m.dominantPhase.getOrElse("?"),
        allocBytes    = m.totalAllocBytes,
        nanos         = m.totalNanos,
        pctCpu        = pctWall / parallelism,
        pctWall       = pctWall,
        aggregate     = true,
      )
    }

    /**
      * Appends the module name, hotness-colored (ms-per-line over the module's
      * summed time / lines) and padded to span the merged def-row name column.
      * Styling is applied to the visible text only, then padded by visible
      * width so the ANSI codes don't throw off column alignment.
      */
    def appendFirstCell(sb: StringBuilder, layout: Layout): Unit = {
      val name = truncate(m.module, layout.nameWidth)
      sb.append(styleModule(name, m.totalNanos, m.totalLines))
      sb.append(" " * (layout.nameWidth - name.length))
    }
  }
}

/**
  * Frame builder for the compiler-top TUI. Each instance owns one piece of
  * mutable state — the rolling thread-occupancy history that feeds the
  * dashboard sparkline. Everything else flows in via [[FrameState]] so the
  * renderer can be exercised without a live `Flix`.
  */
final class Renderer {

  import Renderer.*

  /** Rolling history of active-thread counts feeding the dashboard sparkline. */
  private val threadsHistory = mutable.Queue.empty[Double]

  /** Builds one full frame from the given state and returns the bytes to print. */
  def render(state: FrameState): String = {
    // Active-threads sparkline: history of thread-pool occupancy. Stops
    // updating once compilation is done so the bar reflects the work, not
    // a long tail of zero-occupancy samples while waiting for `q`.
    if (!state.isDone) {
      threadsHistory.enqueue(state.activeThreads.toDouble)
      while (threadsHistory.size > SparkWidth) threadsHistory.dequeue()
    }

    val sb = new StringBuilder
    sb.append(BeginSync)
    sb.append(ClearScreen)
    sb.append('\n')

    renderDashboard(sb, state)
    renderStats(sb, state)
    sb.append('\n')

    state.activeView match {
      case View.Defs    => renderDefTable(sb, state)
      case View.Modules => renderModuleTable(sb, state)
      case View.Help    => renderHelp(sb)
    }

    sb.append(EndSync)
    sb.toString
  }

  /** Renders the per-def table (the body of the defs view). */
  private def renderDefTable(sb: StringBuilder, state: FrameState): Unit = {
    val safeElapsed = state.elapsed.max(1L).toDouble
    val defRows: Vector[Row] = state.visible.map(s => DefnRow(s, safeElapsed, state.parallelism))

    renderHeaderRow(sb, "def (hot)", state.layout, state.activeSort)
    if (defRows.isEmpty) renderEmptyPlaceholder(sb, "(no timings yet)", state.layout)
    else renderTable(sb, defRows, state.layout)
  }

  /** Renders the per-module aggregate table (the body of the modules view). */
  private def renderModuleTable(sb: StringBuilder, state: FrameState): Unit = {
    val safeElapsed = state.elapsed.max(1L).toDouble
    val modRows: Vector[Row] = state.modules.map(m => ModuleRow(m, safeElapsed, state.parallelism))

    renderHeaderRow(sb, "mod (hot)", state.layout, state.activeSort)
    if (modRows.isEmpty) renderEmptyPlaceholder(sb, "(no modules yet)", state.layout)
    else renderTable(sb, modRows, state.layout)
  }

  /**
    * Top dashboard line: current phase + progress bar + active-threads bar.
    * Both bars sit beside each other so the eye picks them up as a pair.
    */
  private def renderDashboard(sb: StringBuilder, state: FrameState): Unit = {
    // Once compilation has finished, force the bar to 100% rather than relying
    // on `phaseTimers.size + 1` (which is +1 ahead of reality during execution
    // and would still under-fill if any phase is uninstrumented).
    val done = if (state.isDone) TotalPhases else (state.phaseTimersSize + 1).min(TotalPhases)
    val filled = (BarWidth.toLong * done / TotalPhases).toInt

    sb.append("  ")
    sb.append(bold(rpad(state.currentPhase, MaxPhaseLen)))
    sb.append(dim("  progress "))
    sb.append(dim("["))
    // Blue while compilation is in progress; green once finished so the bar
    // doubles as a "done" indicator.
    val filledBar = "█" * filled
    sb.append(if (state.isDone) green(filledBar) else blue(filledBar))
    sb.append(dim("░" * (BarWidth - filled)))
    sb.append(dim("] "))
    sb.append(f"$done%2d/$TotalPhases%2d")
    sb.append(dim("   threads "))
    // With parallelism=1 ForkJoin runs work inline on the submitter, leaving
    // `getActiveThreadCount` at 0 most of the time — sparkline goes flat,
    // field reads `0/1` continuously. Show a label instead.
    if (state.parallelism > 1) {
      sb.append(dim(cyan(renderSparkline(state.parallelism.toDouble))))
      sb.append(' ')
      sb.append(styleThreads(state.activeThreads, state.parallelism))
    } else {
      sb.append(dim("single-threaded"))
    }
    sb.append("   ")
    sb.append(renderFilterLegend(state.activeFilter, state.activeView))
    sb.append('\n')
  }

  /**
    * Renders the `[a|f|b]` legend with the active filter highlighted in
    * bold yellow. Always visible so the user sees the available toggles
    * whether the filter is active or not. Driven entirely by
    * [[PhaseFilter.all]]: adding a new filter is a one-line change in
    * [[Model]] and the legend follows automatically.
    *
    * In help view none of the three entries is highlighted — the help tip
    * carries the "active mode" signal instead, so the legend reflects
    * "press a/f/b to leave help" rather than "you are currently filtering".
    */
  private def renderFilterLegend(active: PhaseFilter, view: View): String = {
    val entries = PhaseFilter.all.map(f => filterLegendEntry(f, view != View.Help && f == active))
    s"${dim("[")}${entries.mkString(dim("|"))}${dim("]")}"
  }

  /**
    * Renders one filter-legend entry: the keystroke letter is underlined.
    * The whole word is bold yellow when this is the active filter, dim
    * otherwise. Underline state is toggled surgically so the inactive
    * dim styling and the active bold+yellow styling pass through unchanged
    * on the remaining letters.
    */
  private def filterLegendEntry(f: PhaseFilter, active: Boolean): String = {
    val key = f.key.toString
    val rest = f.label.tail
    if (active) s"$BoldCode$Yellow$UnderlineCode$key$NoUnderlineCode$rest$Reset"
    else        s"$DimCode$UnderlineCode$key$NoUnderlineCode$rest$Reset"
  }

  /**
    * Stats line: elapsed (right-aligned under `progress`) and heap
    * (right-aligned under `threads`).
    */
  private def renderStats(sb: StringBuilder, state: FrameState): Unit = {
    val (heapUsedMb, heapMaxMb) = state.heap
    val heapUsedField = f"$heapUsedMb%4d MB"
    val heapMaxField = f"$heapMaxMb%4d MB"
    val heapRatio = if (heapMaxMb <= 0) 0.0 else heapUsedMb.toDouble / heapMaxMb
    val elapsedField = formatMillis(state.elapsed)

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
    // Align under the `[all|frontend|backend]` legend on the dashboard line above.
    // When help is the active view, the tip carries the "active mode" signal
    // (bold yellow) — the filter legend goes plain because no filter is
    // currently being applied to a visible table. The "?" itself is underlined
    // to match the keystroke-underline convention used in the column headers
    // and the filter legend.
    sb.append("     ")
    val tipText = s"$UnderlineCode?$NoUnderlineCode for help"
    val tipStyled =
      if (state.activeView == View.Help) bold(color(tipText, Yellow))
      else color(tipText, Gray)
    sb.append(tipStyled)
    sb.append('\n')
  }

  /**
    * Prints the parametrized header row + the divider underneath it. The
    * `label` argument is the raw first-column label — `"def (hot)"` for the
    * def table, `"mod (hot)"` for the module table — and gets padded to
    * [[Layout.nameWidth]] here. The location now sits inside the def cells
    * in parens, so the header doesn't have a separate `"location"` column.
    */
  private def renderHeaderRow(sb: StringBuilder, label: String, layout: Layout, activeSort: Sort): Unit = {
    sb.append(' ')
    sb.append(buildHeader(rpad(label, layout.nameWidth), layout, activeSort))
    sb.append(' '); sb.append('\n')
    sb.append(' ')
    sb.append(dim("─" * layout.totalWidth))
    sb.append(' '); sb.append('\n')
  }

  /**
    * Builds a table column header from a pre-padded leading-column label.
    * Each sortable column underlines its [[Sort.key]] letter (`m̲ono`, `i̲nl`,
    * `c̲ls`, `t̲ime`, …) and the leading "(h̲ot)" annotation marks the hotness
    * sort key. The active column is rendered bold yellow; the rest bold cyan.
    *
    * Two callers — `def (hot)` for the def table, `mod (hot)` for the
    * module table. Both pad their leading label to [[Layout.nameWidth]].
    */
  private def buildHeader(leading: String, layout: Layout, activeSort: Sort): String = {
    val sb = new StringBuilder

    sb.append(sortableColumn(leading, Sort.Hotness, activeSort))

    if (layout.showLines) {
      sb.append(' '); sb.append(plainHeader(lpad("lines", 5)))
    }
    if (layout.showCounts) {
      sb.append(' '); sb.append(sortableColumn(lpad("mono", 4), Sort.Mono, activeSort))
      sb.append(' '); sb.append(sortableColumn(lpad("inl",  4), Sort.Inl,  activeSort))
      sb.append(' '); sb.append(sortableColumn(lpad("cls",  4), Sort.Cls,  activeSort))
      sb.append(' '); sb.append(sortableColumn(lpad("size", 5), Sort.Size, activeSort))
    }
    if (layout.showCns) {
      sb.append(' '); sb.append(sortableColumn(lpad("cns", 5), Sort.Cns,   activeSort))
      sb.append(' '); sb.append(sortableColumn(lpad("tv",  4), Sort.Tvars, activeSort))
      sb.append(' '); sb.append(sortableColumn(lpad("ev",  4), Sort.Evars, activeSort))
    }
    if (layout.showPhase) {
      sb.append(' '); sb.append(plainHeader(rpad("phase", 10)))
    }
    sb.append(' '); sb.append(sortableColumn(lpad("alloc", 5), Sort.Alloc, activeSort))
    sb.append(' '); sb.append(sortableColumn(lpad("time", 9), Sort.Time, activeSort))
    sb.append(' '); sb.append(plainHeader(lpad("%cpu", 6)))
    sb.append(' '); sb.append(plainHeader(lpad("%wall", 6)))
    sb.toString
  }

  /**
    * Renders one sortable column header — the first occurrence of
    * [[Sort.key]] in `padded` is underlined, and the whole label is bold
    * yellow when `sort == activeSort`, bold cyan otherwise. The key letter
    * comes from [[Sort]] itself so there is no separate keystroke
    * declaration to keep in sync with the input loop.
    */
  private def sortableColumn(padded: String, sort: Sort, activeSort: Sort): String =
    keyHeader(padded, padded.indexOf(sort.key), activeSort == sort)

  /**
    * Renders a sequence of [[Row]] values as table rows. The first cell is
    * row-type-specific (see [[Row.appendFirstCell]]); the trailing numeric
    * columns are shared via [[appendNumericFields]].
    */
  private def renderTable(sb: StringBuilder, rows: Vector[Row], layout: Layout): Unit = {
    for (r <- rows) {
      sb.append(' ')
      r.appendFirstCell(sb, layout)
      appendNumericFields(sb, r.cells, layout)
      sb.append(' '); sb.append('\n')
    }
  }

  /** Renders a centered placeholder message under an empty table header. */
  private def renderEmptyPlaceholder(sb: StringBuilder, msg: String, layout: Layout): Unit = {
    val pad = (((layout.totalWidth + 2) - msg.length) / 2).max(0)
    sb.append('\n')
    sb.append(" " * pad)
    sb.append(dim(msg))
    sb.append('\n')
  }

  /**
    * Renders the help screen: a static legend explaining each table column
    * and every keystroke the input loop understands. The dashboard + stats
    * lines are still drawn above this body, so compilation progress remains
    * visible while the user reads.
    *
    * The text is literal rather than derived from the [[Sort]] / [[PhaseFilter]]
    * ADTs — the description strings carry context that does not belong in
    * those data types. Add a row here when you add a new column or
    * keystroke; the input loop is the single source of truth for what
    * actually works.
    */
  private def renderHelp(sb: StringBuilder): Unit = {
    // The three groups mirror how Layout / the table renderer gate columns:
    // common columns are always shown; frontend columns only appear under
    // the `frontend` filter; backend columns only under the `backend` filter.
    sb.append("  "); sb.append(bold(cyan("Common columns"))); sb.append('\n')
    appendHelpCol(sb, "def",      "the fully qualified name of the def, with its source location (file:line) in parens")
    appendHelpCol(sb, "lines",    "the number of source-code lines spanned by the def's signature and body")
    appendHelpCol(sb, "phase",    "the compiler phase that consumed the most wall-clock time for the def")
    appendHelpCol(sb, "alloc",    "the amount of memory the compiler allocated while processing the def")
    appendHelpCol(sb, "time",     "the cumulative wall-clock time spent compiling the def")
    appendHelpCol(sb, "%cpu",     "the def's share of total CPU time, computed as time / (elapsed × threads)")
    appendHelpCol(sb, "%wall",    "the def's share of wall-clock time (time / elapsed); max savings if removed")
    sb.append('\n')

    sb.append("  "); sb.append(bold(cyan("Frontend columns"))); sb.append('\n')
    appendHelpCol(sb, "cns",      "the number of type constraints generated during type inference")
    appendHelpCol(sb, "tv",       "the number of type variables in the def's constraint system")
    appendHelpCol(sb, "ev",       "the number of effect variables in the def's constraint system")
    sb.append('\n')

    sb.append("  "); sb.append(bold(cyan("Backend columns"))); sb.append('\n')
    appendHelpCol(sb, "mono",     "the number of monomorphic copies created during monomorphization")
    appendHelpCol(sb, "inl",      "the number of times the def was inlined at a call site")
    appendHelpCol(sb, "cls",      "the number of .class files emitted for the def")
    appendHelpCol(sb, "size",     "the total bytecode size of all .class files emitted for the def")
    sb.append('\n')

    sb.append("  "); sb.append(bold(cyan("Navigation"))); sb.append('\n')
    appendHelpCol(sb, "Tab",      "switch between the per-def and per-module tables")
    appendHelpCol(sb, "a/f/b",    "filter to all / frontend / backend phases")
    appendHelpCol(sb, "?",        "toggle this help screen; Esc returns to the defs table")
  }

  /** Appends one column-description row. */
  private def appendHelpCol(sb: StringBuilder, name: String, desc: String): Unit = {
    sb.append("    ")
    sb.append(rpad(name, 10))
    sb.append("  ")
    sb.append(dim(desc))
    sb.append('\n')
  }

  /**
    * Appends the trailing numeric columns (LOC, mono / inl / cls, cns,
    * phase, time, %cpu, %wall) for one row, honoring the layout's
    * visibility flags. Always emits a leading separator before each column
    * it writes.
    *
    * [[RowCells.aggregate]] skips all warning colors on `time` / `%cpu` /
    * `%wall`. The `style*` thresholds are calibrated for individual defs;
    * at module scale, sums-across-defs trip the thresholds unconditionally
    * and the colors become noise. Counts (mono / inl / cls / cns) render
    * plain in both tables.
    */
  private def appendNumericFields(sb: StringBuilder, cells: RowCells, layout: Layout): Unit = {
    if (layout.showLines) {
      sb.append(' ')
      val linesStr = if (cells.lines > 0) cells.lines.toString else "-"
      sb.append(lpad(linesStr, 5))
    }
    if (layout.showCounts) {
      val mono = sumPhaseCounts(cells.byPhaseCount, MonoCountPhases)
      val cls  = sumPhaseCounts(cells.byPhaseCount, ClsCountPhases)
      sb.append(' '); sb.append(lpad(mono.toString, 4))
      sb.append(' '); sb.append(lpad(cells.inlined.toString, 4))
      sb.append(' '); sb.append(lpad(cls.toString, 4))
      sb.append(' '); sb.append(lpad(formatBytes(cells.classBytes), 5))
    }
    if (layout.showCns) {
      sb.append(' '); sb.append(lpad(cells.cns.toString, 5))
      sb.append(' '); sb.append(lpad(cells.tvars.toString, 4))
      sb.append(' '); sb.append(lpad(cells.evars.toString, 4))
    }
    if (layout.showPhase) {
      sb.append(' ')
      sb.append(rpad(truncate(cells.dominantPhase, 10), 10))
    }
    val allocField = lpad(formatBytes(cells.allocBytes), 5)
    val timeField = lpad(formatMillis(cells.nanos), 9)
    val cpuField  = f"${cells.pctCpu}%5.1f%%"
    val wallField = f"${cells.pctWall}%5.1f%%"
    sb.append(' '); sb.append(allocField)
    sb.append(' '); sb.append(if (cells.aggregate) timeField else styleTime(timeField, cells.nanos))
    sb.append(' '); sb.append(if (cells.aggregate) cpuField else stylePctCpu(cpuField, cells.pctCpu))
    sb.append(' '); sb.append(if (cells.aggregate) wallField else stylePctWall(wallField, cells.pctWall))
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
