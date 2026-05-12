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
  * @param layout          column layout produced by [[Layout.compute]].
  * @param visible         def-table rows, already filtered / sorted / trimmed.
  * @param modules         module-table rows, already aggregated / sorted /
  *                        trimmed.
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
  private final case class RowCells(locLines: Int,
                                    byPhaseCount: Map[String, Long],
                                    inlined: Long,
                                    cns: Long,
                                    tvars: Long,
                                    evars: Long,
                                    dominantPhase: String,
                                    nanos: Long,
                                    pctCpu: Double,
                                    pctWall: Double,
                                    aggregate: Boolean)

  /**
    * A renderable table row. The numeric trailing columns are uniform
    * across def and module rows ([[RowCells]] / [[Renderer.appendNumericFields]]);
    * only the leading "first cell" differs (sym + active marker + location
    * for defs; a single wide module-name field for modules).
    */
  private sealed trait Row {
    /** The trailing numeric columns for this row, ready for [[Renderer.appendNumericFields]]. */
    def cells: RowCells
    /** Appends this row's leading (type-specific) cell to `sb`. */
    def appendFirstCell(sb: StringBuilder, layout: Layout): Unit
  }

  /** Def-table row: hotness-colored sym, active marker, location column. */
  private final case class DefnRow(s: DefnStats, safeElapsed: Double, parallelism: Int) extends Row {
    private val locLines = locLineCount(s.loc)

    /** Numeric cells for this def, with `aggregate = false` so warning colors apply. */
    val cells: RowCells = {
      val pctWall = 100.0 * s.totalNanos / safeElapsed
      RowCells(
        locLines      = locLines,
        byPhaseCount  = s.byPhaseCount,
        inlined       = s.inlined,
        cns           = s.cns,
        tvars         = s.tvars,
        evars         = s.evars,
        dominantPhase = s.dominantPhase.getOrElse("?"),
        nanos         = s.totalNanos,
        pctCpu        = pctWall / parallelism,
        pctWall       = pctWall,
        aggregate     = false,
      )
    }

    /** Appends the sym name (hotness-colored, with `*` if active) and a dim location field. */
    def appendFirstCell(sb: StringBuilder, layout: Layout): Unit = {
      val nameMax  = layout.symWidth - 1
      val nameText = truncate(s.sym.name, nameMax)
      val locField = rpad(truncate(formatLocation(s.loc), layout.locWidth), layout.locWidth)
      val marker   = if (s.isActive) yellow("*") else " "
      sb.append(styleSym(nameText, s.totalNanos, locLines))
      sb.append(marker)
      sb.append(" " * (nameMax - nameText.length))
      sb.append(' ')
      sb.append(dim(locField))
    }
  }

  /** Module-table row: one wide name field spanning sym + location widths. */
  private final case class ModuleRow(m: ModuleStats, safeElapsed: Double, parallelism: Int) extends Row {
    /** Numeric cells for this module, with `aggregate = true` so per-def warning colors are skipped. */
    val cells: RowCells = {
      val pctWall = 100.0 * m.totalNanos / safeElapsed
      RowCells(
        locLines      = m.totalLocLines,
        byPhaseCount  = m.byPhaseCount,
        inlined       = m.totalInlined,
        cns           = m.totalCns,
        tvars         = m.totalTvars,
        evars         = m.totalEvars,
        dominantPhase = m.dominantPhase.getOrElse("?"),
        nanos         = m.totalNanos,
        pctCpu        = pctWall / parallelism,
        pctWall       = pctWall,
        aggregate     = true,
      )
    }

    /** Appends the module name, padded to span the def-table's sym + location columns. */
    def appendFirstCell(sb: StringBuilder, layout: Layout): Unit = {
      val modWidth = layout.symWidth + 1 + layout.locWidth
      sb.append(rpad(truncate(m.module, modWidth), modWidth))
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

    val safeElapsed = state.elapsed.max(1L).toDouble
    val defRows: Vector[Row] = state.visible.map(s => DefnRow(s, safeElapsed, state.parallelism))
    val modRows: Vector[Row] = state.modules.map(m => ModuleRow(m, safeElapsed, state.parallelism))

    // Def table: header, then rows or a centered placeholder if empty.
    renderHeaderRow(sb, rpad("Def (hot)", state.layout.symWidth), withLocation = true, state.layout, state.activeSort)
    if (defRows.isEmpty) renderEmptyPlaceholder(sb, "(no timings yet)", state.layout)
    else renderTable(sb, defRows, state.layout)

    // Module table: only when non-empty, separated by a blank line.
    if (modRows.nonEmpty) {
      val modWidth = state.layout.symWidth + 1 + state.layout.locWidth
      sb.append('\n')
      renderHeaderRow(sb, rpad("Module (hot)", modWidth), withLocation = false, state.layout, state.activeSort)
      renderTable(sb, modRows, state.layout)
    }

    sb.append(EndSync)
    sb.toString
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
    sb.append(renderFilterLegend(state.activeFilter))
    sb.append('\n')
  }

  /**
    * Renders the `[a|f|b]` legend with the active filter highlighted in
    * bold yellow. Always visible so the user sees the available toggles
    * whether the filter is active or not. Driven entirely by
    * [[PhaseFilter.all]]: adding a new filter is a one-line change in
    * [[Model]] and the legend follows automatically.
    */
  private def renderFilterLegend(active: PhaseFilter): String = {
    val entries = PhaseFilter.all.map(f => filterLegendEntry(f, f == active))
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
    sb.append('\n')
  }

  /**
    * Prints the parametrized header row + the divider underneath it. The
    * `leading` argument is a pre-padded first-column label (either
    * `"Def (hot)"` at `symWidth` or `"Module (hot)"` at the combined
    * sym+loc width). `withLocation` toggles the second `"location"` header,
    * which the def table has and the module table doesn't.
    */
  private def renderHeaderRow(sb: StringBuilder, leading: String, withLocation: Boolean, layout: Layout, activeSort: Sort): Unit = {
    sb.append(' ')
    sb.append(buildHeader(leading, withLocation, layout, activeSort))
    sb.append(' '); sb.append('\n')
    sb.append(' ')
    sb.append(dim("─" * layout.totalWidth))
    sb.append(' '); sb.append('\n')
  }

  /**
    * Builds a table column header from a pre-padded leading-column label
    * and a `withLocation` flag. Each sortable column underlines its
    * [[Sort.key]] letter (`m̲ono`, `o̲pt`, `c̲ls`, `t̲ime`, …) and the leading
    * "(h̲ot)" annotation marks the hotness sort key. The active column is
    * rendered bold yellow; the rest bold cyan.
    *
    * Two callers — `Def (hot)` + a `"location"` column for the def table,
    * `Module (hot)` (single wide column, no location) for the module table.
    */
  private def buildHeader(leading: String, withLocation: Boolean, layout: Layout, activeSort: Sort): String = {
    val sb = new StringBuilder

    sb.append(sortableColumn(leading, Sort.Hotness, activeSort))

    if (withLocation) {
      sb.append(' ')
      sb.append(plainHeader(rpad("location", layout.locWidth)))
    }
    if (layout.showLOC) {
      sb.append(' '); sb.append(plainHeader(lpad("LOC", 4)))
    }
    if (layout.showCounts) {
      sb.append(' '); sb.append(sortableColumn(lpad("mono", 4), Sort.Mono, activeSort))
      sb.append(' '); sb.append(sortableColumn(lpad("opt",  4), Sort.Opt,  activeSort))
      sb.append(' '); sb.append(sortableColumn(lpad("inl",  4), Sort.Inl,  activeSort))
      sb.append(' '); sb.append(sortableColumn(lpad("cls",  4), Sort.Cls,  activeSort))
    }
    if (layout.showCns) {
      sb.append(' '); sb.append(sortableColumn(lpad("cns", 5), Sort.Cns,   activeSort))
      sb.append(' '); sb.append(sortableColumn(lpad("tv",  4), Sort.Tvars, activeSort))
      sb.append(' '); sb.append(sortableColumn(lpad("ev",  4), Sort.Evars, activeSort))
    }
    if (layout.showPhase) {
      sb.append(' '); sb.append(plainHeader(rpad("phase", 10)))
    }
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
    * Appends the trailing numeric columns (LOC, mono / opt / cls, cns,
    * phase, time, %cpu, %wall) for one row, honoring the layout's
    * visibility flags. Always emits a leading separator before each column
    * it writes.
    *
    * [[RowCells.aggregate]] skips all warning colors on `time` / `%cpu` /
    * `%wall`. The `style*` thresholds are calibrated for individual defs;
    * at module scale, sums-across-defs trip the thresholds unconditionally
    * and the colors become noise. Counts (mono / opt / cls / cns) render
    * plain in both tables.
    */
  private def appendNumericFields(sb: StringBuilder, cells: RowCells, layout: Layout): Unit = {
    if (layout.showLOC) {
      sb.append(' ')
      val locStr = if (cells.locLines > 0) cells.locLines.toString else "-"
      sb.append(lpad(locStr, 4))
    }
    if (layout.showCounts) {
      val mono = sumPhaseCounts(cells.byPhaseCount, MonoCountPhases)
      val opt  = sumPhaseCounts(cells.byPhaseCount, OptCountPhases)
      val cls  = sumPhaseCounts(cells.byPhaseCount, ClsCountPhases)
      sb.append(' '); sb.append(lpad(mono.toString, 4))
      sb.append(' '); sb.append(lpad(opt.toString, 4))
      sb.append(' '); sb.append(lpad(cells.inlined.toString, 4))
      sb.append(' '); sb.append(lpad(cls.toString, 4))
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
    val timeField = lpad(formatMillis(cells.nanos), 9)
    val cpuField  = f"${cells.pctCpu}%5.1f%%"
    val wallField = f"${cells.pctWall}%5.1f%%"
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
