/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.tools.perf

import ca.uwaterloo.flix.tools.compilertop.Ansi
import ca.uwaterloo.flix.tools.compilertop.Formatting.{lpad, rpad}

import java.nio.file.Path

/**
  * Renders a [[PerfState]] as text. A pure function of its inputs: no terminal IO, no clock.
  *
  * A frame drawn in place on a terminal uses ANSI styles, and every line is padded to exactly
  * `cols - 1` visible characters so that a frame drawn over a previous one leaves nothing behind,
  * and no line reaches the last column, which would make some terminals wrap. A plain frame, for
  * a log, has neither styles nor padding.
  */
object PerfRenderer {

  /** The width assumed when the terminal width is unknown. */
  val DefaultCols: Int = 100

  /** The smallest width the frame is laid out for. Lines wrap on a narrower terminal. */
  val MinCols: Int = 60

  /**
    * At this width and above the throughput line also shows the minimum and the maximum. The
    * full line is 85 columns with seven-digit throughputs, plus the one column left free.
    */
  val WideCols: Int = 90

  /** The width of the throughput sparkline. */
  val SparkWidth: Int = 24

  /** The widest the iteration bar gets. */
  val MaxBarWidth: Int = 50

  /** The block characters of a sparkline, low to high. */
  private val SparkChars: Array[Char] = "▁▂▃▄▅▆▇█".toArray

  /** The width of the experiment label column. */
  private val LabelWidth: Int = 28

  /** The widest the text after the bar gets: the elapsed time and the estimated time left. */
  private val TailWidth: Int = "  h:mm:ss  ~h:mm:ss left".length

  /**
    * Renders one frame. The result ends with a newline after every line, including the last.
    *
    * @param state the snapshot to render.
    * @param cols  the terminal width in columns.
    * @param ansi  whether to use ANSI styles and pad every line to the full width, as drawing in
    *              place requires. Off for a plain frame in a log.
    */
  def render(state: PerfState, cols: Int, ansi: Boolean): String = {
    // Lines are laid out for `cols` columns but padded one short so no line reaches the last column.
    val tier = cols.max(MinCols)
    val w = tier - 1
    val sb = new StringBuilder
    def line(f: Line => Unit): Unit = {
      val l = new Line(w, ansi)
      f(l)
      sb.append(l.result).append('\n')
    }

    line(renderHeader)
    line(_ => ())
    state.experiments.foreach(es => line(renderExperiment(state, es, _)))
    speedupLine(state).foreach(line)
    line(_ => ())
    val focus = state.focus
    line(renderThroughput(focus, tier, _))

    sb.toString
  }

  /**
    * Renders one completed run as a single plain line for non-interactive output.
    */
  def renderLogLine(e: Experiment, iteration: Int, n: Int, run: Run): String = {
    val digits = n.toString.length
    val which = if (iteration == 0) rpad("warm-up", 2 * digits + 5) else s"run ${lpad(iteration.toString, digits)}/$n"
    val tp = PerfStats.throughput(run.lines, run.time)
    s"[${e.key}] $which   ${groupThousands(tp)} l/s   total ${groupThousands(nanosToMillis(run.time))} ms"
  }

  /**
    * Renders the line that names the directory the result files were written to. The label is
    * aligned with the throughput label of the frame, which the line is printed below.
    */
  def renderResultsLine(dir: Path): String =
    s" results     $dir  (JSON data and plots.py)"

  // -- Lines ------------------------------------------------------------------

  private def renderHeader(l: Line): Unit = {
    l.add(" ")
    l.add("Flix Compiler Performance", Ansi.bold)
  }

  private def renderExperiment(state: PerfState, es: ExperimentState, l: Line): Unit = {
    val digits = state.n.toString.length
    val count = s"${lpad(es.runs.length.toString, digits)}/${state.n}"
    val label = rpad(experimentLabel(es), LabelWidth)
    if (es.startNanos.isEmpty) {
      l.add("   ")
      l.add(label, Ansi.dim)
      l.add(s" $count", Ansi.dim)
    } else {
      l.add(" ")
      if (es.isDone) l.add("✓", Ansi.green) else l.add("▶", Ansi.blue)
      l.add(" ")
      l.add(label)
      l.add(s" $count  ")
      // The bar gets the room left by the widest possible tail, so it keeps its width from the
      // first frame to the last rather than growing when the estimate disappears at the end.
      val barWidth = (l.width - l.used - TailWidth).min(MaxBarWidth).max(0)
      renderIterationBar(es.runs.length, es.isActive && state.measuring, state.n, barWidth, l)
      val elapsed = es.elapsedNanos(state.nowNanos).map(formatClock).getOrElse("0:00")
      val eta = if (es.isDone) "" else PerfStats.remainingNanos(es, state.n).map(r => s"  ~${formatClock(r)} left").getOrElse("")
      l.add(s"  $elapsed$eta", Ansi.dim)
    }
  }

  /**
    * Renders the bar of iterations: finished ones filled, the current one half-filled, the rest
    * empty. When there are more runs than cells, each cell stands for several runs.
    */
  private def renderIterationBar(done: Int, running: Boolean, n: Int, width: Int, l: Line): Unit = {
    if (width <= 0 || n <= 0) return
    val cells = n.min(width)
    val filled = ((done.toLong * cells) / n).toInt.min(cells)
    val current = if (running && filled < cells) 1 else 0
    l.add("█" * filled, Ansi.blue)
    l.add("▓" * current, Ansi.blue)
    l.add("░" * (cells - filled - current), Ansi.dim)
  }

  private def speedupLine(state: PerfState): Option[Line => Unit] = {
    for {
      slow <- state.get(Experiment.SingleThreaded)
      fast <- state.get(Experiment.Parallel)
      sp <- PerfStats.speedup(slow.runs, fast.runs)
    } yield {
      (l: Line) =>
        l.add("   ")
        l.add(s"speedup ${slow.threads} → ${fast.threads} threads   ", Ansi.dim)
        l.add(s"${formatOneDecimal(sp)}×", Ansi.bold)
    }
  }

  private def renderThroughput(focus: Option[ExperimentState], tier: Int, l: Line): Unit = {
    l.add(" throughput  ")
    focus.flatMap(es => PerfStats.throughputSummary(es.runs)) match {
      case None =>
        l.add("waiting for the first run", Ansi.dim)
      case Some(t) =>
        val series = focus.map(es => PerfStats.throughputs(es.runs).map(_.toDouble)).getOrElse(Vector.empty)
        l.add(rpad(sparkline(series, SparkWidth), SparkWidth), Ansi.cyan)
        l.add("  ")
        if (tier >= WideCols) l.add(s"min ${groupThousands(t.min)}   ")
        l.add(s"med ${groupThousands(t.median)}", Ansi.blue)
        if (tier >= WideCols) l.add(s"   max ${groupThousands(t.max)}", Ansi.green)
        l.add(" l/s", Ansi.dim)
    }
  }

  // -- Helpers ----------------------------------------------------------------

  /** A human-readable label such as `10 threads, non-incremental`. */
  def experimentLabel(es: ExperimentState): String = {
    val threads = if (es.threads == 1) "1 thread" else s"${es.threads} threads"
    val mode = if (es.experiment.incremental) "incremental" else "non-incremental"
    s"$threads, $mode"
  }

  /**
    * Renders `values` as a sparkline of at most `width` characters, scaled between the minimum
    * and the maximum. When there are more values than cells, each cell shows the mean of its
    * bucket. A constant series renders at mid height.
    */
  def sparkline(values: Seq[Double], width: Int): String = {
    if (values.isEmpty || width <= 0) return ""
    val buckets =
      if (values.length <= width) values
      else (0 until width).map {
        i =>
          val from = (i.toLong * values.length / width).toInt
          val to = ((i + 1).toLong * values.length / width).toInt.max(from + 1)
          val slice = values.slice(from, to)
          slice.sum / slice.length
      }
    val lo = buckets.min
    val hi = buckets.max
    val span = hi - lo
    buckets.map {
      v =>
        if (span == 0.0) SparkChars(SparkChars.length / 2)
        else SparkChars(((v - lo) / span * (SparkChars.length - 1)).round.toInt.min(SparkChars.length - 1))
    }.mkString
  }

  /** Formats `n` with a comma between every three digits, independent of the locale. */
  def groupThousands(n: Long): String = {
    val digits = Math.abs(n).toString
    val grouped = digits.reverse.grouped(3).mkString(",").reverse
    if (n < 0) "-" + grouped else grouped
  }

  /** Formats a duration as `m:ss`, or `h:mm:ss` from one hour. */
  def formatClock(nanos: Long): String = {
    val totalSeconds = (nanos.max(0L) / 1_000_000_000L).toInt
    val h = totalSeconds / 3600
    val m = (totalSeconds % 3600) / 60
    val s = totalSeconds % 60
    if (h > 0) s"$h:${twoDigits(m)}:${twoDigits(s)}" else s"$m:${twoDigits(s)}"
  }

  /** Formats `x` with one decimal, using a period regardless of the locale. */
  def formatOneDecimal(x: Double): String = {
    val tenths = Math.round(Math.abs(x) * 10.0)
    val sign = if (x < 0.0 && tenths > 0) "-" else ""
    s"$sign${tenths / 10}.${tenths % 10}"
  }

  private def twoDigits(n: Int): String = if (n < 10) s"0$n" else n.toString

  private def nanosToMillis(nanos: Long): Long = Math.round(nanos / 1_000_000.0)

  /**
    * Accumulates one line of output while tracking its visible width. Text past `width` is
    * dropped. With `ansi` set, styles are applied and [[result]] pads the line to exactly `width`
    * visible characters; otherwise the text is plain and unpadded.
    */
  private final class Line(val width: Int, ansi: Boolean) {
    private val sb = new StringBuilder
    private var visible = 0

    /** The visible characters used so far. */
    def used: Int = visible

    /** Appends `plain`, truncated to the remaining width. */
    def add(plain: String): Unit = add(plain, identity)

    /** Appends `plain` wrapped in `style`, truncated to the remaining width. */
    def add(plain: String, style: String => String): Unit = {
      val room = width - visible
      if (room <= 0 || plain.isEmpty) return
      val text = if (plain.length <= room) plain else plain.substring(0, room)
      sb.append(if (ansi) style(text) else text)
      visible += text.length
    }

    /** The line, padded to `width` visible characters when `ansi` is set. */
    def result: String = {
      if (ansi && visible < width) add(" " * (width - visible))
      sb.toString
    }
  }

}
