/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.tools.perf

import ca.uwaterloo.flix.util.StatUtils

/**
  * Pure statistics over completed runs. No formatting and no terminal IO.
  *
  * The medians use [[StatUtils.median]] so the live numbers agree with the JSON files that
  * `Xperf` writes.
  */
object PerfStats {

  /**
    * The throughput of the runs so far, in lines per second.
    */
  final case class Throughput(min: Int, median: Int, max: Int)

  /** Returns the throughput in lines per second, or zero if `nanos` is zero. */
  def throughput(lines: Int, nanos: Long): Int =
    if (nanos == 0L) 0 else ((1_000_000_000L * lines).toDouble / nanos.toDouble).toInt

  /** Returns the throughput of each run. */
  def throughputs(runs: Vector[Run]): Vector[Int] = runs.map(r => throughput(r.lines, r.time))

  /** Summarizes the throughput of the runs so far, or `None` if there are no runs. */
  def throughputSummary(runs: Vector[Run]): Option[Throughput] = {
    if (runs.isEmpty) return None
    val ts = throughputs(runs)
    Some(Throughput(ts.min, StatUtils.median(ts).toInt, ts.max))
  }

  /**
    * Estimates the time remaining in the experiment as the median run time multiplied by the
    * number of runs left, or `None` if no run has completed.
    */
  def remainingNanos(es: ExperimentState, n: Int): Option[Long] = {
    if (es.runs.isEmpty) return None
    val medianNanos = StatUtils.median(es.runs.map(_.time)).toLong
    Some(medianNanos * (n - es.runs.length).max(0))
  }

  /**
    * Returns the speedup of `fast` over `slow` as the ratio of their median total times, or
    * `None` if either has no runs.
    */
  def speedup(slow: Vector[Run], fast: Vector[Run]): Option[Double] = {
    if (slow.isEmpty || fast.isEmpty) return None
    val fastTotal = StatUtils.median(fast.map(_.time))
    if (fastTotal == 0.0) None else Some(StatUtils.median(slow.map(_.time)) / fastTotal)
  }

}
