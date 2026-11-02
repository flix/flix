/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.tools.perf

/**
  * A single measured compilation.
  *
  * @param lines        the number of source lines compiled.
  * @param frontendTime the time spent in the frontend (`check`) in nanoseconds.
  * @param backendTime  the time spent in the backend (`codeGen`) in nanoseconds. Zero when only
  *                     the frontend is benchmarked (`--frontend`).
  * @param phases       the time of each phase in nanoseconds, in execution order. A phase name may
  *                     occur more than once if the phase ran more than once.
  */
final case class Run(lines: Int, frontendTime: Long, backendTime: Long, phases: List[(String, Long)]) {

  /** The total compilation time in nanoseconds: the frontend time plus the backend time. */
  def time: Long = frontendTime + backendTime

}

/**
  * One of the experiments run by `Xperf`.
  */
sealed trait Experiment {
  /** A short identifier used as the prefix of log lines. */
  def key: String

  /** Whether the experiment compiles incrementally. */
  def incremental: Boolean
}

object Experiment {

  /** Single-threaded and non-incremental. A fresh compiler is created for every run. */
  case object SingleThreaded extends Experiment {
    val key: String = "1t"
    val incremental: Boolean = false
  }

  /** Multi-threaded and non-incremental. A fresh compiler is created for every run. */
  case object Parallel extends Experiment {
    val key: String = "par"
    val incremental: Boolean = false
  }

  /** Multi-threaded and incremental. One compiler is recompiled with unchanged inputs. */
  case object ParallelIncremental extends Experiment {
    val key: String = "inc"
    val incremental: Boolean = true
  }

}

/**
  * The progress of one experiment.
  *
  * @param experiment the experiment.
  * @param threads    the number of compiler threads the experiment uses.
  * @param runs       the completed measured runs. Warm-up runs are not included.
  * @param startNanos the time the first run started, if any.
  * @param endNanos   the time the experiment finished, if it has.
  */
final case class ExperimentState(experiment: Experiment, threads: Int, runs: Vector[Run], startNanos: Option[Long], endNanos: Option[Long]) {

  /** Whether the experiment is currently running. */
  def isActive: Boolean = startNanos.nonEmpty && endNanos.isEmpty

  /** Whether the experiment has finished. */
  def isDone: Boolean = endNanos.nonEmpty

  /** The wall-clock time spent in the experiment so far, or `None` if it has not started. */
  def elapsedNanos(nowNanos: Long): Option[Long] = startNanos.map(s => endNanos.getOrElse(nowNanos) - s)

}

/**
  * Everything the dashboard renders, as an immutable snapshot.
  *
  * The benchmark thread updates the experiments; the render thread adds the current time when
  * it takes a frame.
  *
  * @param n           the number of measured runs per experiment.
  * @param experiments the experiments in execution order.
  * @param measuring   whether a measured run is executing right now. False between runs and
  *                    during a warm-up run.
  * @param nowNanos    the time the snapshot was taken.
  */
final case class PerfState(n: Int, experiments: Vector[ExperimentState], measuring: Boolean, nowNanos: Long) {

  /** The state of the given experiment, if it is part of the benchmark. */
  def get(e: Experiment): Option[ExperimentState] = experiments.find(_.experiment == e)

  /** The experiment currently running, if any. */
  def active: Option[ExperimentState] = experiments.find(_.isActive)

  /** The experiment whose statistics are shown: the active one, or else the last one with runs. */
  def focus: Option[ExperimentState] = active.orElse(experiments.filter(_.runs.nonEmpty).lastOption)

  /** Marks the given experiment as started at `nanos`, if it has not started already. */
  def withStarted(e: Experiment, nanos: Long): PerfState = update(e) {
    es => if (es.startNanos.isDefined) es else es.copy(startNanos = Some(nanos))
  }

  /** Records a completed measured run of the given experiment. */
  def withRun(e: Experiment, run: Run): PerfState = update(e) {
    es => es.copy(runs = es.runs :+ run)
  }

  /** Marks the given experiment as finished at `nanos`. */
  def withEnded(e: Experiment, nanos: Long): PerfState = update(e) {
    es => es.copy(endNanos = Some(nanos))
  }

  private def update(e: Experiment)(f: ExperimentState => ExperimentState): PerfState =
    copy(experiments = experiments.map(es => if (es.experiment == e) f(es) else es))

}
