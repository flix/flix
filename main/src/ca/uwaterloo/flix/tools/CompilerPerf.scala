/*
 * Copyright 2023 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.api.{CompilerConstants, Flix, PhaseTime}
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.language.phase.unification.EffUnification3
import ca.uwaterloo.flix.util.StatUtils.{minimum, average, median}
import ca.uwaterloo.flix.util.collection.ListOps
import ca.uwaterloo.flix.util.{FileOps, LocalResource, Options}
import org.json4s.JValue
import org.json4s.JsonDSL.*

import java.nio.file.Path

object CompilerPerf {

  /**
    * The default number of compilations.
    */
  private val DefaultN: Int = 50

  /**
    * The number of threads to use for the single-thread experiment.
    */
  private val MinThreads: Int = 1

  /**
    * The number of threads to use for the multi-threaded experiment.
    */
  private val MaxThreads: Int = Runtime.getRuntime.availableProcessors()

  /**
    * The Python program used to generate graphs.
    */
  private val Python =
    """
      |# $ pip install pandas
      |
      |import json
      |import matplotlib
      |import matplotlib.pyplot as plt
      |
      |with open('speedupWithPar.json', 'r') as file:
      |    data = json.load(file)
      |    minThreads = data['minThreads']
      |    maxThreads = data['maxThreads']
      |    xvalues = list(map(lambda obj: obj['phase'], data['results']))
      |    yvalues = list(map(lambda obj: obj['speedup'], data['results']))
      |
      |    fig, ax = plt.subplots()
      |    bars = ax.bar(xvalues, yvalues)
      |
      |    ax.set_title(f'Parallel Speedup ({minThreads} vs. {maxThreads} threads, non-incremental)')
      |    ax.set_ylabel('Speedup')
      |    ax.bar_label(bars, fmt='\n%.1fx')
      |
      |    plt.xticks(rotation=90)
      |    plt.subplots_adjust(bottom=0.30)
      |    plt.ylim(1, 10)
      |
      |    plt.savefig('speedupWithPar.json.png')
      |
      |with open('incrementalism.json', 'r') as file:
      |    data = json.load(file)
      |    threads = data['threads']
      |    xvalues = list(map(lambda obj: obj['phase'], data['results']))
      |    yvalues = list(map(lambda obj: obj['ratio'], data['results']))
      |
      |    fig, ax = plt.subplots()
      |    bars = ax.bar(xvalues, yvalues)
      |
      |    ax.set_title(f'Incrementalism (1.0x is best) ({threads} threads)')
      |    ax.set_ylabel('Incremental Ratio')
      |    ax.bar_label(bars, fmt='\n%.1fx')
      |
      |    plt.xticks(rotation=90)
      |    plt.subplots_adjust(bottom=0.30)
      |    plt.ylim(0.0, 1.1)
      |
      |    plt.savefig('incrementalism.json.png')
      |
      |with open('throughput.json', 'r') as file:
      |    data = json.load(file)
      |    threads = data['threads']
      |    maxy = data['plot']['maxy']
      |    xvalues = list(map(lambda obj: obj['i'], data['results']))
      |    yvalues = list(map(lambda obj: obj['throughput'], data['results']))
      |
      |    fig, ax = plt.subplots()
      |    ax.bar(xvalues, yvalues)
      |
      |    ax.set_title(f'Throughput ({threads} threads, non-incremental)')
      |    ax.set_ylabel('Throughput (lines/sec)')
      |
      |    plt.xticks(rotation=90)
      |    plt.ylim(1, maxy)
      |
      |    plt.savefig('throughput.json.png')
      |
      |with open('throughputWithPar.json', 'r') as file:
      |    data = json.load(file)
      |    threads = data['threads']
      |    maxy = data['plot']['maxy']
      |    xvalues = list(map(lambda obj: obj['i'], data['results']))
      |    yvalues = list(map(lambda obj: obj['throughput'], data['results']))
      |
      |    fig, ax = plt.subplots()
      |    ax.bar(xvalues, yvalues)
      |
      |    ax.set_title(f'Throughput ({threads} threads, non-incremental)')
      |    ax.set_ylabel('Throughput (lines/sec)')
      |
      |    plt.xticks(rotation=90)
      |    plt.ylim(1, maxy)
      |
      |    plt.savefig('throughputWithPar.json.png')
      |
      |with open('throughputWithParInc.json', 'r') as file:
      |    data = json.load(file)
      |    threads = data['threads']
      |    maxy = data['plot']['maxy']
      |    xvalues = list(map(lambda obj: obj['i'], data['results']))
      |    yvalues = list(map(lambda obj: obj['throughput'], data['results']))
      |
      |    fig, ax = plt.subplots()
      |    ax.bar(xvalues, yvalues)
      |
      |    ax.set_title(f'Throughput ({threads} threads, incremental)')
      |    ax.set_ylabel('Throughput (lines/sec)')
      |
      |    plt.xticks(rotation=90)
      |    plt.ylim(1, maxy)
      |
      |    plt.savefig('throughputWithParInc.json.png')
      |
      |with open('time.json', 'r') as file:
      |    data = json.load(file)
      |    threads = data['threads']
      |    xvalues = list(map(lambda obj: obj['phase'], data['results']))
      |    yvalues = list(map(lambda obj: obj['time'], data['results']))
      |
      |    fig, ax = plt.subplots()
      |    ax.bar(xvalues, yvalues)
      |
      |    ax.set_title(f'Time per Phase ({threads} threads, non-incremental)')
      |    ax.set_ylabel('Time (ms)')
      |
      |    plt.xticks(rotation=90)
      |    plt.subplots_adjust(bottom=0.30)
      |    plt.ylim(1)
      |
      |    plt.savefig('time.json.png')
      |
      |with open('timeWithPar.json', 'r') as file:
      |    data = json.load(file)
      |    threads = data['threads']
      |    xvalues = list(map(lambda obj: obj['phase'], data['results']))
      |    yvalues = list(map(lambda obj: obj['time'], data['results']))
      |
      |    fig, ax = plt.subplots()
      |    ax.bar(xvalues, yvalues)
      |
      |    ax.set_title(f'Time per Phase ({threads} threads, non-incremental)')
      |    ax.set_ylabel('Time (ms)')
      |
      |    plt.xticks(rotation=90)
      |    plt.subplots_adjust(bottom=0.30)
      |    plt.ylim(1)
      |
      |    plt.savefig('timeWithPar.json.png')
      |
      |with open('timeWithParInc.json', 'r') as file:
      |    data = json.load(file)
      |    threads = data['threads']
      |    xvalues = list(map(lambda obj: obj['phase'], data['results']))
      |    yvalues = list(map(lambda obj: obj['time'], data['results']))
      |
      |    fig, ax = plt.subplots()
      |    ax.bar(xvalues, yvalues)
      |
      |    ax.set_title(f'Time per Phase ({threads} threads, incremental)')
      |    ax.set_ylabel('Time (ms)')
      |
      |    plt.xticks(rotation=90)
      |    plt.subplots_adjust(bottom=0.30)
      |    plt.ylim(1)
      |
      |    plt.savefig('timeWithParInc.json.png')
      |
      |""".stripMargin

  /**
    * A single compilation: the number of source lines, the time spent in the frontend (`check`)
    * and in the backend (`codeGen`), and the time of every phase.
    *
    * The backend time is zero when only the frontend is benchmarked (`--frontend`).
    */
  case class Run(lines: Int, frontendTime: Long, backendTime: Long, phases: List[(String, Long)])

  /**
    * The merged results of a sequence of compilations (see [[aggregate]]).
    */
  case class Runs(lines: Int, frontendTimes: List[Long], backendTimes: List[Long], phases: List[(String, List[Long])]) {
    /** The total compilation time of every run. */
    def totalTimes: List[Long] = ListOps.zip(frontendTimes, backendTimes).map { case (f, b) => f + b }
  }

  /**
    * Run compiler performance experiments.
    */
  def run(opts: Options): Unit = {
    // Options
    val o = opts.copy(progress = false)

    // The number of iterations.
    val N = o.XPerfN.getOrElse(DefaultN)

    // The experiments to run.
    val mode = o.XPerfMode
    val runSeq = mode == XPerfMode.Sequential || mode == XPerfMode.All
    val runPar = mode == XPerfMode.Parallel || mode == XPerfMode.All
    val runParInc = mode == XPerfMode.All

    // Run the experiments.
    val baseline = aggregate(if (runSeq) perfBaseLine(N, o) else IndexedSeq.empty)
    val baselineWithPar = aggregate(if (runPar) perfBaseLineWithPar(N, o) else IndexedSeq.empty)
    val baselineWithParInc = aggregate(if (runParInc) perfBaseLineWithParInc(N, o) else IndexedSeq.empty)

    // The primary experiment, whose runs drive the summary, and the number of threads it used.
    val (primary, threads) = mode match {
      case XPerfMode.Sequential => (baseline, MinThreads)
      case XPerfMode.Parallel | XPerfMode.All => (baselineWithPar, MaxThreads)
    }

    // Find the number of lines of source code.
    val lines = primary.lines.toLong

    // Compute the total time in seconds.
    val totalTime = (primary.totalTimes.sum / 1_000_000_000L).toInt

    // Whether only the frontend was benchmarked, in which case there is no backend and no overall throughput.
    val frontendOnly = o.XPerfFrontend

    // The frontend, backend, and overall throughput statistics.
    //
    // Note: The throughputs are not additive. A run's overall time is the sum of its frontend and
    // backend times, so the overall throughput is always below both the frontend and the backend throughput.
    val frontendStats = throughputStats(lines, primary.frontendTimes)
    val backendStats = if (frontendOnly) None else Some(throughputStats(lines, primary.backendTimes))
    val overallStats = if (frontendOnly) None else Some(throughputStats(lines, primary.totalTimes))

    // The statistics shown on the console: the overall throughput, or the frontend throughput with `--frontend`.
    val shown = overallStats.getOrElse(frontendStats)

    // Best observed throughput.
    val maxObservedThroughput = throughput(lines, minimum(baseline.totalTimes ::: baselineWithPar.totalTimes ::: baselineWithParInc.totalTimes))

    // Timestamp (in seconds) when the experiment was run.
    val timestamp = System.currentTimeMillis() / 1000

    //
    // Speedup
    //
    val speedupPar =
      ("timestamp" -> timestamp) ~
        ("minThreads" -> MinThreads) ~
        ("maxThreads" -> MaxThreads) ~
        ("incremental" -> false) ~
        ("lines" -> lines) ~
        ("results" -> ListOps.zipTruncate(baseline.phases, baselineWithPar.phases).map {
          case ((phase, times1), (_, times2)) =>
            ("phase" -> phase) ~ ("speedup" -> median(ListOps.zip(times1, times2).map(p => p._1.toDouble / p._2.toDouble)))
        })
    writeFile("speedupWithPar.json", speedupPar, o)

    //
    // Incrementalism
    //
    val incrementalism =
      ("timestamp" -> timestamp) ~
        ("threads" -> MaxThreads) ~
        ("incremental" -> true) ~
        ("lines" -> lines) ~
        ("results" -> ListOps.zipTruncate(baselineWithPar.phases, baselineWithParInc.phases).map {
          case ((phase, times1), (_, times2)) =>
            ("phase" -> phase) ~ ("ratio" -> median(ListOps.zip(times1, times2).map(p => Math.max(0.0, 1.toDouble - (p._2.toDouble / p._1.toDouble)))))
        })
    writeFile("incrementalism.json", incrementalism, o)

    //
    // Throughput
    //
    val throughoutBaseLine =
      ("timestamp" -> timestamp) ~
        ("threads" -> MinThreads) ~
        ("incremental" -> false) ~
        ("lines" -> lines) ~
        ("plot" -> ("maxy" -> maxObservedThroughput)) ~
        ("results" -> baseline.totalTimes.zipWithIndex.map({
          case (time, i) => ("i" -> s"Run $i") ~ ("throughput" -> throughput(lines, time))
        }))
    writeFile("throughput.json", throughoutBaseLine, o)

    val throughputPar =
      ("timestamp" -> timestamp) ~
        ("threads" -> MaxThreads) ~
        ("incremental" -> false) ~
        ("lines" -> lines) ~
        ("plot" -> ("maxy" -> maxObservedThroughput)) ~
        ("results" -> baselineWithPar.totalTimes.zipWithIndex.map({
          case (time, i) => ("i" -> s"Run $i") ~ ("throughput" -> throughput(lines, time))
        }))
    writeFile("throughputWithPar.json", throughputPar, o)

    val throughputParInc =
      ("timestamp" -> timestamp) ~
        ("threads" -> MaxThreads) ~
        ("incremental" -> true) ~
        ("lines" -> lines) ~
        ("plot" -> ("maxy" -> maxObservedThroughput)) ~
        ("results" -> baselineWithParInc.totalTimes.zipWithIndex.map({
          case (time, i) => ("i" -> s"Run $i") ~ ("throughput" -> throughput(lines, time))
        }))
    writeFile("throughputWithParInc.json", throughputParInc, o)

    //
    // Time
    //
    val timeBaseline =
      ("timestamp" -> timestamp) ~
        ("threads" -> MinThreads) ~
        ("incremental" -> false) ~
        ("lines" -> lines) ~
        ("results" -> baseline.phases.map {
          case (phase, times) => ("phase" -> phase) ~ ("time" -> milliseconds(median(times)))
        })
    writeFile("time.json", timeBaseline, o)

    val timeWithPar =
      ("timestamp" -> timestamp) ~
        ("threads" -> MaxThreads) ~
        ("incremental" -> false) ~
        ("lines" -> lines) ~
        ("results" -> baselineWithPar.phases.map {
          case (phase, times) => ("phase" -> phase) ~ ("time" -> milliseconds(median(times)))
        })
    writeFile("timeWithPar.json", timeWithPar, o)

    val timeWithParInc =
      ("timestamp" -> timestamp) ~
        ("threads" -> MaxThreads) ~
        ("incremental" -> true) ~
        ("lines" -> lines) ~
        ("results" -> baselineWithParInc.phases.map {
          case (phase, times) => ("phase" -> phase) ~ ("time" -> milliseconds(median(times)))
        })
    writeFile("timeWithParInc.json", timeWithParInc, o)

    //
    // Summary
    //
    // The `backend` and `overall` fields are omitted (rendered as `JNothing`) with `--frontend`.
    val throughputJSON =
      ("frontend" -> frontendStats.toJSON) ~
        ("backend" -> backendStats.map(_.toJSON)) ~
        ("overall" -> overallStats.map(_.toJSON))

    val summaryJSON =
      ("timestamp" -> timestamp) ~
        ("threads" -> threads) ~
        ("lines" -> lines) ~
        ("iterations" -> N) ~
        ("throughput" -> throughputJSON)
    writeFile("summary.json", summaryJSON, o)

    //
    // Python Plot
    //
    FileOps.writeString(CompilerConstants.PerfDirectory.resolve("plots.py"), Python)

    println("~~~~ Flix Compiler Performance ~~~~")
    println()
    println(f"Throughput (best): ${shown.max}%,6d lines/sec (with $threads threads.)")
    println()
    println(f"  min: ${shown.min}%,6d, max: ${shown.max}%,6d, avg: ${shown.avg}%,6d, median: ${shown.median}%,6d")
    println()
    println(f"Finished $N iterations on $lines%,6d lines of code in $totalTime seconds.")

  }

  /**
    * Runs Flix with one thread and non-incremental.
    */
  private def perfBaseLine(N: Int, o: Options): IndexedSeq[Run] = {
    // Note: The Flix object is created _for every iteration._
    (0 until N).map { _ =>
      val flix = new Flix()
      flix.setOptions(o.copy(threads = MinThreads, incremental = false))

      addInputs(flix)
      runSingle(flix)
    }
  }

  /**
    * Runs Flix with n threads and non-incremental.
    */
  private def perfBaseLineWithPar(N: Int, o: Options): IndexedSeq[Run] = {
    // Note: The Flix object is created _for every iteration._
    (0 until N).map { _ =>
      val flix = new Flix()
      flix.setOptions(o.copy(threads = MaxThreads, incremental = false))

      addInputs(flix)
      runSingle(flix)
    }
  }

  /**
    * Runs Flix with n threads and incrementally.
    */
  private def perfBaseLineWithParInc(N: Int, o: Options): IndexedSeq[Run] = {
    // We create on single Flix object.
    val flix: Flix = new Flix()
    flix.setOptions(o.copy(threads = MaxThreads, incremental = true))

    // We add all inputs.
    addInputs(flix)

    // We compile once.
    runSingle(flix)

    // And then we perform N incremental compilations with no changes to the input.
    (0 until N).map { _ =>
      runSingle(flix)
    }
  }

  /**
    * Runs Flix once.
    */
  private def runSingle(flix: Flix): Run = {
    // Clear caches.
    EffUnification3.Algebra.Cache.clearCaches()

    // Run the frontend.
    val (optRoot, errors) = flix.check()
    if (errors.nonEmpty) {
      println(CompilationMessage.formatAll(errors)(flix.getFormatter, optRoot))
      System.exit(1)
    }
    val root: TypedAst.Root = optRoot.get

    // `check` resets the phase timers, so the total time so far is the frontend time.
    val frontendTime = flix.getTotalTime

    // Run the backend, unless only the frontend is benchmarked.
    val backendTime =
      if (flix.options.XPerfFrontend) 0L
      else {
        flix.codeGen(root)
        flix.getTotalTime - frontendTime
      }

    val lines = root.sources.foldLeft(0) {
      case (acc, (_, sl)) => acc + sl.endLine
    }

    val phases = flix.phaseTimers.map {
      case PhaseTime(phase, time) => phase -> time
    }

    Run(lines, frontendTime, backendTime, phases.toList)
  }

  /**
    * Merges a sequences of runs `l`.
    */
  private def aggregate(l: IndexedSeq[Run]): Runs = {
    if (l.isEmpty) {
      return Runs(0, Nil, Nil, Nil)
    }

    val lines = l.head.lines
    val frontendTimes = l.map(_.frontendTime).toList
    val backendTimes = l.map(_.backendTime).toList
    val phases = l.head.phases.map(_._1)

    val phaseMatrix = l.map(_.phases.map(_._2))
    val transposedMatrix = phaseMatrix.transpose.map(_.toList).toList
    val transposedWithPhases = ListOps.zip(phases, transposedMatrix)

    Runs(lines, frontendTimes, backendTimes, transposedWithPhases)
  }

  /**
    * The throughput statistics (in lines per second) of a sequence of runs.
    */
  private case class ThroughputStats(min: Int, max: Int, avg: Int, median: Int) {
    def toJSON: JValue = ("min" -> min) ~ ("max" -> max) ~ ("avg" -> avg) ~ ("median" -> median)
  }

  /**
    * Returns the throughput statistics of compiling `lines` lines in each of the given `times` (in nanoseconds).
    */
  private def throughputStats(lines: Long, times: List[Long]): ThroughputStats = {
    val throughputs = times.map(throughput(lines, _))
    ThroughputStats(throughputs.min, throughputs.max, average(throughputs.map(_.toLong)).toInt, median(throughputs.map(_.toLong)).toInt)
  }

  /**
    * Returns the throughput per second.
    */
  private def throughput(lines: Long, time: Long): Int = {
    if (time == 0L) -1
    else ((1_000_000_000L * lines).toDouble / time.toDouble).toInt
  }

  /**
    * Returns the given time `l` in milliseconds.
    */
  private def milliseconds(l: Double): Double = l / 1_000_000.0

  /**
    * Adds test code to the benchmarking suite.
    */
  private def addInputs(flix: Flix): Unit = {
    val sctx: SecurityContext = SecurityContext.Unrestricted
    flix.addSource(Path.of("TestArray.flix"), LocalResource.get("/test/ca/uwaterloo/flix/library/TestArray.flix"), sctx)
    flix.addSource(Path.of("TestChain.flix"), LocalResource.get("/test/ca/uwaterloo/flix/library/TestChain.flix"), sctx)
    flix.addSource(Path.of("TestIterator.flix"), LocalResource.get("/test/ca/uwaterloo/flix/library/TestIterator.flix"), sctx)
    flix.addSource(Path.of("TestDelayList.flix"), LocalResource.get("/test/ca/uwaterloo/flix/library/TestDelayList.flix"), sctx)
    flix.addSource(Path.of("TestList.flix"), LocalResource.get("/test/ca/uwaterloo/flix/library/TestList.flix"), sctx)
    flix.addSource(Path.of("TestMap.flix"), LocalResource.get("/test/ca/uwaterloo/flix/library/TestMap.flix"), sctx)
    flix.addSource(Path.of("TestMutDeque.flix"), LocalResource.get("/test/ca/uwaterloo/flix/library/TestMutDeque.flix"), sctx)
    flix.addSource(Path.of("TestMutList.flix"), LocalResource.get("/test/ca/uwaterloo/flix/library/TestMutList.flix"), sctx)
    flix.addSource(Path.of("TestMutMap.flix"), LocalResource.get("/test/ca/uwaterloo/flix/library/TestMutMap.flix"), sctx)
    flix.addSource(Path.of("TestMutSet.flix"), LocalResource.get("/test/ca/uwaterloo/flix/library/TestMutSet.flix"), sctx)
    flix.addSource(Path.of("TestNel.flix"), LocalResource.get("/test/ca/uwaterloo/flix/library/TestNel.flix"), sctx)
    flix.addSource(Path.of("TestOption.flix"), LocalResource.get("/test/ca/uwaterloo/flix/library/TestOption.flix"), sctx)
    flix.addSource(Path.of("TestPrelude.flix"), LocalResource.get("/test/ca/uwaterloo/flix/library/TestPrelude.flix"), sctx)
    flix.addSource(Path.of("TestResult.flix"), LocalResource.get("/test/ca/uwaterloo/flix/library/TestResult.flix"), sctx)
    flix.addSource(Path.of("TestSet.flix"), LocalResource.get("/test/ca/uwaterloo/flix/library/TestSet.flix"), sctx)
    flix.addSource(Path.of("TestValidation.flix"), LocalResource.get("/test/ca/uwaterloo/flix/library/TestValidation.flix"), sctx)
  }

  /**
    * Writes the given `json` to the given `file`.
    */
  private def writeFile(file: String, json: JValue, opts: Options): Unit = {
    val filePath = CompilerConstants.PerfDirectory.resolve(s"$file")
    FileOps.writeJSON(filePath, json)
  }

}
