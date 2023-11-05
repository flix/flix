/*
 * Copyright 2023 Magnus Madsen
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
package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.api.{Flix, PhaseTime}
import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.language.phase.unification.UnificationCache
import ca.uwaterloo.flix.util.{InternalCompilerException, LocalResource, Options, StatUtils}
import org.json4s.JValue
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods

import java.nio.file.{Files, LinkOption, Path}

object BenchmarkCompiler {

  /**
    * The number of compilations to perform when collecting statistics.
    */
  val N = 4

  private val Python =
    """
      |# $ pip install pandas
      |
      |import json
      |import matplotlib
      |import matplotlib.pyplot as plt
      |
      |with open('speedupPar.json', 'r') as file:
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
      |    ax.set_xlabel('Phase')
      |    ax.set_ylabel('Speedup')
      |    ax.bar_label(bars, fmt='\n%.1fx')
      |
      |    plt.xticks(rotation=90)
      |    plt.subplots_adjust(left=0.15, bottom=0.35)
      |    plt.ylim(1, 10)
      |
      |    plt.savefig('speedupPar.json.png')
      |
      |with open('speedupInc.json', 'r') as file:
      |    data = json.load(file)
      |    threads = data['threads']
      |    xvalues = list(map(lambda obj: obj['phase'], data['results']))
      |    yvalues = list(map(lambda obj: obj['speedup'], data['results']))
      |
      |    fig, ax = plt.subplots()
      |    bars = ax.bar(xvalues, yvalues)
      |
      |    ax.set_title(f'Incremental Speedup ({threads} threads)')
      |    ax.set_xlabel('Phase')
      |    ax.set_ylabel('Speedup')
      |    ax.bar_label(bars, fmt='\n%.1fx')
      |
      |    plt.xticks(rotation=90)
      |    plt.subplots_adjust(left=0.15, bottom=0.35)
      |    plt.ylim(1, 10)
      |
      |    plt.savefig('speedupInc.json.png')
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
      |    ax.set_xlabel('Iteration')
      |    ax.set_ylabel('Throughput (lines/sec)')
      |
      |    plt.xticks(rotation=90)
      |    plt.ylim(1, maxy)
      |
      |    plt.savefig('throughput.json.png')
      |
      |with open('throughputPar.json', 'r') as file:
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
      |    ax.set_xlabel('Iteration')
      |    ax.set_ylabel('Throughput (lines/sec)')
      |
      |    plt.xticks(rotation=90)
      |    plt.ylim(1, maxy)
      |
      |    plt.savefig('throughputPar.json.png')
      |
      |with open('throughputParInc.json', 'r') as file:
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
      |    ax.set_xlabel('Iteration')
      |    ax.set_ylabel('Throughput (lines/sec)')
      |
      |    plt.xticks(rotation=90)
      |    plt.ylim(1, maxy)
      |
      |    plt.savefig('throughputParInc.json.png')
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
      |    ax.set_xlabel('Phase')
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
      |    ax.set_xlabel('Phase')
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
      |    ax.set_xlabel('Phase')
      |    ax.set_ylabel('Time (ms)')
      |
      |    plt.xticks(rotation=90)
      |    plt.subplots_adjust(bottom=0.30)
      |    plt.ylim(1)
      |
      |    plt.savefig('timeWithParInc.json.png')
      |
      |""".stripMargin

  case class Run(lines: Int, time: Long, phases: List[(String, Long)])

  def run(o: Options, frontend: Boolean): Unit = {

    var flix: Flix = null

    val baseline = (0 until N).map { _ =>
      flix = new Flix()
      flix.setOptions(o.copy(incremental = false, loadClassFiles = false, threads = 1))
      flushCaches()

      addInputs(flix)
      val compilationResult = flix.compile().toHardFailure.get
      val phases = flix.phaseTimers.map {
        case PhaseTime(phase, time, _) => phase -> time
      }
      Run(compilationResult.getTotalLines, compilationResult.totalTime, phases.toList)
    }

    val baselineWithPar = (0 until N).map { _ =>
      flix = new Flix()
      flix.setOptions(o.copy(incremental = false, loadClassFiles = false))
      flushCaches()

      addInputs(flix)
      val compilationResult = flix.compile().toHardFailure.get
      val phases = flix.phaseTimers.map {
        case PhaseTime(phase, time, _) => phase -> time
      }
      Run(compilationResult.getTotalLines, compilationResult.totalTime, phases.toList)
    }


    flix = new Flix()
    flix.setOptions(flix.options.copy(incremental = true, loadClassFiles = false))
    val baselineWithParInc = (0 until N).map { _ =>
      flushCaches()

      addInputs(flix)
      val compilationResult = flix.compile().toHardFailure.get
      val phases = flix.phaseTimers.map {
        case PhaseTime(phase, time, _) => phase -> time
      }
      Run(compilationResult.getTotalLines, compilationResult.totalTime, phases.toList)
    }

    // The number of threads used.
    val threads = o.threads

    // Find the number of lines of source code.
    val lines = baselineWithPar.head.lines.toLong

    // Find the timings of each run.
    val timings = baselineWithPar.map(_.time).toList

    // Compute the total time in seconds.
    val totalTime = (timings.sum / 1_000_000_000L).toInt

    // Find the throughput of each run.
    val throughputs = timings.map(throughput(lines, _))

    // Compute the minimum throughput (per second).
    val min = throughputs.min

    // Compute the maximum throughput (per second).
    val max = throughputs.max

    // Compute the average throughput (per second).
    val avg = StatUtils.avg(throughputs.map(_.toLong)).toInt

    // Compute the median throughput (per second).
    val median = StatUtils.median(throughputs.map(_.toLong)).toInt

    // Compute the fastest iteration.
    val bestIter = timings.indexOf(timings.min)

    // Compute the ration between the slowest and fastest run.
    val bestWorstRatio = timings.max.toDouble / timings.min.toDouble

    val minThreads = 1
    val maxThreads = o.threads

    val maxObservedThroughput = throughput(lines, Math.min(baseline.last.time, Math.min(baselineWithPar.last.time, baselineWithParInc.last.time)))

    val timestamp = System.currentTimeMillis() / 1000


    //
    // Speedup
    //
    val speedupPar =
    ("timestamp" -> timestamp) ~
      ("minThreads" -> minThreads) ~
      ("maxThreads" -> maxThreads) ~
      ("incremental" -> false) ~
      ("lines" -> lines) ~
      ("results" -> baseline.last.phases.zip(baselineWithPar.last.phases).map {
        case ((phase, time1), (_, time2)) =>
          ("phase" -> phase) ~ ("speedup" -> time1.toDouble / time2.toDouble)
      })
    writeToDisk("speedupPar.json", speedupPar)(flix)

    // Note: Baseline is withPar.
    val speedupInc =
      ("timestamp" -> timestamp) ~
        ("threads" -> threads) ~
        ("incremental" -> true) ~
        ("lines" -> lines) ~
        ("results" -> baselineWithPar.last.phases.zip(baselineWithParInc.last.phases).map {
          case ((phase, time1), (_, time2)) =>
            ("phase" -> phase) ~ ("speedup" -> time1.toDouble / time2.toDouble)
        })
    writeToDisk("speedupInc.json", speedupInc)(flix)

    ///
    /// Throughput
    ///
    val throughoutBaseLine =
    ("timestamp" -> timestamp) ~
      ("threads" -> minThreads) ~
      ("incremental" -> false) ~
      ("lines" -> lines) ~
      ("plot" -> ("maxy" -> maxObservedThroughput)) ~
      ("results" -> baseline.zipWithIndex.map({
        case (Run(_, time, _), i) => ("i" -> s"Run $i") ~ ("throughput" -> throughput(lines, time))
      }))
    writeToDisk("throughput.json", throughoutBaseLine)(flix)

    val throughputPar =
      ("timestamp" -> timestamp) ~
        ("threads" -> maxThreads) ~
        ("incremental" -> false) ~
        ("lines" -> lines) ~
        ("plot" -> ("maxy" -> maxObservedThroughput)) ~
        ("results" -> baselineWithPar.zipWithIndex.map({
          case (Run(_, time, _), i) => ("i" -> s"Run $i") ~ ("throughput" -> throughput(lines, time))
        }))
    writeToDisk("throughputPar.json", throughputPar)(flix)

    val throughputParInc =
      ("timestamp" -> timestamp) ~
        ("threads" -> maxThreads) ~
        ("incremental" -> true) ~
        ("lines" -> lines) ~
        ("plot" -> ("maxy" -> maxObservedThroughput)) ~
        ("results" -> baselineWithParInc.zipWithIndex.map({
          case (Run(_, time, _), i) => ("i" -> s"Run $i") ~ ("throughput" -> throughput(lines, time))
        }))
    writeToDisk("throughputParInc.json", throughputParInc)(flix)

    //
    // Time
    //
    val timeBaseline =
    ("timestamp" -> timestamp) ~
      ("threads" -> minThreads) ~
      ("incremental" -> false) ~
      ("lines" -> lines) ~
      ("results" -> baseline.last.phases.map {
        case (phase, time) => ("phase" -> phase) ~ ("time" -> milliseconds(time))
      })
    writeToDisk("time.json", timeBaseline)(flix)

    val timeWithPar =
      ("timestamp" -> timestamp) ~
        ("threads" -> maxThreads) ~
        ("incremental" -> false) ~
        ("lines" -> lines) ~
        ("results" -> baselineWithPar.last.phases.map {
          case (phase, time) => ("phase" -> phase) ~ ("time" -> milliseconds(time))
        })
    writeToDisk("timeWithPar.json", timeWithPar)(flix)

    val timeWithParInc =
      ("timestamp" -> timestamp) ~
        ("threads" -> maxThreads) ~
        ("incremental" -> true) ~
        ("lines" -> lines) ~
        ("results" -> baselineWithParInc.last.phases.map {
          case (phase, time) => ("phase" -> phase) ~ ("time" -> milliseconds(time))
        })
    writeToDisk("timeWithParInc.json", timeWithParInc)(flix)

    //
    // Summary
    //
    val summaryJSON =
    ("timestamp" -> timestamp) ~
      ("threads" -> threads) ~
      ("lines" -> lines) ~
      ("iterations" -> N) ~
      ("throughput" -> ("min" -> min) ~ ("max" -> max) ~ ("avg" -> avg) ~ ("median" -> median))
    val s = JsonMethods.pretty(JsonMethods.render(summaryJSON))
    writeToDisk("summary.json", s)(flix)

    //
    // Python Plot
    //
    writeToDisk("plots.py", Python)(flix)

    println("~~~~ Flix Compiler Throughput ~~~~")
    println()
    println(f"Throughput (best): $max%,6d lines/sec (with $threads threads.)")
    println()
    println(f"  min: $min%,6d, max: $max%,6d, avg: $avg%,6d, median: $median%,6d")
    println()
    println(f"  The highest throughput was in iteration: $bestIter (out of $N).")
    println(f"  The ratio between the best and worst iteration was: $bestWorstRatio%1.1fx.")
    println()
    println(f"Finished $N iterations on $lines%,6d lines of code in $totalTime seconds.")

  }

  /**
    * Returns the throughput per second.
    */
  private def throughput(lines: Long, time: Long): Int = ((1_000_000_000L * lines).toDouble / time.toDouble).toInt

  private def milliseconds(l: Long): Long = l / 1_000_000

  /**
    * Flushes (clears) all caches.
    */
  private def flushCaches(): Unit = {
    UnificationCache.GlobalBool.clear()
    UnificationCache.GlobalBdd.clear()
  }

  /**
    * Adds test code to the benchmarking suite.
    */
  private def addInputs(flix: Flix): Unit = {
    flix.addSourceCode("TestArray.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestArray.flix"))
    flix.addSourceCode("TestChain.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestChain.flix"))
    flix.addSourceCode("TestIterator.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestIterator.flix"))
    flix.addSourceCode("TestDelayList.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestDelayList.flix"))
    flix.addSourceCode("TestList.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestList.flix"))
    flix.addSourceCode("TestMap.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestMap.flix"))
    flix.addSourceCode("TestMutDeque.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestMutDeque.flix"))
    flix.addSourceCode("TestMutList.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestMutList.flix"))
    flix.addSourceCode("TestMutMap.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestMutMap.flix"))
    flix.addSourceCode("TestMutSet.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestMutSet.flix"))
    flix.addSourceCode("TestNel.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestNel.flix"))
    flix.addSourceCode("TestOption.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestOption.flix"))
    flix.addSourceCode("TestPrelude.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestPrelude.flix"))
    flix.addSourceCode("TestResult.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestResult.flix"))
    flix.addSourceCode("TestSet.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestSet.flix"))
    flix.addSourceCode("TestValidation.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestValidation.flix"))
  }

  case object SummaryStatistics {
    /**
      * Builds the summary statistics from the given data.
      */
    def from[T](data: Seq[T])(implicit numeric: Numeric[T]): SummaryStatistics = {
      SummaryStatistics(
        min = numeric.toDouble(data.min),
        max = numeric.toDouble(data.max),
        mean = StatUtils.avg(data),
        median = StatUtils.median(data),
        stdDev = StatUtils.stdDev(data)
      )
    }
  }

  /**
    * A collection of summary statistics.
    */
  case class SummaryStatistics(min: Double, max: Double, mean: Double, median: Double, stdDev: Double)

  private def writeToDisk(fileName: String, json: JValue)(implicit flix: Flix): Unit = {
    writeToDisk(fileName, JsonMethods.pretty(JsonMethods.render(json)))
  }

  // TODO: Drop flix.output?
  // TODO: Move into FileOps
  // TODO: Reconcile with ASTPrinter
  private def writeToDisk(fileName: String, s: String)(implicit flix: Flix): Unit = {
    val buildAstsPath = flix.options.output.getOrElse(Path.of("./build/")).resolve("perf/")
    val filePath = buildAstsPath.resolve(s"$fileName")
    Files.createDirectories(buildAstsPath)

    // Check if the file already exists.
    if (Files.exists(filePath)) {
      // Check that the file is a regular file.
      if (!Files.isRegularFile(filePath, LinkOption.NOFOLLOW_LINKS)) {
        throw InternalCompilerException(s"Unable to write to non-regular file: '$filePath'.", SourceLocation.Unknown)
      }

      // Check if the file is writable.
      if (!Files.isWritable(filePath)) {
        throw InternalCompilerException(s"Unable to write to read-only file: '$filePath'.", SourceLocation.Unknown)
      }
    }
    Files.write(filePath, s.getBytes)
  }

}
