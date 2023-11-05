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
import org.json4s.{JInt, JValue}
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
      |with open('speedup_full_par.json', 'r') as file:
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
      |    plt.savefig('speedup_full_par.png')
      |
      |with open('speedup_inc_par.json', 'r') as file:
      |    data = json.load(file)
      |    minThreads = data['minThreads']
      |    maxThreads = data['maxThreads']
      |    xvalues = list(map(lambda obj: obj['phase'], data['results']))
      |    yvalues = list(map(lambda obj: obj['speedup'], data['results']))
      |
      |    fig, ax = plt.subplots()
      |    bars = ax.bar(xvalues, yvalues)
      |
      |    ax.set_title(f'Parallel Speedup ({minThreads} vs. {maxThreads} threads, incremental)')
      |    ax.set_xlabel('Phase')
      |    ax.set_ylabel('Speedup')
      |    ax.bar_label(bars, fmt='\n%.1fx')
      |
      |    plt.xticks(rotation=90)
      |    plt.subplots_adjust(left=0.15, bottom=0.35)
      |    plt.ylim(1, 10)
      |
      |    plt.savefig('speedup_inc_par.png')
      |
      |with open('speedup_phases_inc.json', 'r') as file:
      |    data = json.load(file)
      |    threads = data['threads']
      |    xvalues = list(map(lambda obj: obj['phase'], data['results']))
      |    yvalues = list(map(lambda obj: obj['speedup'], data['results']))
      |
      |    fig, ax = plt.subplots()
      |    bars = ax.bar(xvalues, yvalues)
      |
      |    ax.set_title(f'Incremental Speedup ({threads} threads, incremental)')
      |    ax.set_xlabel('Phase')
      |    ax.set_ylabel('Speedup')
      |    ax.bar_label(bars, fmt='\n%.1fx')
      |
      |    plt.xticks(rotation=90)
      |    plt.subplots_adjust(left=0.15, bottom=0.35)
      |    plt.ylim(1, 10)
      |
      |    plt.savefig('speedup_phases_inc.json.png')
      |
      |with open('throughput_incr_par.json', 'r') as file:
      |    data = json.load(file)
      |    threads = data['threads']
      |    xvalues = list(map(lambda obj: obj['i'], data['results']))
      |    yvalues = list(map(lambda obj: obj['time'], data['results']))
      |
      |    fig, ax = plt.subplots()
      |    ax.bar(xvalues, yvalues)
      |
      |    ax.set_title(f'Throughput ({threads} threads, incremental)')
      |    ax.set_xlabel('Iteration')
      |    ax.set_ylabel('Throughput (lines/sec)')
      |
      |    plt.xticks(rotation=90)
      |    plt.subplots_adjust(left=0.15, bottom=0.35)
      |    plt.ylim(1)
      |
      |    plt.savefig('throughput_incr_par.png')
      |
      |with open('throughput_seq_full.json', 'r') as file:
      |    data = json.load(file)
      |    threads = data['threads']
      |    xvalues = list(map(lambda obj: obj['i'], data['results']))
      |    yvalues = list(map(lambda obj: obj['time'], data['results']))
      |
      |    fig, ax = plt.subplots()
      |    ax.bar(xvalues, yvalues)
      |
      |    ax.set_title(f'Throughput ({threads} threads, non-incremental)')
      |    ax.set_xlabel('Iteration')
      |    ax.set_ylabel('Throughput (lines/sec)')
      |
      |    plt.xticks(rotation=90)
      |    plt.subplots_adjust(left=0.15, bottom=0.35)
      |    plt.ylim(1)
      |
      |    plt.savefig('throughput_seq_full.png')
      |
      |
      |with open('throughput_full_par.json', 'r') as file:
      |    data = json.load(file)
      |    threads = data['threads']
      |    xvalues = list(map(lambda obj: obj['i'], data['results']))
      |    yvalues = list(map(lambda obj: obj['time'], data['results']))
      |
      |    fig, ax = plt.subplots()
      |    ax.bar(xvalues, yvalues)
      |
      |    ax.set_title(f'Throughput ({threads} threads, non-incremental)')
      |    ax.set_xlabel('Iteration')
      |    ax.set_ylabel('Throughput (lines/sec)')
      |
      |    plt.xticks(rotation=90)
      |    plt.subplots_adjust(left=0.15, bottom=0.35)
      |    plt.ylim(1)
      |
      |    plt.savefig('throughput_full_par.png')
      |
      |with open('time_phases.json', 'r') as file:
      |    data = json.load(file)
      |    threads = data['threads']
      |    xvalues = list(map(lambda obj: obj['phase'], data['results']))
      |    yvalues = list(map(lambda obj: obj['time'], data['results']))
      |
      |    fig, ax = plt.subplots()
      |    ax.bar(xvalues, yvalues)
      |
      |    ax.set_title(f'Time ({threads} threads, non-incremental)')
      |    ax.set_xlabel('Phase')
      |    ax.set_ylabel('Total Time (ms)')
      |
      |    plt.xticks(rotation=90)
      |    plt.subplots_adjust(left=0.15, bottom=0.35)
      |    plt.ylim(1)
      |
      |    plt.savefig('time_phases.png')
      |
      |with open('time_phases_incr.json', 'r') as file:
      |    data = json.load(file)
      |    threads = data['threads']
      |    xvalues = list(map(lambda obj: obj['phase'], data['results']))
      |    yvalues = list(map(lambda obj: obj['time'], data['results']))
      |
      |    fig, ax = plt.subplots()
      |    ax.bar(xvalues, yvalues)
      |
      |    ax.set_title(f'Time ({threads} threads, incremental)')
      |    ax.set_xlabel('Phase')
      |    ax.set_ylabel('Total Time (ms)')
      |
      |    plt.xticks(rotation=90)
      |    plt.subplots_adjust(left=0.15, bottom=0.35)
      |    plt.ylim(1)
      |
      |    plt.savefig('time_phases_incr.png')
      |
      |""".stripMargin

  case class Run(lines: Int, time: Long, phases: List[(String, Long)])

  def run(o: Options, frontend: Boolean): Unit = {

    var flix: Flix = null
    val maxThreadResults = (0 until N).map { _ =>
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

    val minThreadResults = (0 until N).map { _ =>
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

    flix = new Flix()
    flix.setOptions(flix.options.copy(incremental = true, loadClassFiles = false))
    val incrementalResults = (0 until N).map { _ =>
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
    val lines = maxThreadResults.head.lines.toLong

    // Find the timings of each run.
    val timings = maxThreadResults.map(_.time).toList

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

    val MinThreads = 1
    val MaxThreads = o.threads

    // Graphs i want:
    // Parallelism: Per phase speedup 1thread versus N threads.
    // Incrementalism: Per phase speedup?
    // throughput: loc/sec per iteration
    // Maybe throughput per phase, but ignoring fast phases?

    // TODO: files we want:
    // phases.json
    // summary.json

    // TODO: What about the flags: incremental? threads?

    // TODO: What about A/B comparisons?

    writeToDisk("plots.py", Python)(flix)

    val timestamp = System.currentTimeMillis() / 1000

    val speedupPhases =
      ("timestamp" -> timestamp) ~
        ("threads" -> threads) ~
        ("lines" -> lines) ~
        ("results" -> maxThreadResults.last.phases.zip(incrementalResults.last.phases).map {
          case ((phase, time), (_, incrementalTime)) => ("phase" -> phase) ~
            ("speedup" -> time.toDouble / incrementalTime.toDouble)
        })
    writeToDisk("speedup_phases_inc.json", speedupPhases)(flix)

    val throughputSingleThread =
      ("timestamp" -> timestamp) ~
        ("threads" -> MinThreads) ~
        ("lines" -> lines) ~
        ("results" -> minThreadResults.zipWithIndex.map(x => ("i" -> ("Iter " + x._2.toString)) ~ ("time" -> JInt(throughput(lines, x._1.time)))))
    writeToDisk("throughput_seq_full.json", throughputSingleThread)(flix)

    val throughputMaxThread =
      ("timestamp" -> timestamp) ~
        ("threads" -> MaxThreads) ~
        ("lines" -> lines) ~
        ("results" -> maxThreadResults.zipWithIndex.map(x => ("i" -> ("Iter " + x._2.toString)) ~ ("time" -> JInt(throughput(lines, x._1.time)))))
    writeToDisk("throughput_full_par.json", throughputMaxThread)(flix)

    val throughputWithIncr =
      ("timestamp" -> timestamp) ~
        ("threads" -> MaxThreads) ~
        ("lines" -> lines) ~
        ("results" -> incrementalResults.zipWithIndex.map(x => ("i" -> ("Iter " + x._2.toString)) ~ ("time" -> JInt(throughput(lines, x._1.time)))))
    writeToDisk("throughput_incr_par.json", throughputWithIncr)(flix)

    val parallelismJSON =
      ("timestamp" -> timestamp) ~
        ("lines" -> lines) ~
        ("minThreads" -> MinThreads) ~
        ("maxThreads" -> MaxThreads) ~
        ("results" -> maxThreadResults.last.phases.zip(minThreadResults.last.phases).map {
          case ((phase, time), (_, oneThreadTime)) => ("phase" -> phase) ~
            ("speedup" -> oneThreadTime.toDouble / time.toDouble)
        })
    writeToDisk("speedup_full_par.json", parallelismJSON)(flix)

    val speedupWithIncrWithPar =
      ("timestamp" -> timestamp) ~
        ("lines" -> lines) ~
        ("minThreads" -> MinThreads) ~
        ("maxThreads" -> MaxThreads) ~
        ("results" -> incrementalResults.last.phases.zip(maxThreadResults.last.phases).map {
          case ((phase, time1), (_, time2)) => ("phase" -> phase) ~
            ("speedup" -> time2.toDouble / time1.toDouble)
        })
    writeToDisk("speedup_inc_par.json", speedupWithIncrWithPar)(flix)

    val summaryJSON =
      ("timestamp" -> timestamp) ~
        ("threads" -> threads) ~
        ("lines" -> lines) ~
        ("iterations" -> N) ~
        ("throughput" -> ("min" -> min) ~ ("max" -> max) ~ ("avg" -> avg) ~ ("median" -> median))
    val s = JsonMethods.pretty(JsonMethods.render(summaryJSON))
    writeToDisk("summary.json", s)(flix)

    val timePhases =
      ("timestamp" -> timestamp) ~
        ("threads" -> threads) ~
        ("lines" -> lines) ~
        ("results" -> maxThreadResults.last.phases.map {
          case ((phase, time)) => ("phase" -> phase) ~ ("time" -> milliseconds(time))
        })
    writeToDisk("time_phases.json", timePhases)(flix)

    val timePhasesIncr =
      ("timestamp" -> timestamp) ~
        ("threads" -> threads) ~
        ("lines" -> lines) ~
        ("results" -> incrementalResults.last.phases.map {
          case ((phase, time)) => ("phase" -> phase) ~ ("time" -> milliseconds(time))
        })
    writeToDisk("time_phases_incr.json", timePhasesIncr)(flix)

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
