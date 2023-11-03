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
  val N = 3

  private val Python =
    """
      |
      |# $ pip install pandas
      |
      |import json
      |import pandas as pd
      |import matplotlib
      |import matplotlib.pyplot as plt
      |
      |with open('iterations.json', 'r') as file:
      |    data = json.load(file)
      |    df = pd.DataFrame(data['results'])
      |    print(df)
      |    df.plot(x='i', y='time')
      |
      |    # Plot the DataFrame as a bar chart
      |    fig, ax = plt.subplots()
      |
      |    p = ax.bar(list(map(lambda x: "Iter " + str(x), df["i"])), df["time"])
      |    ax.set_xlabel('Iteration')
      |    ax.set_ylabel('Throughput')
      |    ax.get_yaxis().set_major_formatter(matplotlib.ticker.FuncFormatter(lambda x, p: format(int(x), ',')))
      |
      |    plt.xticks(rotation=90)
      |    plt.subplots_adjust(left=0.15, bottom=0.35)
      |
      |    # Save the plot as an image file (e.g., PNG, PDF, SVG, etc.)
      |    plt.savefig('iterations.png')  # Change the filename and format as needed
      |
      |
      |with open('phases.json', 'r') as file:
      |    data = json.load(file)
      |    df = pd.DataFrame(data['results'])
      |    print(df)
      |    df.plot(x='phase', y='time')
      |
      |    # Plot the DataFrame as a bar chart
      |    fig, ax = plt.subplots()
      |
      |    p = ax.bar(df["phase"], df["time"])
      |    ax.set_xlabel('Phase')
      |    ax.set_ylabel('Time')
      |    ax.get_yaxis().set_major_formatter(matplotlib.ticker.FuncFormatter(lambda x, p: format(int(x), ',')))
      |
      |    plt.xticks(rotation=90)
      |    plt.subplots_adjust(left=0.15, bottom=0.35)
      |
      |    # Save the plot as an image file (e.g., PNG, PDF, SVG, etc.)
      |    plt.savefig('phases.png')  # Change the filename and format as needed
      |
      |""".stripMargin

  case class Run(lines: Int, time: Long, phases: List[(String, Long)])

  def run(o: Options, frontend: Boolean): Unit = {

    var flix = newFlix(o)

    //
    // Collect data from N iterations.
    //
    val results = (0 until N).map { _ =>
      flix = newFlix(o)

      val compilationResult = flix.compile().toHardFailure.get
      val phases = flix.phaseTimers.map {
        case PhaseTime(phase, time, _) => phase -> time
      }
      Run(compilationResult.getTotalLines, compilationResult.totalTime, phases.toList)
    }

    // The number of threads used.
    val threads = o.threads

    // Find the number of lines of source code.
    val lines = results.head.lines.toLong

    // Find the timings of each run.
    val timings = results.map(_.time).toList

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

    // TODO: files we want:
    // phases.json
    // iterations.json
    // summary.json

    // TODO: What about the flags: incremental? threads?

    // TODO: What about A/B comparisons?

    writeToDisk("graphs.py", Python)(flix)

    val timestamp = System.currentTimeMillis() / 1000

    val iterations =
      ("timestamp" -> timestamp) ~
        ("threads" -> threads) ~
        ("lines" -> lines) ~
        ("results" -> results.zipWithIndex.map(x => ("i" -> x._2) ~ ("time" -> JInt(throughput(lines, x._1.time)))))
    writeToDisk("iterations.json", iterations)(flix)

    val phases =
      ("timestamp" -> timestamp) ~
        ("threads" -> threads) ~
        ("lines" -> lines) ~
        ("results" -> results.last.phases.map {
          case (phase, time) => ("phase" -> phase) ~ ("time" -> milliseconds(time))
        })
    writeToDisk("phases.json", phases)(flix)

    val summaryJSON =
      ("timestamp" -> timestamp) ~
        ("threads" -> threads) ~
        ("lines" -> lines) ~
        ("iterations" -> N) ~
        ("throughput" -> ("min" -> min) ~ ("max" -> max) ~ ("avg" -> avg) ~ ("median" -> median))
    val s = JsonMethods.pretty(JsonMethods.render(summaryJSON))
    writeToDisk("summary.json", s)(flix)

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
    * Returns a Flix object configured with the benchmark program.
    */
  private def newFlix(o: Options): Flix = {
    val flix = new Flix()
    flushCaches()

    flix.setOptions(opts = o.copy(incremental = false, loadClassFiles = false))

    addInputs(flix)

    flix
  }

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
