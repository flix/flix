/*
 * Copyright 2023 Magnus Madsen, 2024 Jakob Schneider Villumsen
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
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.language.phase.unification.zhegalkin.ZhegalkinCache
import ca.uwaterloo.flix.util.StatUtils.{average, median}
import ca.uwaterloo.flix.util.{FileOps, LocalResource, Options, StatUtils}
import org.json4s.{JValue, JsonAST}
import org.json4s.JsonDSL.*

import java.nio.file.Path

object BenchmarkInliner {

  def run(opts: Options): Unit = {
    val o = opts.copy(progress = false, loadClassFiles = false)

    println("Benchmarking inliner. This may take a while...")

    // Experiments:
    // 1. Compiler throughput and code size
    //    (a) without inlining
    //    (b) with old inliner
    //    (c) with new inliner
    runExperiment(o, "compilerBenchmark.json")(BenchmarkThroughput.run)

    // 2. Flix program speedup (sample programs, datalog engine, parser library)
    //    (a) without inlining
    //    (b) with old inliner
    //    (c) with new inliner
    // TODO: Vary thresholds for new inliner

    println("Done. Results written to 'build/perf'")

  }

  /**
    * Runs `experiment` with the following options:
    * (a) without inlining
    * (b) with old inliner
    * (c) with new inliner
    *
    * Writes to json file `experimentName.json`
    */
  private def runExperiment(opts: Options, experimentName: String)(experiment: Options => List[JsonAST.JObject]): Unit = {
    // Disable both inliners
    val o1 = opts.copy(xnooptimizer = true, xnooptimizer1 = true)
    val res1 = experiment(o1)
    val res1JSON = "InlinerNone" -> res1

    // Disable new inliner (run old)
    val o2 = opts.copy(xnooptimizer1 = true)
    val res2 = experiment(o2)
    val res2JSON = "InlinerOld" -> res2

    // Disable old inliner (run new)
    val o3 = opts.copy(xnooptimizer = true)
    val res3 = experiment(o3)
    val res3JSON = "InlinerNew" -> res3

    val summary = res1JSON ~ res2JSON ~ res3JSON
    writeFile(experimentName, summary)

  }

  private object BenchmarkThroughput {

    /**
      * The default number of compilations.
      */
    private val DefaultN: Int = 7

    /**
      * The number of threads to use for the single-thread experiment.
      */
    private val MinThreads: Int = 1

    /**
      * The number of threads to use for the multithreaded experiment.
      */
    private val MaxThreads: Int = Runtime.getRuntime.availableProcessors()

    private case class Run(lines: Int, time: Long, phases: List[(String, Long)], codeSize: Int)

    private case class Runs(lines: Int, times: List[Long], phases: List[(String, List[Long])], codeSize: List[Int])

    /**
      * Run compiler performance experiments.
      */
    def run(opts: Options): List[JsonAST.JObject] = {

      // The number of iterations.
      val N = opts.XPerfN.getOrElse(DefaultN)

      val runs = scala.collection.mutable.ListBuffer.empty[JsonAST.JObject]

      val limit = if (opts.xnooptimizer && opts.xnooptimizer1) 1 else 50

      for (inliningRounds <- 1 to limit) {
        // Set the inlining rounds
        val o = opts.copy(inlinerRounds = inliningRounds, inliner1Rounds = inliningRounds)

        // Run the experiments.
        val baseline = aggregate(if (o.XPerfPar) IndexedSeq.empty else perfBaseLine(N, o))
        val baselineWithPar = aggregate(perfBaseLineWithPar(N, o))
        val baselineWithParInc = aggregate(if (o.XPerfPar) IndexedSeq.empty else perfBaseLineWithParInc(N, o))

        // Find the number of lines of source code.
        val lines = baselineWithPar.lines.toLong

        // Find the timings of each run.
        val timings = baselineWithPar.times

        // Find the throughput of each run.
        val throughputs = timings.map(throughput(lines, _))

        // Compute the minimum throughput (per second).
        val min = throughputs.min

        // Compute the maximum throughput (per second).
        val max = throughputs.max

        // Compute the average throughput (per second).
        val avg = average(throughputs.map(_.toLong)).toInt

        // Compute the median throughput (per second).
        val mdn = median(throughputs.map(_.toLong)).toInt

        // Best observed throughput.
        val maxObservedThroughput = throughput(lines,
          Math.min(baseline.times.min,
            Math.min(baselineWithPar.times.min, baselineWithParInc.times.min)))

        // Timestamp (in seconds) when the experiment was run.
        val timestamp = System.currentTimeMillis() / 1000

        //
        // The combine function uses to merge data from different runs.
        //
        def combine[T](xs: Seq[T])(implicit numeric: Numeric[T]): Double =
          if (N <= 4)
            numeric.toDouble(xs.last)
          else
            StatUtils.median(xs)

        //
        // Speedup
        //
        val speedupPar =
          "speedupWithPar" -> {
            ("inliningRounds" -> inliningRounds) ~
              ("timestamp" -> timestamp) ~
              ("minThreads" -> MinThreads) ~
              ("maxThreads" -> MaxThreads) ~
              ("incremental" -> false) ~
              ("lines" -> lines) ~
              ("results" -> baseline.phases.zip(baselineWithPar.phases).map {
                case ((phase, times1), (_, times2)) =>
                  ("phase" -> phase) ~ ("speedup" -> combine(times1.zip(times2).map(p => p._1.toDouble / p._2.toDouble)))
              })
          }

        // Note: Baseline is withPar.
        val speedupInc =
          "speedupWithInc" -> {
            ("inliningRounds" -> inliningRounds) ~
              ("timestamp" -> timestamp) ~
              ("threads" -> MaxThreads) ~
              ("incremental" -> true) ~
              ("lines" -> lines) ~
              ("results" -> baselineWithPar.phases.zip(baselineWithParInc.phases).map {
                case ((phase, times1), (_, times2)) =>
                  ("phase" -> phase) ~ ("speedup" -> combine(times1.zip(times2).map(p => p._1.toDouble / p._2.toDouble)))
              })
          }

        //
        // Throughput
        //
        val throughputBaseLine =
          "throughput" -> {
            ("inliningRounds" -> inliningRounds) ~
              ("timestamp" -> timestamp) ~
              ("threads" -> MinThreads) ~
              ("incremental" -> false) ~
              ("lines" -> lines) ~
              ("plot" -> ("maxy" -> maxObservedThroughput)) ~
              ("results" -> baseline.times.zipWithIndex.map({
                case (time, i) => ("i" -> s"Run $i") ~ ("throughput" -> throughput(lines, time))
              }))
          }

        val throughputPar =
          "throughputWithPar" -> {
            ("inliningRounds" -> inliningRounds) ~
              ("timestamp" -> timestamp) ~
              ("threads" -> MaxThreads) ~
              ("incremental" -> false) ~
              ("lines" -> lines) ~
              ("plot" -> ("maxy" -> maxObservedThroughput)) ~
              ("results" -> baselineWithPar.times.zipWithIndex.map({
                case (time, i) => ("i" -> s"Run $i") ~ ("throughput" -> throughput(lines, time))
              }))
          }

        val throughputParInc =
          "throughputWithParInc" -> {
            ("inliningRounds" -> inliningRounds) ~
              ("timestamp" -> timestamp) ~
              ("threads" -> MaxThreads) ~
              ("incremental" -> true) ~
              ("lines" -> lines) ~
              ("plot" -> ("maxy" -> maxObservedThroughput)) ~
              ("results" -> baselineWithParInc.times.zipWithIndex.map({
                case (time, i) => ("i" -> s"Run $i") ~ ("throughput" -> throughput(lines, time))
              }))
          }

        //
        // Time
        //
        val timeBaseline =
          "time" -> {
            ("inliningRounds" -> inliningRounds) ~
              ("timestamp" -> timestamp) ~
              ("threads" -> MinThreads) ~
              ("incremental" -> false) ~
              ("lines" -> lines) ~
              ("results" -> baseline.phases.map {
                case (phase, times) => ("phase" -> phase) ~ ("time" -> milliseconds(combine(times)))
              })
          }

        val timeWithPar =
          "timeWithPar" -> {
            ("inliningRounds" -> inliningRounds) ~
              ("timestamp" -> timestamp) ~
              ("threads" -> MaxThreads) ~
              ("incremental" -> false) ~
              ("lines" -> lines) ~
              ("results" -> baselineWithPar.phases.map {
                case (phase, times) => ("phase" -> phase) ~ ("time" -> milliseconds(combine(times)))
              })
          }

        val timeWithParInc =
          "timeWithParInc" -> {
            ("inliningRounds" -> inliningRounds) ~
              ("timestamp" -> timestamp) ~
              ("threads" -> MaxThreads) ~
              ("incremental" -> true) ~
              ("lines" -> lines) ~
              ("results" -> baselineWithParInc.phases.map {
                case (phase, times) => ("phase" -> phase) ~ ("time" -> milliseconds(combine(times)))
              })
          }

        //
        // Summary
        //
        val summaryJSON =
          "summary" -> {
            ("inliningRounds" -> inliningRounds) ~
              ("timestamp" -> timestamp) ~
              ("threads" -> MaxThreads) ~
              ("lines" -> lines) ~
              ("iterations" -> N) ~
              ("throughput" -> ("min" -> min) ~ ("max" -> max) ~ ("avg" -> avg) ~ ("median" -> mdn))
          }
        // val s = JsonMethods.pretty(JsonMethods.render(summaryJSON))

        //
        // Python Plot
        //
        // FileOps.writeString(Path.of("./build/").resolve("perf/").resolve("plots.py"), Python)
        runs.addAll(List(speedupPar, speedupInc, throughputBaseLine, throughputPar, throughputParInc, timeBaseline, timeWithPar, timeWithParInc, summaryJSON))
      }
      runs.toList
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
      // Note: The Flix object is created _once_.
      val flix: Flix = new Flix()
      flix.setOptions(o.copy(threads = MaxThreads, incremental = true))
      (0 until N).map { _ =>
        addInputs(flix)
        runSingle(flix)
      }
    }

    /**
      * Runs Flix once.
      */
    private def runSingle(flix: Flix): Run = {
      // Clear caches.
      ZhegalkinCache.clearCaches()
      val result = flix.compile().unsafeGet
      val totalLines = result.getTotalLines
      val phases = flix.phaseTimers.map {
        case PhaseTime(phase, time) => phase -> time
      }
      val totalTime = flix.getTotalTime

      Run(totalLines, totalTime, phases.toList, result.codeSize)
    }

    /**
      * Merges a sequences of runs `l`.
      */
    private def aggregate(l: IndexedSeq[Run]): Runs = {
      if (l.isEmpty) {
        return Runs(0, List(0), Nil, List(0))
      }

      val lines = l.head.lines
      val times = l.map(_.time).toList
      val codeSizes = l.map(_.codeSize).toList

      val phases = l.head.phases.map(_._1)
      val phaseMatrix = l.map(_.phases.map(_._2))
      val transposedMatrix = phaseMatrix.transpose.map(_.toList).toList
      val transposedWithPhases = phases.zip(transposedMatrix)

      Runs(lines, times, transposedWithPhases, codeSizes)
    }

    /**
      * Returns the throughput per second.
      */
    private def throughput(lines: Long, time: Long): Int = ((1_000_000_000L * lines).toDouble / time.toDouble).toInt

    /**
      * Returns the given time `l` in milliseconds.
      */
    private def milliseconds(l: Double): Double = l / 1_000_000.0

    /**
      * Adds test code to the benchmarking suite.
      */
    private def addInputs(flix: Flix): Unit = {
      implicit val sctx: SecurityContext = SecurityContext.AllPermissions
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
  }

  /**
    * Writes the given `json` to the given `file`.
    */
  private def writeFile(file: String, json: JValue): Unit = {
    val directory = Path.of("./build/").resolve("perf/")
    val filePath = directory.resolve(s"$file")
    FileOps.writeJSON(filePath, json)
  }

}
