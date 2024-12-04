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
import ca.uwaterloo.flix.util.{FileOps, LibLevel, LocalResource, Options, StatUtils}
import org.json4s.{JValue, JsonAST}
import org.json4s.JsonDSL.*

import java.nio.file.Path

object BenchmarkInliner {

  def run(opts: Options): Unit = {
    println("Benchmarking inliner. This may take a while...")
    val programBenchmark = BenchmarkPrograms.run(opts)
    writeFile("programsBenchmark.json", programBenchmark)
    println("Done with 'programs'")

    // Experiments:
    // 1. Compiler throughput and code size
    //    (a) without inlining
    //    (b) with old inliner
    //    (c) with new inliner
    runExperiment(opts.copy(progress = false, loadClassFiles = false), "compilerBenchmark.json")(BenchmarkThroughput.run)

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

  private object BenchmarkPrograms {

    private sealed trait InlinerType

    private object InlinerType {

      private case object NoInliner extends InlinerType

      private case object Old extends InlinerType

      private case object New extends InlinerType

      def from(options: Options): InlinerType = {
        if (options.xnooptimizer && options.xnooptimizer1)
          NoInliner
        else if (options.xnooptimizer1)
          Old
        else
          New
      }
    }

    private val NumberOfRuns = 1000

    private val NumberOfSamples = 100

    /**
      * Represents a run of a single program.
      *
      * @param name            The name of the program
      * @param lines           The number of lines of source code in the program
      * @param inlinerType     Which inliner was used (if any)
      * @param inliningRounds  The number of rounds of inlining
      * @param runningTime     The running time of the program
      * @param compilationTime The median time taken to compile the program
      * @param phases          The median running time of each compilation phase
      * @param codeSize        The number of bytes of the compiled program
      */
    private case class Run(name: String, lines: Int, inlinerType: InlinerType, inliningRounds: Int, runningTime: Long, compilationTime: Long, phases: List[(String, Long)], codeSize: Int) {
      def toJson: JsonAST.JObject = {
        ("lines" -> lines) ~
          ("inlinerType" -> inlinerType.toString) ~
          ("inliningRounds" -> inliningRounds) ~
          ("runningTime" -> runningTime) ~
          ("compilationTime" -> compilationTime) ~
          ("phases" -> phases) ~
          ("codeSize" -> codeSize)
      }
    }

    private case class Stats[T](min: T, max: T, average: Double, median: Double)

    private def stats[T](xs: Seq[T])(implicit numeric: Numeric[T]): Stats[T] = {
      Stats(xs.min, xs.max, average(xs), median(xs))
    }

    private val programs: Map[String, String] = Map(
      "map10KLength" -> map10KLength,
      "map10KLengthOptimized" -> map10KLengthOptimized,
      "filterMap10K" -> filterMap10K,
      "filterMap10KOptimized" -> filterMap10KOptimized,
      "List.filter" -> listFilter,
      "List.foldLeft" -> listFoldLeft,
      "List.foldRight" -> listFoldRight,
      "List.map" -> listMap,
      "List.length" -> listLength,
      "List.reverse" -> listReverse,
      "List.filterMap" -> listFilterMap
    )

    def run(opts: Options): JsonAST.JObject = {

      // TODO: Best performance increase
      // TODO: Worst performance increase
      // TODO: Avg and median performance increase.

      val programExperiments = benchmark(opts)

      val runningTimeStats = programExperiments.map {
        case (name, runs) => name -> stats(runs.map(_.runningTime))
      }

      val compilationTimeStats = programExperiments.map {
        case (name, runs) => name -> stats(runs.map(_.compilationTime))
      }

      // Timestamp (in seconds) when the experiment was run.
      val timestamp = System.currentTimeMillis() / 1000

      ("timestamp" -> timestamp) ~
        ("programs" -> {
          programExperiments.map {
            case (name, runs) => name -> {
              ("summary" -> {
                ("runningTime" -> {
                  val stats = runningTimeStats.apply(name)
                  ("best" -> stats.min) ~
                    ("worst" -> stats.max) ~
                    ("average" -> stats.average) ~
                    ("median" -> stats.median)
                }) ~
                  ("compilationTime" -> {
                    val stats = compilationTimeStats.apply(name)
                    ("best" -> stats.min) ~
                      ("worst" -> stats.max) ~
                      ("average" -> stats.average) ~
                      ("median" -> stats.median)
                  })
              }) ~
                ("results" -> runs.map(_.toJson))
            }
          }
        })
    }

    private def benchmark(opts: Options): Map[String, List[Run]] = {
      implicit val sctx: SecurityContext = SecurityContext.AllPermissions
      val o0 = opts.copy(xnooptimizer = true, xnooptimizer1 = true, lib = LibLevel.All, progress = false, inlinerRounds = 0, inliner1Rounds = 0)
      val o1 = opts.copy(xnooptimizer = false, xnooptimizer1 = true, lib = LibLevel.All, progress = false)
      val o2 = opts.copy(xnooptimizer = true, xnooptimizer1 = false, lib = LibLevel.All, progress = false)
      val allOptions = o0 :: (1 to 50).map(r => o1.copy(inlinerRounds = r, inliner1Rounds = r)).toList ::: (1 to 50).map(r => o2.copy(inlinerRounds = r, inliner1Rounds = r)).toList
      val runConfigs = allOptions.flatMap(c => programs.map { case (name, prog) => (c, name, prog) })

      val runs = scala.collection.mutable.ListBuffer.empty[Run]
      for ((o, name, prog) <- runConfigs) {
        // Clear caches.
        val compilationTimings = scala.collection.mutable.ListBuffer.empty[(Long, List[(String, Long)])]

        for (_ <- 1 to 10) {
          val flix = new Flix().setOptions(o)
          ZhegalkinCache.clearCaches()
          flix.addSourceCode(s"$name.flix", prog)
          val result = flix.compile().unsafeGet
          val phaseTimes = flix.phaseTimers.map { case PhaseTime(phase, time) => phase -> time }.toList
          val timing = (result.totalTime, phaseTimes)
          compilationTimings += timing
        }

        val runningTimes = scala.collection.mutable.ListBuffer.empty[Long]
        val flix = new Flix().setOptions(o)
        ZhegalkinCache.clearCaches()
        flix.addSourceCode(s"$name.flix", prog)
        val result = flix.compile().unsafeGet
        result.getMain match {
          case Some(mainFunc) =>
            for (_ <- 1 to NumberOfSamples) {
              for (_ <- 1 to NumberOfRuns) {
                val t0 = System.nanoTime()
                mainFunc(Array.empty)
                val tDelta = System.nanoTime() - t0
                runningTimes += milliseconds(tDelta).toLong
              }
            }
          case None => throw new RuntimeException(s"undefined main method for program '$name'")
        }

        val lines = result.getTotalLines
        val inlinerType = InlinerType.from(o)
        val inliningRounds = o.inliner1Rounds
        val runningTime = median(runningTimes.toSeq).toLong
        val compilationTime = median(compilationTimings.map(_._1).toSeq).toLong
        val phaseTimings = compilationTimings.flatMap(_._2).foldLeft(Map.empty[String, Seq[Long]]) {
          case (acc, (phase, time)) => acc.get(phase) match {
            case Some(timings) => acc + (phase -> timings.appended(time))
            case None => acc + (phase -> Seq(time))
          }
        }.map { case (phase, timings) => phase -> median(timings).toLong }.toList
        val codeSize = result.codeSize

        runs += Run(name, lines, inlinerType, inliningRounds, runningTime, compilationTime, phaseTimings, codeSize)
      }

      runs.foldLeft(Map.empty[String, List[Run]]) {
        case (acc, run) => acc.get(run.name) match {
          case Some(runsOfProgram) => acc + (run.name -> (run :: runsOfProgram))
          case None => acc + (run.name -> List(run))
        }
      }
    }

    private def listFilter: String = {
      s"""
         |def main(): Unit \\ IO = {
         |    List.range(0, 10_000) |> List.filter(x -> Int32.modulo(x, 2) == 0) |> blackhole
         |}
         |
         |def blackhole(t: a): Unit \\ IO =
         |    Ref.fresh(Static, t); ()
         |
         |""".stripMargin
    }

    private def listFoldLeft: String = {
      s"""
         |def main(): Unit \\ IO = {
         |    List.range(0, 10_000) |> List.foldLeft(Add.add, 0) |> blackhole
         |}
         |
         |def blackhole(t: a): Unit \\ IO =
         |    Ref.fresh(Static, t); ()
         |
         |""".stripMargin
    }

    private def listFoldRight: String = {
      s"""
         |def main(): Unit \\ IO = {
         |    List.range(0, 10_000) |> List.foldRight(Add.add, 0) |> blackhole
         |}
         |
         |def blackhole(t: a): Unit \\ IO =
         |    Ref.fresh(Static, t); ()
         |
         |""".stripMargin
    }

    private def listMap: String = {
      s"""
         |def main(): Unit \\ IO = {
         |    List.range(0, 10_000) |> List.map(x -> x + 1) |> blackhole
         |}
         |
         |def blackhole(t: a): Unit \\ IO =
         |    Ref.fresh(Static, t); ()
         |
         |""".stripMargin
    }

    private def listLength: String = {
      s"""
         |def main(): Unit \\ IO = {
         |    List.range(0, 10_000) |> List.length |> blackhole
         |}
         |
         |def blackhole(t: a): Unit \\ IO =
         |    Ref.fresh(Static, t); ()
         |
         |""".stripMargin
    }

    private def listReverse: String = {
      s"""
         |def main(): Unit \\ IO = {
         |    List.range(0, 10_000) |> List.reverse |> blackhole
         |}
         |
         |def blackhole(t: a): Unit \\ IO =
         |    Ref.fresh(Static, t); ()
         |
         |""".stripMargin
    }

    private def listFilterMap: String = {
      s"""
         |def main(): Unit \\ IO = {
         |    List.range(0, 10_000) |> List.filterMap(x -> if (Int32.remainder(x, 2) == 0) Some(x) else None) |> blackhole
         |}
         |
         |def blackhole(t: a): Unit \\ IO =
         |    Ref.fresh(Static, t); ()
         |
         |""".stripMargin
    }

    private def map10KLength: String = {
      s"""
         |def main(): Unit \\ IO = {
         |    let l1 = range(0, 10_000);
         |    let l2 = map(x -> x + 1, l1);
         |    let l3 = length(l3);
         |    blackhole(l3)
         |}
         |
         |pub def map(f: a -> b, l: List[a]) : List[b] = {
         |    def mp(xs, acc) = match xs {
         |        case Nil     => acc
         |        case z :: zs => mp(zs, f(z) :: acc)
         |    };
         |    rev(mp(l, Nil))
         |}
         |
         |pub def rev(l: List[a]): List[a] = {
         |    def rv(xs, acc) = match xs {
         |        case Nil     => acc
         |        case z :: zs => rv(zs, z :: acc)
         |    };
         |    rv(l, Nil)
         |}
         |
         |pub def range(bot: Int32, top: Int32): List[Int32] = {
         |    def rng(i, acc) = if (i < bot) acc else rng(i - 1, i :: acc);
         |    rng(top - 1, Nil)
         |}
         |
         |pub def length(l: List[a]): Int32 = {
         |    def len(xs, acc) = match xs {
         |        case Nil     => acc
         |        case _ :: zs => len(zs, acc + 1)
         |    };
         |    len(l, 0)
         |}
         |
         |def blackhole(t: a): Unit \\ IO =
         |    Ref.fresh(Static, t); ()
         |
         |""".stripMargin
    }

    private def map10KLengthOptimized: String = {
      s"""
         |def main(): Unit \\ IO = {
         |    let top = 10_000 - 1;
         |    let l1 = rng(top, Nil);
         |    let l2 = mp(l1, Nil);
         |    let l3 = rv(l2, Nil);
         |    let l4 = ln(l3, 0);
         |    blackhole(l4)
         |}
         |
         |pub def mp(xs: List[Int32], acc: List[Int32]): List[Int32] = match xs {
         |    case Nil     => acc
         |    case z :: zs => mp(zs, z + 1 :: acc)
         |}
         |
         |pub def rv(xs: List[a], acc: List[a]): List[a] = match xs {
         |    case Nil     => acc
         |    case z :: zs => rv(zs, z :: acc)
         |}
         |
         |pub def ln(xs: List[a], acc: Int32): Int32 = match xs {
         |    case Nil     => acc
         |    case _ :: zs => ln(zs, acc + 1)
         |}
         |
         |def blackhole(t: a): Unit \\ IO =
         |    Ref.fresh(Static, t); ()
         |
         |""".stripMargin
    }

    private def filterMap10K: String = {
      s"""
         |def main(): Unit \\ IO = {
         |    let l1 = range(0, 10_000);
         |    let l2 = filterMap(x -> if (Int32.remainder(x, 2) == 0) Some(x) else None, l1);
         |    blackhole(l2)
         |}
         |
         |pub def filterMap(f: a -> Option[b] \\ ef, l: List[a]): List[b] \\ ef = {
         |    def fmp(ll, acc) = match ll {
         |        case Nil     => acc
         |        case x :: xs => match f(x) {
         |            case None    => fmp(xs, acc)
         |            case Some(y) => fmp(xs, y :: acc)
         |        }
         |    };
         |    rev(fmp(l, Nil))
         |}
         |
         |pub def rev(l: List[a]): List[a] = {
         |    def rv(xs, acc) = match xs {
         |        case Nil     => acc
         |        case z :: zs => rv(zs, z :: acc)
         |    };
         |    rv(l, Nil)
         |}
         |
         |pub def range(bot: Int32, top: Int32): List[Int32] = {
         |    def rng(i, acc) = if (i < bot) acc else rng(i - 1, i :: acc);
         |    rng(top - 1, Nil)
         |}
         |
         |def blackhole(t: a): Unit \\ IO =
         |    Ref.fresh(Static, t); ()
         |
         |""".stripMargin

    }

    private def filterMap10KOptimized: String = {
      s"""
         |def main(): Unit \\ IO = {
         |    let top = 10_000 - 1;
         |    let l1 = rng(top, Nil);
         |    let l2 = fmp(l1, Nil);
         |    let l3 = rv(l2, Nil);
         |    blackhole(l3)
         |}
         |
         |pub def fmp(l: List[Int32], acc: List[Int32]): List[Int32] = match l {
         |    case Nil     => acc
         |    case x :: xs =>
         |        if (Int32.remainder(x, 2) == 0)
         |            fmp(xs, acc)
         |        else
         |            fmp(xs, x :: acc)
         |}
         |
         |pub def rv(xs: List[a], acc: List[a]): List[a] = match xs {
         |    case Nil     => acc
         |    case z :: zs => rv(zs, z :: acc)
         |}
         |
         |pub def rng(i: Int32, acc: List[Int32]): List[Int32] = if (i < 0) acc else rng(i - 1, i :: acc)
         |
         |def blackhole(t: a): Unit \\ IO =
         |    Ref.fresh(Static, t); ()
         |
         |""".stripMargin
    }

    /**
      * Returns the given time `l` in milliseconds.
      */
    private def milliseconds(l: Long): Double = l / 1_000_000.0

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
