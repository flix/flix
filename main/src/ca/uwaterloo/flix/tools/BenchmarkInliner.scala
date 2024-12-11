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

  private val Python: String =
    """
      |# $ pip install pandas matplotlib
      |
      |import json
      |import matplotlib
      |import matplotlib.pyplot as plt
      |import numpy as np
      |
      |with open("programsBenchmark.json") as file:
      |    data = json.load(file)
      |
      |    programs = data['programs']
      |    summaries = {}
      |    for obj in programs:
      |        name = obj['programName']
      |        summary = obj['summary']
      |        summaries[name] = summary
      |
      |    # Plot layout stuff
      |    x = np.arange(len(summaries))
      |    width = 0.25
      |    multiplier = 0
      |
      |    fix, ax = plt.subplots(layout='constrained')
      |
      |    for program, summary in summaries.items():
      |        offset = width * multiplier
      |        rects = ax.bar(x + offset, summary['worst'], width, label=program)
      |        ax.bar_label(rects, padding=3)
      |        multiplier += 1
      |
      |
      |    ax.set_ylabel('Running time (ms)')
      |    ax.set_title('Running times of Flix benchmark programs')
      |    ax.set_xticks(x + width, list(map(lambda obj: obj['programName'], programs)))
      |    plt.savefig('benchmarkProgramsRunningTimes.png')
      |
      |""".stripMargin

  def run(opts: Options): Unit = {
    FileOps.writeString(Path.of("./build/").resolve("perf/").resolve("plots.py"), Python)

    val t0 = System.currentTimeMillis()

    println("Benchmarking inliner. This may take a while...")

    val programBenchmark = BenchmarkPrograms.run(opts)
    writeFile("programsBenchmark.json", programBenchmark)

    println("Done. Results written to 'build/perf'")

    val tDelta = System.currentTimeMillis() - t0
    val seconds = tDelta.toDouble / 1000
    println(s"Took $seconds seconds")

    // Experiments:
    // 1. Compiler throughput and code size
    //    (a) without inlining
    //    (b) with old inliner
    //    (c) with new inliner

    // 2. Flix program speedup (sample programs, datalog engine, parser library)
    //    (a) without inlining
    //    (b) with old inliner
    //    (c) with new inliner
    // TODO: Vary thresholds for new inliner


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

    private val MaxInliningRounds = 50

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

      val cartesianRuns = programExperiments.map {
        case (name, runs) => name -> cartesian(runs, runs)
      }

      val relativeRunningTimes = cartesianRuns.map {
        case (name, runsXRuns) => name -> runsXRuns.map {
          case (run1, run2) => (run1.inlinerType, run2.inlinerType,
            run1.inliningRounds, run2.inliningRounds,
            run1.runningTime.toDouble / run2.runningTime.toDouble)
        }
      }

      val relativeRunningTimeStats = relativeRunningTimes.map {
        case (name, runs) => name -> {
          val relativeTimings = runs.map {
            case (_, _, _, _, timing) => timing
          }
          stats(relativeTimings)
        }
      }


      val relativeCompilationTimes = cartesianRuns.map {
        case (name, runsXRuns) => name -> runsXRuns.map {
          case (run1, run2) => (run1.inlinerType, run2.inlinerType,
            run1.inliningRounds, run2.inliningRounds,
            run1.compilationTime.toDouble / run2.compilationTime.toDouble)
        }
      }
      val relativeCompilationTimeStats = relativeCompilationTimes.map {
        case (name, runs) => name -> {
          val relativeTimings = runs.map {
            case (_, _, _, _, timing) => timing
          }
          stats(relativeTimings)
        }
      }

      // Timestamp (in seconds) when the experiment was run.
      val timestamp = System.currentTimeMillis() / 1000

      ("timestamp" -> timestamp) ~
        ("programs" -> {
          programExperiments.map {
            case (name, runs) =>
              ("programName" -> name) ~ {
                ("summary" -> {
                  ("runningTime" -> {
                    val stats = runningTimeStats.apply(name)
                    ("best" -> stats.min) ~
                      ("worst" -> stats.max) ~
                      ("average" -> stats.average) ~
                      ("median" -> stats.median)
                  }) ~
                    ("relativeRunningTime" -> {
                      val relativeTimings = relativeRunningTimes.apply(name)
                      val stats = relativeRunningTimeStats.apply(name)
                      val best = relativeTimings.filter { case (_, _, _, _, timing) => timing == stats.min }.map {
                        case (type1, type2, rounds1, rounds2, timing) =>
                          ("inlinerType1" -> type1.toString) ~
                            ("inlinerType2" -> type2.toString) ~
                            ("inlinerRounds1" -> rounds1.toString) ~
                            ("inlinerRounds2" -> rounds2.toString) ~
                            ("speedup" -> timing)
                      }
                      val worst = relativeTimings.filter { case (_, _, _, _, timing) => timing == stats.max }.map {
                        case (type1, type2, rounds1, rounds2, timing) =>
                          ("inlinerType1" -> type1.toString) ~
                            ("inlinerType2" -> type2.toString) ~
                            ("inlinerRounds1" -> rounds1.toString) ~
                            ("inlinerRounds2" -> rounds2.toString) ~
                            ("speedup" -> timing)
                      }
                      ("best" -> best) ~
                        ("worst" -> worst)
                    }) ~
                    ("compilationTime" -> {
                      val stats = compilationTimeStats.apply(name)
                      ("best" -> stats.min) ~
                        ("worst" -> stats.max) ~
                        ("average" -> stats.average) ~
                        ("median" -> stats.median)
                    }) ~
                  ("relativeCompilationTime" -> {
                    val relativeTimings = relativeCompilationTimes.apply(name)
                    val stats = relativeCompilationTimeStats.apply(name)
                    val best = relativeTimings.filter { case (_, _, _, _, timing) => timing == stats.min }.map {
                      case (type1, type2, rounds1, rounds2, timing) =>
                        ("inlinerType1" -> type1.toString) ~
                          ("inlinerType2" -> type2.toString) ~
                          ("inlinerRounds1" -> rounds1.toString) ~
                          ("inlinerRounds2" -> rounds2.toString) ~
                          ("speedup" -> timing)
                    }
                    val worst = relativeTimings.filter { case (_, _, _, _, timing) => timing == stats.max }.map {
                      case (type1, type2, rounds1, rounds2, timing) =>
                        ("inlinerType1" -> type1.toString) ~
                          ("inlinerType2" -> type2.toString) ~
                          ("inlinerRounds1" -> rounds1.toString) ~
                          ("inlinerRounds2" -> rounds2.toString) ~
                          ("speedup" -> timing)
                    }
                    ("best" -> best) ~
                      ("worst" -> worst)
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
      val allOptions = o0 :: (1 to MaxInliningRounds).map(r => o1.copy(inlinerRounds = r, inliner1Rounds = r)).toList ::: (1 to MaxInliningRounds).map(r => o2.copy(inlinerRounds = r, inliner1Rounds = r)).toList
      val runConfigs = allOptions.flatMap(c => programs.map { case (name, prog) => (c, name, prog) })

      val runs = scala.collection.mutable.ListBuffer.empty[Run]
      for ((o, name, prog) <- runConfigs) {
        val compilationTimings = scala.collection.mutable.ListBuffer.empty[(Long, List[(String, Long)])]

        for (_ <- 1 to 10) {
          val flix = new Flix().setOptions(o)
          // Clear caches.
          ZhegalkinCache.clearCaches()
          flix.addSourceCode(s"$name.flix", prog)
          val result = flix.compile().unsafeGet
          val phaseTimes = flix.phaseTimers.map { case PhaseTime(phase, time) => phase -> time }.toList
          val timing = (result.totalTime, phaseTimes)
          compilationTimings += timing
        }

        val runningTimes = scala.collection.mutable.ListBuffer.empty[Long]
        val flix = new Flix().setOptions(o)
        // Clear caches.
        ZhegalkinCache.clearCaches()
        flix.addSourceCode(s"$name.flix", prog)
        val result = flix.compile().unsafeGet
        result.getMain match {
          case Some(mainFunc) =>
            for (_ <- 1 to NumberOfSamples) {
              val t0 = System.nanoTime()
              // Iterate the program NumberOfRuns time
              for (_ <- 1 to NumberOfRuns) {
                mainFunc(Array.empty)
              }
              val tDelta = System.nanoTime() - t0
              runningTimes += tDelta
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
         def main(): Unit \\ IO = {
         |    let l1 = range(0, 10_000);
         |    let l2 = map(x -> x + 1, l1);
         |    let l3 = length(l2);
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
         |pub def rng(i: Int32, acc: List[Int32]): List[Int32] = if (i < 0) acc else rng(i - 1, i :: acc)
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

    /**
      * Returns the cartesian product of `xs` and `ys`.
      */
    private def cartesian[A, B](xs: Seq[A], ys: Seq[B]): Seq[(A, B)] = {
      xs.flatMap {
        x => ys.map(y => (x, y))
      }
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
