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

  /**
    * Set this to `true` for additional details during benchmarking.
    */
  private val Verbose: Boolean = true

  private val Python: String =
    """
      |# $ pip install matplotlib numpy
      |
      |import json
      |import matplotlib.pyplot as plt
      |import matplotlib.lines as mlines
      |import numpy as np
      |
      |def get_normalized_data(metric: str, inliner_type: str, data):
      |    xs = []
      |    ys = []
      |    for obj in data['programs']:
      |        norm = None # Should crash if we do not find the norm first
      |        for run in obj['results']:
      |            if run['inlinerType'] == 'NoInliner':
      |                norm = run[metric]
      |        for run in obj['results']:
      |            if run['inlinerType'].lower() == inliner_type.lower():
      |                xs.append(run['inliningRounds'])
      |                ys.append(run[metric] / norm)
      |    return [xs, ys]
      |
      |def get_normalized_data_with_program(metric: str, inliner_type: str, data):
      |    \"\"\"
      |    Returns the data for a given metric and inliner type sampled from all runs in the shape
      |    of a list of 3-tuples.
      |    For an entry `[pr, xs, ys]`, `pr` is the name of the program, `xs` is the number of inlining rounds
      |    and `ys` is the metric normalized by the same metric when no inlining was made.
      |    \"\"\"
      |    ps = []
      |    for obj in data['programs']:
      |        pr = obj['programName']
      |        xs = []
      |        ys = []
      |        norm = None # Should crash if we do not find the norm first
      |        for run in obj['results']:
      |            if run['inlinerType'] == 'NoInliner':
      |                norm = run[metric]
      |        for run in obj['results']:
      |            if run['inlinerType'].lower() == inliner_type.lower():
      |                xs.append(run['inliningRounds'])
      |                ys.append(run[metric] / norm)
      |        ps.append([pr, xs, ys])
      |    return ps
      |
      |def get_data_for(metric: str, inliner_type: str, inliner_rounds: int, data):
      |    result = []
      |    for benchmark in data['programs']:
      |        for run in benchmark['results']:
      |            it = run['inlinerType'].lower()
      |            ir = run['inliningRounds']
      |            if it == inliner_type.lower() and ir == inliner_rounds:
      |                result.append([benchmark['programName'], run[metric]])
      |    return result
      |
      |def gen_colors(n):
      |    \"\"\"
      |    Returns n distinct colors, up to 11.
      |    \"\"\"
      |    if n > 11:
      |        raise Exception(f"parameter n was {n} but can at most be 11")
      |    colors = list(map(lambda i: 'C' + str(i), range(n)))
      |    colors[-1] = 'purple'
      |    return colors
      |
      |def get_color_map(progs):
      |    colors = gen_colors(len(progs))
      |    result = {}
      |    for p, c in zip(progs, colors):
      |        result[p] = c
      |    return result
      |
      |def get_summary_data(benchmark: str, metric: str, data):
      |    keys = []
      |    values = []
      |    for obj in data['programs']:
      |        keys.append(obj['programName'])
      |        values.append(obj['summary'][benchmark][metric])
      |    return [keys, values]
      |
      |def nanos_to_milis(n):
      |    return n / 1_000_000
      |
      |with open("programsBenchmark.json") as file:
      |    data = json.load(file)
      |    COLOR_MAP = get_color_map(list(map(lambda obj: obj['programName'], data['programs'])))
      |
      |
      |    #################################################################
      |    ####### WORST RUNNING TIMES PER PROGRAM #########################
      |    #################################################################
      |
      |    fig, ax = plt.subplots()
      |    keys, values = get_summary_data('runningTime', 'worst', data)
      |    times = list(map(nanos_to_milis, values))
      |    colors = list(map(lambda k: COLOR_MAP[k], keys))
      |    bar_container = ax.bar(keys, times, label=keys, color=colors)
      |    ax.bar_label(bar_container, fmt='{:,.1f}')
      |    ax.set_xticks(ax.get_xticks(), labels=[]) # Remove program names from x-axis
      |    ax.set_xlabel('Programs')
      |    ax.set_ylabel('Running time (ms)')
      |    ax.set_yscale('linear')
      |    ax.set_ylim(top=750)
      |    ax.set_title('Slowest running times')
      |    ax.legend(title='Program', loc='upper left', ncols=3, fontsize=7)
      |    plt.savefig('worstRunningTimes.png', dpi=300)
      |
      |
      |    #################################################################
      |    ####### BEST RUNNING TIMES PER PROGRAM ##########################
      |    #################################################################
      |
      |    fig, ax = plt.subplots()
      |    keys, values = get_summary_data('runningTime', 'best', data)
      |    times = list(map(nanos_to_milis, values))
      |    colors = list(map(lambda k: COLOR_MAP[k], keys))
      |    bar_container = ax.bar(keys, times, label=keys, color=colors)
      |    ax.bar_label(bar_container, fmt='{:,.1f}')
      |    ax.set_xticks(ax.get_xticks(), labels=[]) # Remove program names from x-axis
      |    ax.set_xlabel('Programs')
      |    ax.set_ylabel('Running time (ms)')
      |    ax.set_yscale('linear')
      |    ax.set_ylim(top=750)
      |    ax.set_title('Fastest running times')
      |    ax.legend(title='Program', loc='upper left', ncols=3, fontsize=7)
      |    plt.savefig('bestRunningTimes.png', dpi=300)
      |    ax.set_ylim(top=120)
      |    ax.set_title('Fastest running times (scaled to fit)')
      |    plt.savefig('bestRunningTimesZoomed.png', dpi=300)
      |
      |
      |    #################################################################
      |    ####### CODE SIZE PER INLINING ROUND AND TYPE ###################
      |    #################################################################
      |
      |    cross_figure = mlines.Line2D([], [], color='gray', marker='x', markersize=8, linestyle='', label='New Inliner')
      |    circle_figure = mlines.Line2D([], [], color='gray', marker='o', markersize=8, linestyle='', label='Old Inliner')
      |
      |    fig, ax = plt.subplots()
      |    old_data = get_normalized_data_with_program('codeSize', 'Old', data)
      |    new_data = get_normalized_data_with_program('codeSize', 'New', data)
      |
      |    for pr, xs, ys in old_data:
      |        ax.plot(xs, ys, 'o', color=COLOR_MAP[pr])
      |
      |    for pr, xs, ys in new_data:
      |        ax.plot(xs, ys, 'x', color=COLOR_MAP[pr])
      |
      |    ax.set_title('Code size (normalized by code size without inlining)')
      |    ax.set_xlabel('Inlining Rounds')
      |    ax.set_ylabel('Factor')
      |    ax.set_ylim(top=0.95)
      |    ax.grid(visible=True, which='both')
      |    ax.legend(handles=[cross_figure, circle_figure], title='Inliner Used', loc='upper right')
      |    plt.savefig('codeSizePerRounds.png', dpi=300)
      |
      |
      |    #################################################################
      |    ####### RUNNING TIME PER INLINING ROUND AND TYPE ################
      |    #################################################################
      |
      |    fig, ax = plt.subplots()
      |    old_data = get_normalized_data_with_program('runningTime', 'Old', data)
      |    new_data = get_normalized_data_with_program('runningTime', 'New', data)
      |
      |    for pr, xs, ys in old_data:
      |        ax.plot(xs, ys, 'o', color=COLOR_MAP[pr])
      |
      |    for pr, xs, ys in new_data:
      |        ax.plot(xs, ys, 'x', color=COLOR_MAP[pr])
      |
      |    ax.set_title('Running Time (normalized by running time without inlining)')
      |    ax.set_xlabel('Inlining Rounds')
      |    ax.set_ylabel('Factor')
      |    ax.grid(visible=True, which='both')
      |    ax.legend(handles=[cross_figure, circle_figure], title='Inliner Used', loc='upper right')
      |    plt.savefig('runningTimePerRounds.png', dpi=300)
      |
      |
      |    #################################################################
      |    ####### COMPILATION TIME PER INLINING ROUND AND TYPE ############
      |    #################################################################
      |
      |    fig, ax = plt.subplots()
      |    old_data = get_normalized_data_with_program('compilationTime', 'Old', data)
      |    new_data = get_normalized_data_with_program('compilationTime', 'New', data)
      |
      |    for pr, xs, ys in old_data:
      |        ax.plot(xs, ys, 'o', color=COLOR_MAP[pr])
      |
      |    for pr, xs, ys in new_data:
      |        ax.plot(xs, ys, 'x', color=COLOR_MAP[pr])
      |
      |    ax.set_title('Compilation Time (normalized by compilation time without inlining)')
      |    ax.set_xlabel('Inlining Rounds')
      |    ax.set_ylabel('Factor')
      |    ax.grid(visible=True, which='both')
      |    ax.legend(handles=[cross_figure, circle_figure], title='Inliner Used', loc='upper right')
      |    plt.savefig('compilationTimePerRounds.png', dpi=300)
      |
      |
      |    #################################################################
      |    ####### SPEEDUP PER PROGRAM VS NO INLINING ######################
      |    #################################################################
      |
      |    fig, ax = plt.subplots(layout='constrained')
      |    old_data = get_data_for(metric='runningTime', inliner_type='NoInliner', inliner_rounds=0, data=data)
      |    new_data = get_data_for(metric='runningTime', inliner_type='New', inliner_rounds=3, data=data)
      |
      |    progs = []
      |    tmp = []
      |    for p0, r0 in old_data:
      |        progs.append(p0)
      |        for p1, r1 in new_data:
      |            if p0 == p1:
      |                tmp.append(r0 / r1)
      |
      |    print(progs)
      |
      |    speedups = {}
      |    speedups['baseline'] = np.repeat(1, 11)
      |    speedups['speedup'] = np.array(tmp)
      |
      |    x = np.arange(len(old_data))
      |    width = 0.4
      |    multiplier = 0
      |
      |    for attr, val in speedups.items():
      |        offset = width * multiplier
      |        rects = ax.bar(x + offset, val, width, label=attr)
      |        ax.bar_label(rects, padding=3, fmt='{:,.1f}x' if attr == 'speedup' else '')
      |        multiplier += 1
      |
      |    ax.set_ylabel('Speedup')
      |    ax.set_title('Median Running Time Speedup')
      |    ax.set_xticks(x + (width / 2), np.arange(len(old_data)) + 1)
      |    ylim_top = 10
      |    ax.set_ylim(top=ylim_top)
      |    ax.set_yticks(range(ylim_top + 1), labels=list(map(lambda x: f"{x}x" if x != 0 else '', range(ylim_top + 1))))
      |
      |    plt.savefig('runningTimeSpeedup.png', dpi=300)
      |
      |
      |    #################################################################
      |    ####### SPEEDUP PER PROGRAM VS OLD INLINER ######################
      |    #################################################################
      |
      |    fig, ax = plt.subplots(layout='constrained')
      |    old_data = get_data_for(metric='runningTime', inliner_type='Old', inliner_rounds=3, data=data)
      |    new_data = get_data_for(metric='runningTime', inliner_type='New', inliner_rounds=3, data=data)
      |
      |    progs = []
      |    tmp = []
      |    for p0, r0 in old_data:
      |        progs.append(p0)
      |        for p1, r1 in new_data:
      |            if p0 == p1:
      |                tmp.append(r0 / r1)
      |
      |    print(progs)
      |
      |    speedups = {}
      |    speedups['baseline'] = np.repeat(1, 11)
      |    speedups['speedup'] = np.array(tmp)
      |
      |    x = np.arange(len(old_data))
      |    width = 0.4
      |    multiplier = 0
      |
      |    for attr, val in speedups.items():
      |        offset = width * multiplier
      |        rects = ax.bar(x + offset, val, width, label=attr)
      |        ax.bar_label(rects, padding=3, fmt='{:,.1f}x' if attr == 'speedup' else '')
      |        multiplier += 1
      |
      |    ax.set_ylabel('Speedup')
      |    ax.set_title('Median Running Time Speedup vs. Old Inliner')
      |    ax.set_xticks(x + (width / 2), np.arange(len(old_data)) + 1)
      |    ylim_top = 2
      |    ax.set_ylim(top=ylim_top)
      |    ax.set_yticks(range(ylim_top + 1), labels=list(map(lambda x: f"{x}x" if x != 0 else '', range(ylim_top + 1))))
      |
      |    plt.savefig('runningTimeSpeedupOldInliner.png', dpi=300)
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
    debug(s"Took $seconds seconds")

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

    private val NumberOfRuns = 100_000

    private val NumberOfSamples = 1000

    private val MaxInliningRounds = 5

    /**
      * Represents a run of a single program.
      *
      * @param name            The name of the program
      * @param lines           The number of lines of source code in the program
      * @param inlinerType     Which inliner was used (if any)
      * @param inliningRounds  The number of rounds of inlining
      * @param runningTime     The median running time of the program
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
      debug(s"Running up to $MaxInliningRounds inlining rounds, drawing $NumberOfSamples samples of timing $NumberOfRuns runs of each program")
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
      val o0 = opts.copy(xnooptimizer = true, xnooptimizer1 = true, lib = LibLevel.All, progress = false, incremental = false, inlinerRounds = 0, inliner1Rounds = 0)
      val o1 = opts.copy(xnooptimizer = false, xnooptimizer1 = true, lib = LibLevel.All, progress = false, incremental = false)
      val o2 = opts.copy(xnooptimizer = true, xnooptimizer1 = false, lib = LibLevel.All, progress = false, incremental = false)
      val allOptions = o0 :: (1 to MaxInliningRounds).map(r => o1.copy(inlinerRounds = r, inliner1Rounds = r)).toList ::: (1 to MaxInliningRounds).map(r => o2.copy(inlinerRounds = r, inliner1Rounds = r)).toList
      val runConfigs = allOptions.flatMap(c => programs.map { case (name, prog) => (c, name, prog) })

      val runs = scala.collection.mutable.ListBuffer.empty[Run]
      for ((o, name, prog) <- runConfigs) {
        debug(s"Benchmarking $name")
        debug("Benchmarking compiler")

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

        debug("Benchmarking running time")
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
        debug(s"Done benchmarking $name with inliner '${InlinerType.from(o)}' with ${o.inliner1Rounds} rounds")
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

  private def debug(s: String): Unit = {
    if (Verbose) {
      println(s)
    }
  }

}
