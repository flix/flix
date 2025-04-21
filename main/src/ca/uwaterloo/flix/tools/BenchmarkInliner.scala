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
import ca.uwaterloo.flix.runtime.CompilationResult
import ca.uwaterloo.flix.util.StatUtils.{average, median}
import ca.uwaterloo.flix.util.collection.ListMap
import ca.uwaterloo.flix.util.{FileOps, LibLevel, Options}
import org.json4s.JsonAST
import org.json4s.JsonDSL.*

import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, SimpleFileVisitor}
import java.util.zip.{ZipEntry, ZipOutputStream}
import java.util.{Calendar, GregorianCalendar}
import scala.collection.mutable
import scala.io.StdIn
import scala.util.{Failure, Success, Using}

object BenchmarkInliner {

  private val RunningTimeWarmupTime: Int = 5

  private val RunningTimeBenchmarkTime: Int = 5

  private val CompilationWarmupTime: Int = 0

  private val CompilationBenchmarkTime: Int = 2

  private val MaxInliningRounds: Int = 5

  private val NumberOfRuns: Int = 1000

  /**
    * Set this to `true` for additional details during benchmarking.
    */
  private val Verbose: Boolean = true

  private val MicroBenchmarks: Map[String, String] = Map(
    "map10KLength" -> map10KLength,
    "map10KLengthEff" -> map10KLengthEff,
    "filterMap10K" -> filterMap10K,
  )

  private val MediumBenchmarks: Map[String, String] = Map(
    "List.filter" -> listFilter,
    "List.foldLeft" -> listFoldLeft,
    "List.foldRight" -> listFoldRight,
    "List.length" -> listLength,
    "List.reverse" -> listReverse,
    "map10KLengthOptimized" -> map10KLengthOptimized,
    "filterMap10KOptimized" -> filterMap10KOptimized,
    "List.map" -> listMap,
    "List.filterMap" -> listFilterMap,
  )

  private val MacroBenchmarks: Map[String, String] = Map(
    "FordFulkerson" -> fordFulkerson,
    "parsers" -> parsers
  )

  private def baseDir: Path = Path.of("./build/").normalize()

  private def classDir: Path = baseDir.resolve("class/").normalize()

  private def classDirFor(path: String): Path = classDir.resolve(path).normalize()

  private def jarDir: Path = baseDir.resolve("jar/").normalize()

  private def jarDirFor(path: String): Path = jarDir.resolve(path).normalize()

  private def benchOutputPath: Path = baseDir.resolve("perf/").normalize()

  private def scriptOutputPath: Path = baseDir.resolve("scripts/").normalize()

  private def benchmarkScriptPath: Path = scriptOutputPath.resolve("benchmark.sh").normalize()

  private def runScriptPath: Path = scriptOutputPath.resolve("all.sh").normalize()

  private def pythonPath: Path = scriptOutputPath.resolve("plots.py").normalize()

  def generateSetup(opts: Options, micro: Boolean = true): Unit = {
    println("Generating setup...")

    // TODO: Maybe pass this as a program config to the run instance
    // TODO: Then create public pre-made configs in this object
    val programs = if (micro) MicroBenchmarks else MacroBenchmarks

    println("Building jars...")
    writeJars(programs, opts)
    FileOps.writeString(runScriptPath, mkRunScript(programs.size))
    FileOps.writeString(pythonPath, Python)
    Files.createDirectories(benchOutputPath)
    println("")
  }

  private def mkRunScript(programCount: Int): String = {
    val compilerEstimate = estimateTimeMinutes(programCount, CompilationWarmupTime, CompilationBenchmarkTime)
    val flixEstimate = estimateTimeMinutes(programCount, RunningTimeWarmupTime, RunningTimeBenchmarkTime)
    val totalEstimate = compilerEstimate + flixEstimate
    s"""#!/bin/bash
       |
       |echo "Compiler benchmark time estimate: $compilerEstimate minutes"
       |echo "Flix benchmark time estimate: $flixEstimate minutes"
       |echo "Total time estimate: $totalEstimate minutes"
       |
       |echo "Benchmarking Compiler..."
       |java -jar flix.jar Xinliner
       |
       |echo "Benchmarking programs..."
       |bash $benchmarkScriptPath
       |
       |echo "Done"
       |
       |""".stripMargin
  }

  def runCompilerBenchmark(opts: Options, micro: Boolean = true): Unit = {
    val programs = if (micro) MicroBenchmarks else MacroBenchmarks
    val outFileName = if (micro) "micro.json" else "macro.json"

    val pid = java.lang.ProcessHandle.current().pid()
    println(s"Please connect profiling tools if necessary (e.g., async-profiler). PID: $pid")
    val input = StdIn.readLine("Ready to proceed? [Y/n] ")
    input.toLowerCase match {
      case "n" | "no" | "abort" =>
        println("Aborting...")
        return
      case _ =>
    }

    println("Benchmarking inliner compilation...")
    val t0 = System.nanoTime()
    val benchmarks = runBenchmarking(programs, opts)
    val filePath = benchOutputPath.resolve(outFileName).normalize()
    FileOps.writeJSON(filePath, benchmarks)

    println(s"Done. Results written to '$filePath'")

    val tDelta = System.nanoTime() - t0
    val seconds = nanosToMinutes(tDelta)
    println(s"Took $seconds minutes total")
  }

  private def debug(s: String): Unit = {
    if (Verbose) {
      println(s)
    }
  }

  private def writeJars(programs: Map[String, String], opts: Options): Unit = {
    val configs = mkConfigurations(opts.copy(loadClassFiles = false))
      .flatMap(o => programs.map { case (name, prog) => (o, name, prog) })
    configs.foreach(buildAndWrite)
    val snippets = configs.map {
      case (o, name, _) => mkScriptSnippet(BenchmarkFile(name, o))
    }
    val script = mkScript(snippets)
    FileOps.writeString(benchmarkScriptPath, script)
  }

  private def buildAndWrite(config: (Options, String, String)): Unit = {
    val (opts, name, prog) = config

    // Build
    implicit val sctx: SecurityContext = SecurityContext.AllPermissions
    val file = BenchmarkFile(name, opts)
    val baseline = BenchmarkFile.BaselineFile(file)
    Files.createDirectories(file.BuildDir)
    val flix = new Flix().setOptions(opts.copy(output = Some(file.BuildDir)))
    flix.addSourceCode(name, prog)
    flix.addSourceCode("mainProg", mainProg(baseline.toString))
    flix.compile().unsafeGet

    // Jar
    Files.createDirectories(jarDir)
    val classFiles =
      FromBootstrap.getAllFiles(file.ClassFilesDir)
        .map { path =>
          (path, FromBootstrap.convertPathToRelativeFileName(file.ClassFilesDir, path))
        }.sortBy(snd)


    Using(new ZipOutputStream(Files.newOutputStream(file.JarFilePath))) { zip =>
      val (manifestName, manifestContent) = FromBootstrap.manifest
      FromBootstrap.addToZip(zip, manifestName, manifestContent.getBytes)

      for ((buildFile, fileNameWithSlashes) <- classFiles) {
        FromBootstrap.addToZip(zip, fileNameWithSlashes, buildFile)
      }
    } match {
      case Success(_) =>
      case Failure(e) => throw e
    }
  }

  private case class BenchmarkFile(private val name: String, private val opts: Options) {
    private val FileName: String = {
      val rounds = InlinerRounds.rounds(opts)
      s"${name}_$rounds"
    }
    private val JarName: String = s"$FileName.jar"
    val BuildDir: Path = classDirFor(s"$FileName/")
    val ClassFilesDir: Path = BuildDir.resolve("class/").normalize()
    val JarFilePath: Path = jarDirFor(JarName)
    val OutputFile: Path = benchOutputPath.resolve(s"$FileName.json").normalize()
  }

  private object BenchmarkFile {
    def BaselineFile(file: BenchmarkFile): Path = {
      val baseOpts = file.opts.copy(inlinerRounds = 0, xnooptimizer = true)
      BenchmarkFile(file.name, baseOpts).OutputFile
    }
  }

  private def mkScriptSnippet(file: BenchmarkFile): String = {
    s"""rm -f ${file.OutputFile}
       |echo "Benchmarking ${file.JarFilePath}"
       |java -jar ${file.JarFilePath} >> ${file.OutputFile}
       |""".stripMargin
  }

  private def mkScript(snippets: List[String]): String = {
    s"""#!/bin/bash
       |
       |${snippets.mkString("\n")}
       |
       |""".stripMargin
  }

  private def fst[A, B](x: (A, B)): A = x._1

  private def snd[A, B](x: (A, B)): B = x._2

  private def estimateTimeMinutes(programsCount: Int, warmupTime: Int, benchmarkTime: Int): Int = {
    val timeCalc = (time: Int) => {
      val allProgsTime = time * programsCount
      val withInlining = allProgsTime * MaxInliningRounds
      val withoutInlining = allProgsTime
      withInlining + withoutInlining
    }
    timeCalc(warmupTime) + timeCalc(benchmarkTime)
  }

  private def runBenchmarking(programs: Map[String, String], opts: Options): JsonAST.JObject = {
    val totalTime = estimateTimeMinutes(programs.size, CompilationWarmupTime, CompilationBenchmarkTime)
    debug(s"Programs        : ${programs.size}")
    debug(s"Rounds          : $MaxInliningRounds")
    debug(s"Warmup          : $CompilationWarmupTime minutes")
    debug(s"Bench           : $CompilationBenchmarkTime minutes")
    debug(s"Total (Compiler): $totalTime minutes")

    val runConfigs = mkConfigurations(opts).flatMap(o => programs.map { case (name, prog) => (o, name, prog) })
    val programExperiments = benchmarkWithIndividualMaxTime(runConfigs, minutesToNanos(CompilationWarmupTime), minutesToNanos(CompilationBenchmarkTime))

    val compilationTimeStats = programExperiments.m.map {
      case (name, runs) => name -> stats(runs.map(_.compilationTime))
    }

    // Timestamp (in seconds) when the experiment was run
    val timestamp = nanosToSeconds(System.nanoTime())

    ("timestamp" -> timestamp) ~
      ("programs" -> {
        programExperiments.m.map {
          case (name, runs) =>
            ("programName" -> name) ~ {
              ("summary" -> {
                "compilationTime" -> {
                  val stats = compilationTimeStats.apply(name)
                  ("best" -> stats.min) ~
                    ("worst" -> stats.max) ~
                    ("average" -> stats.average) ~
                    ("median" -> stats.median)
                }
              }) ~
                ("results" -> runs.map(_.toJson))
            }
        }
      })
  }

  private object InlinerRounds {
    def rounds(options: Options): Int = {
      if (options.xnooptimizer)
        0
      else
        options.inlinerRounds
    }
  }

  /**
    * Represents a run of a single program.
    *
    * @param name            The name of the program
    * @param lines           The number of lines of source code in the program
    * @param inliningRounds  The number of rounds of inlining
    * @param compilationTime The median time taken to compile the program
    * @param phases          The median running time of each compilation phase
    * @param codeSize        The number of bytes of the compiled program
    */
  private case class Run(name: String, lines: Int, inliningRounds: Int, compilationTime: Long, phases: List[(String, Long)], codeSize: Int) {
    def toJson: JsonAST.JObject = {
      ("lines" -> lines) ~
        ("inliningRounds" -> inliningRounds) ~
        ("compilationTime" -> compilationTime) ~
        ("phases" -> phases) ~
        ("codeSize" -> codeSize)
    }
  }

  private case class Stats[T](min: T, max: T, average: Double, median: Double)

  private def stats[T](xs: Seq[T])(implicit numeric: Numeric[T]): Stats[T] = {
    Stats(xs.min, xs.max, average(xs), median(xs))
  }

  private def mkConfigurations(opts: Options): List[Options] = {
    val o0 = opts.copy(xnooptimizer = true, inlinerRounds = 0, lib = LibLevel.All, progress = false, incremental = false)
    val o1 = o0.copy(xnooptimizer = false)
    o0 :: (1 to MaxInliningRounds).map(r => o1.copy(inlinerRounds = r)).toList
  }

  private def benchmarkWithIndividualMaxTime(runConfigs: List[(Options, String, String)], maxWarmupNanos: Long, maxNanos: Long): ListMap[String, Run] = {
    implicit val sctx: SecurityContext = SecurityContext.AllPermissions
    val runs = scala.collection.mutable.ListBuffer.empty[Run]
    for ((config, name, prog) <- runConfigs) {
      debug(s"Benchmarking $name with ${InlinerRounds.rounds(config)} rounds")
      debug(s"Warming up for ${nanosToMinutes(maxWarmupNanos)} minutes...")

      val t0Compiler = System.nanoTime()
      val _ = benchmarkCompilationWithMaxTime(config, name, prog, maxWarmupNanos)
      val (compilationTimings, result) = benchmarkCompilationWithMaxTime(config, name, prog, maxNanos)
      val tDeltaCompiler = System.nanoTime() - t0Compiler

      debug(s"Took ${nanosToMinutes(tDeltaCompiler)} minutes total")

      runs += collectRun(config, name, compilationTimings, result.get)
    }
    ListMap.from(runs.map(r => (r.name, r)))
  }

  private def benchmarkCompilationWithMaxTime(o: Options, name: String, prog: String, maxNanos: Long)(implicit sctx: SecurityContext): (Seq[(Long, List[(String, Long)])], Option[CompilationResult]) = {
    val compilationTimings = scala.collection.mutable.ListBuffer.empty[(Long, List[(String, Long)])]
    var usedTime = 0L
    var result: Option[CompilationResult] = None
    while (usedTime < maxNanos) {
      val t0 = System.nanoTime()
      val flix = new Flix().setOptions(o)
      ZhegalkinCache.clearCaches()
      flix.addSourceCode(s"$name.flix", prog)
      val compilationResult = flix.compile().unsafeGet
      val phaseTimes = flix.phaseTimers.map { case PhaseTime(phase, time) => phase -> time }.toList
      val timing = (compilationResult.totalTime, phaseTimes)
      compilationTimings += timing
      usedTime += (System.nanoTime() - t0)
      result = Some(compilationResult)
    }
    (compilationTimings.toSeq, result)
  }

  private def collectRun(o: Options, name: String, compilationTimings: Seq[(Long, List[(String, Long)])], result: CompilationResult): Run = {
    val lines = result.getTotalLines
    val inliningRounds = o.inlinerRounds
    val compilationTime = median(compilationTimings.map(fst)).toLong
    // TODO: Use ListMap
    val phaseTimings = compilationTimings.flatMap(snd).foldLeft(Map.empty[String, Seq[Long]]) {
      case (acc, (phase, time)) => acc.get(phase) match {
        case Some(timings) => acc + (phase -> timings.appended(time))
        case None => acc + (phase -> Seq(time))
      }
    }.map { case (phase, timings) => phase -> median(timings).toLong }.toList
    val codeSize = result.codeSize

    Run(name, lines, inliningRounds, compilationTime, phaseTimings, codeSize)
  }


  private def minutesToNanos(minutes: Long): Long = {
    secondsToNanos(minutes * 60)
  }

  private def secondsToNanos(seconds: Long): Long = {
    seconds * 1_000_000_000
  }

  private def nanosToSeconds(nanos: Long): Long = {
    nanos / 1_000_000_000
  }

  private def nanosToMinutes(nanos: Long): Long = {
    nanosToSeconds(nanos) / 60
  }

  private object FromBootstrap {

    private val ENOUGH_OLD_CONSTANT_TIME: Long = new GregorianCalendar(2014, Calendar.JUNE, 27, 0, 0, 0).getTimeInMillis

    /**
      * @param root the root directory to compute a relative path from the given path
      * @param path the path to be converted to a relative path based on the given root directory
      * @return relative file name separated by slashes, like `path/to/file.ext`
      */
    def convertPathToRelativeFileName(root: Path, path: Path): String =
      root.relativize(path).toString.replace('\\', '/')


    private class FileVisitor extends SimpleFileVisitor[Path] {
      val result: mutable.ListBuffer[Path] = mutable.ListBuffer.empty

      override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
        result += file
        FileVisitResult.CONTINUE
      }
    }

    /**
      * Returns all files in the given path `p`.
      */
    def getAllFiles(p: Path): List[Path] = {
      if (Files.isReadable(p) && Files.isDirectory(p)) {
        val visitor = new FileVisitor
        Files.walkFileTree(p, visitor)
        visitor.result.toList
      } else {
        Nil
      }
    }

    /**
      * Adds an entry to the given zip file.
      */
    def addToZip(zip: ZipOutputStream, name: String, p: Path): Unit = {
      if (Files.exists(p)) {
        addToZip(zip, name, Files.readAllBytes(p))
      }
    }

    /**
      * Adds an entry to the given zip file.
      */
    def addToZip(zip: ZipOutputStream, name: String, d: Array[Byte]): Unit = {
      val entry = new ZipEntry(name)
      entry.setTime(ENOUGH_OLD_CONSTANT_TIME)
      zip.putNextEntry(entry)
      zip.write(d)
      zip.closeEntry()
    }

    def manifest: (String, String) = {
      ("META-INF/MANIFEST.MF",
        """Manifest-Version: 1.0
          |Main-Class: Main
          |""".stripMargin)
    }
  }

  private def mainProg(baselineFilePath: String): String = {
    s"""
       |import java.lang.System
       |import java.lang.ProcessHandle
       |
       |pub def main(): Unit \\ Exec + IO = run {
       |    //
       |    // Constants
       |    //
       |    let warmupTime = ${RunningTimeWarmupTime}i64;
       |    let benchTime  = ${RunningTimeBenchmarkTime}i64;
       |    let runs       = $NumberOfRuns;
       |
       |    //
       |    // Benchmarking functions
       |    //
       |    def doSampling(usedNanos, maxNanos, usedRuns) = {
       |        if (usedNanos < maxNanos and usedRuns < runs) {
       |            let t0 = System.nanoTime();
       |            runBenchmark();
       |            let tDelta = System.nanoTime() - t0;
       |            doSampling(usedNanos + tDelta, maxNanos, usedRuns + 1)
       |        } else {
       |            usedNanos
       |        }
       |    };
       |
       |    def bench(usedNanos, maxNanos, samples) = {
       |        if (usedNanos < maxNanos) {
       |            let sample = doSampling(usedNanos, maxNanos, 0) - usedNanos;
       |            bench(usedNanos + sample, maxNanos, sample :: samples)
       |        } else {
       |            List.reverse(samples)
       |        }
       |    };
       |
       |    let pid = ProcessHandle.current().pid();
       |    Console.eprintln("Please connect profiling tools if necessary (e.g., async-profiler). PID: $${pid}");
       |
       |    let totalTime = warmupTime + benchTime;
       |    Console.eprintln("Expected total time: $${totalTime}");
       |
       |    Console.eprintln("Benchmarking for $${benchTime} minutes, running $${runs} times for each sample");
       |
       |    Console.eprintln("Warming up for $${warmupTime} minutes");
       |
       |    discard bench(0i64, minutesToNanos(warmupTime), List.empty());
       |    let samples = bench(0i64, minutesToNanos(benchTime), List.empty());
       |    let json = toJSON(samples);
       |
       |    Console.println(ToString.toString(json));
       |    Console.eprintln("Done")
       |
       |} with Console.runWithIO
       |
       |pub def minutesToNanos(minutes: Int64): Int64 = {
       |    secondsToNanos(minutes * 60i64)
       |}
       |
       |pub def secondsToNanos(seconds: Int64): Int64 = {
       |    seconds * 1_000_000_000i64
       |}
       |
       |pub def nanosToSeconds(nanos: Int64): Int64 = {
       |    nanos / 1_000_000_000i64
       |}
       |
       |def toJSON(samples: List[Int64]): JSON = {
       |    JSON.Obj(
       |        List#{
       |            ("baseline",
       |                JVal.Lit(Lit.Str("$baselineFilePath"))
       |            ),
       |            ("samples",
       |                JVal.Arr(List.toVector(samples) |> Vector.map(n -> JVal.Lit(Lit.Num(n))))
       |            )
       |        }
       |    )
       |}
       |
       |enum JSON {
       |    case Obj(List[(String, JVal)])
       |}
       |
       |enum JVal {
       |    case Obj(JSON)
       |    case Arr(Vector[JVal])
       |    case Lit(Lit)
       |}
       |
       |enum Lit {
       |    case Str(String)
       |    case Num(Int64)
       |    case Null
       |}
       |
       |instance ToString[JSON] {
       |    pub def toString(x: JSON): String = match x {
       |        case JSON.Obj(kvs) =>
       |            let str = kvs
       |                |> List.map(match (k, v) -> "$${JSON.quote(k)}:$${ToString.toString(v)}")
       |                |> List.join(",");
       |            "{$${str}}"
       |    }
       |}
       |
       |
       |instance ToString[JVal] {
       |    pub def toString(x: JVal): String = match x {
       |        case JVal.Obj(obj) => ToString.toString(obj)
       |        case JVal.Arr(arr) => "[$${Vector.join(",", arr)}]"
       |        case JVal.Lit(lit) => ToString.toString(lit)
       |    }
       |}
       |
       |
       |instance ToString[Lit] {
       |    pub def toString(x: Lit): String = match x {
       |        case Lit.Str(s) => JSON.quote(s)
       |        case Lit.Num(n) => Int64.toString(n)
       |        case Lit.Null   => "null"
       |    }
       |}
       |
       |
       |mod JSON {
       |    pub def quote(s: String): String = {
       |        let escaped = s
       |            |> String.replace(src = "\\\\", dst = "\\\\\\\\")
       |            |> String.replace(src = "\\\"", dst = "\\\\\\"");
       |        String.concat(escaped, "\\\"") |> String.concat("\\\"")
       |    }
       |}
       |""".stripMargin
  }

  private def listFilter: String = {
    """
      |pub def runBenchmark(): Unit \ IO = {
      |    List.range(0, 10_000) |> List.filter(x -> Int32.modulo(x, 2) == 0) |> blackhole
      |}
      |
      |def blackhole(t: a): Unit \ IO =
      |    Ref.fresh(Static, t); ()
      |
      |""".stripMargin
  }

  private def listFoldLeft: String = {
    """
      |pub def runBenchmark(): Unit \ IO = {
      |    List.range(0, 10_000) |> List.foldLeft(Add.add, 0) |> blackhole
      |}
      |
      |def blackhole(t: a): Unit \ IO =
      |    Ref.fresh(Static, t); ()
      |
      |""".stripMargin
  }

  private def listFoldRight: String = {
    """
      |pub def runBenchmark(): Unit \ IO = {
      |    List.range(0, 10_000) |> List.foldRight(Add.add, 0) |> blackhole
      |}
      |
      |def blackhole(t: a): Unit \ IO =
      |    Ref.fresh(Static, t); ()
      |
      |""".stripMargin
  }

  private def listMap: String = {
    """
      |pub def runBenchmark(): Unit \ IO = {
      |    List.range(0, 10_000) |> List.map(x -> x + 1) |> blackhole
      |}
      |
      |def blackhole(t: a): Unit \ IO =
      |    Ref.fresh(Static, t); ()
      |
      |""".stripMargin
  }

  private def listLength: String = {
    """
      |pub def runBenchmark(): Unit \ IO = {
      |    List.range(0, 10_000) |> List.length |> blackhole
      |}
      |
      |def blackhole(t: a): Unit \ IO =
      |    Ref.fresh(Static, t); ()
      |
      |""".stripMargin
  }

  private def listReverse: String = {
    """
      |pub def runBenchmark(): Unit \ IO = {
      |    List.range(0, 10_000) |> List.reverse |> blackhole
      |}
      |
      |def blackhole(t: a): Unit \ IO =
      |    Ref.fresh(Static, t); ()
      |
      |""".stripMargin
  }

  private def listFilterMap: String = {
    """
      |pub def runBenchmark(): Unit \ IO = {
      |    List.range(0, 10_000) |> List.filterMap(x -> if (Int32.remainder(x, 2) == 0) Some(x) else None) |> blackhole
      |}
      |
      |def blackhole(t: a): Unit \ IO =
      |    Ref.fresh(Static, t); ()
      |
      |""".stripMargin
  }

  private def map10KLengthEff: String = {
    """
      |pub def runBenchmark(): Unit \ IO = {
      |    run {
      |        let l1 = range(0, 10_000);
      |        let l2 = map(l1);
      |        let l3 = length(l2);
      |        blackhole(l3)
      |    } with handler Transform {
      |        def f(x, k) = k(x + 1)
      |    }
      |}
      |
      |pub eff Transform {
      |    def f(x: Int32): Int32
      |}
      |
      |pub def map(l: List[Int32]) : List[Int32] \ Transform = {
      |    def mp(xs, acc) = match xs {
      |        case Nil     => acc
      |        case z :: zs => mp(zs, Transform.f(z) :: acc)
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
      |def blackhole(t: a): Unit \ IO =
      |    Ref.fresh(Static, t); ()
      |
      |""".stripMargin
  }

  private def map10KLength: String = {
    """
      |pub def runBenchmark(): Unit \ IO = {
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
      |def blackhole(t: a): Unit \ IO =
      |    Ref.fresh(Static, t); ()
      |
      |""".stripMargin
  }

  private def map10KLengthOptimized: String = {
    """
      |pub def runBenchmark(): Unit \ IO = {
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
      |def blackhole(t: a): Unit \ IO =
      |    Ref.fresh(Static, t); ()
      |
      |""".stripMargin
  }

  private def filterMap10K: String = {
    """
      |pub def runBenchmark(): Unit \ IO = {
      |    let l1 = range(0, 10_000);
      |    let l2 = filterMap(x -> if (Int32.remainder(x, 2) == 0) Some(x) else None, l1);
      |    blackhole(l2)
      |}
      |
      |pub def filterMap(f: a -> Option[b] \ ef, l: List[a]): List[b] \ ef = {
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
      |def blackhole(t: a): Unit \ IO =
      |    Ref.fresh(Static, t); ()
      |
      |""".stripMargin

  }

  private def filterMap10KOptimized: String = {
    """
      |pub def runBenchmark(): Unit \ IO = {
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
      |def blackhole(t: a): Unit \ IO =
      |    Ref.fresh(Static, t); ()
      |
      |""".stripMargin
  }

  private def fordFulkerson: String = {
    """
      |///
      |/// The Ford-Fulkerson algorithm finds the maximum flow of a flow network.
      |/// Here it is implemented using a combination of functional programming
      |/// and datalog.
      |///
      |pub def runBenchmark(): Unit \ IO =
      |    FordFulkerson.exampleGraph01() |> FordFulkerson.maxFlow(0, 5) |> blackhole
      |
      |mod FordFulkerson {
      |
      |    use Path.{Path, Bot};
      |
      |    ///
      |    /// Returns the maximum flow from `src` to `dst` in the flow network `g`.
      |    /// N.B.: `g` is a directed graph with upper bounds / capacity on the edges.
      |    /// No pre-assigned flow is allowed.
      |    ///
      |    /// The following assumptions also apply:
      |    /// - `src` and `dst` is connected in `g`
      |    /// - `g` contains no negative cycles
      |    /// - `g` is labeled in ascending order from `src` to `sink`
      |    /// - The label of `src` has the lowest value in the graph
      |    /// - The label of `dst` has the highest value in the graph
      |    ///
      |    pub def maxFlow(src: t, dst: t, g: m[(t, Int32, t)]): Int32 \ Foldable.Aef[m] with Foldable[m], Order[t] =
      |        def fordFulkerson(flowNetwork) = match augmentingPath(src, dst, flowNetwork) {
      |            case None       => getMaxFlow(dst, flowNetwork)
      |            case Some(path) =>
      |                let incr = minCapacity(path, flowNetwork);
      |                let updatedNetwork = increaseFlow(path, incr, flowNetwork);
      |                fordFulkerson(updatedNetwork)
      |        };
      |        // Init with 0 flow
      |        fordFulkerson(zeroFlow(g))
      |
      |    ///
      |    /// Returns a flow network with zero flow.
      |    ///
      |    def zeroFlow(g: m[(t, Int32, t)]): Vector[(t, Int32, Int32, t)] \ Foldable.Aef[m] with Foldable[m], Order[t] =
      |        Foldable.toVector(g) |> Vector.map(match (x, y, z) -> (x, y, 0, z))
      |
      |    ///
      |    /// Returns the sum of the flows on all directly ingoing edges to `dst`.
      |    ///
      |    def getMaxFlow(dst: t, g: m[(t, Int32, Int32, t)]): Int32 \ Foldable.Aef[m] with Foldable[m], Order[t] =
      |        g
      |        |> Foldable.toVector
      |        |> Vector.filterMap(match (_, _, f, d) -> if (d == dst) Some(f) else None)
      |        |> Vector.sum
      |
      |    ///
      |    /// Returns an augmenting path if one exists.
      |    ///
      |    /// An edge is in an augmenting path if its flow can be increased, i.e., the flow is strictly less than the capacity,
      |    /// or if it has non-zero flow.
      |    ///
      |    def augmentingPath(src: t, dst: t, g: m[(t, Int32, Int32, t)]): Option[Path[t]] \ Foldable.Aef[m] with Foldable[m], Order[t] =
      |        let edges = inject g into Edge;
      |        let rules = #{
      |            Reach(x, y; init(y, x)) :- Edge(x, u, f, y),                 if ((u - f) > 0). // Forward edge
      |            Reach(x, z; cons(z, p)) :- Reach(x, y; p), Edge(y, u, f, z), if ((u - f) > 0). // Forward edge
      |            Reach(x, y; init(y, x)) :- Edge(y, u, f, x),                 if (f > 0).       // Back edge
      |            Reach(x, z; cons(z, p)) :- Reach(x, y; p), Edge(z, u, f, y), if (f > 0).       // Back edge
      |        };
      |        let result = query edges, rules select fn from Reach(src, dst; fn);
      |        Vector.head(result)
      |
      |    ///
      |    /// Returns the most constraining capacity of `g` on the `Path` `p`.
      |    ///
      |    def minCapacity(p: Path[t], g: m[(t, Int32, Int32, t)]): Int32 \ Foldable.Aef[m] with Foldable[m], Order[t] =
      |        let onPath = (s, d) -> isForwardEdge(s, d, p) or isBackEdge(s, d, p);
      |        let optMin = g |> Foldable.filter(match (s, _, _, d) -> onPath(s, d))
      |            |> List.map(match (_, u, f, _) -> u - f)
      |            |> List.minimum;
      |        match optMin {
      |            case Some(u) => u
      |            case None    => unreachable!() // This function is only called by `maxFlow` if an augmenting path was found
      |        }
      |
      |    ///
      |    /// Returns a new flow network where the edges in `g` on the `Path` `p` has been adjusted by `incr`.
      |    ///
      |    def increaseFlow(p: Path[t], incr: Int32, g: m[(t, Int32, Int32, t)]): Vector[(t, Int32, Int32, t)] \ Foldable.Aef[m] with Foldable[m], Order[t] =
      |        g
      |        |> Foldable.toVector
      |        |> Vector.map(match (s, u, f, d) ->
      |            if (isForwardEdge(s, d, p))
      |                (s, u, f + incr, d)
      |            else if (isBackEdge(s, d, p))
      |                (s, u, f - incr, d)
      |            else
      |                (s, u, f, d)
      |        )
      |
      |    ///
      |    /// Returns true if `src` is an edge pointing to `dst` on the `Path` `p`.
      |    ///
      |    def isForwardEdge(src: t, dst: t, p: Path[t]): Bool with Eq[t] =
      |        match (indexOf(src, p), indexOf(dst, p)) { // A path is sorted in reverse order
      |            case (Some(si), Some(di)) if di + 1 == si => true
      |            case _ => false
      |        }
      |
      |    ///
      |    /// Returns true if `dst` is an edge pointing to `src` on the `Path` `p`.
      |    ///
      |    def isBackEdge(src: t, dst: t, p: Path[t]): Bool with Eq[t] =
      |        match (indexOf(src, p), indexOf(dst, p)) { // A path is sorted in reverse order
      |            case (Some(si), Some(di)) if si + 1 == di => true
      |            case _ => false
      |        }
      |
      |    pub enum Path[a] with ToString {
      |        case Path(List[a])
      |        case Bot // Infinitely long path
      |    }
      |
      |    instance Eq[Path[a]] {
      |        pub def eq(x: Path[a], y: Path[a]): Bool = match (x, y) {
      |            case (Bot, Bot)           => true
      |            case (Path(xs), Path(ys)) => List.length(xs) == List.length(ys)
      |            case _                    => false
      |        }
      |    }
      |
      |    instance Order[Path[a]] {
      |        pub def compare(x: Path[a], y: Path[a]): Comparison = match (x, y) {
      |            case (Bot, Bot)           => Comparison.EqualTo
      |            case (Bot, _)             => Comparison.LessThan
      |            case (_, Bot)             => Comparison.GreaterThan
      |            case (Path(xs), Path(ys)) => List.length(xs) <=> List.length(ys)
      |        }
      |    }
      |
      |    instance LowerBound[Path[a]] {
      |        // The longest list
      |        pub def minValue(): Path[a] = Bot
      |    }
      |
      |    instance PartialOrder[Path[a]] {
      |        pub def lessEqual(x: Path[a], y: Path[a]): Bool = match (x, y) {
      |            case (Bot, _)             => true
      |            case (Path(xs), Path(ys)) => List.length(xs) >= List.length(ys)
      |            case _                    => false
      |        }
      |    }
      |
      |    instance JoinLattice[Path[a]] {
      |        pub def leastUpperBound(x: Path[a], y: Path[a]): Path[a] = match (x, y) {
      |            case (Bot, p)             => p
      |            case (p, Bot)             => p
      |            case (Path(xs), Path(ys)) => if (List.length(xs) <= List.length(ys)) x else y
      |        }
      |    }
      |
      |    instance MeetLattice[Path[a]] {
      |        pub def greatestLowerBound(x: Path[a], y: Path[a]): Path[a] = match (x, y) {
      |            case (Bot, _)             => Bot
      |            case (_, Bot)             => Bot
      |            case (Path(xs), Path(ys)) => if (List.length(xs) > List.length(ys)) x else y
      |        }
      |    }
      |
      |    ///
      |    /// Returns a `Path` from `x` to `y`.
      |    ///
      |    pub def init(y: a, x: a): Path[a] =
      |        Path(y :: x :: Nil)
      |
      |    ///
      |    /// Extends the `Path` `p` with `z`.
      |    ///
      |    pub def cons(z: a, p: Path[a]): Path[a] = match p {
      |        case Bot      => Bot
      |        case Path(xs) => Path(z :: xs)
      |    }
      |
      |    ///
      |    /// Returns the index of `a` in the `Path` `p`.
      |    /// Note that a `Path` is sorted in descending order.
      |    ///
      |    pub def indexOf(x: a, p: Path[a]): Option[Int32] with Eq[a] = match p {
      |        case Bot      => None
      |        case Path(xs) => List.indexOf(x, xs)
      |    }
      |
      |    //////////////////////////////////////////
      |    // Tests                                //
      |    //////////////////////////////////////////
      |
      |    ///
      |    /// Returns the following graph:
      |    ///
      |    /// ```
      |    ///      1---2
      |    ///     /|\  |\
      |    ///    0 | \ | 5
      |    ///     \|  \|/
      |    ///      3---4
      |    /// ```
      |    ///
      |    /// The edges are directed as follows (ordered from left to right, top to bottom):
      |    ///
      |    /// ```
      |    /// 0 -> 1, capacity 10
      |    /// 0 -> 3, capacity 10
      |    /// 1 -> 3, capacity 2
      |    /// 1 -> 2, capacity 4
      |    /// 1 -> 4, capacity 8
      |    /// 3 -> 4, capacity 9
      |    /// 4 -> 2, capacity 6
      |    /// 2 -> 5, capacity 10
      |    /// 4 -> 5, capacity 10
      |    /// ```
      |    ///
      |    /// The maximum flow is `19`.
      |    ///
      |    pub def exampleGraph01(): Set[(Int32, Int32, Int32)] =
      |        Set#{ (0, 10, 1), (0, 10, 3), (1, 2, 3), (1, 4, 2), (1, 8, 4), (2, 10, 5), (3, 9, 4), (4, 6, 2), (4, 10, 5) }
      |
      |}
      |
      |def blackhole(t: a): Unit \ IO =
      |    Ref.fresh(Static, t); ()
      |
      |""".stripMargin
  }

  private def parsers: String = {
    """
      |pub def runBenchmark(): Unit \ IO = {
      |    ArithParser.parse("1+((2/3+4*(5*(6/7)))+41)")
      |    |> Option.map(eval)
      |    |> blackhole
      |}
      |
      |enum Exp with Eq, ToString {
      |    case Num(Int32),
      |    case Add(Exp, Exp),
      |    case Sub(Exp, Exp),
      |    case Mul(Exp, Exp),
      |    case Div(Exp, Exp)
      |}
      |
      |def eval(exp0: Exp): Int32 = match exp0 {
      |    case Exp.Num(n)          => n
      |    case Exp.Add(exp1, exp2) => eval(exp1) + eval(exp2)
      |    case Exp.Sub(exp1, exp2) => eval(exp1) - eval(exp2)
      |    case Exp.Mul(exp1, exp2) => eval(exp1) * eval(exp2)
      |    case Exp.Div(exp1, exp2) => eval(exp1) / eval(exp2)
      |}
      |
      |mod ArithParser {
      |    use Parser.{nibble, number, literal, using, otherwise, then, thenIgnoringLeft, thenIgnoringRight};
      |
      |    pub def parse(s: String): Option[Exp] =
      |        let prog = Parser.fromString(s) |> exp;
      |        prog |> DelayList.head |> Option.map(fst)
      |
      |    // Each production rule ends with ... `using` func, where func is a function
      |    // that produces the corresponding AST node.
      |
      |    def exp(input: Input[Char]): ParseResult[Exp, Char] = input |> (
      |        ((term `thenIgnoringRight` literal('+') `then` term) `using` plus)  `otherwise`
      |        ((term `thenIgnoringRight` literal('-') `then` term) `using` minus) `otherwise`
      |        term
      |    )
      |
      |    def term(input: Input[Char]): ParseResult[Exp, Char] = input |> (
      |        ((factor `thenIgnoringRight` literal('*') `then` factor) `using` times)  `otherwise`
      |        ((factor `thenIgnoringRight` literal('/') `then` factor) `using` divide) `otherwise`
      |        factor
      |    )
      |
      |    def factor(input: Input[Char]): ParseResult[Exp, Char] = input |> (
      |        (nibble(number) `using` value) `otherwise`
      |        (nibble(literal('(')) `thenIgnoringLeft` exp `thenIgnoringRight` nibble(literal(')')))
      |    )
      |
      |    def value(nums: Input[Char]): Exp =
      |        let optInt = Parser.stringify(nums) |> Int32.fromString;
      |        match optInt {
      |            case Some(n) => Exp.Num(n)
      |            case None    => unreachable!()
      |        }
      |
      |    def plus(exp: (Exp, Exp)): Exp = match exp {
      |        case (left, right) => Exp.Add(left, right)
      |    }
      |
      |    def minus(exp: (Exp, Exp)): Exp = match exp {
      |        case (left, right) => Exp.Sub(left, right)
      |    }
      |
      |    def times(exp: (Exp, Exp)): Exp = match exp {
      |        case (left, right) => Exp.Mul(left, right)
      |    }
      |
      |    def divide(exp: (Exp, Exp)): Exp = match exp {
      |        case (left, right) => Exp.Div(left, right)
      |    }
      |}
      |
      |pub type alias Input[a] = DelayList[a]
      |
      |pub type alias ParseResult[a, b] = DelayList[(a, Input[b])]
      |
      |pub type alias Parser[a, b] = Input[b] -> ParseResult[a, b]
      |
      |mod Parser {
      |
      |    use DelayList.{ENil, ECons, LCons, LList};
      |
      |    ///
      |    /// Returns a parser that always succeeds with value `b`
      |    /// regardless of input.
      |    ///
      |    pub def succeed(a: a): Parser[a, b] =
      |        inp -> ECons((a, inp), ENil)
      |
      |    ///
      |    /// Returns a parser that always fails regardless of input.
      |    ///
      |    /// Equivalent to the empty string ϵ.
      |    ///
      |    pub def fail(_: Input[b]): ParseResult[a, b] =
      |        ENil
      |
      |    ///
      |    /// Returns a parser that succeeds with value `x`
      |    /// if the input is non-empty and the first
      |    /// element `x` of the input satisfies
      |    /// the predicate `p(x)`.
      |    ///
      |    /// The parser fails otherwise.
      |    ///
      |    pub def satisfy(p: a -> Bool): Parser[a, a] =
      |        inp -> match inp {
      |            case ENil                 => fail(inp)
      |            case ECons(x, xs) if p(x) => succeed(x,       xs)
      |            case LCons(x, xs) if p(x) => succeed(x, force xs)
      |            case LList(xs)            => LList(lazy satisfy(p, force xs))
      |            case _                    => fail(inp)
      |        }
      |
      |    ///
      |    /// Returns a parser that succeeds with value `a`
      |    /// if the first element of the input is equal to `a`.
      |    ///
      |    /// The parser fails otherwise.
      |    ///
      |    pub def literal(a: a): Parser[a, a] with Eq[a] =
      |        satisfy(Eq.eq(a))
      |
      |    ///
      |    /// Returns a parser that recognizes the alternation
      |    /// of `p1` and `p2` (i.e. it recognizes both `p1` and `p2`).
      |    ///
      |    /// Equivalent to `p1` | `p2`.
      |    ///
      |    pub def otherwise(p1: Parser[a, b], p2: Parser[a, b]): Parser[a, b] =
      |        inp -> DelayList.append(p1(inp), p2(inp))
      |
      |    ///
      |    /// Returns a parser that recognizes the concatenation of
      |    /// `p1` and `p2`.
      |    ///
      |    /// Equivalent to `p1p2`.
      |    ///
      |    pub def then(p1: Parser[a, b], p2: Parser[c, b]): Parser[(a, c), b] =
      |        inp ->
      |            forM (
      |                (x1, rest1) <- p1(inp);
      |                (x2, rest2) <- p2(rest1)
      |            ) yield ((x1, x2), rest2)
      |
      |    ///
      |    /// Returns a parser that recognizes the concatenation of
      |    /// `p1` and `p2` but discards the result of `p1`.
      |    ///
      |    pub def thenIgnoringLeft(p1: Parser[a, b], p2: Parser[c, b]): Parser[c, b] =
      |        (p1 `then` p2) `using` snd
      |
      |    ///
      |    /// Returns a parser that recognizes the concatenation of
      |    /// `p1` and `p2` but discards the result of `p2`.
      |    ///
      |    pub def thenIgnoringRight(p1: Parser[a, b], p2: Parser[c, b]): Parser[a, b] =
      |        (p1 `then` p2) `using` fst
      |
      |    ///
      |    /// Returns a parser that applies `f` to all recognized
      |    /// values of `p`.
      |    ///
      |    /// Useful for producing AST nodes.
      |    ///
      |    pub def using(p: Parser[a, b], f: a -> c): Parser[c, b] =
      |        inp ->
      |            forM (
      |                (x, rest) <- p(inp)
      |            ) yield (f(x), rest)
      |
      |    ///
      |    /// Returns a parser that recognizes zero or more repetitions
      |    /// of `p`.
      |    ///
      |    /// Note that it always succeeds, so the result will always
      |    /// be non-empty, but the rest of the inp may be empty as well as
      |    /// the result.
      |    /// I.e. both the recognized value of may be empty (but still exist) and
      |    /// the unconsumed may be empty (but still exist).
      |    ///
      |    /// Equivalent to `p*`.
      |    ///
      |    pub def many(p: Parser[a, b]): Parser[DelayList[a], b] =
      |        inp -> inp // Wrap in lambda so the recursive call does not immediately happen
      |            |> (((p `then` many(p)) `using` cons) `otherwise` succeed(ENil))
      |
      |    ///
      |    /// Returns a parser that recognizes one or more repetitions
      |    /// of `p`.
      |    ///
      |    /// Note that unlike `many`, this parser may fail, i.e.
      |    /// not recognize anything.
      |    ///
      |    /// Equivalent to `p+`.
      |    ///
      |    pub def some(p: Parser[a, b]): Parser[DelayList[a], b] =
      |        (p `then` many(p)) `using` cons
      |
      |    ///
      |    /// Returns a parser that recognizes numbers (consecutive integer characters).
      |    ///
      |    /// All possible parses of the number will be recognized.
      |    ///
      |    /// E.g. `123` is recognized as `123, 12, 1`.
      |    ///
      |    /// The longest match will be the first result.
      |    ///
      |    pub def number(inp: Input[Char]): ParseResult[DelayList[Char], Char] =
      |        let digit = c -> '0' <= c and c <= '9';
      |        inp |> some(satisfy(digit))
      |
      |    ///
      |    /// Returns a parser that recognizes words
      |    /// (consecutive non-integer, non-whitespace characters).
      |    ///
      |    /// All possible parses of the word will be recognized.
      |    ///
      |    /// E.g. `hello` is recognized as `hello, hell, hel, he, h`.
      |    ///
      |    /// The longest match will be the first result.
      |    ///
      |    pub def word(inp: Input[Char]): ParseResult[DelayList[Char], Char] =
      |        let lowercase = c -> 'a' <= c and c <= 'z';
      |        let uppercase = c -> 'A' <= c and c <= 'Z';
      |        let letter = c -> lowercase(c) or uppercase(c);
      |        inp |> some(satisfy(letter))
      |
      |    ///
      |    /// Returns a parser that recognizes the sequence `lit`.
      |    ///
      |    /// This is generalization of `literal`.
      |    ///
      |    pub def literalSequence(lit: m[a]): Parser[DelayList[a], a] \ Foldable.Aef[m] with Eq[a], Foldable[m] =
      |        literalSequenceHelper(Foldable.toList(lit))
      |
      |    ///
      |    /// Helper for `literalSequence` which does not have the foldable associated effect.
      |    ///
      |    def literalSequenceHelper(lit: List[a]): Parser[DelayList[a], a] with Eq[a] =
      |        inp -> inp // Wrap in lambda so the recursive call does not immediately happen
      |            |> match lit {
      |                case Nil     => succeed(ENil)
      |                case x :: xs => (literal(x) `then` (literalSequenceHelper(xs))) `using` cons
      |            }
      |
      |    ///
      |    /// Returns parser that returns the the value `c` if `p` is succesful.
      |    ///
      |    pub def return(p: Parser[a, b], c: c): Parser[c, b] =
      |        p `using` constant(c)
      |
      |    ///
      |    /// Returns a parser that recognizes the string `s`.
      |    ///
      |    pub def string(s: String): Parser[DelayList[Char], Char] =
      |        s |> (String.toList >> literalSequence)
      |
      |    ///
      |    /// Returns a parser that ignores whitespace on both
      |    /// sides of `p`.
      |    ///
      |    pub def nibble(p: Parser[a, Char]): Parser[a, Char] =
      |        whitespace `thenIgnoringLeft` p `thenIgnoringRight` whitespace
      |
      |    ///
      |    /// Returns a parser that recognizes whitespace.
      |    ///
      |    pub def whitespace(inp: Input[Char]): ParseResult[DelayList[Char], Char] =
      |        let chars = String.toList(" \t\n");
      |        inp |> (many(any(literal, chars)))
      |
      |    ///
      |    /// Returns a parser that recognizes any of the elements in `syms`.
      |    ///
      |    pub def any(f: a -> Parser[b, c], syms: m[a]): Parser[b, c] \ Foldable.Aef[m] with Foldable[m] =
      |        Foldable.foldRight(f >> otherwise, fail, syms)
      |
      |    ///
      |    /// Returns the string `s` as an `Input` type.
      |    ///
      |    pub def fromString(s: String): Input[Char] =
      |        String.toList(s) |> List.toDelayList
      |
      |    ///
      |    /// Returns `chars` as a string.
      |    ///
      |    pub def stringify(chars: m[Char]): String \ Foldable.Aef[m] with Foldable[m] = region r {
      |        let sb = StringBuilder.empty(r);
      |        let ap = sb |> flip(StringBuilder.append);
      |        Foldable.forEach(ap, chars);
      |        StringBuilder.toString(sb)
      |    }
      |
      |    ///
      |    /// Returns the tuple as a list, i.e.
      |    /// `(x, xs)` is returned as `x :: xs`.
      |    ///
      |    def cons(xs: (a, DelayList[a])): DelayList[a] =
      |        ECons(fst(xs), snd(xs))
      |
      |}
      |
      |def blackhole(t: a): Unit \ IO =
      |    Ref.fresh(Static, t); ()
      |
      |""".stripMargin
  }

  private def Python: String =
    """
      |# $ pip install matplotlib numpy
      |
      |import json
      |import matplotlib.pyplot as plt
      |import matplotlib.lines as mlines
      |import numpy as np
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
      |    Returns n distinct colors, up to 13.
      |    \"\"\"
      |    lim = 13
      |    if n > lim:
      |        raise Exception(f"parameter n was {n} but can at most be {lim}")
      |    colors = list(map(lambda i: 'C' + str(i), range(n)))
      |    if n > 10:
      |        colors[-1] = 'purple'
      |    if n > 11:
      |        colors[-2] = '#764B00'
      |    if n > 12:
      |        colors[-3] = 'yellow'
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
      |    ax.set_ylim(top=round(max(times))+250)
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
      |    ax.set_ylim(top=round(max(times))+250)
      |    ax.set_title('Fastest running times')
      |    ax.legend(title='Program', loc='upper left', ncols=3, fontsize=7)
      |    plt.savefig('bestRunningTimes.png', dpi=300)
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
      |    speedups = {}
      |    speedups['baseline'] = np.repeat(1, len(progs))
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
      |    ylim_top = round(max(tmp)) + 2
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
      |    speedups = {}
      |    speedups['baseline'] = np.repeat(1, len(progs))
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
      |    ylim_top = round(max(tmp)) + 1
      |    ax.set_ylim(top=ylim_top)
      |    ax.set_yticks(range(ylim_top + 1), labels=list(map(lambda x: f"{x}x" if x != 0 else '', range(ylim_top + 1))))
      |
      |    plt.savefig('runningTimeSpeedupOldInliner.png', dpi=300)
      |
      |    print(list(zip(range(1, len(progs) + 1), progs)))
      |
      |""".stripMargin

}
