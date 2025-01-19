/*
 * Copyright 2025 Magnus Madsen
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
package ca.uwaterloo.flix.language.phase.unification.zhegalkin

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.util.StatUtils.{average, median}
import ca.uwaterloo.flix.util.LocalResource

object ZheglakinPerf {

  private val RQ3 = "RQ3: Performance Gain of Per-Operation Caching"

  private val Iterations: Int = 1

  private object Config {
    val Default: Config = Config(cacheUnion = false, cacheInter = false, cacheXor = false)
  }

  private case class Config(
                             cacheCstInter: Boolean = false,  // TODO
                             cacheUnion: Boolean = false,
                             cacheInter: Boolean = false,
                             cacheXor: Boolean = false,
                             cacheSVE: Boolean = false,  // TODO
                           ) {

    override def toString: String = s"{cacheUnion = $cacheUnion, cacheInter = $cacheInter, cacheXor = $cacheXor}"
  }

  def main(args: Array[String]): Unit = {
    rq3(Iterations)
  }


  private def rq3(n: Int): Unit = {
    println(RQ3)

    val m1 = runConfig(Config.Default, n).mdn
    val m2 = runConfig(Config.Default.copy(cacheCstInter = true), n).mdn
    val m3 = runConfig(Config.Default.copy(cacheUnion = true), n).mdn
    val m4 = runConfig(Config.Default.copy(cacheInter = true), n).mdn
    val m5 = runConfig(Config.Default.copy(cacheXor = true), n).mdn
    val m6 = runConfig(Config.Default.copy(cacheCstInter = true, cacheUnion = true, cacheInter = true, cacheXor = true, cacheSVE = true), n).mdn
    val m7 = runConfig(Config.Default.copy(cacheInter = true), n).mdn

    println("-" * 80)
    println("\\textbf{Cached Operation} & \\textsf{Baseline} & $c_1 \\cap z_2$ & $z_1 \\cup z_2$ & $z_1 \\cap z_2$ & $z_1 \\xor z_2$ & $\\textsf{SVE}(z_1, z_2)$ & \\textsf{All} & \\textsf{Best} \\\\")
    println("\\midrule")
    println(f"\\textsf{Throughput (\\textsf{median})} & $m1%,7d & $m2%,7d & $m3%,7d & $m4%,7d & $m5%,7d & $m6%,7d & $m7%,7d \\\\")
    println("-" * 80)
  }

  /**
    * Run a specific configuration.
    */
  private def runConfig(c: Config, n: Int): Throughput = {
    val baseLine = aggregate(runN(n, c))

    // Find the number of lines of source code.
    val lines = baseLine.lines.toLong

    // Find the timings of each run.
    val timings = baseLine.times

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

    Throughput(min, max, avg, mdn)
  }

  /**
    * Runs Flix multiple times.
    */
  private def runN(N: Int, c: Config): IndexedSeq[Run] = {
    (0 until N).map { _ =>
      ZhegalkinCache.EnableUnionCache = c.cacheUnion
      ZhegalkinCache.EnableInterCache = c.cacheInter
      ZhegalkinCache.EnableXorCache = c.cacheXor

      val flix = new Flix()
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

    // Run the Flix compiler
    val (root, errors) = flix.check()
    if (errors.nonEmpty) {
      throw new RuntimeException(s"Errors were present after compilation: ${errors.mkString(", ")}")
    }
    val totalLines = root.get.sources.foldLeft(0) {
      case (acc, (_, sl)) => acc + sl.endLine
    }

    val totalTime = flix.getTotalTime

    Run(totalLines, totalTime)
  }

  /**
    * Merges a sequences of runs `l`.
    */
  private def aggregate(l: IndexedSeq[Run]): Runs = {
    if (l.isEmpty) {
      return Runs(0, List(0))
    }

    val lines = l.head.lines
    val times = l.map(_.time).toList
    Runs(lines, times)
  }

  /**
    * Returns the throughput per second.
    */
  private def throughput(lines: Long, time: Long): Int = ((1_000_000_000L * lines).toDouble / time.toDouble).toInt

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

  case class Run(lines: Int, time: Long)

  case class Runs(lines: Int, times: List[Long])

  case class Throughput(min: Int, max: Int, avg: Int, mdn: Int) {
    override def toString: String = f"min: $min%,7d, max: $max%,7d, avg: $avg%,7d, median: $mdn%,7d"
  }

}
