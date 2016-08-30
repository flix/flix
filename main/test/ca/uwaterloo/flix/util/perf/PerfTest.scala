/*
 * Copyright 2016 Luqman Aden
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

package ca.uwaterloo.flix.util.perf

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.util.{AsciiTable, Options, Verbosity}

import scala.collection.mutable.ListBuffer


object PerfTest {
  def bench(prog_str: String, prog: String, opts: Options, trials: Int): Double = {
    var sum: Double = 0

    for (_ <- 1 to trials) {
      val model = new Flix()
        .setOptions(opts)
        .addPath(s"$prog.flix")
        .addPath("Belnap.flix")
        .addStr(prog_str)
        .solve().get

      sum += model.getTime.solver / 1000000
    }

    sum / trials
  }

  def compare(progs: Array[String], optA: Options, optB: Options): Unit = {
    val trials = 3
    val results = new AsciiTable().withCols("     N     " +: progs: _*)

    // Early exit if any of the inputs are invalid
    for (prog <- progs) {
      if (!RandomLatticeGraph.lattices.contains(prog))
        return
    }

    results.writeHeader(System.out)

    for (n <- 4 to 10) {
      val N = math.pow(2, n).toInt
      var row = ListBuffer[String]()

      row += N.toString

      for (prog <- progs) {
        // Generate the flix program to test
        val prog_str = RandomLatticeGraph.generate(prog, N).get

        // Compile with optA
        val aTime = bench(prog_str, prog, optA, trials)

        // and with optB
        val bTime = bench(prog_str, prog, optB, trials)

        val diff = aTime / bTime

        row += f"$diff%1.2fx"
      }

      results.mkCenteredRow(row.toList)
      results.writeRows(System.out)
    }

    results.writeFooter(System.out)
  }

  def main(args: Array[String]): Unit = {
    compare(args,
      Options.Default.copy(verbosity = Verbosity.Silent, optimize = false),
      Options.Default.copy(verbosity = Verbosity.Silent, optimize = true))
  }
}
