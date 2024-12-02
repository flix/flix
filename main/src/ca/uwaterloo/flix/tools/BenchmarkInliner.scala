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

import ca.uwaterloo.flix.util.Options

object BenchmarkInliner {

  def run(opts: Options): Unit = {
    val o = opts.copy(progress = false, loadClassFiles = false)

    // Experiments:
    // 1. Compiler throughput
    //    (a) without inlining
    //    (b) with old inliner
    //    (c) with new inliner
    runExperiment(o)(CompilerPerf.run)

    // 2. Flix program speedup (sample programs, datalog engine, parser library)
    //    (a) without inlining
    //    (b) with old inliner
    //    (c) with new inliner
    // 3. Jar size
    //    (a) without inlining
    //    (b) with old inliner
    //    (c) with new inliner
    // TODO: Vary thresholds for new inliner

  }

  /**
    * Runs `experiment` with the following options:
    * (a) without inlining
    * (b) with old inliner
    * (c) with new inliner
    */
  private def runExperiment(opts: Options)(experiment: Options => Unit): Unit = {
    // TODO: Copy experiments from CompilerPerf
    // Disable both inliners
    val o1 = opts.copy(xnooptimizer = true, xnooptimizer1 = true)
    experiment(o1)

    // Disable new inliner (run old)
    val o2 = opts.copy(xnooptimizer1 = true)
    experiment(o2)

    // Disable old inliner (run new)
    val o3 = opts.copy(xnooptimizer = true)
    experiment(o3)
  }

}
