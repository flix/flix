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

  case class Run(lines: Int, time: Long, phases: List[(String, Long)])

  case class Runs(lines: Int, times: List[Long], phases: List[(String, List[Long])])

  def run(opts: Options): Unit = {

  }

}
