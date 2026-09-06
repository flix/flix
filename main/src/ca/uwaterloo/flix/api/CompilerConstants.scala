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
package ca.uwaterloo.flix.api

import java.nio.file.Path

/**
  * A collection of Flix compiler constants that are hard coded.
  */
object CompilerConstants {

  /**
    * The directory where the pretty printed ASTs are written (see `--Xprint-phases`).
    */
  val AstDirectory: Path = Path.of("./build/asts/")

  /**
    * The number of backend phases, i.e. the number of `phase` calls made by [[Flix.codeGen]].
    *
    * Must be updated when a phase is added to or removed from `codeGen`.
    */
  val BackendPhaseCount: Int = 13

  /**
    * The directory where the recorded type constraint graphs are written.
    */
  val ConstraintGraphDirectory: Path = Path.of("./build/constraint-graphs/")

  /**
    * The number of frontend phases, i.e. the number of `phase` calls made by [[Flix.check]].
    *
    * Must be updated when a phase is added to or removed from `check`.
    */
  val FrontendPhaseCount: Int = 19

  /**
    * The JVM bytecode version used when generating class files.
    */
  val JvmTargetVersion: Int = org.objectweb.asm.Opcodes.V21

  /**
    * The maximum number of variables an equation may contain before it is
    * considered too complex for set unification.
    */
  val MaxEffUnificationVars: Int = 11

  /**
    * The maximum number of rounds the inliner can run.
    */
  val MaxOptimizerRounds: Int = 5

  /**
    * The maximum amount of fuel the parser can consume without making progress.
    */
  val MaxParserFuel: Int = 2048

  /**
    * The directory where the compiler performance data and graphs are written (see `Xperf`).
    */
  val PerfDirectory: Path = Path.of("./build/perf/")

  /**
    * How long (in seconds) an idle worker thread in the compiler's thread pool stays alive
    * before it exits.
    *
    * Ensures that a pool which is never shut down (e.g. because a compilation crashed)
    * does not pin its threads and their stacks forever.
    */
  val ThreadKeepAliveSeconds: Long = 60L

  /**
    * The minimum stack size (in bytes) of each worker thread in the compiler's thread pool.
    * Workers get the larger of this value and the JVM's default thread stack size (`-Xss`).
    *
    * The JVM default (typically 1-2 MB) is easily exhausted by the deeply recursive visitors
    * in the compiler when given large or deeply nested inputs. Only address space is reserved
    * up front; physical memory is committed lazily as the stack grows, so a generous size is
    * essentially free unless it is actually used.
    */
  val ThreadStackSize: Long = 64L * 1024L * 1024L

  /**
    * The total number of phases run by a full compilation, i.e. by [[Flix.compile]].
    */
  val TotalPhases: Int = FrontendPhaseCount + BackendPhaseCount

  /**
    * The virtual file name used by the playground.
    */
  val VirtualPlaygroundFile: Path = Path.of("__PLAY__.flix")

  /**
    * The virtual file name used by the shell.
    */
  val VirtualShellFile: Path = Path.of("__SHELL__.flix")

  /**
    * The virtual file name used by tests.
    */
  val VirtualTestFile: Path = Path.of("__TEST__.flix")

}
