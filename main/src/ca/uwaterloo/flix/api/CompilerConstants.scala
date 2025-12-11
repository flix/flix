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
    * The virtual file name used in the shell.
    */
  val VirtualShellFile: Path = Path.of("__SHELL__.flix")

  /**
    * The virtual file name used in tests.
    */
  val VirtualTestFile: Path = Path.of("__TEST__.flix")

  /**
    * The virtual file name used in the playground.
    */
  val VirtualPlaygroundFile: Path = Path.of("__PLAY__.flix")

}
