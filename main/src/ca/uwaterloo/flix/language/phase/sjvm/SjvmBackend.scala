/*
 * Copyright 2020-2021 Jonathan Lindegaard Starup
 * Copyright 2017 Magnus Madsen
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

package ca.uwaterloo.flix.language.phase.sjvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.ErasedAst.Root
import ca.uwaterloo.flix.language.phase.Phase
import ca.uwaterloo.flix.runtime.CompilationResult
import ca.uwaterloo.flix.util.Validation

import java.nio.file.{Path, Paths}

object SjvmBackend extends Phase[Root, CompilationResult] {
  /**
   * The directory where to place the generated class files.
   */
  val TargetDirectory: Path = Paths.get("./target/flix/")

  /**
   * Emits JVM bytecode for the given AST `root`.
   */
  def run(input: Root)(implicit flix: Flix): Validation[CompilationResult, CompilationError] = flix.phase("SjvmBackend") {

    //
    // Put the AST root into implicit scope.
    //
    implicit val r: Root = input

    val allClasses = flix.subphase("CodeGen")(codeGen(input))

    //
    // Write each class (and interface) to disk.
    //
    // NB: In interactive and test mode we skip writing the files to disk.
    if (flix.options.writeClassFiles && !flix.options.test) {
      flix.subphase("WriteClasses") {
        for ((jvmName, jvmClass) <- allClasses) {
          ??? //JvmOps.writeClass(TargetDirectory, jvmClass)
        }
      }
    }

    ???
  }

  def codeGen(root: Root)(implicit flix: Flix): Map[String, Array[Byte]] = {
    ???
  }

}
