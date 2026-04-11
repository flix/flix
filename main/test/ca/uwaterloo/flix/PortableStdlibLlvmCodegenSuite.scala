/*
 * Copyright 2026 Magnus Madsen
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

package ca.uwaterloo.flix

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.util.{CompilationTarget, FileOps, Options, StdlibProfile}
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Paths}

/**
  * Smoke tests that codegen the portable conformance suite using the LLVM backend.
  *
  * This ensures the portable stdlib profile remains compatible with `--Xtarget llvm-*`.
  */
class PortableStdlibLlvmCodegenSuite extends AnyFunSuite {

  private val TestOptions: Options =
    Options.TestWithLibAll.copy(
      stdlibProfile = StdlibProfile.Portable,
      target = CompilationTarget.LlvmNative,
      incremental = false,
      outputJvm = false,
    )

  private val preludeFile = Paths.get("main/test/flix/Prelude.flix")

  private val testDir = Paths.get("main/test/flix/portable")

  private def compile(file: java.nio.file.Path): Unit = {
    val flix = new Flix()
    val outDir = Files.createTempDirectory("flix-llvm-portable-")
    flix.setOptions(TestOptions.copy(outputPath = outDir))
    implicit val sctx: SecurityContext = SecurityContext.Unrestricted
    flix.addFile(preludeFile)
    flix.addFile(file)

    val (optRoot, errors) = flix.check()
    if (errors.nonEmpty) {
      fail(CompilationMessage.formatAll(errors)(flix.getFormatter, optRoot))
    }
    flix.codeGen(optRoot.get)
  }

  for (p <- FileOps.getFlixFilesIn(testDir, 1)) {
    test(p.getFileName.toString)(compile(p))
  }
}
