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

package ca.uwaterloo.flix.api

import ca.uwaterloo.flix.language.phase.llvm.{LlvmWasmDriver, LlvmWasmExportWriter}
import ca.uwaterloo.flix.util.Options

import java.nio.file.{Files, Path}

object WasmRunSupport {

  def runWasmtime(options: Options, rootDir: Path, args: Array[String]): Either[String, Unit] = {
    val componentWasm = LlvmWasmDriver.componentWasmPath(options.outputPath, options.artifactName)
    val exportsManifest = LlvmWasmExportWriter.manifestPath(options.outputPath, options.artifactName)
    val cargoToml = LlvmWasmDriver.resolveWasmtimeRunnerManifest(options.outputPath)

    if (!Files.exists(componentWasm)) {
      return Left(s"Missing wasm component artifact: $componentWasm")
    }

    if (!Files.exists(exportsManifest)) {
      return Left(s"Missing wasm exports manifest: $exportsManifest")
    }

    val cmd =
      List(
        "cargo",
        "+stable",
        "run",
        "--quiet",
        "--manifest-path",
        cargoToml.toString,
        "--bin",
        "run_flix",
        "--",
        componentWasm.toAbsolutePath.normalize().toString,
        "--exports",
        exportsManifest.toAbsolutePath.normalize().toString,
        "--rootDir",
        rootDir.toAbsolutePath.normalize().toString,
        "--budget",
        "500"
      ) ::: args.toList.flatMap(arg => List("--argv", arg))

    val pb = new ProcessBuilder(cmd: _*)
    pb.inheritIO()
    val exit = pb.start().waitFor()
    if (exit == 0) Right(())
    else Left(s"Wasmtime runner exited with code: $exit")
  }
}
