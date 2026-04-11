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

package ca.uwaterloo.flix.language.phase.llvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.util.{FileOps, InternalCompilerException}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, LinkOption, Path}

object LlvmWriter {

  /**
    * Writes the LLVM IR module to `outputPath/llvm/module.ll`.
    *
    * Returns the number of bytes written.
    */
  def run(module: LlvmBackend.Module)(implicit flix: Flix): Int = {
    val bytes = module.text.getBytes(StandardCharsets.UTF_8)
    val path = modulePath(flix.options.outputPath)
    writeFile(path, bytes)
    bytes.length
  }

  /**
    * Returns the location of the textual LLVM IR module for the current build.
    */
  def modulePath(outputPath: Path): Path =
    outputPath.resolve("llvm/").resolve("module.ll").toAbsolutePath

  private def writeFile(path: Path, bytes: Array[Byte]): Unit = {
    Files.createDirectories(path.getParent)

    if (Files.exists(path)) {
      if (!Files.isRegularFile(path, LinkOption.NOFOLLOW_LINKS)) {
        throw InternalCompilerException(s"Unable to write to non-regular file: '$path'.", SourceLocation.Unknown)
      }
      if (!Files.isWritable(path)) {
        throw InternalCompilerException(s"Unable to write to read-only file: '$path'.", SourceLocation.Unknown)
      }
      if (!(FileOps.isEmpty(path) || path.getFileName.toString.endsWith(".ll"))) {
        throw InternalCompilerException(s"Refusing to overwrite non-empty non-.ll file: '$path'.", SourceLocation.Unknown)
      }
    }

    Files.write(path, bytes)
  }

}
