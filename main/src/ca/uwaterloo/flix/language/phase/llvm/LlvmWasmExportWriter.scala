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
import ca.uwaterloo.flix.language.ast.LoweredAst
import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.util.{ArtifactNames, InternalCompilerException}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, LinkOption, Path}

/**
  * Writes a manifest of invokable wasm defs (def-id mapping) for hosts.
  *
  * This is used by embeddings (JS/WASI) to map from Flix symbols to the `def-id` numeric space
  * exposed by the component runtime WIT surface.
  */
object LlvmWasmExportWriter {

  /**
    * Writes `outputPath/llvm/flix_wasm_exports.json` if there are any invokable defs.
    *
    * Returns the path to the manifest (or `None` if there are no invokable defs).
    */
  def run(root: LoweredAst.Root)(implicit flix: Flix): Option[Path] = {
    val entries = LlvmWasmDefs.compute(root)
    if (entries.isEmpty) return None

    val json = render(entries)
    val bytes = json.getBytes(StandardCharsets.UTF_8)
    val path = manifestPath(flix.options.outputPath, flix.options.artifactName)
    writeFile(path, bytes)
    Some(path)
  }

  /**
    * Returns the location of the wasm def-id manifest for the current build.
    */
  def manifestPath(outputPath: Path, artifactName: String = ArtifactNames.DefaultBaseName): Path =
    outputPath.resolve("llvm/").resolve(ArtifactNames.wasmExportsManifestFileName(artifactName)).toAbsolutePath

  private def render(entries: List[LlvmWasmDefs.Entry]): String = {
    val sb = new StringBuilder(8 * 1024)

    sb.append("{\n")
    sb.append(s"""  "schema": "flix-llvm-wasm-exports-v0",\n""")
    sb.append(s"""  "count": ${entries.length},\n""")
    sb.append(s"""  "defs": [\n""")

    entries.zipWithIndex.foreach { case (e, idx) =>
      val params = e.signature.params.map(_.displayName)
      val arity = params.length
      val result = e.signature.result.displayName

      sb.append("    {\n")
      sb.append(s"""      "defId": ${e.defId},\n""")
      sb.append(s"""      "symbol": "${escapeJson(e.sym.toString)}",\n""")
      sb.append(s"""      "isMain": ${e.isMain},\n""")
      sb.append(s"""      "isExport": ${e.isExport},\n""")
      sb.append(s"""      "arity": $arity,\n""")
      sb.append(s"""      "params": [${params.map(p => "\"" + escapeJson(p) + "\"").mkString(", ")}],\n""")
      sb.append(s"""      "result": "${escapeJson(result)}"\n""")
      sb.append("    }")
      if (idx != entries.length - 1) sb.append(",")
      sb.append("\n")
    }

    sb.append("  ]\n")
    sb.append("}\n")

    sb.toString()
  }

  private def escapeJson(s: String): String = {
    val b = new StringBuilder(s.length + 8)
    s.foreach {
      case '"' => b.append("\\\"")
      case '\\' => b.append("\\\\")
      case '\b' => b.append("\\b")
      case '\f' => b.append("\\f")
      case '\n' => b.append("\\n")
      case '\r' => b.append("\\r")
      case '\t' => b.append("\\t")
      case c if c < ' ' =>
        b.append(f"\\u${c.toInt}%04x")
      case c =>
        b.append(c)
    }
    b.toString()
  }

  private def writeFile(path: Path, bytes: Array[Byte]): Unit = {
    Files.createDirectories(path.getParent)

    if (Files.exists(path)) {
      if (!Files.isRegularFile(path, LinkOption.NOFOLLOW_LINKS)) {
        throw InternalCompilerException(s"Unable to write to non-regular file: '$path'.", SourceLocation.Unknown)
      }
      if (!Files.isWritable(path)) {
        throw InternalCompilerException(s"Unable to write to read-only file: '$path'.", SourceLocation.Unknown)
      }
      if (!path.getFileName.toString.endsWith(".json")) {
        throw InternalCompilerException(s"Refusing to overwrite non-.json file: '$path'.", SourceLocation.Unknown)
      }
    }

    Files.write(path, bytes)
  }
}
