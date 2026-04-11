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
import ca.uwaterloo.flix.language.ast.{LoweredAst, SourceLocation}
import ca.uwaterloo.flix.language.phase.ExportAbi
import ca.uwaterloo.flix.util.{ArtifactNames, InternalCompilerException}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, LinkOption, Path}

/**
  * Writes a manifest of portable effect-op signatures keyed by the same `(effId, opIndex)` pair
  * used by the wasm runtime `unknown(effId, opId)` request path.
  *
  * This is the first compiler-emitted substrate for async wasm host bindings. Hosts can combine
  * this manifest with generic suspension arg inspection/resume APIs without introducing a second
  * async mechanism beside Flix effects.
  */
object LlvmWasmEffectManifestWriter {

  private val PortableListConsTagId: Long = 0L
  private val PortableListNilTagId: Long = 1L
  private val PortableOptionNoneTagId: Long = 0L
  private val PortableOptionSomeTagId: Long = 1L
  private val PortableResultErrTagId: Long = 0L
  private val PortableResultOkTagId: Long = 1L

  case class Entry(effect: String,
                   effSymId: Long,
                   op: String,
                   opSymbol: String,
                   opIndex: Int,
                   signature: ExportAbi.Signature)

  def run(root: LoweredAst.Root)(implicit flix: Flix): Option[Path] = {
    val entries = compute(root)
    if (entries.isEmpty) return None

    val path = manifestPath(flix.options.outputPath, flix.options.artifactName)
    writeFile(path, render(entries).getBytes(StandardCharsets.UTF_8))
    Some(path)
  }

  def compute(root: LoweredAst.Root): List[Entry] = {
    val effIds = LlvmEffectIds.effectSymIds(root)
    val opIds = LlvmEffectIds.opIndices(root)

    root.effects.values.toList
      .sortBy(_.sym.toString)
      .flatMap { eff =>
        eff.ops.toList.sortBy(_.sym.name).flatMap { op =>
          op.portableSignature.map { sig =>
            Entry(
              effect = eff.sym.toString,
              effSymId = effIds(eff.sym),
              op = op.sym.name,
              opSymbol = op.sym.toString,
              opIndex = opIds(op.sym),
              signature = sig,
            )
          }
        }
      }
      .sortBy(e => (e.effSymId, e.opIndex))
  }

  def manifestPath(outputPath: Path, artifactName: String = ArtifactNames.DefaultBaseName): Path =
    outputPath.resolve("llvm/").resolve(ArtifactNames.wasmEffectsManifestFileName(artifactName)).toAbsolutePath

  private def render(entries: List[Entry]): String = {
    val sb = new StringBuilder(8 * 1024)
    sb.append("{\n")
    sb.append("""  "schema": "flix-llvm-wasm-effects-v0",""").append('\n')
    sb.append(s"""  "count": ${entries.length},""").append('\n')
    sb.append("""  "ops": [""").append('\n')

    entries.zipWithIndex.foreach { case (entry, idx) =>
      sb.append("    {\n")
      sb.append(s"""      "effect": "${escapeJson(entry.effect)}",""").append('\n')
      sb.append(s"""      "effSymId": ${entry.effSymId},""").append('\n')
      sb.append(s"""      "op": "${escapeJson(entry.op)}",""").append('\n')
      sb.append(s"""      "opSymbol": "${escapeJson(entry.opSymbol)}",""").append('\n')
      sb.append(s"""      "opIndex": ${entry.opIndex},""").append('\n')
      sb.append("""      "params": [""")
      sb.append(entry.signature.params.map(renderAbiType).mkString(", "))
      sb.append("],\n")
      sb.append(s"""      "result": ${renderAbiType(entry.signature.result)}""").append('\n')
      sb.append("    }")
      if (idx != entries.length - 1) sb.append(',')
      sb.append('\n')
    }

    sb.append("  ]\n")
    sb.append("}\n")
    sb.toString()
  }

  private def renderAbiType(tpe: ExportAbi.AbiType): String = tpe match {
    case ExportAbi.AbiType.Unit => """{"kind":"unit"}"""
    case ExportAbi.AbiType.Bool => """{"kind":"bool"}"""
    case ExportAbi.AbiType.Int8 => """{"kind":"int8"}"""
    case ExportAbi.AbiType.Int16 => """{"kind":"int16"}"""
    case ExportAbi.AbiType.Int32 => """{"kind":"int32"}"""
    case ExportAbi.AbiType.Int64 => """{"kind":"int64"}"""
    case ExportAbi.AbiType.Float32 => """{"kind":"float32"}"""
    case ExportAbi.AbiType.Float64 => """{"kind":"float64"}"""
    case ExportAbi.AbiType.String => """{"kind":"string"}"""
    case ExportAbi.AbiType.Bytes => """{"kind":"bytes"}"""
    case ExportAbi.AbiType.List(elm) =>
      s"""{"kind":"list","element":${renderAbiType(elm)},"repr":{"nilTagId":$PortableListNilTagId,"consTagId":$PortableListConsTagId}}"""
    case ExportAbi.AbiType.Array(elm) =>
      s"""{"kind":"array","element":${renderAbiType(elm)}}"""
    case ExportAbi.AbiType.Tuple(elms) =>
      s"""{"kind":"tuple","elements":[${elms.map(renderAbiType).mkString(",")}]}"""
    case ExportAbi.AbiType.Option(elm) =>
      s"""{"kind":"option","element":${renderAbiType(elm)},"repr":{"noneTagId":$PortableOptionNoneTagId,"someTagId":$PortableOptionSomeTagId}}"""
    case ExportAbi.AbiType.Result(ok, err) =>
      s"""{"kind":"result","ok":${renderAbiType(ok)},"err":${renderAbiType(err)},"repr":{"errTagId":$PortableResultErrTagId,"okTagId":$PortableResultOkTagId}}"""
    case ExportAbi.AbiType.Record(fields) =>
      s"""{"kind":"record","fields":[${fields.map { case (label, t) =>
        s"""{"label":"${escapeJson(label)}","type":${renderAbiType(t)}}"""
      }.mkString(",")}]}"""
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
