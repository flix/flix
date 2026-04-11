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

package ca.uwaterloo.flix.util

object ArtifactNames {

  val DefaultBaseName: String = "flix"

  def baseName(raw: String): String = {
    val normalized = Option(raw).map(_.trim).getOrElse("")
      .map {
        case c if c.isLetterOrDigit => c
        case c @ ('.' | '_' | '-') => c
        case _ => '-'
      }
      .mkString
      .replaceAll("-{2,}", "-")
      .stripPrefix(".")
      .stripSuffix(".")
      .stripPrefix("-")
      .stripSuffix("-")
      .stripPrefix("_")
      .stripSuffix("_")

    if (normalized.nonEmpty) normalized else DefaultBaseName
  }

  def nativeExecutableFileName(raw: String): String = {
    val base = baseName(raw)
    if (isWindows) s"$base.exe" else base
  }

  def nativeStaticLibraryFileName(raw: String): String =
    s"lib${baseName(raw)}.a"

  def nativeSharedLibraryFileName(raw: String): String = {
    val base = baseName(raw)
    if (isWindows) s"$base.dll"
    else if (isMac) s"lib$base.dylib"
    else s"lib$base.so"
  }

  def nativeExportsHeaderFileName(raw: String): String =
    s"${baseName(raw)}.h"

  def wasmExportsManifestFileName(raw: String): String =
    s"${baseName(raw)}.exports.json"

  def wasmEffectsManifestFileName(raw: String): String =
    s"${baseName(raw)}.effects.json"

  def wasmBindingsJsFileName(raw: String): String =
    s"${baseName(raw)}.bindings.mjs"

  def wasmBindingsTypesFileName(raw: String): String =
    s"${baseName(raw)}.bindings.d.ts"

  def wasmTypedExportComponentFileName(raw: String): String =
    s"${baseName(raw)}.exports.component.wasm"

  def wasmTypedExportComponentJsFileName(raw: String): String =
    s"${baseName(raw)}.exports.component.js"

  def wasmTypedExportComponentTypesFileName(raw: String): String =
    s"${baseName(raw)}.exports.component.d.ts"

  def wasmTypedExportWitDirName(raw: String): String =
    s"${baseName(raw)}.exports.wit"

  def wasmCoreFileName(raw: String): String =
    s"${baseName(raw)}.core.wasm"

  def wasmComponentFileName(raw: String): String =
    s"${baseName(raw)}.component.wasm"

  def wasmComponentJsFileName(raw: String): String =
    s"${baseName(raw)}.component.js"

  private def isWindows: Boolean =
    System.getProperty("os.name", "").toLowerCase.contains("win")

  private def isMac: Boolean =
    System.getProperty("os.name", "").toLowerCase.contains("mac")
}
