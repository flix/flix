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

import ca.uwaterloo.flix.language.ast.{LoweredAst, SourceLocation}
import ca.uwaterloo.flix.language.phase.ExportAbi
import ca.uwaterloo.flix.util.{ArtifactNames, InternalCompilerException}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, LinkOption, Path, StandardCopyOption}
import scala.jdk.CollectionConverters.*

/**
  * Packages public export artifacts into a stable SDK layout that does not expose backend-internal
  * build directories.
  */
object LlvmExportSdkWriter {

  case class ExportEntry(symbol: String, signature: ExportAbi.Signature)

  case class NativeArtifacts(root: Path,
                             manifest: Path,
                             header: Path,
                             staticLibrary: Option[Path],
                             sharedLibrary: Option[Path])

  case class WasmArtifacts(root: Path,
                           manifest: Path,
                           component: Path,
                           witDir: Path,
                           effectsManifest: Option[Path],
                           jsDir: Option[Path],
                           bindingsJs: Option[Path],
                           bindingsTypes: Option[Path],
                           typedComponentJs: Option[Path],
                           typedComponentTypes: Option[Path])

  def exportEntries(root: LoweredAst.Root): List[ExportEntry] =
    root.defs.values.toList
      .filter(_.ann.isExport)
      .sortBy(_.sym.toString)
      .map { defn =>
        val sig = defn.exportedSignature.getOrElse {
          throw InternalCompilerException(s"Missing portable export signature for '${defn.sym}'.", defn.loc)
        }
        ExportEntry(defn.sym.toString, sig)
      }

  def nativeSdkDir(outputPath: Path): Path =
    outputPath.resolve("sdk").toAbsolutePath.normalize()

  def nativeManifestPath(outputPath: Path): Path =
    nativeSdkDir(outputPath).resolve("manifest.json")

  def nativeIncludeDir(outputPath: Path): Path =
    nativeSdkDir(outputPath).resolve("include")

  def nativeLibDir(outputPath: Path): Path =
    nativeSdkDir(outputPath).resolve("lib")

  def nativeHeaderPath(outputPath: Path, artifactName: String = ArtifactNames.DefaultBaseName): Path =
    nativeIncludeDir(outputPath).resolve(ArtifactNames.nativeExportsHeaderFileName(artifactName))

  def nativeStaticLibraryPath(outputPath: Path, artifactName: String = ArtifactNames.DefaultBaseName): Path =
    nativeLibDir(outputPath).resolve(ArtifactNames.nativeStaticLibraryFileName(artifactName))

  def nativeSharedLibraryPath(outputPath: Path, artifactName: String = ArtifactNames.DefaultBaseName): Path =
    nativeLibDir(outputPath).resolve(ArtifactNames.nativeSharedLibraryFileName(artifactName))

  def wasmSdkDir(outputPath: Path): Path =
    outputPath.resolve("sdk").toAbsolutePath.normalize()

  def wasmManifestPath(outputPath: Path): Path =
    wasmSdkDir(outputPath).resolve("manifest.json")

  def wasmEffectsManifestPath(outputPath: Path, artifactName: String = ArtifactNames.DefaultBaseName): Path =
    wasmSdkDir(outputPath).resolve(ArtifactNames.wasmEffectsManifestFileName(artifactName))

  def wasmComponentDir(outputPath: Path): Path =
    wasmSdkDir(outputPath).resolve("component")

  def wasmComponentPath(outputPath: Path, artifactName: String = ArtifactNames.DefaultBaseName): Path =
    wasmComponentDir(outputPath).resolve(ArtifactNames.wasmTypedExportComponentFileName(artifactName))

  def wasmWitDir(outputPath: Path): Path =
    wasmSdkDir(outputPath).resolve("wit")

  def wasmJsDir(outputPath: Path): Path =
    wasmSdkDir(outputPath).resolve("js")

  def wasmBindingsJsPath(outputPath: Path, artifactName: String = ArtifactNames.DefaultBaseName): Path =
    wasmJsDir(outputPath).resolve(ArtifactNames.wasmBindingsJsFileName(artifactName))

  def wasmBindingsTypesPath(outputPath: Path, artifactName: String = ArtifactNames.DefaultBaseName): Path =
    wasmJsDir(outputPath).resolve(ArtifactNames.wasmBindingsTypesFileName(artifactName))

  def wasmTypedComponentJsPath(outputPath: Path, artifactName: String = ArtifactNames.DefaultBaseName): Path =
    wasmJsDir(outputPath).resolve(ArtifactNames.wasmTypedExportComponentJsFileName(artifactName))

  def wasmTypedComponentTypesPath(outputPath: Path, artifactName: String = ArtifactNames.DefaultBaseName): Path =
    wasmJsDir(outputPath).resolve(ArtifactNames.wasmTypedExportComponentTypesFileName(artifactName))

  def packageNative(entries: List[ExportEntry],
                    header: Path,
                    staticLibrary: Option[Path],
                    sharedLibrary: Option[Path],
                    artifactName: String,
                    outputPath: Path): NativeArtifacts = {
    if (entries.isEmpty) {
      throw InternalCompilerException("Cannot package native export SDK without @Export definitions.", SourceLocation.Unknown)
    }

    val root = nativeSdkDir(outputPath)
    resetDirectory(root)

    val includeDir = nativeIncludeDir(outputPath)
    val libDir = nativeLibDir(outputPath)
    Files.createDirectories(includeDir)
    Files.createDirectories(libDir)

    val sdkHeader = nativeHeaderPath(outputPath, artifactName)
    copyFile(header, sdkHeader)

    val sdkStatic = staticLibrary.map { src =>
      val dest = nativeStaticLibraryPath(outputPath, artifactName)
      copyFile(src, dest)
      dest
    }
    val sdkShared = sharedLibrary.map { src =>
      val dest = nativeSharedLibraryPath(outputPath, artifactName)
      copyFile(src, dest)
      dest
    }

    val manifest = nativeManifestPath(outputPath)
    writeFile(manifest, renderNativeManifest(entries, artifactName, sdkHeader, sdkStatic, sdkShared, root).getBytes(StandardCharsets.UTF_8))

    NativeArtifacts(root, manifest, sdkHeader, sdkStatic, sdkShared)
  }

  def packageWasm(entries: List[ExportEntry],
                  typedEntries: List[LlvmWasmTypedExportsWriter.Entry],
                  wasmImports: List[LlvmWasmImportsWriter.Entry],
                  effectManifest: Option[Path],
                  typedComponent: Path,
                  typedWitDir: Path,
                  artifactName: String,
                  outputPath: Path,
                  emitJs: Boolean): WasmArtifacts = {
    if (entries.isEmpty) {
      throw InternalCompilerException("Cannot package wasm export SDK without @Export definitions.", SourceLocation.Unknown)
    }

    val root = wasmSdkDir(outputPath)
    resetDirectory(root)

    val componentDir = wasmComponentDir(outputPath)
    val sdkComponent = wasmComponentPath(outputPath, artifactName)
    Files.createDirectories(componentDir)
    copyFile(typedComponent, sdkComponent)

    val sdkWitDir = wasmWitDir(outputPath)
    copyDirectory(typedWitDir, sdkWitDir)

    val sdkEffectsManifest = effectManifest.map { src =>
      val dest = wasmEffectsManifestPath(outputPath, artifactName)
      copyFile(src, dest)
      dest
    }

    val jsArtifacts =
      if (emitJs) {
        val sdkJsDir = wasmJsDir(outputPath)
        LlvmWasmDriver.transpileComponentToJs(typedComponent, sdkJsDir, wasmImports)
        val bindings = LlvmWasmBindingWriter.write(typedEntries, wasmImports, sdkJsDir, artifactName)
        Some((sdkJsDir, bindings.js, bindings.dts, wasmTypedComponentJsPath(outputPath, artifactName), wasmTypedComponentTypesPath(outputPath, artifactName)))
      } else None

    val manifest = wasmManifestPath(outputPath)
    writeFile(manifest, renderWasmManifest(entries, artifactName, sdkComponent, sdkWitDir, sdkEffectsManifest, jsArtifacts, root).getBytes(StandardCharsets.UTF_8))

    WasmArtifacts(
      root = root,
      manifest = manifest,
      component = sdkComponent,
      witDir = sdkWitDir,
      effectsManifest = sdkEffectsManifest,
      jsDir = jsArtifacts.map(_._1),
      bindingsJs = jsArtifacts.map(_._2),
      bindingsTypes = jsArtifacts.map(_._3),
      typedComponentJs = jsArtifacts.map(_._4),
      typedComponentTypes = jsArtifacts.map(_._5)
    )
  }

  private def renderNativeManifest(entries: List[ExportEntry],
                                   artifactName: String,
                                   header: Path,
                                   staticLibrary: Option[Path],
                                   sharedLibrary: Option[Path],
                                   root: Path): String = {
    val sb = new StringBuilder(4096)
    sb.append("{\n")
    sb.append("""  "schema": "flix-export-sdk-v0",""").append('\n')
    sb.append(s"""  "name": "${escapeJson(ArtifactNames.baseName(artifactName))}",""").append('\n')
    sb.append("""  "target": "native",""").append('\n')
    sb.append("""  "abi": "portable-v0",""").append('\n')
    sb.append("""  "execModel": "ok-thrown-suspended",""").append('\n')
    sb.append("""  "artifacts": {""").append('\n')
    sb.append(s"""    "header": "${escapeJson(relative(root, header))}"""")
    staticLibrary.foreach(path => sb.append(",\n").append(s"""    "staticLibrary": "${escapeJson(relative(root, path))}""""))
    sharedLibrary.foreach(path => sb.append(",\n").append(s"""    "sharedLibrary": "${escapeJson(relative(root, path))}""""))
    sb.append('\n').append("  },\n")
    sb.append(renderExports(entries))
    sb.append("}\n")
    sb.toString()
  }

  private def renderWasmManifest(entries: List[ExportEntry],
                                 artifactName: String,
                                 component: Path,
                                 witDir: Path,
                                 effectsManifest: Option[Path],
                                 jsArtifacts: Option[(Path, Path, Path, Path, Path)],
                                 root: Path): String = {
    val sb = new StringBuilder(4096)
    sb.append("{\n")
    sb.append("""  "schema": "flix-export-sdk-v0",""").append('\n')
    sb.append(s"""  "name": "${escapeJson(ArtifactNames.baseName(artifactName))}",""").append('\n')
    sb.append("""  "target": "wasm",""").append('\n')
    sb.append("""  "abi": "portable-v0",""").append('\n')
    sb.append("""  "execModel": "ok-thrown-suspended",""").append('\n')
    sb.append("""  "artifacts": {""").append('\n')
    sb.append(s"""    "component": "${escapeJson(relative(root, component))}",""").append('\n')
    sb.append(s"""    "witDir": "${escapeJson(relative(root, witDir))}"""")
    effectsManifest.foreach { path =>
      sb.append(",\n").append(s"""    "effectsManifest": "${escapeJson(relative(root, path))}"""")
    }
    jsArtifacts.foreach { case (jsDir, bindingsJs, bindingsTypes, typedJs, typedTypes) =>
      sb.append(",\n").append(s"""    "jsDir": "${escapeJson(relative(root, jsDir))}",""").append('\n')
      sb.append(s"""    "bindingsJs": "${escapeJson(relative(root, bindingsJs))}",""").append('\n')
      sb.append(s"""    "bindingsTypes": "${escapeJson(relative(root, bindingsTypes))}",""").append('\n')
      sb.append(s"""    "typedComponentJs": "${escapeJson(relative(root, typedJs))}",""").append('\n')
      sb.append(s"""    "typedComponentTypes": "${escapeJson(relative(root, typedTypes))}"""")
    }
    sb.append('\n').append("  },\n")
    sb.append(renderExports(entries))
    sb.append("}\n")
    sb.toString()
  }

  private def renderExports(entries: List[ExportEntry]): String = {
    val sb = new StringBuilder(2048)
    sb.append("""  "exports": [""").append('\n')
    entries.zipWithIndex.foreach { case (entry, idx) =>
      sb.append("    {\n")
      sb.append(s"""      "symbol": "${escapeJson(entry.symbol)}",""").append('\n')
      sb.append(s"""      "params": [${entry.signature.params.map(p => "\"" + escapeJson(p.displayName) + "\"").mkString(", ")}],""").append('\n')
      sb.append(s"""      "result": "${escapeJson(entry.signature.result.displayName)}"""").append('\n')
      sb.append("    }")
      if (idx != entries.length - 1) sb.append(',')
      sb.append('\n')
    }
    sb.append("  ]\n")
    sb.toString()
  }

  private def relative(root: Path, path: Path): String =
    root.toAbsolutePath.normalize().relativize(path.toAbsolutePath.normalize()).toString.replace('\\', '/')

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
      case c if c < ' ' => b.append(f"\\u${c.toInt}%04x")
      case c => b.append(c)
    }
    b.toString()
  }

  private def resetDirectory(dir: Path): Unit = {
    if (Files.exists(dir)) {
      Files.walk(dir)
        .sorted(java.util.Comparator.reverseOrder())
        .iterator()
        .asScala
        .foreach(Files.deleteIfExists)
    }
    Files.createDirectories(dir)
  }

  private def copyDirectory(src: Path, dest: Path): Unit = {
    Files.walk(src).iterator().asScala.foreach { current =>
      val rel = src.relativize(current)
      val out = dest.resolve(rel.toString)
      if (Files.isDirectory(current)) {
        Files.createDirectories(out)
      } else {
        copyFile(current, out)
      }
    }
  }

  private def copyFile(src: Path, dest: Path): Unit = {
    Files.createDirectories(dest.getParent)
    Files.copy(src, dest, StandardCopyOption.REPLACE_EXISTING)
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
    }

    Files.write(path, bytes)
  }
}
