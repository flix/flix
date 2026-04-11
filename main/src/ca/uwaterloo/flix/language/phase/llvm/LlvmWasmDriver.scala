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
import ca.uwaterloo.flix.language.phase.WasmImportInterface
import ca.uwaterloo.flix.util.{ArtifactNames, Build, InternalCompilerException, ZigToolchain}

import java.nio.charset.StandardCharsets
import java.nio.file.{FileSystem, FileSystemNotFoundException, FileSystems, Files, Path, Paths, StandardCopyOption}
import scala.jdk.CollectionConverters.*

/**
  * Drives the host toolchain to turn textual LLVM IR (`.ll`) into a wasm component + JS package.
  *
  * Browser-first bring-up:
  *   - core module is `wasm32-freestanding` (no WASI preview1),
  *   - the component world is described by WIT (`runtime/wit/flix-bindings`),
  *   - we use `wasm-tools component embed/new` to produce a component,
  *   - we optionally use `jco transpile` to generate a browser/Node-friendly JS bundle.
  */
object LlvmWasmDriver {

  private case class WitBuildInputs(bindingsDir: Path, glueC: Path)

  private def zigCommand: List[String] = ZigToolchain.usableCommand.getOrElse {
    throw InternalCompilerException(
      "LLVM-wasm toolchain requires a usable Zig command. Set FLIX_ZIG_CMD if Zig is managed through a wrapper such as anyzig.",
      SourceLocation.Unknown
    )
  }

  case class Artifacts(coreWasm: Path,
                       componentWasm: Path,
                       componentJs: Path,
                       exportsManifest: Path,
                       jsOutDir: Path,
                       nodeRunner: Path,
                       typedExportComponent: Option[Path],
                       typedExportWitDir: Option[Path],
                       typedExportComponentJs: Option[Path],
                       typedExportComponentTypes: Option[Path])

  private val BundledRuntimeSourceDir: String = "/runtime/src"
  private val BundledWitGlueCResource: String = "/runtime/src/wit/flix.c"
  private val BundledWitGlueHResource: String = "/runtime/src/wit/flix.h"
  private val BundledSysJsResource: String = "/tools/wasm-runner-js/sys.js"
  private val BundledRunFlixResource: String = "/tools/wasm-runner-js/run-flix.mjs"
  private val BundledRunnerResource: String = "/tools/wasm-runner-js/runner.mjs"
  private val BundledEffectHandlersResource: String = "/tools/wasm-runner-js/effect-handlers.mjs"
  private val BundledWitEffectBindingsResource: String = "/tools/wasm-runner-js/wit-effect-bindings.mjs"
  private val BundledNodeHandlersResource: String = "/tools/wasm-runner-js/node-handlers.mjs"
  private val BundledNodeTcpHandlersResource: String = "/tools/wasm-runner-js/node-tcp-handlers.mjs"
  private val BundledNodeProcessHandlersResource: String = "/tools/wasm-runner-js/node-process-handlers.mjs"
  private val BundledWasmtimeCargoTomlResource: String = "/tools/wasm-runner-rs/Cargo.toml"
  private val BundledWasmtimeCargoLockResource: String = "/tools/wasm-runner-rs/Cargo.lock"
  private val BundledWasmtimeLibResource: String = "/tools/wasm-runner-rs/src/lib.rs"
  private val BundledWasmtimeEffectsResource: String = "/tools/wasm-runner-rs/src/effects.rs"
  private val BundledWasmtimeHostResource: String = "/tools/wasm-runner-rs/src/host.rs"
  private val BundledWasmtimeRunnerResource: String = "/tools/wasm-runner-rs/src/runner.rs"
  private val BundledWasmtimeBinResource: String = "/tools/wasm-runner-rs/src/bin/run_flix.rs"
  private val BundledWitBindingsResource: String = "/runtime/wit/flix-bindings/bindings.wit"
  private val BundledWitRuntimeDepResource: String = "/runtime/wit/flix-bindings/deps/runtime.wit"
  private val BundledWitSysDepResource: String = "/runtime/wit/flix-bindings/deps/sys.wit"

  private val DefaultWitBindingsDir: Path =
    Paths.get("runtime/wit/flix-bindings").toAbsolutePath.normalize()

  private val DefaultWitGlueDir: Path =
    Paths.get("runtime/src/wit").toAbsolutePath.normalize()

  private val DefaultSysJs: Path =
    Paths.get("tools/wasm-runner-js/sys.js").toAbsolutePath.normalize()

  private val DefaultNodeRunner: Path =
    Paths.get("tools/wasm-runner-js/run-flix.mjs").toAbsolutePath.normalize()

  private val DefaultRunnerModule: Path =
    Paths.get("tools/wasm-runner-js/runner.mjs").toAbsolutePath.normalize()

  private val DefaultEffectHandlersModule: Path =
    Paths.get("tools/wasm-runner-js/effect-handlers.mjs").toAbsolutePath.normalize()

  private val DefaultWitEffectBindingsModule: Path =
    Paths.get("tools/wasm-runner-js/wit-effect-bindings.mjs").toAbsolutePath.normalize()

  private val DefaultNodeHandlersModule: Path =
    Paths.get("tools/wasm-runner-js/node-handlers.mjs").toAbsolutePath.normalize()

  private val DefaultNodeTcpHandlersModule: Path =
    Paths.get("tools/wasm-runner-js/node-tcp-handlers.mjs").toAbsolutePath.normalize()

  private val DefaultNodeProcessHandlersModule: Path =
    Paths.get("tools/wasm-runner-js/node-process-handlers.mjs").toAbsolutePath.normalize()

  private val DefaultWasmtimeRunnerCargoToml: Path =
    Paths.get("tools/wasm-runner-rs/Cargo.toml").toAbsolutePath.normalize()

  private val WitBindgenCliVersion = "0.53.1"
  private val WacCliVersion = "0.9.0"

  private val DefaultWitBindgenCli: Path =
    Paths.get("build").resolve("tools").resolve(s"wit-bindgen-cli-$WitBindgenCliVersion").resolve("bin").resolve("wit-bindgen").toAbsolutePath.normalize()

  private val DefaultWacCli: Path =
    Paths.get("build").resolve("tools").resolve(s"wac-cli-$WacCliVersion").resolve("bin").resolve("wac").toAbsolutePath.normalize()

  def run(modulePath: Path,
          typedExports: List[LlvmWasmTypedExportsWriter.Entry] = Nil,
          wasmImports: List[LlvmWasmImportsWriter.Entry] = Nil,
          emitJs: Boolean = true)(implicit flix: Flix): Artifacts = {
    val outDir = flix.options.outputPath.resolve("llvm").toAbsolutePath
    Files.createDirectories(outDir)

    val wasmDir = wasmDirPath(flix.options.outputPath)
    Files.createDirectories(wasmDir)

    val optFlag = flix.options.build match {
      case Build.Development => "-O0"
      case Build.Production => "-O2"
    }

    val runtimeZig = resolveRuntimeZig(outDir)
    val witBuildInputs = prepareWitBuildInputs(outDir, wasmDir, wasmImports)
    val witBindingsDir = witBuildInputs.bindingsDir
    val witGlueC = witBuildInputs.glueC
    val runtimeObj = compileRuntime(runtimeZig, wasmDir, optFlag)
    val moduleObj = compileModule(modulePath, wasmDir, optFlag)
    val witObj = compileWitGlue(witGlueC, wasmDir.resolve("flix_wit_glue.wasm.o"), wasmDir, optFlag)

    val coreWasm = coreWasmPath(flix.options.outputPath, flix.options.artifactName)
    linkCore(coreWasm, wasmDir, optFlag, List(moduleObj, runtimeObj, witObj))

    val embeddedCore = wasmDir.resolve("flix-llvm-wasm.core.embed.wasm")
    embedWit(coreWasm, embeddedCore, wasmDir, witBindingsDir)

    val componentWasm = componentWasmPath(flix.options.outputPath, flix.options.artifactName)
    componentize(embeddedCore, componentWasm, wasmDir)

    if (emitJs) {
      transpileComponentToJs(componentWasm, jsOutDirPath(flix.options.outputPath), wasmImports)
    }
    val jsOutDir = jsOutDirPath(flix.options.outputPath)
    val componentJs = componentJsPath(flix.options.outputPath, flix.options.artifactName)
    val nodeRunner = if (emitJs) resolveNodeRunner(outDir) else outDir.resolve("wasm-runner-js").resolve("run-flix.mjs")
    val exportsManifest = LlvmWasmExportWriter.manifestPath(flix.options.outputPath, flix.options.artifactName)
    val typedExportsArtifacts = buildTypedExportComponent(componentWasm, wasmDir, outDir, witBindingsDir, flix.options.outputPath, flix.options.artifactName, typedExports, wasmImports, optFlag)
    if (emitJs) {
      typedExportsArtifacts.foreach { case (typedComponent, _) =>
        transpileComponentToJs(typedComponent, jsOutDirPath(flix.options.outputPath), wasmImports)
      }
    }

    Artifacts(
      coreWasm = coreWasm,
      componentWasm = componentWasm,
      componentJs = componentJs,
      exportsManifest = exportsManifest,
      jsOutDir = jsOutDir,
      nodeRunner = nodeRunner,
      typedExportComponent = typedExportsArtifacts.map(_._1),
      typedExportWitDir = typedExportsArtifacts.map(_._2),
      typedExportComponentJs = typedExportsArtifacts.map(_ => LlvmWasmTypedExportsWriter.typedComponentJsPath(flix.options.outputPath, flix.options.artifactName)),
      typedExportComponentTypes = typedExportsArtifacts.map(_ => LlvmWasmTypedExportsWriter.typedComponentTypesPath(flix.options.outputPath, flix.options.artifactName))
    )
  }

  def wasmDirPath(outputPath: Path): Path =
    outputPath.resolve("llvm").resolve("wasm").toAbsolutePath.normalize()

  def coreWasmPath(outputPath: Path, artifactName: String = ArtifactNames.DefaultBaseName): Path =
    wasmDirPath(outputPath).resolve(ArtifactNames.wasmCoreFileName(artifactName))

  def componentWasmPath(outputPath: Path, artifactName: String = ArtifactNames.DefaultBaseName): Path =
    wasmDirPath(outputPath).resolve(ArtifactNames.wasmComponentFileName(artifactName))

  def jsOutDirPath(outputPath: Path): Path =
    wasmDirPath(outputPath).resolve("js")

  def componentJsPath(outputPath: Path, artifactName: String = ArtifactNames.DefaultBaseName): Path =
    jsOutDirPath(outputPath).resolve(ArtifactNames.wasmComponentJsFileName(artifactName))

  /**
    * Resolves the Zig runtime support file for the LLVM backend.
    *
    * Bring-up behavior:
    *   1. Prefer `runtime/src/flix_rt_llvm.zig` relative to the current working directory.
    *   2. Otherwise, extract the bundled runtime source tree from `flix.jar` into `outDir`.
    */
  private def resolveRuntimeZig(outDir: Path): Path = {
    val runtimeDir = outDir.resolve("runtime/src").toAbsolutePath.normalize()
    val cwdRuntimeDir = Paths.get("runtime/src").toAbsolutePath.normalize()

    if (Files.exists(cwdRuntimeDir.resolve("flix_rt_llvm.zig"))) {
      copyTree(cwdRuntimeDir, runtimeDir)
    } else {
      copyBundledTree(BundledRuntimeSourceDir, runtimeDir)
    }

    runtimeDir.resolve("flix_rt_llvm.zig")
  }

  private def copyTree(sourceDir: Path, destDir: Path): Unit = {
    Files.walk(sourceDir).forEach { src =>
      if (Files.isRegularFile(src)) {
        val rel = sourceDir.relativize(src)
        val dest = destDir.resolve(rel.toString).toAbsolutePath.normalize()
        Option(dest.getParent).foreach(parent => Files.createDirectories(parent))
        Files.copy(src, dest, StandardCopyOption.REPLACE_EXISTING)
      }
    }
  }

  private def copyBundledTree(resourceDir: String, destDir: Path): Unit = {
    val resourceUrl = Option(getClass.getResource(resourceDir)).getOrElse {
      throw InternalCompilerException(
        s"Missing bundled LLVM runtime resource tree '$resourceDir'.",
        SourceLocation.Unknown
      )
    }

    val resourceUri = resourceUrl.toURI

    def copyFrom(root: Path, closeFs: Option[FileSystem]): Unit = {
      try {
        Files.walk(root).forEach { src =>
          if (Files.isRegularFile(src)) {
            val rel = root.relativize(src)
            val dest = destDir.resolve(rel.toString).toAbsolutePath.normalize()
            Option(dest.getParent).foreach(parent => Files.createDirectories(parent))
            Files.copy(src, dest, StandardCopyOption.REPLACE_EXISTING)
          }
        }
      } finally {
        closeFs.foreach(_.close())
      }
    }

    if (resourceUri.getScheme == "jar") {
      val (fs, closeFs) =
        try (FileSystems.getFileSystem(resourceUri), None)
        catch {
          case _: FileSystemNotFoundException =>
            val created = FileSystems.newFileSystem(resourceUri, Map.empty[String, AnyRef].asJava)
            (created, Some(created))
        }
      copyFrom(fs.getPath(resourceDir), closeFs)
    } else {
      copyFrom(Paths.get(resourceUri), None)
    }
  }

  private def compileRuntime(runtimeZig: Path, wasmDir: Path, optFlag: String): Path = {
    val out = wasmDir.resolve("flix_rt_llvm.wasm.o")
    val cmd = zigCommand ::: List(
      "cc",
      "-target", "wasm32-freestanding",
      "-c",
      "-Wno-override-module",
      optFlag,
      runtimeZig.toString,
      "-o", out.toString
    )
    val (exit, output) = exec(cmd, wasmDir)
    if (exit != 0) {
      throw InternalCompilerException(
        s"LLVM-wasm toolchain failed while compiling runtime (exit $exit):\n${cmd.mkString(" ")}\n\n$output",
        SourceLocation.Unknown
      )
    }
    out
  }

  private def compileModule(modulePath: Path, wasmDir: Path, optFlag: String): Path = {
    val out = wasmDir.resolve("module.wasm.o")
    val cmd = zigCommand ::: List(
      "cc",
      "-target", "wasm32-freestanding",
      "-c",
      "-Wno-override-module",
      optFlag,
      modulePath.toString,
      "-o", out.toString
    )
    val (exit, output) = exec(cmd, wasmDir)
    if (exit != 0) {
      throw InternalCompilerException(
        s"LLVM-wasm toolchain failed while compiling module object (exit $exit):\n${cmd.mkString(" ")}\n\n$output",
        SourceLocation.Unknown
      )
    }
    out
  }

  /**
    * Compiles the generated WIT C glue.
    *
    * Bring-up shortcut: compile as `wasm32-wasi` (headers available) but link into a freestanding core module.
    * This avoids introducing a WASI dependency as long as we do not link wasi-libc.
    */
  private def compileWitGlue(witGlueC: Path, out: Path, wasmDir: Path, optFlag: String): Path = {
    val cmd = zigCommand ::: List(
      "cc",
      "-target", "wasm32-wasi",
      "-c",
      optFlag,
      witGlueC.toString,
      "-o", out.toString
    )
    val (exit, output) = exec(cmd, wasmDir)
    if (exit != 0) {
      throw InternalCompilerException(
        s"LLVM-wasm toolchain failed while compiling WIT glue (exit $exit):\n${cmd.mkString(" ")}\n\n$output",
        SourceLocation.Unknown
      )
    }
    out
  }

  private def linkCore(outWasm: Path, wasmDir: Path, optFlag: String, objs: List[Path]): Unit = {
    val cmd = zigCommand ::: List(
      "cc",
      "-target", "wasm32-freestanding",
      "-Wl,--no-entry",
      // Canonical ABI requires `cabi_realloc` to be exported by the core module.
      // (The implementation is provided by `runtime/src/flix_rt_llvm.zig`.)
      "-Wl,--export=cabi_realloc",
      optFlag
    ) ::: objs.map(_.toString) ::: List("-o", outWasm.toString)

    val (exit, output) = exec(cmd, wasmDir)
    if (exit != 0) {
      throw InternalCompilerException(
        s"LLVM-wasm toolchain failed while linking core wasm (exit $exit):\n${cmd.mkString(" ")}\n\n$output",
        SourceLocation.Unknown
      )
    }
  }

  private def embedWit(inWasm: Path, outWasm: Path, cwd: Path, witBindingsDir: Path): Unit = {
    if (!Files.exists(witBindingsDir)) {
      throw InternalCompilerException(s"Missing WIT bindings directory: '$witBindingsDir'.", SourceLocation.Unknown)
    }

    val cmd = List(
      "wasm-tools",
      "component",
      "embed",
      witBindingsDir.toString,
      inWasm.toString,
      "--world",
      "flix",
      "-o",
      outWasm.toString
    )
    val (exit, output) = exec(cmd, cwd)
    if (exit != 0) {
      throw InternalCompilerException(
        s"wasm-tools failed while embedding WIT metadata (exit $exit):\n${cmd.mkString(" ")}\n\n$output",
        SourceLocation.Unknown
      )
    }
  }

  private def componentize(inWasm: Path, outComponent: Path, cwd: Path): Unit = {
    val cmd = List(
      "wasm-tools",
      "component",
      "new",
      inWasm.toString,
      "-o",
      outComponent.toString
    )
    val (exit, output) = exec(cmd, cwd)
    if (exit != 0) {
      throw InternalCompilerException(
        s"wasm-tools failed while creating a component (exit $exit):\n${cmd.mkString(" ")}\n\n$output",
        SourceLocation.Unknown
      )
    }
  }

  private def linkTypedExportAdapter(outWasm: Path,
                                     wasmDir: Path,
                                     optFlag: String,
                                     glueC: Path,
                                     implC: Path,
                                     shimC: Path,
                                     componentTypeObj: Path,
                                     includeDir: Path): Unit = {
    val cmd = zigCommand ::: List(
      "cc",
      "-target", "wasm32-freestanding",
      "-Wl,--no-entry",
      "-I",
      includeDir.toString,
      "-nostdlibinc",
      optFlag
    ) ::: List(glueC.toString, implC.toString, shimC.toString, componentTypeObj.toString, "-o", outWasm.toString)

    val (exit, output) = exec(cmd, wasmDir)
    if (exit != 0) {
      throw InternalCompilerException(
        s"LLVM-wasm toolchain failed while linking typed export adapter (exit $exit):\n${cmd.mkString(" ")}\n\n$output",
        SourceLocation.Unknown
      )
    }
  }

  private def composeTypedExportComponent(baseComponent: Path, adapterComponent: Path, outComponent: Path, composeWac: Path, cwd: Path): Unit = {
    val wac = resolveWacCli()
    val cmd = List(
      wac.toString,
      "compose",
      composeWac.toString,
      "-d",
      s"flix:base=${baseComponent.toString}",
      "-d",
      s"flix:adapter=${adapterComponent.toString}",
      "-o",
      outComponent.toString
    )
    val (exit, output) = exec(cmd, cwd)
    if (exit != 0) {
      throw InternalCompilerException(
        s"wasm-tools failed while composing typed export component (exit $exit):\n${cmd.mkString(" ")}\n\n$output",
        SourceLocation.Unknown
      )
    }
  }

  private[llvm] def transpileComponentToJs(component: Path, jsDir: Path, wasmImports: List[LlvmWasmImportsWriter.Entry] = Nil): Path = {
    Files.createDirectories(jsDir)

    // Ensure a default sys implementation is available for the transpiled output.
    // Hosts are free to ignore/replace this mapping.
    copyDefaultSysJs(jsDir)
    copyDefaultEffectHandlersJs(jsDir)
    copyDefaultWitEffectBindingsJs(jsDir)
    writeJsImportStubs(jsDir, wasmImports)

    val cmd = List(
      "jco",
      "transpile",
      component.toString,
      "-o",
      jsDir.toString,
      "--map",
      "flix:sys/sys=./sys.js"
    ) ::: wasmImports
      .map(_.interfaceId)
      .distinct
      .sortBy(_.qualifiedInterface)
      .flatMap(id => List("--map", s"${id.qualifiedInterface}=./${id.jsModuleFileName}"))

    val cwd = Option(component.getParent).getOrElse(jsDir)
    val (exit, output) = exec(cmd, cwd)
    if (exit != 0) {
      throw InternalCompilerException(
        s"jco transpile failed (exit $exit):\n${cmd.mkString(" ")}\n\n$output",
        SourceLocation.Unknown
      )
    }

    jsDir
  }

  private def copyDefaultSysJs(jsDir: Path): Unit = {
    val dest = jsDir.resolve("sys.js")
    if (Files.exists(DefaultSysJs)) {
      Files.copy(DefaultSysJs, dest, StandardCopyOption.REPLACE_EXISTING)
      return
    }

    copyBundledResource(BundledSysJsResource, dest)
  }

  private def copyDefaultEffectHandlersJs(jsDir: Path): Unit = {
    val dest = jsDir.resolve("effect-handlers.mjs")
    if (Files.exists(DefaultEffectHandlersModule)) {
      Files.copy(DefaultEffectHandlersModule, dest, StandardCopyOption.REPLACE_EXISTING)
      return
    }

    copyBundledResource(BundledEffectHandlersResource, dest)
  }

  private def copyDefaultWitEffectBindingsJs(jsDir: Path): Unit = {
    val dest = jsDir.resolve("wit-effect-bindings.mjs")
    if (Files.exists(DefaultWitEffectBindingsModule)) {
      Files.copy(DefaultWitEffectBindingsModule, dest, StandardCopyOption.REPLACE_EXISTING)
      return
    }

    copyBundledResource(BundledWitEffectBindingsResource, dest)
  }

  private def writeJsImportStubs(jsDir: Path, wasmImports: List[LlvmWasmImportsWriter.Entry]): Unit = {
    wasmImports
      .groupBy(_.interfaceId)
      .toList
      .sortBy(_._1.qualifiedInterface)
      .foreach { case (id, entries) =>
        val dest = jsDir.resolve(id.jsModuleFileName)
        val methods = entries.groupBy(_.spec.func).keys.toList.sorted
        val body = new StringBuilder(1024)
        body.append("// Generated by the Flix compiler (LLVM-wasm host import stub)\n")
        body.append(s"// Interface: ${id.qualifiedInterface}\n")
        body.append("let impl = Object.freeze({});\n\n")
        body.append(s"export function ${id.jsSetterName}(next) {\n")
        body.append("  impl = next ?? {};\n")
        body.append("}\n\n")
        methods.foreach { func =>
          body.append(s"export function ${WasmImportInterface.jsName(func)}(...args) {\n")
          body.append(s"""  const fn = impl[${renderJsString(func)}];\n""")
          body.append("  if (typeof fn !== \"function\") {\n")
          body.append(s"""    throw new Error("Missing JS host implementation for ${escapeJsString(id.qualifiedInterface)}#${escapeJsString(func)}");\n""")
          body.append("  }\n")
          body.append("  return fn(...args);\n")
          body.append("}\n\n")
        }
        writeFile(dest, body.toString().getBytes(StandardCharsets.UTF_8))
      }
  }

  private def resolveNodeRunner(outDir: Path): Path = {
    val runnerDir = outDir.resolve("wasm-runner-js")
    Files.createDirectories(runnerDir)

    copyRunnerSupportFile(DefaultNodeRunner, BundledRunFlixResource, runnerDir.resolve("run-flix.mjs"))
    copyRunnerSupportFile(DefaultRunnerModule, BundledRunnerResource, runnerDir.resolve("runner.mjs"))
    copyRunnerSupportFile(DefaultEffectHandlersModule, BundledEffectHandlersResource, runnerDir.resolve("effect-handlers.mjs"))
    copyRunnerSupportFile(DefaultWitEffectBindingsModule, BundledWitEffectBindingsResource, runnerDir.resolve("wit-effect-bindings.mjs"))
    copyRunnerSupportFile(DefaultNodeHandlersModule, BundledNodeHandlersResource, runnerDir.resolve("node-handlers.mjs"))
    copyRunnerSupportFile(DefaultNodeTcpHandlersModule, BundledNodeTcpHandlersResource, runnerDir.resolve("node-tcp-handlers.mjs"))
    copyRunnerSupportFile(DefaultNodeProcessHandlersModule, BundledNodeProcessHandlersResource, runnerDir.resolve("node-process-handlers.mjs"))

    runnerDir.resolve("run-flix.mjs")
  }

  private def copyRunnerSupportFile(defaultPath: Path, bundledResource: String, dest: Path): Unit = {
    if (Files.exists(defaultPath)) {
      Files.copy(defaultPath, dest, StandardCopyOption.REPLACE_EXISTING)
    } else {
      copyBundledResource(bundledResource, dest)
    }
  }

  def resolveWasmtimeRunnerManifest(outputPath: Path): Path = {
    val outDir = outputPath.resolve("llvm").toAbsolutePath.normalize()
    if (Files.exists(DefaultWasmtimeRunnerCargoToml)) {
      return DefaultWasmtimeRunnerCargoToml
    }

    val runnerDir = outDir.resolve("tools").resolve("wasm-runner-rs")
    Files.createDirectories(runnerDir.resolve("src").resolve("bin"))
    copyBundledResource(BundledWasmtimeCargoTomlResource, runnerDir.resolve("Cargo.toml"))
    copyBundledResource(BundledWasmtimeCargoLockResource, runnerDir.resolve("Cargo.lock"))
    copyBundledResource(BundledWasmtimeLibResource, runnerDir.resolve("src").resolve("lib.rs"))
    copyBundledResource(BundledWasmtimeEffectsResource, runnerDir.resolve("src").resolve("effects.rs"))
    copyBundledResource(BundledWasmtimeHostResource, runnerDir.resolve("src").resolve("host.rs"))
    copyBundledResource(BundledWasmtimeRunnerResource, runnerDir.resolve("src").resolve("runner.rs"))
    copyBundledResource(BundledWasmtimeBinResource, runnerDir.resolve("src").resolve("bin").resolve("run_flix.rs"))

    val witDir = outDir.resolve("runtime").resolve("wit").resolve("flix-bindings")
    Files.createDirectories(witDir.resolve("deps"))
    copyBundledResource(BundledWitBindingsResource, witDir.resolve("bindings.wit"))
    copyBundledResource(BundledWitRuntimeDepResource, witDir.resolve("deps").resolve("runtime.wit"))
    copyBundledResource(BundledWitSysDepResource, witDir.resolve("deps").resolve("sys.wit"))

    runnerDir.resolve("Cargo.toml")
  }

  private def resolveWitBindingsDir(outDir: Path): Path = {
    if (Files.exists(DefaultWitBindingsDir)) {
      return DefaultWitBindingsDir
    }

    val bindingsDir = outDir.resolve("runtime").resolve("wit").resolve("flix-bindings")
    Files.createDirectories(bindingsDir.resolve("deps"))
    copyBundledResource(BundledWitBindingsResource, bindingsDir.resolve("bindings.wit"))
    copyBundledResource(BundledWitRuntimeDepResource, bindingsDir.resolve("deps").resolve("runtime.wit"))
    copyBundledResource(BundledWitSysDepResource, bindingsDir.resolve("deps").resolve("sys.wit"))
    bindingsDir
  }

  private def prepareWitBuildInputs(outDir: Path, wasmDir: Path, wasmImports: List[LlvmWasmImportsWriter.Entry]): WitBuildInputs = {
    if (wasmImports.isEmpty) {
      return WitBuildInputs(resolveWitBindingsDir(outDir), resolveWitGlue(outDir))
    }

    val baseBindingsDir = resolveWitBindingsDir(outDir)
    val generatedBindingsDir = outDir.resolve("runtime").resolve("wit").resolve("generated-bindings")
    val generatedDepsDir = generatedBindingsDir.resolve("deps")
    Files.createDirectories(generatedDepsDir)

    val runtimeDep = baseBindingsDir.resolve("deps").resolve("runtime.wit")
    val sysDep = baseBindingsDir.resolve("deps").resolve("sys.wit")
    if (!Files.exists(runtimeDep)) {
      throw InternalCompilerException(s"Missing runtime WIT dependency: '$runtimeDep'.", SourceLocation.Unknown)
    }
    if (!Files.exists(sysDep)) {
      throw InternalCompilerException(s"Missing sys WIT dependency: '$sysDep'.", SourceLocation.Unknown)
    }

    Files.copy(runtimeDep, generatedDepsDir.resolve("runtime.wit"), StandardCopyOption.REPLACE_EXISTING)
    Files.copy(sysDep, generatedDepsDir.resolve("sys.wit"), StandardCopyOption.REPLACE_EXISTING)
    Files.writeString(generatedBindingsDir.resolve("bindings.wit"), LlvmWasmImportsWriter.renderBindings(wasmImports), StandardCharsets.UTF_8)
    LlvmWasmImportsWriter.renderDeps(wasmImports).foreach {
      case (fileName, contents) =>
        Files.writeString(generatedDepsDir.resolve(fileName), contents, StandardCharsets.UTF_8)
    }

    val generatedGlueDir = outDir.resolve("wit").resolve("generated-import-glue")
    Files.createDirectories(generatedGlueDir)
    val witBindgen = resolveWitBindgenCli()
    val witCmd = List(
      witBindgen.toString,
      "c",
      generatedBindingsDir.toString,
      "--world",
      "flix",
      "--out-dir",
      generatedGlueDir.toString
    )
    val (witExit, witOutput) = exec(witCmd, wasmDir)
    if (witExit != 0) {
      throw InternalCompilerException(
        s"wit-bindgen failed while generating wasm import glue (exit $witExit):\n${witCmd.mkString(" ")}\n\n$witOutput",
        SourceLocation.Unknown
      )
    }

    WitBuildInputs(generatedBindingsDir, generatedGlueDir.resolve("flix.c"))
  }

  private def resolveWitBindgenCli(): Path = {
    if (versionMatchesWitBindgen(DefaultWitBindgenCli)) {
      return DefaultWitBindgenCli
    }

    val installRoot = DefaultWitBindgenCli.getParent.getParent
    Files.createDirectories(installRoot)
    val cmd = List(
      "cargo",
      "+stable",
      "install",
      "wit-bindgen-cli",
      "--version",
      WitBindgenCliVersion,
      "--locked",
      "--root",
      installRoot.toString
    )
    val (exit, output) = exec(cmd, Paths.get(".").toAbsolutePath.normalize())
    if (exit != 0 || !versionMatchesWitBindgen(DefaultWitBindgenCli)) {
      throw InternalCompilerException(
        s"Failed to install wit-bindgen-cli $WitBindgenCliVersion for typed wasm exports.\n${cmd.mkString(" ")}\n\n$output",
        SourceLocation.Unknown
      )
    }
    DefaultWitBindgenCli
  }

  private def versionMatchesWitBindgen(bin: Path): Boolean = {
    if (!Files.exists(bin)) return false
    val (exit, output) = exec(List(bin.toString, "--version"), bin.getParent)
    exit == 0 && {
      val trimmed = output.trim
      trimmed == s"wit-bindgen $WitBindgenCliVersion" || trimmed == s"wit-bindgen-cli $WitBindgenCliVersion"
    }
  }

  private def resolveWacCli(): Path = {
    if (versionMatchesWac(DefaultWacCli)) {
      return DefaultWacCli
    }

    val installRoot = DefaultWacCli.getParent.getParent
    Files.createDirectories(installRoot)
    val cmd = List(
      "cargo",
      "+stable",
      "install",
      "wac-cli",
      "--version",
      WacCliVersion,
      "--root",
      installRoot.toString
    )
    val (exit, output) = exec(cmd, Paths.get(".").toAbsolutePath.normalize())
    if (exit != 0 || !versionMatchesWac(DefaultWacCli)) {
      throw InternalCompilerException(
        s"Failed to install wac-cli $WacCliVersion for typed wasm export composition.\n${cmd.mkString(" ")}\n\n$output",
        SourceLocation.Unknown
      )
    }
    DefaultWacCli
  }

  private def versionMatchesWac(bin: Path): Boolean = {
    if (!Files.exists(bin)) return false
    val (exit, output) = exec(List(bin.toString, "--version"), bin.getParent)
    exit == 0 && {
      val trimmed = output.trim
      trimmed == s"wac $WacCliVersion" || trimmed == s"wac-cli $WacCliVersion"
    }
  }

  private def buildTypedExportComponent(baseComponentWasm: Path,
                                        wasmDir: Path,
                                        outDir: Path,
                                        witBindingsDir: Path,
                                        outputPath: Path,
                                        artifactName: String,
                                        typedExports: List[LlvmWasmTypedExportsWriter.Entry],
                                        wasmImports: List[LlvmWasmImportsWriter.Entry],
                                        optFlag: String): Option[(Path, Path)] = {
    if (typedExports.isEmpty) return None

    val typedWitDir = LlvmWasmTypedExportsWriter.typedWitDirPath(outputPath, artifactName)
    val adapterWorkDir = wasmDir.resolve("typed-exports").resolve("adapter")
    val adapterWitDir = adapterWorkDir.resolve("wit")
    val adapterGenDir = adapterWorkDir.resolve("gen")
    Files.createDirectories(typedWitDir.resolve("deps"))
    Files.createDirectories(adapterWitDir.resolve("deps"))
    Files.createDirectories(adapterGenDir)

    val runtimeDep = witBindingsDir.resolve("deps").resolve("runtime.wit")
    val sysDep = witBindingsDir.resolve("deps").resolve("sys.wit")
    if (!Files.exists(runtimeDep)) {
      throw InternalCompilerException(s"Missing runtime WIT dependency: '$runtimeDep'.", SourceLocation.Unknown)
    }
    if (!Files.exists(sysDep)) {
      throw InternalCompilerException(s"Missing sys WIT dependency: '$sysDep'.", SourceLocation.Unknown)
    }

    Files.list(witBindingsDir.resolve("deps")).iterator().asScala.foreach { dep =>
      if (Files.isRegularFile(dep)) {
        Files.copy(dep, typedWitDir.resolve("deps").resolve(dep.getFileName.toString), StandardCopyOption.REPLACE_EXISTING)
      }
    }
    Files.copy(runtimeDep, adapterWitDir.resolve("deps").resolve("runtime.wit"), StandardCopyOption.REPLACE_EXISTING)
    Files.writeString(typedWitDir.resolve("bindings.wit"), LlvmWasmTypedExportsWriter.renderPublicBindings(typedExports, wasmImports.map(_.interfaceId.qualifiedInterface)), StandardCharsets.UTF_8)
    Files.writeString(adapterWitDir.resolve("bindings.wit"), LlvmWasmTypedExportsWriter.renderAdapterBindings(typedExports), StandardCharsets.UTF_8)

    val witBindgen = resolveWitBindgenCli()
    val witCmd = List(
      witBindgen.toString,
      "c",
      adapterWitDir.toString,
      "--world",
      "adapter",
      "--out-dir",
      adapterGenDir.toString
    )
    val (witExit, witOutput) = exec(witCmd, wasmDir)
    if (witExit != 0) {
      throw InternalCompilerException(
        s"wit-bindgen failed while generating typed export adapter glue (exit $witExit):\n${witCmd.mkString(" ")}\n\n$witOutput",
        SourceLocation.Unknown
      )
    }

    val adapterImpl = adapterGenDir.resolve("typed_exports_impl.c")
    val adapterShim = adapterGenDir.resolve("typed_exports_shim.c")
    val adapterStdlibH = adapterGenDir.resolve("stdlib.h")
    val adapterStringH = adapterGenDir.resolve("string.h")
    Files.writeString(adapterImpl, LlvmWasmTypedExportsWriter.renderAdapterImpl(typedExports), StandardCharsets.UTF_8)
    Files.writeString(adapterShim, LlvmWasmTypedExportsWriter.renderAdapterShimC(), StandardCharsets.UTF_8)
    Files.writeString(adapterStdlibH, LlvmWasmTypedExportsWriter.renderFreestandingStdlibHeader(), StandardCharsets.UTF_8)
    Files.writeString(adapterStringH, LlvmWasmTypedExportsWriter.renderFreestandingStringHeader(), StandardCharsets.UTF_8)

    val adapterCore = adapterGenDir.resolve("typed_exports_adapter.core.wasm")
    val adapterComponent = adapterGenDir.resolve("typed_exports_adapter.component.wasm")
    val componentTypeObj = adapterGenDir.resolve("adapter_component_type.o")

    linkTypedExportAdapter(adapterCore, wasmDir, optFlag, adapterGenDir.resolve("adapter.c"), adapterImpl, adapterShim, componentTypeObj, adapterGenDir)
    componentize(adapterCore, adapterComponent, wasmDir)

    val composeWac = adapterWorkDir.resolve("compose.wac")
    Files.writeString(composeWac, LlvmWasmTypedExportsWriter.renderCompositionWac(), StandardCharsets.UTF_8)

    val typedComponent = LlvmWasmTypedExportsWriter.typedComponentPath(outputPath, artifactName)
    composeTypedExportComponent(baseComponentWasm, adapterComponent, typedComponent, composeWac, wasmDir)

    Some((typedComponent, typedWitDir))
  }

  private def resolveWitGlue(outDir: Path): Path = {
    val witGlueC = DefaultWitGlueDir.resolve("flix.c")
    val witGlueH = DefaultWitGlueDir.resolve("flix.h")
    if (Files.exists(witGlueC) && Files.exists(witGlueH)) {
      return witGlueC
    }

    val witDir = outDir.resolve("wit")
    Files.createDirectories(witDir)
    copyBundledResource(BundledWitGlueCResource, witDir.resolve("flix.c"))
    copyBundledResource(BundledWitGlueHResource, witDir.resolve("flix.h"))
    witDir.resolve("flix.c")
  }

  private def copyBundledResource(resource: String, dest: Path): Unit = {
    val is = Option(getClass.getResourceAsStream(resource)).getOrElse {
      throw InternalCompilerException(
        s"Missing LLVM-wasm support resource '$resource'.",
        SourceLocation.Unknown
      )
    }

    try {
      Files.copy(is, dest, StandardCopyOption.REPLACE_EXISTING)
    } finally {
      is.close()
    }
  }

  private def writeFile(path: Path, bytes: Array[Byte]): Unit = {
    Option(path.getParent).foreach(parent => Files.createDirectories(parent))
    Files.write(path, bytes)
  }

  private def renderJsString(s: String): String =
    "\"" + escapeJsString(s) + "\""

  private def escapeJsString(s: String): String =
    s.flatMap {
      case '\\' => "\\\\"
      case '"' => "\\\""
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case c => c.toString
    }

  private def exec(cmd: List[String], cwd: Path): (Int, String) = {
    val pb = new ProcessBuilder(cmd.asJava)
    pb.redirectErrorStream(true)
    pb.directory(cwd.toFile)

    val p = pb.start()
    val output = new String(p.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
    val exit = p.waitFor()
    (exit, output)
  }
}
