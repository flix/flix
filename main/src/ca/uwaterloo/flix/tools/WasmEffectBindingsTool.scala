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

package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.language.phase.ExportAbi
import ca.uwaterloo.flix.util.{FileOps, InternalCompilerException, Result}
import org.json4s.DefaultFormats
import org.json4s.jvalue2extractable
import org.json4s.native.JsonMethods.parse

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import scala.collection.mutable

  /**
  * Generates Flix effects and host binding metadata from a WIT world for the async wasm effect path.
  *
  * The generated output is a structured SDK bundle with:
  *   - generated Flix effect source,
  *   - a manifest for unknown-effect dispatch,
  *   - typed JS/TS host bindings,
  *   - a self-contained Rust/Wasmtime host crate,
  *   - a copy of the source WIT world,
  *   - and a generated README describing the integration path.
  *
  * This v0 slice is intentionally still narrow:
  *   - imported interfaces and world-level freestanding imports,
  *   - portable recursive ABI types including nested resource handles,
  *   - no WIT variants / enums / flags.
  */
object WasmEffectBindingsTool {

  val OutputSchemaVersion: String = "v2"
  val RustSerdeJsonVersion: String = "1.0.146"

  case class Config(witDir: Path,
                    world: String,
                    outDir: Path,
                    rootModule: String = "Wit")

  case class Generated(flixFile: Path,
                       bindingsFile: Path,
                       jsFile: Path,
                       dtsFile: Path,
                       jsBrowserHostStubFile: Path,
                       jsPackageFile: Path,
                       rustFile: Path,
                       rustLibFile: Path,
                       rustHostStubFile: Path,
                       rustCargoTomlFile: Path,
                       rustWitDir: Path,
                       readmeFile: Path)

  private val BundledCargoTomlResource = "/tools/wit-flix-gen-rs/Cargo.toml"
  private val BundledMainRsResource = "/tools/wit-flix-gen-rs/src/main.rs"
  private val BundledEffectHandlersJsResource = "/tools/wasm-runner-js/effect-handlers.mjs"
  private val BundledWitEffectRuntimeJsResource = "/tools/wasm-runner-js/wit-effect-bindings.mjs"
  private val BundledRustEffectsRsResource = "/tools/wasm-runner-rs/src/effects.rs"
  private val BundledRustRunnerRsResource = "/tools/wasm-runner-rs/src/runner.rs"
  private val BundledRuntimeBindingsWitResources = List(
    "/runtime/wit/flix-bindings/bindings.wit" -> "bindings.wit",
    "/runtime/wit/flix-bindings/deps/runtime.wit" -> "deps/runtime.wit",
    "/runtime/wit/flix-bindings/deps/sys.wit" -> "deps/sys.wit",
  )

  private val DefaultCargoToml: Path =
    Paths.get("tools/wit-flix-gen-rs/Cargo.toml").toAbsolutePath.normalize()
  private val DefaultEffectHandlersJs: Path =
    Paths.get("tools/wasm-runner-js/effect-handlers.mjs").toAbsolutePath.normalize()
  private val DefaultWitEffectRuntimeJs: Path =
    Paths.get("tools/wasm-runner-js/wit-effect-bindings.mjs").toAbsolutePath.normalize()
  private val DefaultRustEffectsRs: Path =
    Paths.get("tools/wasm-runner-rs/src/effects.rs").toAbsolutePath.normalize()
  private val DefaultRustRunnerRs: Path =
    Paths.get("tools/wasm-runner-rs/src/runner.rs").toAbsolutePath.normalize()
  private val DefaultRuntimeBindingsWitDir: Path =
    Paths.get("runtime/wit/flix-bindings").toAbsolutePath.normalize()

  private implicit val formats: DefaultFormats.type = DefaultFormats

  def run(config: Config): Result[Generated, String] = {
    validateConfig(config).flatMap { _ =>
      runHelper(config).flatMap { ir =>
        lower(config, ir).map { lowered =>
          FileOps.writeString(lowered.flixFile, lowered.flixSource)
          FileOps.writeString(lowered.bindingsFile, renderBindings(lowered.entries))
          copySupportModule(resolveEffectHandlersJs(config.outDir), lowered.effectHandlersJs)
          copySupportModule(resolveWitEffectRuntimeJs(config.outDir), lowered.witEffectRuntimeJs)
          FileOps.writeString(lowered.jsFile, renderGeneratedJs(lowered.entries))
          FileOps.writeString(lowered.dtsFile, renderGeneratedDts(lowered.entries))
          FileOps.writeString(lowered.jsBrowserHostStubFile, renderGeneratedJsBrowserHostStub(lowered.entries))
          FileOps.writeString(lowered.jsPackageFile, renderGeneratedJsPackageJson())
          FileOps.writeString(lowered.rustFile, renderGeneratedRust(lowered.entries))
          FileOps.writeString(lowered.rustLibFile, renderGeneratedRustLib())
          FileOps.writeString(lowered.rustHostStubFile, renderGeneratedRustHostStub(lowered.entries))
          FileOps.writeString(lowered.rustCargoTomlFile, renderGeneratedRustCargoToml())
          copySupportModule(resolveRustEffectsRs(config.outDir), lowered.rustEffectsFile)
          copySupportModule(resolveRustRunnerRs(config.outDir), lowered.rustRunnerFile)
          copyRuntimeBindingsWit(lowered.rustWitDir)
          copyDirectoryContents(config.witDir, lowered.copiedWitDir)
          FileOps.writeString(lowered.readmeFile, renderGeneratedReadme(config, lowered))
          Generated(
            lowered.flixFile,
            lowered.bindingsFile,
            lowered.jsFile,
            lowered.dtsFile,
            lowered.jsBrowserHostStubFile,
            lowered.jsPackageFile,
            lowered.rustFile,
            lowered.rustLibFile,
            lowered.rustHostStubFile,
            lowered.rustCargoTomlFile,
            lowered.rustWitDir,
            lowered.readmeFile,
          )
        }
      }
    }
  }

  private case class LoweredEntry(interfaceId: String,
                                  interfaceKey: String,
                                  containerKind: String,
                                  effectPath: String,
                                  effectName: String,
                                  rawOpName: String,
                                  publicName: String,
                                  memberKind: String,
                                  resourceName: Option[String],
                                  opSymbol: String,
                                  rawParams: List[(String, ExportAbi.AbiType)],
                                  rawResult: ExportAbi.AbiType,
                                  publicParams: List[(String, JsonAbiType)],
                                  publicResult: JsonAbiType)

  private case class InterfaceGroup(interfaceId: String,
                                    interfaceKey: String,
                                    containerKind: String,
                                    effectPath: String,
                                    effectName: String,
                                    jsKey: String,
                                    rustField: String,
                                    resources: List[String],
                                    entries: List[LoweredEntry])

  private case class RustRecordDef(name: String,
                                   effectName: String,
                                   witName: Option[String],
                                   fields: List[(String, String, JsonAbiType)])

  private case class LoweredFiles(flixFile: Path,
                                  flixSource: String,
                                  effectHandlersJs: Path,
                                  witEffectRuntimeJs: Path,
                                  jsFile: Path,
                                  dtsFile: Path,
                                  jsBrowserHostStubFile: Path,
                                  jsPackageFile: Path,
                                  rustFile: Path,
                                  rustLibFile: Path,
                                  rustHostStubFile: Path,
                                  rustCargoTomlFile: Path,
                                  rustEffectsFile: Path,
                                  rustRunnerFile: Path,
                                  rustWitDir: Path,
                                  bindingsFile: Path,
                                  copiedWitDir: Path,
                                  readmeFile: Path,
                                  entries: List[LoweredEntry])

  private case class WorldIr(schema: String, world: String, imports: List[ImportInterface])

  private case class ImportInterface(kind: String,
                                     interface: String,
                                     namespace: String,
                                     package_name: String,
                                     interface_name: String,
                                     version: String,
                                     resources: List[ImportResource] = Nil,
                                     functions: List[ImportFunction])

  private case class ImportResource(name: String)

  private case class ImportFunction(name: String,
                                    kind: String = "freestanding",
                                    resource: Option[String] = None,
                                    params: List[ImportParam],
                                    result: JsonAbiType)

  private case class ImportParam(name: String, tpe: JsonAbiType)

  private case class JsonAbiType(kind: String,
                                 element: Option[JsonAbiType] = None,
                                 elements: List[JsonAbiType] = Nil,
                                 ok: Option[JsonAbiType] = None,
                                 err: Option[JsonAbiType] = None,
                                 fields: List[JsonRecordField] = Nil,
                                 name: Option[String] = None,
                                 resource: Option[String] = None,
                                 ownership: Option[String] = None)

  private case class JsonRecordField(label: String, tpe: JsonAbiType)

  private def validateConfig(config: Config): Result[Unit, String] = {
    if (!Files.isDirectory(config.witDir)) {
      return Result.Err(s"WIT directory does not exist: ${config.witDir}")
    }
    if (!config.rootModule.matches("[A-Z][A-Za-z0-9_]*")) {
      return Result.Err(s"Invalid root module '${config.rootModule}'. Expected an uppercase Flix identifier.")
    }
    Result.Ok(())
  }

  private def runHelper(config: Config): Result[WorldIr, String] = {
    val manifest = resolveHelperManifest(config.outDir).toAbsolutePath.normalize()
    val cmd = List(
      "cargo", "+stable", "run", "--quiet",
      "--manifest-path", manifest.toString,
      "--",
      "--wit", config.witDir.toAbsolutePath.normalize().toString,
      "--world", config.world
    )
    val (exit, output) = exec(cmd, manifest.getParent)
    if (exit != 0) {
      Result.Err(
        s"""Failed to lower WIT world '${config.world}'.
           |
           |Command:
           |  ${cmd.mkString(" ")}
           |
           |Output:
           |$output
           |""".stripMargin
      )
    } else {
      try {
        Result.Ok(parse(output).extract[WorldIr])
      } catch {
        case t: Throwable =>
          Result.Err(s"Failed to parse generated WIT world IR.\n\n$output\n\n${t.getMessage}")
      }
    }
  }

  private def resolveHelperManifest(outDir: Path): Path = {
    if (Files.exists(DefaultCargoToml)) {
      return DefaultCargoToml.toAbsolutePath.normalize()
    }

    val toolDir = outDir.resolve(".flix-tools").resolve("wit-flix-gen-rs").toAbsolutePath.normalize()
    Files.createDirectories(toolDir.resolve("src"))
    copyBundledResource(BundledCargoTomlResource, toolDir.resolve("Cargo.toml"))
    copyBundledResource(BundledMainRsResource, toolDir.resolve("src").resolve("main.rs"))
    toolDir.resolve("Cargo.toml").toAbsolutePath.normalize()
  }

  private def resolveEffectHandlersJs(outDir: Path): Path = {
    if (Files.exists(DefaultEffectHandlersJs)) {
      return DefaultEffectHandlersJs
    }

    val path = outDir.resolve(".flix-tools").resolve("effect-handlers.mjs")
    copyBundledResource(BundledEffectHandlersJsResource, path)
    path
  }

  private def resolveWitEffectRuntimeJs(outDir: Path): Path = {
    if (Files.exists(DefaultWitEffectRuntimeJs)) {
      return DefaultWitEffectRuntimeJs
    }

    val path = outDir.resolve(".flix-tools").resolve("wit-effect-runtime.mjs")
    copyBundledResource(BundledWitEffectRuntimeJsResource, path)
    path
  }

  private def resolveRustEffectsRs(outDir: Path): Path = {
    if (Files.exists(DefaultRustEffectsRs)) {
      return DefaultRustEffectsRs
    }

    val path = outDir.resolve(".flix-tools").resolve("rust-effects.rs")
    copyBundledResource(BundledRustEffectsRsResource, path)
    path
  }

  private def resolveRustRunnerRs(outDir: Path): Path = {
    if (Files.exists(DefaultRustRunnerRs)) {
      return DefaultRustRunnerRs
    }

    val path = outDir.resolve(".flix-tools").resolve("rust-runner.rs")
    copyBundledResource(BundledRustRunnerRsResource, path)
    path
  }

  private type EffectDecl = (String, String, String, List[(String, ExportAbi.AbiType)], ExportAbi.AbiType)

  private def lower(config: Config, ir: WorldIr): Result[LoweredFiles, String] = {
    if (ir.schema != "flix-wit-world-v0") {
      return Result.Err(s"Unsupported WIT IR schema: ${ir.schema}")
    }

    val entries = mutable.ListBuffer.empty[LoweredEntry]
    val grouped = mutable.LinkedHashMap.empty[String, mutable.ListBuffer[EffectDecl]]
    val seenEffects = mutable.Set.empty[String]

    val sortedImports = ir.imports.sortBy(_.interface)
    for (iface <- sortedImports) {
      if (iface.kind != "interface" && iface.kind != "world") {
        return Result.Err(s"Unsupported WIT import kind '${iface.kind}' for ${iface.interface}.")
      }
      val modulePath = config.rootModule
      val effectName =
        if (iface.kind == "world") {
          s"${toUpperIdent(iface.namespace)}${toUpperIdent(iface.package_name)}${toUpperIdent(iface.interface_name)}World"
        } else {
          s"${toUpperIdent(iface.namespace)}${toUpperIdent(iface.package_name)}${toUpperIdent(iface.interface_name)}"
        }
      val effectPath = s"${config.rootModule}.$effectName"
      if (!seenEffects.add(effectPath)) {
        return Result.Err(s"Duplicate generated effect path '$effectPath'. Use a different root module or simplify conflicting WIT interfaces.")
      }

      val sigs = grouped.getOrElseUpdate(modulePath, mutable.ListBuffer.empty)

      for (fn <- iface.functions.sortBy(_.name)) {
        val publicName = toLowerIdent(fn.name)
        val rawExplicitParams = Result.traverse(fn.params) { p =>
          toRawAbiType(p.tpe, s"${iface.interface}#${fn.name} parameter '${p.name}'")
            .map(t => toLowerIdent(p.name) -> t)
        }
        val publicExplicitParams = Result.traverse(fn.params) { p =>
          toPublicAbiType(p.tpe, s"${iface.interface}#${fn.name} parameter '${p.name}'")
            .map(t => toLowerIdent(p.name) -> t)
        }
        val rawResult = toRawAbiType(fn.result, s"${iface.interface}#${fn.name} result")
        val publicResult = toPublicAbiType(fn.result, s"${iface.interface}#${fn.name} result")
        (rawExplicitParams, publicExplicitParams, rawResult, publicResult) match {
          case (Result.Ok(rawPs0), Result.Ok(publicPs0), Result.Ok(rawR), Result.Ok(publicR)) =>
            val publicPs = fn.kind match {
              case "method" =>
                (resourceParamName(fn.resource) -> JsonAbiType(kind = "resource", resource = Some(fn.resource.getOrElse("resource")), ownership = Some("borrow"))) :: publicPs0
              case _ => publicPs0
            }
            val rawPs = fn.kind match {
              case "method" =>
                (resourceParamName(fn.resource) -> ExportAbi.AbiType.Int64) :: rawPs0
              case _ => rawPs0
            }
            val needsWrapper = shouldEmitPublicWrapper(fn.kind, publicPs, publicR)
            val rawOpName = if (needsWrapper) s"${publicName}Raw" else publicName
            sigs += ((effectName, iface.interface, rawOpName, rawPs, rawR))
            entries += LoweredEntry(
              interfaceId = iface.interface,
              interfaceKey = interfaceKeyOf(iface),
              containerKind = iface.kind,
              effectPath = effectPath,
              effectName = effectName,
              rawOpName = rawOpName,
              publicName = publicName,
              memberKind = fn.kind,
              resourceName = fn.resource,
              opSymbol = s"$effectPath.$rawOpName",
              rawParams = rawPs,
              rawResult = rawR,
              publicParams = publicPs,
              publicResult = publicR
            )
          case (Result.Err(e), _, _, _) => return Result.Err(e)
          case (_, Result.Err(e), _, _) => return Result.Err(e)
          case (_, _, Result.Err(e), _) => return Result.Err(e)
          case (_, _, _, Result.Err(e)) => return Result.Err(e)
        }
      }

      for (resource <- iface.resources.sortBy(_.name)) {
        val publicName = s"${toLowerIdent(resource.name)}Drop"
        val rawOpName = s"${publicName}Raw"
        val rawPs = List(resourceParamName(Some(resource.name)) -> ExportAbi.AbiType.Int64)
        val publicPs = List(resourceParamName(Some(resource.name)) -> JsonAbiType(kind = "resource", resource = Some(resource.name), ownership = Some("own")))
        val rawR = ExportAbi.AbiType.Unit
        val publicR = JsonAbiType("unit")
        sigs += ((effectName, iface.interface, rawOpName, rawPs, rawR))
        entries += LoweredEntry(
          interfaceId = iface.interface,
          interfaceKey = interfaceKeyOf(iface),
          containerKind = iface.kind,
          effectPath = effectPath,
          effectName = effectName,
          rawOpName = rawOpName,
          publicName = publicName,
          memberKind = "drop",
          resourceName = Some(resource.name),
          opSymbol = s"$effectPath.$rawOpName",
          rawParams = rawPs,
          rawResult = rawR,
          publicParams = publicPs,
          publicResult = publicR
        )
      }
    }

    val flixDir = config.outDir.resolve("flix").toAbsolutePath.normalize()
    val manifestDir = config.outDir.resolve("manifest").toAbsolutePath.normalize()
    val jsDir = config.outDir.resolve("js").toAbsolutePath.normalize()
    val jsInternalDir = jsDir.resolve("internal").toAbsolutePath.normalize()
    val rustDir = config.outDir.resolve("rust").toAbsolutePath.normalize()
    val rustSrcDir = rustDir.resolve("src").toAbsolutePath.normalize()
    val rustWitDir = rustDir.resolve("wit").resolve("flix-bindings").toAbsolutePath.normalize()
    val copiedWitDir = config.outDir.resolve("wit").toAbsolutePath.normalize()
    val flixFile = flixDir.resolve(s"${config.rootModule}.flix").toAbsolutePath.normalize()
    val effectHandlersJs = jsInternalDir.resolve("effect-handlers.mjs").toAbsolutePath.normalize()
    val witEffectRuntimeJs = jsInternalDir.resolve("wit-effect-runtime.mjs").toAbsolutePath.normalize()
    val jsFile = jsDir.resolve("index.mjs").toAbsolutePath.normalize()
    val dtsFile = jsDir.resolve("index.d.ts").toAbsolutePath.normalize()
    val jsBrowserHostStubFile = jsDir.resolve("browser-host.stub.mjs").toAbsolutePath.normalize()
    val jsPackageFile = jsDir.resolve("package.json").toAbsolutePath.normalize()
    val rustFile = rustSrcDir.resolve("wit_effect_bindings.rs").toAbsolutePath.normalize()
    val rustLibFile = rustSrcDir.resolve("lib.rs").toAbsolutePath.normalize()
    val rustHostStubFile = rustDir.resolve("examples").resolve("host_stub.rs").toAbsolutePath.normalize()
    val rustCargoTomlFile = rustDir.resolve("Cargo.toml").toAbsolutePath.normalize()
    val rustEffectsFile = rustSrcDir.resolve("effects.rs").toAbsolutePath.normalize()
    val rustRunnerFile = rustSrcDir.resolve("runner.rs").toAbsolutePath.normalize()
    val bindingsFile = manifestDir.resolve("wit-effect-bindings.json").toAbsolutePath.normalize()
    val readmeFile = config.outDir.resolve("README.md").toAbsolutePath.normalize()
    val flixSource = renderFlix(config, entries.toList)
    Result.Ok(LoweredFiles(
      flixFile = flixFile,
      flixSource = flixSource,
      effectHandlersJs = effectHandlersJs,
      witEffectRuntimeJs = witEffectRuntimeJs,
      jsFile = jsFile,
      dtsFile = dtsFile,
      jsBrowserHostStubFile = jsBrowserHostStubFile,
      jsPackageFile = jsPackageFile,
      rustFile = rustFile,
      rustLibFile = rustLibFile,
      rustHostStubFile = rustHostStubFile,
      rustCargoTomlFile = rustCargoTomlFile,
      rustEffectsFile = rustEffectsFile,
      rustRunnerFile = rustRunnerFile,
      rustWitDir = rustWitDir,
      bindingsFile = bindingsFile,
      copiedWitDir = copiedWitDir,
      readmeFile = readmeFile,
      entries = entries.toList.sortBy(e => (e.effectPath, e.publicName))
    ))
  }

  private def renderFlix(config: Config, entries: List[LoweredEntry]): String = {
    flixFreshCounter = 0
    val sb = new StringBuilder(8 * 1024)
    sb.append("///\n")
    sb.append(s"/// Generated by `flix bind wasm-effects --wit ${config.witDir.toAbsolutePath.normalize()} --world ${config.world}`.\n")
    sb.append("///\n")
    sb.append("/// This v0 binding surface supports portable recursive async ABI types.\n")
    sb.append("///\n")

    sb.append(s"pub mod ${config.rootModule} {\n\n")

    groupInterfaces(entries).sortBy(_.effectName).foreach { group =>
      group.resources.sorted.foreach { resource =>
        sb.append(s"""    /// Generated from WIT resource `${group.interfaceId}#${resource}`.\n""")
        sb.append(s"    pub enum ${flixResourceTypeName(group.effectName, resource)}(Int64)\n\n")
      }

      val rawEntries = group.entries.sortBy(_.rawOpName)
      sb.append(s"""    /// Generated from ${if (group.containerKind == "world") "WIT world" else "WIT interface"} `${group.interfaceId}`.\n""")
      sb.append(s"    pub eff ${group.effectName} {\n")
      rawEntries.foreach { entry =>
        val paramsText = entry.rawParams.map { case (name, tpe) => s"$name: ${flixTypeOf(tpe)}" }.mkString(", ")
        sb.append(s"        def ${entry.rawOpName}($paramsText): ${flixTypeOf(entry.rawResult)}\n")
      }
      sb.append("    }\n\n")

      group.entries.filter(shouldEmitPublicWrapper).sortBy(_.publicName).foreach { entry =>
        val paramsText = entry.publicParams.map { case (name, tpe) => s"$name: ${flixPublicTypeOf(group.effectName, tpe)}" }.mkString(", ")
        sb.append(s"    pub def ${entry.publicName}($paramsText): ${flixPublicTypeOf(group.effectName, entry.publicResult)} \\ ${group.effectName} = {\n")
        val rawArgs = entry.publicParams.map { case (name, tpe) => renderFlixLowerExpr(group.effectName, name, tpe) }.mkString(", ")
        sb.append(s"        let rawResult = ${group.effectName}.${entry.rawOpName}($rawArgs);\n")
        sb.append(s"        ${renderFlixLiftExpr(group.effectName, "rawResult", entry.publicResult)}\n")
        sb.append("    }\n\n")
      }
    }

    sb.append("}\n")

    sb.toString()
  }

  private def renderBindings(entries: List[LoweredEntry]): String = {
    val sb = new StringBuilder(4 * 1024)
    sb.append("{\n")
    sb.append("""  "schema": "flix-wasm-effect-bindings-v0",""").append('\n')
    sb.append(s"""  "count": ${entries.length},""").append('\n')
    sb.append("""  "ops": [""").append('\n')
    entries.zipWithIndex.foreach { case (entry, idx) =>
      sb.append("    {\n")
      sb.append(s"""      "interface": "${escapeJson(entry.interfaceId)}",""").append('\n')
      sb.append(s"""      "interfaceKey": "${escapeJson(entry.interfaceKey)}",""").append('\n')
      sb.append(s"""      "containerKind": "${escapeJson(entry.containerKind)}",""").append('\n')
      sb.append(s"""      "effect": "${escapeJson(entry.effectPath)}",""").append('\n')
      sb.append(s"""      "op": "${escapeJson(entry.publicName)}",""").append('\n')
      sb.append(s"""      "opSymbol": "${escapeJson(entry.opSymbol)}",""").append('\n')
      sb.append(s"""      "func": "${escapeJson(entry.publicName)}",""").append('\n')
      sb.append(s"""      "memberKind": "${escapeJson(entry.memberKind)}",""").append('\n')
      entry.resourceName.foreach { resource =>
        sb.append(s"""      "resource": "${escapeJson(resource)}",""").append('\n')
      }
      sb.append("""      "rawParams": [""").append(entry.rawParams.map { case (_, tpe) => renderAbiType(tpe) }.mkString(", ")).append("],\n")
      sb.append(s"""      "rawResult": ${renderAbiType(entry.rawResult)},""").append('\n')
      sb.append("""      "params": [""").append(entry.publicParams.map { case (_, tpe) => renderPublicAbiType(tpe) }.mkString(", ")).append("],\n")
      sb.append(s"""      "result": ${renderPublicAbiType(entry.publicResult)}""").append('\n')
      sb.append("    }")
      if (idx != entries.length - 1) sb.append(',')
      sb.append('\n')
    }
    sb.append("  ]\n")
    sb.append("}\n")
    sb.toString()
  }

  private def renderGeneratedJs(entries: List[LoweredEntry]): String = {
    val interfaces = groupInterfaces(entries)
    val sb = new StringBuilder(8 * 1024)
    sb.append("""import { makeWitEffectHandlers, makeWitEffectUnknownHandler } from "./internal/wit-effect-runtime.mjs";""").append('\n')
    sb.append('\n')
    sb.append("export const bindings = ").append(renderBindings(entries).trim).append(";\n")
    sb.append('\n')
    sb.append("export const interfaces = {\n")
    interfaces.foreach { iface =>
      sb.append(s"""  ${iface.jsKey}: "${escapeJson(iface.interfaceId)}",""").append('\n')
    }
    sb.append("};\n\n")
    sb.append(
      """function normalizeImplementations(implementations) {
        |  if (!implementations || typeof implementations !== "object") {
        |    return implementations;
        |  }
        |
        |  const out = Object.create(null);
        |  for (const [key, value] of Object.entries(implementations)) {
        |    out[key] = value;
        |  }
        |""".stripMargin
    )
    interfaces.foreach { iface =>
      sb.append(s"""  if (implementations.${iface.jsKey} != null) out["${escapeJson(iface.interfaceId)}"] = implementations.${iface.jsKey};""").append('\n')
      sb.append(s"""  if (implementations.${iface.jsKey} != null) out["${escapeJson(iface.interfaceKey)}"] = implementations.${iface.jsKey};""").append('\n')
    }
    sb.append(
      """  return out;
        |}
        |
        |export function makeHandlers(implementations) {
        |  return makeWitEffectHandlers(bindings, normalizeImplementations(implementations));
        |}
        |
        |export async function makeUnknownHandler(effectManifestSource, implementations) {
        |  return makeWitEffectUnknownHandler(effectManifestSource, bindings, normalizeImplementations(implementations));
        |}
        |""".stripMargin
    )
    sb.toString()
  }

  private def renderGeneratedDts(entries: List[LoweredEntry]): String = {
    val interfaces = groupInterfaces(entries)
    val sb = new StringBuilder(8 * 1024)
    sb.append("export type Awaitable<T> = T | Promise<T>;\n\n")
    interfaces.foreach { iface =>
      sb.append(s"export interface ${iface.effectName} {\n")
      iface.entries.foreach { entry =>
        val params = entry.publicParams.map { case (name, tpe) => s"$name: ${renderTsPublicType(tpe)}" }.mkString(", ")
        sb.append(s"  ${entry.publicName}($params): Awaitable<${renderTsPublicType(entry.publicResult)}>;\n")
      }
      sb.append("}\n\n")
    }
    sb.append("export interface Implementations {\n")
    interfaces.foreach { iface =>
      sb.append(s"""  ${iface.jsKey}?: ${iface.effectName};""").append('\n')
      sb.append(s"""  "${escapeJson(iface.interfaceId)}"?: ${iface.effectName};""").append('\n')
      sb.append(s"""  "${escapeJson(iface.interfaceKey)}"?: ${iface.effectName};""").append('\n')
    }
    sb.append("}\n\n")
    sb.append(
      """export declare const bindings: {
        |  readonly schema: "flix-wasm-effect-bindings-v0";
        |  readonly count: number;
        |  readonly ops: readonly unknown[];
        |};
        |
        |export declare const interfaces: Record<string, string>;
        |
        |export declare function makeHandlers(implementations: Implementations): Record<string, (...args: any[]) => Awaitable<unknown>>;
        |export declare function makeUnknownHandler(effectManifestSource: string | URL | object, implementations: Implementations): Promise<(input: any) => Promise<void>>;
        |""".stripMargin
    )
    sb.toString()
  }

  private def renderGeneratedJsPackageJson(): String =
    """{
      |  "name": "flix-wasm-effect-bindings",
      |  "private": true,
      |  "type": "module",
      |  "exports": {
      |    ".": "./index.mjs"
      |  },
      |  "types": "./index.d.ts"
      |}
      |""".stripMargin

  private def renderGeneratedJsBrowserHostStub(entries: List[LoweredEntry]): String = {
    val interfaces = groupInterfaces(entries)
    val sb = new StringBuilder(8 * 1024)
    sb.append("""import { makeUnknownHandler } from "./index.mjs";""").append('\n')
    sb.append('\n')
    sb.append(
      """// Fill these handlers with browser-specific implementations.
        |// This file is intentionally only a typed skeleton; the host still owns
        |// component loading, runner wiring, and any browser worker / persistence setup.
        |
        |export const browserImplementations = {
        |""".stripMargin
    )
    interfaces.foreach { iface =>
      sb.append(s"  ${iface.jsKey}: {\n")
      iface.entries.foreach { entry =>
        val params = entry.publicParams.indices.map(i => s"arg$i").mkString(", ")
        sb.append(s"    ${entry.publicName}: async ($params) => {\n")
        sb.append(s"""      throw new Error("TODO: implement ${escapeJsString(iface.interfaceId)}#${escapeJsString(entry.publicName)} for the browser host");\n""")
        sb.append("    },\n")
      }
      sb.append("  },\n")
    }
    sb.append("};\n\n")
    sb.append(
      """export async function makeBrowserUnknownHandler(effectManifestSource, implementations = browserImplementations) {
        |  return makeUnknownHandler(effectManifestSource, implementations);
        |}
        |""".stripMargin
    )
    sb.toString()
  }

  private def renderGeneratedRustLib(): String =
    """pub mod bindings {
      |    wasmtime::component::bindgen!({
      |        path: "wit/flix-bindings",
      |        world: "flix",
      |    });
      |}
      |
      |pub mod effects;
      |pub mod runner;
      |pub mod wit_effect_bindings;
      |
      |pub use wit_effect_bindings::*;
      |""".stripMargin

  private def renderGeneratedRustCargoToml(): String =
    """[package]
      |name = "flix-wit-effect-bindings"
      |version = "0.1.0"
      |edition = "2021"
      |publish = false
      |
      |[dependencies]
      |anyhow = "1"
      |serde = { version = "1", features = ["derive"] }
      |serde_json = "=1.0.146"
      |wasmtime = { version = "38", features = ["component-model"] }
      |""".stripMargin

  private def renderGeneratedRustHostStub(entries: List[LoweredEntry]): String = {
    val interfaces = groupInterfaces(entries)
    val records = collectRustRecords(entries)
    val imports = (interfaces.map(_.effectName) :::
      interfaces.flatMap(iface => iface.resources.map(resource => flixResourceTypeName(iface.effectName, resource))) :::
      records.map(_.name) :::
      List("WitEffectHandler")).distinct.sorted

    val sb = new StringBuilder(12 * 1024)
    sb.append(
      """use anyhow::Result;
        |use flix_wit_effect_bindings::{
        |    effects::{AsyncResult, EffectManifest},
        |""".stripMargin
    )
    imports.foreach { name =>
      sb.append(s"    $name,\n")
    }
    sb.append("};\n\n")
    sb.append(
      """// Fill these host structs with Wasmtime-specific state and trait implementations.
        |// This file is intentionally only a typed skeleton; the host still owns
        |// component loading, linker setup, store state, and task driving.
        |
        |""".stripMargin
    )
    interfaces.foreach { iface =>
      val hostStruct = s"${iface.effectName}Host"
      sb.append(s"pub struct $hostStruct;\n\n")
      sb.append(s"impl ${iface.effectName} for $hostStruct {\n")
      iface.entries.foreach { entry =>
        val params = entry.publicParams.map { case (name, tpe) =>
          s"${toRustIdent(name)}: ${renderRustPublicType(tpe, iface.effectName, records)}"
        }.mkString(", ")
        sb.append(s"    fn ${toRustIdent(entry.publicName)}(&mut self")
        if (params.nonEmpty) sb.append(", ").append(params)
        sb.append(s") -> Result<AsyncResult<${renderRustPublicType(entry.publicResult, iface.effectName, records)}>> {\n")
        sb.append(s"""        todo!("TODO: implement ${escapeRustString(iface.interfaceId)}#${escapeRustString(entry.publicName)} for the Wasmtime host")\n""")
        sb.append("    }\n")
      }
      sb.append("}\n\n")
    }

    val hostTypeArgs = interfaces.map(iface => s"${iface.effectName}Host")
    val hostValueArgs = interfaces.map(iface => s"${iface.rustField}: ${iface.effectName}Host")
    sb.append("pub fn make_handler(\n")
    sb.append("    manifest: EffectManifest")
    if (hostValueArgs.nonEmpty) {
      sb.append(",\n")
      sb.append(hostValueArgs.map(arg => s"    $arg").mkString(",\n"))
    }
    sb.append("\n)")
    if (hostTypeArgs.isEmpty) sb.append(" -> WitEffectHandler")
    else sb.append(s" -> WitEffectHandler<${hostTypeArgs.mkString(", ")}>")
    sb.append(" {\n")
    sb.append("    WitEffectHandler::new(manifest")
    if (interfaces.nonEmpty) {
      sb.append(", ")
      sb.append(interfaces.map(_.rustField).mkString(", "))
    }
    sb.append(")\n")
    sb.append("}\n")
    sb.toString()
  }

  private def renderGeneratedReadme(config: Config, lowered: LoweredFiles): String =
    s"""# Flix Wasm Effect Bindings SDK
       |
       |Generated by:
       |
       |```bash
       |flix bind wasm-effects --wit ${config.witDir.toAbsolutePath.normalize()} --world ${config.world} --out ${config.outDir.toAbsolutePath.normalize()}
       |```
       |
       |This bundle contains:
       |
       |- `flix/${config.rootModule}.flix`: generated Flix effects and typed wrappers.
       |- `manifest/wit-effect-bindings.json`: binding manifest for unknown-effect dispatch.
       |- `js/`: typed JS/TS host SDK.
       |- `js/browser-host.stub.mjs`: browser host skeleton built on top of `js/index.mjs`.
       |- `rust/`: typed Rust/Wasmtime host SDK crate.
       |- `rust/examples/host_stub.rs`: Wasmtime host skeleton built on top of `rust/`.
       |- `wit/`: a copy of the source WIT world directory used to generate this bundle.
       |
       |Recommended usage:
       |
       |1. Add `flix/${config.rootModule}.flix` to the Flix project that will run on `llvm-wasm`.
       |2. Build that Flix project for wasm.
       |3. In the host, use:
       |   - `js/browser-host.stub.mjs` + `js/index.mjs` for browser/JS integration, or
       |   - `rust/examples/host_stub.rs` + `rust/` as a path dependency for Wasmtime/Rust integration.
       |4. Use `manifest/wit-effect-bindings.json` together with the wasm build's effect manifest when wiring unknown-effect handlers.
       |
       |The generated Rust crate is self-contained with the Flix runtime WIT and helper modules under `rust/`.
       |""".stripMargin

  private def renderGeneratedRust(entries: List[LoweredEntry]): String = {
    val interfaces = groupInterfaces(entries)
    val records = collectRustRecords(entries)
    val sb = new StringBuilder(32 * 1024)
    sb.append(
      """use anyhow::Result;
        |use crate::{
        |    bindings,
        |    effects::{AsyncResult, EffectManifest, EffectOp, ValueData, box_value, decode_args, resume_throw_string},
        |    runner::{OpHandler, PendingIo},
        |};
        |use wasmtime::Store;
        |
        |use bindings::exports::flix::runtime::runtime::{Ctx, Guest, OpRequest, Suspension};
        |
        |""".stripMargin
    )

    if (records.nonEmpty) {
      sb.append(
        """fn lookup_record_field<'a>(value: &'a ValueData, label: &str) -> Result<&'a ValueData> {
          |    match value {
          |        ValueData::Record(fields) => fields
          |            .iter()
          |            .find(|(field_label, _)| field_label == label)
          |            .map(|(_, field_value)| field_value)
          |            .ok_or_else(|| anyhow::anyhow!("missing field '{}'", label)),
          |        other => anyhow::bail!("expected record, got {:?}", other),
          |    }
          |}
          |
          |""".stripMargin
      )
    }

    interfaces.foreach { iface =>
      iface.resources.sorted.foreach { resource =>
        sb.append("#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]\n")
        sb.append(s"pub struct ${flixResourceTypeName(iface.effectName, resource)}(pub i64);\n\n")
      }
    }

    records.foreach { record =>
      sb.append("#[derive(Clone, Debug, PartialEq)]\n")
      sb.append(s"pub struct ${record.name} {\n")
      record.fields.foreach { case (_, rustField, fieldTpe) =>
        sb.append(s"    pub $rustField: ${renderRustPublicType(fieldTpe, record.effectName, records)},\n")
      }
      sb.append("}\n\n")
    }

    interfaces.foreach { iface =>
      sb.append(s"pub trait ${iface.effectName} {\n")
      iface.entries.foreach { entry =>
        val params = entry.publicParams.map { case (name, tpe) => s"${toRustIdent(name)}: ${renderRustPublicType(tpe, iface.effectName, records)}" }.mkString(", ")
        sb.append(s"    fn ${toRustIdent(entry.publicName)}(&mut self")
        if (params.nonEmpty) sb.append(", ").append(params)
        sb.append(s") -> Result<AsyncResult<${renderRustPublicType(entry.publicResult, iface.effectName, records)}>>;\n")
      }
      sb.append("}\n\n")
    }

    val rustGenerics = interfaces.map(iface => s"H${iface.effectName}")
    val genericDecl = if (rustGenerics.isEmpty) "" else rustGenerics.mkString("<", ", ", ">")
    sb.append(s"pub struct WitEffectHandler$genericDecl {\n")
    sb.append("    manifest: EffectManifest,\n")
    interfaces.foreach(iface => sb.append(s"    pub ${iface.rustField}: H${iface.effectName},\n"))
    sb.append("}\n\n")
    sb.append(s"impl$genericDecl WitEffectHandler$genericDecl {\n")
    sb.append(s"    pub fn new(manifest: EffectManifest")
    interfaces.foreach(iface => sb.append(s", ${iface.rustField}: H${iface.effectName}"))
    sb.append(") -> Self {\n")
    sb.append("        Self {\n")
    sb.append("            manifest,\n")
    interfaces.foreach(iface => sb.append(s"            ${iface.rustField},\n"))
    sb.append("        }\n")
    sb.append("    }\n")
    sb.append("}\n\n")
    val opHandlerGenericDecl = if (rustGenerics.isEmpty) "<T>" else ("<T, " + rustGenerics.mkString(", ") + ">")
    sb.append(s"impl$opHandlerGenericDecl OpHandler<T> for WitEffectHandler$genericDecl\n")
    if (interfaces.nonEmpty) {
      sb.append("where\n")
      interfaces.foreach { iface =>
        sb.append(s"    H${iface.effectName}: ${iface.effectName},\n")
      }
    }
    sb.append("{\n")
    sb.append(
      """    fn handle_op(
        |        &mut self,
        |        store: &mut Store<T>,
        |        rt: &Guest,
        |        ctx: Ctx,
        |        suspension: Suspension,
        |        req: OpRequest,
        |    ) -> Result<()> {
        |        match req {
        |            OpRequest::Unknown(r) => {
        |                let Some(op) = self.manifest.lookup(r.eff_id, r.op_id as u32).cloned() else {
        |                    resume_throw_string(store, rt, ctx, suspension, format!("unsupported effect op: {}:{}", r.eff_id, r.op_id))?;
        |                    return Ok(());
        |                };
        |
        |                match op.op_symbol.as_str() {
        |""".stripMargin
    )
    interfaces.foreach { iface =>
      iface.entries.foreach { entry =>
        sb.append(s"""                    "${entry.opSymbol}" => handle_${toRustIdent(iface.effectName)}_${toRustIdent(entry.publicName)}(store, rt, ctx, suspension, &op, &mut self.${iface.rustField}),""").append('\n')
      }
    }
    sb.append(
      """                    other => {
        |                        resume_throw_string(store, rt, ctx, suspension, format!("unexpected async effect op: {}", other))?;
        |                        Ok(())
        |                    }
        |                }
        |            }
        |            other => anyhow::bail!("unexpected op request kind: {:?}", other),
        |        }
        |    }
        |}
        |
        |""".stripMargin
    )

    interfaces.foreach { iface =>
      iface.entries.foreach { entry =>
        val rustEffect = toRustIdent(iface.effectName)
        val rustOp = toRustIdent(entry.publicName)
        val hostTrait = iface.effectName
        sb.append(s"fn handle_${rustEffect}_${rustOp}<T, H: $hostTrait>(\n")
        sb.append("    store: &mut Store<T>,\n")
        sb.append("    rt: &Guest,\n")
        sb.append("    ctx: Ctx,\n")
        sb.append("    suspension: Suspension,\n")
        sb.append("    op: &EffectOp,\n")
        sb.append("    host: &mut H,\n")
        sb.append(") -> Result<()> {\n")
        val rustArgsName = if (entry.publicParams.isEmpty) "_args" else "args"
        sb.append(s"    let $rustArgsName = decode_args(store, rt, ctx, suspension, op)?;\n")
        entry.publicParams.zipWithIndex.foreach { case ((name, tpe), idx) =>
          val decodeFnName = "decode_" + rustEffect + "_" + rustOp + "_param" + idx
          sb.append(s"    let ${toRustIdent(name)} = $decodeFnName(&$rustArgsName[$idx])?;\n")
        }
        sb.append(s"    match host.${rustOp}(")
        sb.append(entry.publicParams.map { case (name, _) => toRustIdent(name) }.mkString(", "))
        sb.append(")? {\n")
        sb.append("        AsyncResult::Ready(result) => {\n")
        sb.append(s"            let value = encode_${rustEffect}_${rustOp}_result(result);\n")
        sb.append("            let boxed = box_value(store, rt, ctx, &op.result, &value)?;\n")
        sb.append("            rt.call_resume_ok(&mut *store, ctx, suspension, boxed)?;\n")
        sb.append("            let _ = boxed.resource_drop(&mut *store);\n")
        sb.append("            Ok(())\n")
        sb.append("        }\n")
        sb.append("        AsyncResult::Pending => Err(PendingIo.into()),\n")
        sb.append("    }\n")
        sb.append("}\n\n")

        entry.publicParams.zipWithIndex.foreach { case ((_, tpe), idx) =>
          val fnName = "decode_" + rustEffect + "_" + rustOp + "_param" + idx
          sb.append(s"fn $fnName(value: &ValueData) -> Result<${renderRustPublicType(tpe, iface.effectName, records)}> {\n")
          sb.append(indent(renderRustDecodePublicBody("value", tpe, iface.effectName, records), 1)).append('\n')
          sb.append("}\n\n")
        }

        val resultParamName = if (entry.publicResult.kind == "unit") "_value" else "value"
        sb.append(s"fn encode_${rustEffect}_${rustOp}_result($resultParamName: ${renderRustPublicType(entry.publicResult, iface.effectName, records)}) -> ValueData {\n")
        sb.append(indent(renderRustEncodePublicBody(resultParamName, entry.publicResult, iface.effectName, records), 1)).append('\n')
        sb.append("}\n\n")
      }
    }

    sb.toString()
  }

  private def groupInterfaces(entries: List[LoweredEntry]): List[InterfaceGroup] = {
    val groups = mutable.LinkedHashMap.empty[String, mutable.ListBuffer[LoweredEntry]]
    entries.foreach { entry =>
      val buf = groups.getOrElseUpdate(entry.effectPath, mutable.ListBuffer.empty)
      buf += entry
    }
    groups.values.toList.map { xs =>
      val head = xs.head
      InterfaceGroup(
        interfaceId = head.interfaceId,
        interfaceKey = head.interfaceKey,
        containerKind = head.containerKind,
        effectPath = head.effectPath,
        effectName = head.effectName,
        jsKey = lowerFirst(head.effectName),
        rustField = toRustIdent(head.effectName),
        resources = xs.flatMap(_.resourceName).distinct.sorted.toList,
        entries = xs.toList.sortBy(_.publicName)
      )
    }
  }

  private def interfaceKeyOf(iface: ImportInterface): String =
    if (iface.kind == "world") s"${iface.namespace}.${iface.package_name}.${iface.interface_name}.world"
    else s"${iface.namespace}.${iface.package_name}.${iface.interface_name}"

  private def resourceParamName(resourceName: Option[String]): String =
    toLowerIdent(resourceName.getOrElse("resource"))

  private def toRawAbiType(tpe: JsonAbiType, context: String): Result[ExportAbi.AbiType, String] =
    tpe.kind match {
      case "unit" => Result.Ok(ExportAbi.AbiType.Unit)
      case "bool" => Result.Ok(ExportAbi.AbiType.Bool)
      case "int8" => Result.Ok(ExportAbi.AbiType.Int8)
      case "int16" => Result.Ok(ExportAbi.AbiType.Int16)
      case "int32" => Result.Ok(ExportAbi.AbiType.Int32)
      case "int64" => Result.Ok(ExportAbi.AbiType.Int64)
      case "float32" => Result.Ok(ExportAbi.AbiType.Float32)
      case "float64" => Result.Ok(ExportAbi.AbiType.Float64)
      case "string" => Result.Ok(ExportAbi.AbiType.String)
      case "bytes" => Result.Ok(ExportAbi.AbiType.Bytes)
      case "resource" => Result.Ok(ExportAbi.AbiType.Int64)
      case "list" =>
        Result.traverseOpt(tpe.element)(elm => toRawAbiType(elm, context)).flatMap {
          case Some(elm) => Result.Ok(ExportAbi.AbiType.List(elm))
          case None => Result.Err(s"Malformed WIT list type in $context.")
        }
      case "array" =>
        Result.traverseOpt(tpe.element)(elm => toRawAbiType(elm, context)).flatMap {
          case Some(elm) => Result.Ok(ExportAbi.AbiType.Array(elm))
          case None => Result.Err(s"Malformed WIT array type in $context.")
        }
      case "tuple" =>
        Result.traverse(tpe.elements)(elm => toRawAbiType(elm, context)).map(ts => ExportAbi.AbiType.Tuple(ts))
      case "option" =>
        Result.traverseOpt(tpe.element)(elm => toRawAbiType(elm, context)).flatMap {
          case Some(elm) => Result.Ok(ExportAbi.AbiType.Option(elm))
          case None => Result.Err(s"Malformed WIT option type in $context.")
        }
      case "result" =>
        Result.mapN(
          Result.traverseOpt(tpe.ok)(elm => toRawAbiType(elm, context)),
          Result.traverseOpt(tpe.err)(elm => toRawAbiType(elm, context))
        ) {
          case (ok, err) => ExportAbi.AbiType.Result(ok.getOrElse(ExportAbi.AbiType.Unit), err.getOrElse(ExportAbi.AbiType.Unit))
        }
      case "record" =>
        Result.traverse(tpe.fields.sortBy(_.label)) { field =>
          toRawAbiType(field.tpe, s"$context field '${field.label}'").map(t => toLowerIdent(field.label) -> t)
        }.map(fields => ExportAbi.AbiType.Record(fields))
      case other =>
        Result.Err(s"Unsupported WIT type '$other' in $context.")
    }

  private def toPublicAbiType(tpe: JsonAbiType, context: String): Result[JsonAbiType, String] =
    tpe.kind match {
      case "unit" | "bool" | "int8" | "int16" | "int32" | "int64" | "float32" | "float64" | "string" | "bytes" =>
        Result.Ok(tpe.copy())
      case "resource" =>
        val resourceName = tpe.resource.getOrElse {
          return Result.Err(s"Malformed WIT resource type in $context.")
        }
        val ownership = tpe.ownership.getOrElse {
          return Result.Err(s"Missing ownership on WIT resource type in $context.")
        }
        Result.Ok(JsonAbiType(kind = "resource", resource = Some(resourceName), ownership = Some(ownership)))
      case "list" | "array" | "option" =>
        Result.traverseOpt(tpe.element)(elm => toPublicAbiType(elm, context)).flatMap {
          case Some(elm) => Result.Ok(tpe.copy(element = Some(elm), elements = Nil, ok = None, err = None, fields = Nil))
          case None => Result.Err(s"Malformed WIT ${tpe.kind} type in $context.")
        }
      case "tuple" =>
        Result.traverse(tpe.elements)(elm => toPublicAbiType(elm, context)).map(elms => tpe.copy(elements = elms, element = None, ok = None, err = None, fields = Nil))
      case "result" =>
        Result.mapN(
          Result.traverseOpt(tpe.ok)(elm => toPublicAbiType(elm, context)),
          Result.traverseOpt(tpe.err)(elm => toPublicAbiType(elm, context))
        ) {
          case (ok, err) => tpe.copy(ok = ok, err = err, element = None, elements = Nil, fields = Nil)
        }
      case "record" =>
        Result.traverse(tpe.fields.sortBy(_.label)) { field =>
          toPublicAbiType(field.tpe, s"$context field '${field.label}'").map(t => field.copy(tpe = t))
        }.map(fields => tpe.copy(fields = fields, element = None, elements = Nil, ok = None, err = None))
      case other =>
        Result.Err(s"Unsupported WIT type '$other' in $context.")
    }

  private def containsResource(tpe: JsonAbiType): Boolean = tpe.kind match {
    case "resource" => true
    case "list" | "array" | "option" => tpe.element.exists(containsResource)
    case "tuple" => tpe.elements.exists(containsResource)
    case "result" => tpe.ok.exists(containsResource) || tpe.err.exists(containsResource)
    case "record" => tpe.fields.exists(f => containsResource(f.tpe))
    case _ => false
  }

  private def shouldEmitPublicWrapper(kind: String, params: List[(String, JsonAbiType)], result: JsonAbiType): Boolean =
    kind == "constructor" || kind == "method" || kind == "drop" || params.exists(p => containsResource(p._2)) || containsResource(result)

  private def shouldEmitPublicWrapper(entry: LoweredEntry): Boolean =
    shouldEmitPublicWrapper(entry.memberKind, entry.publicParams, entry.publicResult)

  private def flixResourceTypeName(effectName: String, resourceName: String): String =
    s"$effectName${toUpperIdent(resourceName)}"

  private def flixResourceCtor(effectName: String, resourceName: String): String = {
    val tpe = flixResourceTypeName(effectName, resourceName)
    s"$tpe.$tpe"
  }

  private def flixPublicTypeOf(effectName: String, tpe: JsonAbiType): String = tpe.kind match {
    case "unit" => "Unit"
    case "bool" => "Bool"
    case "int8" => "Int8"
    case "int16" => "Int16"
    case "int32" => "Int32"
    case "int64" => "Int64"
    case "float32" => "Float32"
    case "float64" => "Float64"
    case "string" => "String"
    case "bytes" => "Array[Int8, Static]"
    case "resource" => flixResourceTypeName(effectName, tpe.resource.get)
    case "list" => s"List[${flixPublicTypeOf(effectName, tpe.element.get)}]"
    case "array" => s"Array[${flixPublicTypeOf(effectName, tpe.element.get)}, Static]"
    case "tuple" => s"(${tpe.elements.map(flixPublicTypeOf(effectName, _)).mkString(", ")})"
    case "option" => s"Option[${flixPublicTypeOf(effectName, tpe.element.get)}]"
    case "result" => s"Result[${flixPublicTypeOf(effectName, tpe.err.getOrElse(JsonAbiType("unit")))}, ${flixPublicTypeOf(effectName, tpe.ok.getOrElse(JsonAbiType("unit")))}]"
    case "record" => s"{${tpe.fields.sortBy(_.label).map(f => s"${toLowerIdent(f.label)} = ${flixPublicTypeOf(effectName, f.tpe)}").mkString(", ")}}"
    case other => throw InternalCompilerException(s"Unexpected public WIT type '$other'.", SourceLocation.Unknown)
  }

  private def renderFlixLowerExpr(effectName: String, expr: String, tpe: JsonAbiType): String = tpe.kind match {
    case "unit" | "bool" | "int8" | "int16" | "int32" | "int64" | "float32" | "float64" | "string" | "bytes" => expr
    case "resource" =>
      s"match $expr { case ${flixResourceCtor(effectName, tpe.resource.get)}(id) => id }"
    case "list" =>
      val x = freshFlixName("x")
      s"List.map($x -> ${renderFlixLowerExpr(effectName, x, tpe.element.get)}, $expr)"
    case "array" =>
      val x = freshFlixName("x")
      s"Array.map(Static, $x -> ${renderFlixLowerExpr(effectName, x, tpe.element.get)}, $expr)"
    case "tuple" =>
      val names = tpe.elements.indices.map(_ => freshFlixName("x")).toList
      val pat = names.mkString(", ")
      val elms = names.zip(tpe.elements).map { case (name, elmTpe) => renderFlixLowerExpr(effectName, name, elmTpe) }.mkString(", ")
      s"match $expr { case ($pat) => ($elms) }"
    case "option" =>
      val x = freshFlixName("x")
      s"match $expr { case None => None case Some($x) => Some(${renderFlixLowerExpr(effectName, x, tpe.element.get)}) }"
    case "result" =>
      val okv = freshFlixName("ok")
      val errv = freshFlixName("err")
      val okT = tpe.ok.getOrElse(JsonAbiType("unit"))
      val errT = tpe.err.getOrElse(JsonAbiType("unit"))
      s"match $expr { case Ok($okv) => Ok(${renderFlixLowerExpr(effectName, okv, okT)}) case Err($errv) => Err(${renderFlixLowerExpr(effectName, errv, errT)}) }"
    case "record" =>
      val fields = tpe.fields.sortBy(_.label).map { field =>
        s"${toLowerIdent(field.label)} = ${renderFlixLowerExpr(effectName, s"$expr#${toLowerIdent(field.label)}", field.tpe)}"
      }.mkString(", ")
      s"{$fields}"
    case other => throw InternalCompilerException(s"Unexpected public WIT type '$other'.", SourceLocation.Unknown)
  }

  private def renderFlixLiftExpr(effectName: String, expr: String, tpe: JsonAbiType): String = tpe.kind match {
    case "unit" | "bool" | "int8" | "int16" | "int32" | "int64" | "float32" | "float64" | "string" | "bytes" => expr
    case "resource" =>
      s"${flixResourceCtor(effectName, tpe.resource.get)}($expr)"
    case "list" =>
      val x = freshFlixName("x")
      s"List.map($x -> ${renderFlixLiftExpr(effectName, x, tpe.element.get)}, $expr)"
    case "array" =>
      val x = freshFlixName("x")
      s"Array.map(Static, $x -> ${renderFlixLiftExpr(effectName, x, tpe.element.get)}, $expr)"
    case "tuple" =>
      val names = tpe.elements.indices.map(_ => freshFlixName("x")).toList
      val pat = names.mkString(", ")
      val elms = names.zip(tpe.elements).map { case (name, elmTpe) => renderFlixLiftExpr(effectName, name, elmTpe) }.mkString(", ")
      s"match $expr { case ($pat) => ($elms) }"
    case "option" =>
      val x = freshFlixName("x")
      s"match $expr { case None => None case Some($x) => Some(${renderFlixLiftExpr(effectName, x, tpe.element.get)}) }"
    case "result" =>
      val okv = freshFlixName("ok")
      val errv = freshFlixName("err")
      val okT = tpe.ok.getOrElse(JsonAbiType("unit"))
      val errT = tpe.err.getOrElse(JsonAbiType("unit"))
      s"match $expr { case Ok($okv) => Ok(${renderFlixLiftExpr(effectName, okv, okT)}) case Err($errv) => Err(${renderFlixLiftExpr(effectName, errv, errT)}) }"
    case "record" =>
      val fields = tpe.fields.sortBy(_.label).map { field =>
        s"${toLowerIdent(field.label)} = ${renderFlixLiftExpr(effectName, s"$expr#${toLowerIdent(field.label)}", field.tpe)}"
      }.mkString(", ")
      s"{$fields}"
    case other => throw InternalCompilerException(s"Unexpected public WIT type '$other'.", SourceLocation.Unknown)
  }

  private def renderPublicAbiType(tpe: JsonAbiType): String = {
    val parts = mutable.ListBuffer.empty[String]
    parts += s""""kind":"${escapeJson(tpe.kind)}""""
    tpe.resource.foreach(r => parts += s""""resource":"${escapeJson(r)}"""")
    tpe.ownership.foreach(o => parts += s""""ownership":"${escapeJson(o)}"""")
    tpe.element.foreach(e => parts += s""""element":${renderPublicAbiType(e)}""")
    if (tpe.elements.nonEmpty) parts += s""""elements":[${tpe.elements.map(renderPublicAbiType).mkString(", ")}]"""
    tpe.ok.foreach(ok => parts += s""""ok":${renderPublicAbiType(ok)}""")
    tpe.err.foreach(err => parts += s""""err":${renderPublicAbiType(err)}""")
    if (tpe.fields.nonEmpty) {
      val fs = tpe.fields.sortBy(_.label).map(f => s"""{"label":"${escapeJson(f.label)}","type":${renderPublicAbiType(f.tpe)}}""")
      parts += s""""fields":[${fs.mkString(", ")}]"""
    }
    "{" + parts.mkString(",") + "}"
  }

  private def renderTsType(tpe: ExportAbi.AbiType): String = tpe match {
    case ExportAbi.AbiType.Unit => "void"
    case ExportAbi.AbiType.Bool => "boolean"
    case ExportAbi.AbiType.Int8 => "number"
    case ExportAbi.AbiType.Int16 => "number"
    case ExportAbi.AbiType.Int32 => "number"
    case ExportAbi.AbiType.Int64 => "bigint"
    case ExportAbi.AbiType.Float32 => "number"
    case ExportAbi.AbiType.Float64 => "number"
    case ExportAbi.AbiType.String => "string"
    case ExportAbi.AbiType.Bytes => "Uint8Array | number[]"
    case ExportAbi.AbiType.List(elm) => s"Array<${renderTsType(elm)}>"
    case ExportAbi.AbiType.Array(elm) => s"Array<${renderTsType(elm)}>"
    case ExportAbi.AbiType.Tuple(elms) => s"[${elms.map(renderTsType).mkString(", ")}]"
    case ExportAbi.AbiType.Option(elm) => s"${renderTsType(elm)} | null"
    case ExportAbi.AbiType.Result(ok, err) => s"""{ tag: "ok"; val: ${renderTsType(ok)} } | { tag: "err"; val: ${renderTsType(err)} }"""
    case ExportAbi.AbiType.Record(fields) =>
      fields.map { case (label, fieldTpe) => s"$label: ${renderTsType(fieldTpe)}" }.mkString("{ ", "; ", " }")
  }

  private def renderTsPublicType(tpe: JsonAbiType): String = tpe.kind match {
    case "unit" => "void"
    case "bool" => "boolean"
    case "int8" | "int16" | "int32" | "float32" | "float64" => "number"
    case "int64" => "bigint"
    case "string" => "string"
    case "bytes" => "Uint8Array | number[]"
    case "resource" =>
      s"""{ __resource: "${escapeJson(tpe.resource.get)}"; __ownership: "${escapeJson(tpe.ownership.get)}"; id: bigint }"""
    case "list" | "array" => s"Array<${renderTsPublicType(tpe.element.get)}>"
    case "tuple" => s"[${tpe.elements.map(renderTsPublicType).mkString(", ")}]"
    case "option" => s"${renderTsPublicType(tpe.element.get)} | null"
    case "result" =>
      val ok = renderTsPublicType(tpe.ok.getOrElse(JsonAbiType("unit")))
      val err = renderTsPublicType(tpe.err.getOrElse(JsonAbiType("unit")))
      s"""{ tag: "ok"; val: ${ok} } | { tag: "err"; val: ${err} }"""
    case "record" =>
      tpe.fields.sortBy(_.label).map { field => s"${field.label}: ${renderTsPublicType(field.tpe)}" }.mkString("{ ", "; ", " }")
    case other => throw InternalCompilerException(s"Unexpected public WIT type '$other'.", SourceLocation.Unknown)
  }

  private def collectRustRecords(entries: List[LoweredEntry]): List[RustRecordDef] = {
    val acc = mutable.LinkedHashMap.empty[String, RustRecordDef]
    def visit(effectName: String, tpe: JsonAbiType): Unit = tpe.kind match {
      case "list" | "array" | "option" =>
        tpe.element.foreach(visit(effectName, _))
      case "tuple" =>
        tpe.elements.foreach(visit(effectName, _))
      case "result" =>
        tpe.ok.foreach(visit(effectName, _))
        tpe.err.foreach(visit(effectName, _))
      case "record" =>
        tpe.fields.foreach(field => visit(effectName, field.tpe))
        val key = s"$effectName:${publicRecordKey(tpe)}"
        if (!acc.contains(key)) {
          val name = tpe.name match {
            case Some(recordName) => s"${effectName}${toUpperIdent(recordName)}"
            case None => s"Record${acc.size + 1}"
          }
          val rustFields = tpe.fields.sortBy(_.label).map(field => (field.label, toRustIdent(field.label), field.tpe))
          acc.put(key, RustRecordDef(name, effectName, tpe.name, rustFields))
        }
      case _ => ()
    }
    entries.foreach { entry =>
      entry.publicParams.foreach { case (_, tpe) => visit(entry.effectName, tpe) }
      visit(entry.effectName, entry.publicResult)
    }
    acc.values.toList
  }

  private def renderRustType(tpe: ExportAbi.AbiType, records: List[RustRecordDef]): String = tpe match {
    case ExportAbi.AbiType.Unit => "()"
    case ExportAbi.AbiType.Bool => "bool"
    case ExportAbi.AbiType.Int8 => "i8"
    case ExportAbi.AbiType.Int16 => "i16"
    case ExportAbi.AbiType.Int32 => "i32"
    case ExportAbi.AbiType.Int64 => "i64"
    case ExportAbi.AbiType.Float32 => "f32"
    case ExportAbi.AbiType.Float64 => "f64"
    case ExportAbi.AbiType.String => "String"
    case ExportAbi.AbiType.Bytes => "Vec<u8>"
    case ExportAbi.AbiType.List(elm) => s"Vec<${renderRustType(elm, records)}>"
    case ExportAbi.AbiType.Array(elm) => s"Vec<${renderRustType(elm, records)}>"
    case ExportAbi.AbiType.Tuple(elms) =>
      if (elms.length == 1) s"(${renderRustType(elms.head, records)},)"
      else s"(${elms.map(renderRustType(_, records)).mkString(", ")})"
    case ExportAbi.AbiType.Option(elm) => s"Option<${renderRustType(elm, records)}>"
    case ExportAbi.AbiType.Result(ok, err) => s"std::result::Result<${renderRustType(ok, records)}, ${renderRustType(err, records)}>"
    case r@ExportAbi.AbiType.Record(_) =>
      rustRecordName(r, records)
  }

  private def renderRustPublicType(tpe: JsonAbiType, effectName: String, records: List[RustRecordDef]): String = tpe.kind match {
    case "unit" => "()"
    case "bool" => "bool"
    case "int8" => "i8"
    case "int16" => "i16"
    case "int32" => "i32"
    case "int64" => "i64"
    case "float32" => "f32"
    case "float64" => "f64"
    case "string" => "String"
    case "bytes" => "Vec<u8>"
    case "resource" => s"${flixResourceTypeName(effectName, tpe.resource.get)}"
    case "list" | "array" => s"Vec<${renderRustPublicType(tpe.element.get, effectName, records)}>"
    case "tuple" =>
      val elms = tpe.elements.map(renderRustPublicType(_, effectName, records))
      if (elms.length == 1) s"(${elms.head},)" else s"(${elms.mkString(", ")})"
    case "option" => s"Option<${renderRustPublicType(tpe.element.get, effectName, records)}>"
    case "result" =>
      val ok = renderRustPublicType(tpe.ok.getOrElse(JsonAbiType("unit")), effectName, records)
      val err = renderRustPublicType(tpe.err.getOrElse(JsonAbiType("unit")), effectName, records)
      s"std::result::Result<$ok, $err>"
    case "record" =>
      rustPublicRecordName(tpe, records, effectName)
    case other => throw InternalCompilerException(s"Unexpected public WIT type '$other'.", SourceLocation.Unknown)
  }

  private def renderRustDecodePublicBody(valueExpr: String, tpe: JsonAbiType, effectName: String, records: List[RustRecordDef]): String = tpe.kind match {
    case "unit" | "bool" | "int8" | "int16" | "int32" | "int64" | "float32" | "float64" | "string" | "bytes" =>
      renderRustDecodeBody(valueExpr, toRawAbiTypeOrThrow(tpe, "public rust decode"), records)
    case "resource" =>
      val rustTpe = flixResourceTypeName(effectName, tpe.resource.get)
      s"""match $valueExpr {
         |    ValueData::Int64(x) => ${renderRustAnyhowOk(rustTpe, s"$rustTpe(*x)")},
         |    other => anyhow::bail!("expected resource handle, got {:?}", other),
         |}""".stripMargin
    case "list" =>
      val elm = tpe.element.get
      val elmRustTpe = renderRustPublicType(elm, effectName, records)
      s"""match $valueExpr {
         |    ValueData::List(xs) => xs.iter().map(|x| -> Result<$elmRustTpe> {
         |${indent(renderRustDecodePublicBody("x", elm, effectName, records), 2)}
         |    }).collect::<std::result::Result<Vec<$elmRustTpe>, anyhow::Error>>(),
         |    other => anyhow::bail!("expected list, got {:?}", other),
         |}""".stripMargin
    case "array" =>
      val elm = tpe.element.get
      val elmRustTpe = renderRustPublicType(elm, effectName, records)
      s"""match $valueExpr {
         |    ValueData::Array(xs) => xs.iter().map(|x| -> Result<$elmRustTpe> {
         |${indent(renderRustDecodePublicBody("x", elm, effectName, records), 2)}
         |    }).collect::<std::result::Result<Vec<$elmRustTpe>, anyhow::Error>>(),
         |    other => anyhow::bail!("expected array, got {:?}", other),
         |}""".stripMargin
    case "tuple" =>
      val rustTpe = renderRustPublicType(tpe, effectName, records)
      val tupleValues = tpe.elements.indices.map { idx =>
        s"""(${renderRustDecodePublicBody(s"&xs[$idx]", tpe.elements(idx), effectName, records)})?"""
      }.mkString(", ")
      s"""match $valueExpr {
         |    ValueData::Tuple(xs) => {
         |        anyhow::ensure!(xs.len() == ${tpe.elements.length}, "expected tuple arity ${tpe.elements.length}, got {}", xs.len());
         |        ${renderRustAnyhowOk(rustTpe, s"(${tupleValues}${if (tpe.elements.length == 1) "," else ""})")}
         |    }
         |    other => anyhow::bail!("expected tuple, got {:?}", other),
         |}""".stripMargin
    case "option" =>
      val elm = tpe.element.get
      s"""match $valueExpr {
         |    ValueData::Option(None) => Ok(None),
         |    ValueData::Option(Some(v)) => Ok(Some((${renderRustDecodePublicBody("v.as_ref()", elm, effectName, records)})?)),
         |    other => anyhow::bail!("expected option, got {:?}", other),
         |}""".stripMargin
    case "result" =>
      val okT = tpe.ok.getOrElse(JsonAbiType("unit"))
      val errT = tpe.err.getOrElse(JsonAbiType("unit"))
      s"""match $valueExpr {
         |    ValueData::Result(Ok(v)) => Ok(Ok((${renderRustDecodePublicBody("v.as_ref()", okT, effectName, records)})?)),
         |    ValueData::Result(Err(v)) => Ok(Err((${renderRustDecodePublicBody("v.as_ref()", errT, effectName, records)})?)),
         |    other => anyhow::bail!("expected result, got {:?}", other),
         |}""".stripMargin
    case "record" =>
      val rustTpe = rustPublicRecordName(tpe, records, effectName)
      val fields = tpe.fields.sortBy(_.label).map { field =>
        val rustField = toRustIdent(field.label)
        s"$rustField: (${renderRustDecodePublicBody(s"""lookup_record_field($valueExpr, "${escapeRustString(field.label)}")?""", field.tpe, effectName, records)})?"
      }.mkString(", ")
      s"""match $valueExpr {
         |    ValueData::Record(_) => ${renderRustAnyhowOk(rustTpe, s"$rustTpe { $fields }")},
         |    other => anyhow::bail!("expected record, got {:?}", other),
         |}""".stripMargin
    case other => throw InternalCompilerException(s"Unexpected public WIT type '$other'.", SourceLocation.Unknown)
  }

  private def renderRustEncodePublicBody(valueExpr: String, tpe: JsonAbiType, effectName: String, records: List[RustRecordDef]): String = tpe.kind match {
    case "unit" | "bool" | "int8" | "int16" | "int32" | "int64" | "float32" | "float64" | "string" | "bytes" =>
      renderRustEncodeBody(valueExpr, toRawAbiTypeOrThrow(tpe, "public rust encode"), records)
    case "resource" => s"ValueData::Int64($valueExpr.0)"
    case "list" =>
      val elm = tpe.element.get
      s"ValueData::List($valueExpr.into_iter().map(|x| ${renderRustEncodePublicBody("x", elm, effectName, records)}).collect())"
    case "array" =>
      val elm = tpe.element.get
      s"ValueData::Array($valueExpr.into_iter().map(|x| ${renderRustEncodePublicBody("x", elm, effectName, records)}).collect())"
    case "tuple" =>
      val binders = tpe.elements.indices.map(i => s"v$i").mkString(", ")
      val encoded = tpe.elements.indices.map(i => renderRustEncodePublicBody(s"v$i", tpe.elements(i), effectName, records)).mkString(", ")
      s"""{ let ($binders${if (tpe.elements.length == 1) "," else ""}) = $valueExpr; ValueData::Tuple(vec![$encoded]) }"""
    case "option" =>
      val elm = tpe.element.get
      s"ValueData::Option($valueExpr.map(|x| Box::new(${renderRustEncodePublicBody("x", elm, effectName, records)})))"
    case "result" =>
      val okT = tpe.ok.getOrElse(JsonAbiType("unit"))
      val errT = tpe.err.getOrElse(JsonAbiType("unit"))
      s"""match $valueExpr {
         |    Ok(v) => ValueData::Result(Ok(Box::new(${renderRustEncodePublicBody("v", okT, effectName, records)}))),
         |    Err(v) => ValueData::Result(Err(Box::new(${renderRustEncodePublicBody("v", errT, effectName, records)}))),
         |}""".stripMargin
    case "record" =>
      val fields = tpe.fields.sortBy(_.label).map { field =>
        s"""("${escapeRustString(field.label)}".to_string(), ${renderRustEncodePublicBody(s"$valueExpr.${toRustIdent(field.label)}", field.tpe, effectName, records)})"""
      }.mkString(", ")
      s"ValueData::Record(vec![$fields])"
    case other => throw InternalCompilerException(s"Unexpected public WIT type '$other'.", SourceLocation.Unknown)
  }

  private def renderRustDecodeBody(valueExpr: String, tpe: ExportAbi.AbiType, records: List[RustRecordDef]): String = tpe match {
    case ExportAbi.AbiType.Unit =>
      val rustTpe = renderRustType(tpe, records)
      s"""match $valueExpr {
         |    ValueData::Unit => ${renderRustAnyhowOk(rustTpe, "()")},
         |    other => anyhow::bail!("expected unit, got {:?}", other),
         |}""".stripMargin
    case ExportAbi.AbiType.Bool =>
      val rustTpe = renderRustType(tpe, records)
      s"""match $valueExpr {
         |    ValueData::Bool(x) => ${renderRustAnyhowOk(rustTpe, "*x")},
         |    other => anyhow::bail!("expected bool, got {:?}", other),
         |}""".stripMargin
    case ExportAbi.AbiType.Int8 =>
      val rustTpe = renderRustType(tpe, records)
      s"""match $valueExpr {
         |    ValueData::Int8(x) => ${renderRustAnyhowOk(rustTpe, "*x")},
         |    other => anyhow::bail!("expected int8, got {:?}", other),
         |}""".stripMargin
    case ExportAbi.AbiType.Int16 =>
      val rustTpe = renderRustType(tpe, records)
      s"""match $valueExpr {
         |    ValueData::Int16(x) => ${renderRustAnyhowOk(rustTpe, "*x")},
         |    other => anyhow::bail!("expected int16, got {:?}", other),
         |}""".stripMargin
    case ExportAbi.AbiType.Int32 =>
      val rustTpe = renderRustType(tpe, records)
      s"""match $valueExpr {
         |    ValueData::Int32(x) => ${renderRustAnyhowOk(rustTpe, "*x")},
         |    other => anyhow::bail!("expected int32, got {:?}", other),
         |}""".stripMargin
    case ExportAbi.AbiType.Int64 =>
      val rustTpe = renderRustType(tpe, records)
      s"""match $valueExpr {
         |    ValueData::Int64(x) => ${renderRustAnyhowOk(rustTpe, "*x")},
         |    other => anyhow::bail!("expected int64, got {:?}", other),
         |}""".stripMargin
    case ExportAbi.AbiType.Float32 =>
      val rustTpe = renderRustType(tpe, records)
      s"""match $valueExpr {
         |    ValueData::Float32(x) => ${renderRustAnyhowOk(rustTpe, "*x")},
         |    other => anyhow::bail!("expected float32, got {:?}", other),
         |}""".stripMargin
    case ExportAbi.AbiType.Float64 =>
      val rustTpe = renderRustType(tpe, records)
      s"""match $valueExpr {
         |    ValueData::Float64(x) => ${renderRustAnyhowOk(rustTpe, "*x")},
         |    other => anyhow::bail!("expected float64, got {:?}", other),
         |}""".stripMargin
    case ExportAbi.AbiType.String =>
      val rustTpe = renderRustType(tpe, records)
      s"""match $valueExpr {
         |    ValueData::String(x) => ${renderRustAnyhowOk(rustTpe, "x.clone()")},
         |    other => anyhow::bail!("expected string, got {:?}", other),
         |}""".stripMargin
    case ExportAbi.AbiType.Bytes =>
      val rustTpe = renderRustType(tpe, records)
      s"""match $valueExpr {
         |    ValueData::Bytes(x) => ${renderRustAnyhowOk(rustTpe, "x.clone()")},
         |    other => anyhow::bail!("expected bytes, got {:?}", other),
         |}""".stripMargin
    case ExportAbi.AbiType.List(elm) =>
      val elmRustTpe = renderRustType(elm, records)
      s"""match $valueExpr {
         |    ValueData::List(xs) => xs.iter().map(|x| -> Result<$elmRustTpe> {
         |${indent(renderRustDecodeBody("x", elm, records), 2)}
         |    }).collect::<std::result::Result<Vec<$elmRustTpe>, anyhow::Error>>(),
         |    other => anyhow::bail!("expected list, got {:?}", other),
         |}""".stripMargin
    case ExportAbi.AbiType.Array(elm) =>
      val elmRustTpe = renderRustType(elm, records)
      s"""match $valueExpr {
         |    ValueData::Array(xs) => xs.iter().map(|x| -> Result<$elmRustTpe> {
         |${indent(renderRustDecodeBody("x", elm, records), 2)}
         |    }).collect::<std::result::Result<Vec<$elmRustTpe>, anyhow::Error>>(),
         |    other => anyhow::bail!("expected array, got {:?}", other),
         |}""".stripMargin
    case ExportAbi.AbiType.Tuple(elms) =>
      val rustTpe = renderRustType(tpe, records)
      val tupleValues = elms.indices.map { idx =>
        s"""(${renderRustDecodeBody(s"&xs[$idx]", elms(idx), records)})?"""
      }.mkString(", ")
      s"""match $valueExpr {
         |    ValueData::Tuple(xs) => {
         |        anyhow::ensure!(xs.len() == ${elms.length}, "expected tuple arity ${elms.length}, got {}", xs.len());
         |        ${renderRustAnyhowOk(rustTpe, s"(${tupleValues}${if (elms.length == 1) "," else ""})")}
         |    }
         |    other => anyhow::bail!("expected tuple, got {:?}", other),
         |}""".stripMargin
    case ExportAbi.AbiType.Option(elm) =>
      val rustTpe = renderRustType(tpe, records)
      s"""match $valueExpr {
         |    ValueData::Option(None) => ${renderRustAnyhowOk(rustTpe, "None")},
         |    ValueData::Option(Some(x)) => ${renderRustAnyhowOk(rustTpe, s"Some((${renderRustDecodeBody("x.as_ref()", elm, records)})?)")},
         |    other => anyhow::bail!("expected option, got {:?}", other),
         |}""".stripMargin
    case ExportAbi.AbiType.Result(ok, err) =>
      val rustTpe = renderRustType(tpe, records)
      val okRustTpe = renderRustType(ok, records)
      val errRustTpe = renderRustType(err, records)
      s"""match $valueExpr {
         |    ValueData::Result(Ok(x)) => ${renderRustAnyhowOk(rustTpe, s"std::result::Result::<$okRustTpe, $errRustTpe>::Ok((${renderRustDecodeBody("x.as_ref()", ok, records)})?)")},
         |    ValueData::Result(Err(x)) => ${renderRustAnyhowOk(rustTpe, s"std::result::Result::<$okRustTpe, $errRustTpe>::Err((${renderRustDecodeBody("x.as_ref()", err, records)})?)")},
         |    other => anyhow::bail!("expected result, got {:?}", other),
         |}""".stripMargin
    case r@ExportAbi.AbiType.Record(fields) =>
      val defn = records.find(rd => abiFingerprintFromDef(rd) == abiFingerprint(r)).getOrElse {
        throw InternalCompilerException(s"Missing generated Rust record for ABI shape ${abiFingerprint(r)}.", SourceLocation.Unknown)
      }
      val fieldDecoders = defn.fields.zipWithIndex.map { case ((label, rustField, fieldTpe), idx) =>
        s"""let $rustField = match fields.get($idx) {
           |    Some((field_label, field_value)) if field_label == "$label" => (${renderRustDecodeBody("field_value", toRawAbiTypeOrThrow(fieldTpe, s"raw rust record field '$label'"), records)})?,
           |    Some((field_label, _)) => anyhow::bail!("expected field '$label', got '{}'", field_label),
           |    None => anyhow::bail!("missing field '$label'"),
           |};""".stripMargin
      }.mkString("\n")
      val ctorFields = defn.fields.map { case (_, rustField, _) => s"$rustField: $rustField" }.mkString(", ")
      s"""match $valueExpr {
         |    ValueData::Record(fields) => {
         |        anyhow::ensure!(fields.len() == ${defn.fields.length}, "expected ${defn.fields.length} record fields, got {}", fields.len());
         |${indent(fieldDecoders, 2)}
         |        ${renderRustAnyhowOk(defn.name, s"${defn.name} { $ctorFields }")}
         |    }
         |    other => anyhow::bail!("expected record, got {:?}", other),
         |}""".stripMargin
  }

  private def renderRustEncodeBody(valueExpr: String, tpe: ExportAbi.AbiType, records: List[RustRecordDef]): String = tpe match {
    case ExportAbi.AbiType.Unit => "ValueData::Unit"
    case ExportAbi.AbiType.Bool => s"ValueData::Bool($valueExpr)"
    case ExportAbi.AbiType.Int8 => s"ValueData::Int8($valueExpr)"
    case ExportAbi.AbiType.Int16 => s"ValueData::Int16($valueExpr)"
    case ExportAbi.AbiType.Int32 => s"ValueData::Int32($valueExpr)"
    case ExportAbi.AbiType.Int64 => s"ValueData::Int64($valueExpr)"
    case ExportAbi.AbiType.Float32 => s"ValueData::Float32($valueExpr)"
    case ExportAbi.AbiType.Float64 => s"ValueData::Float64($valueExpr)"
    case ExportAbi.AbiType.String => s"ValueData::String($valueExpr)"
    case ExportAbi.AbiType.Bytes => s"ValueData::Bytes($valueExpr)"
    case ExportAbi.AbiType.List(elm) =>
      s"ValueData::List($valueExpr.into_iter().map(|x| ${renderRustEncodeBody("x", elm, records)}).collect())"
    case ExportAbi.AbiType.Array(elm) =>
      s"ValueData::Array($valueExpr.into_iter().map(|x| ${renderRustEncodeBody("x", elm, records)}).collect())"
    case ExportAbi.AbiType.Tuple(elms) =>
      val vars = elms.indices.map(i => s"v$i")
      val destruct = if (elms.length == 1) s"let (${vars.mkString(", ")},) = $valueExpr;" else s"let (${vars.mkString(", ")}) = $valueExpr;"
      val values = vars.zip(elms).map { case (v, elm) => renderRustEncodeBody(v, elm, records) }.mkString(", ")
      s"""{
         |    $destruct
         |    ValueData::Tuple(vec![$values])
         |}""".stripMargin
    case ExportAbi.AbiType.Option(elm) =>
      s"""match $valueExpr {
         |    None => ValueData::Option(None),
         |    Some(x) => ValueData::Option(Some(Box::new(${renderRustEncodeBody("x", elm, records)}))),
         |}""".stripMargin
    case ExportAbi.AbiType.Result(ok, err) =>
      s"""match $valueExpr {
         |    Ok(x) => ValueData::Result(Ok(Box::new(${renderRustEncodeBody("x", ok, records)}))),
         |    Err(x) => ValueData::Result(Err(Box::new(${renderRustEncodeBody("x", err, records)}))),
         |}""".stripMargin
    case r@ExportAbi.AbiType.Record(_) =>
      val defn = records.find(rd => abiFingerprintFromDef(rd) == abiFingerprint(r)).getOrElse {
        throw InternalCompilerException(s"Missing generated Rust record for ABI shape ${abiFingerprint(r)}.", SourceLocation.Unknown)
      }
      val fields = defn.fields.map { case (label, rustField, fieldTpe) =>
        s"""("$label".to_string(), ${renderRustEncodeBody(s"$valueExpr.$rustField", toRawAbiTypeOrThrow(fieldTpe, s"raw rust record field '$label'"), records)})"""
      }.mkString(", ")
      s"ValueData::Record(vec![$fields])"
  }

  private def copySupportModule(src: Path, dest: Path): Unit = {
    Files.createDirectories(dest.getParent)
    Files.copy(src, dest, StandardCopyOption.REPLACE_EXISTING)
  }

  private def copyDirectoryContents(srcDir: Path, destDir: Path): Unit = {
    Files.createDirectories(destDir)
    FileOps.getFilesIn(srcDir, Int.MaxValue).foreach { src =>
      val rel = srcDir.relativize(src)
      val dest = destDir.resolve(rel)
      Files.createDirectories(dest.getParent)
      Files.copy(src, dest, StandardCopyOption.REPLACE_EXISTING)
    }
  }

  private def copyRuntimeBindingsWit(destDir: Path): Unit = {
    Files.createDirectories(destDir)
    if (Files.isDirectory(DefaultRuntimeBindingsWitDir)) {
      copyDirectoryContents(DefaultRuntimeBindingsWitDir, destDir)
    } else {
      BundledRuntimeBindingsWitResources.foreach { case (resource, rel) =>
        copyBundledResource(resource, destDir.resolve(rel))
      }
    }
  }

  private def indent(s: String, level: Int): String = {
    val pad = "    " * level
    s.linesIterator.map(line => if (line.isEmpty) line else pad + line).mkString("\n")
  }

  private def renderRustAnyhowOk(rustType: String, expr: String): String =
    s"std::result::Result::<$rustType, anyhow::Error>::Ok($expr)"

  private def lowerFirst(s: String): String =
    if (s.isEmpty) s else s"${s.head.toLower}${s.tail}"

  private def toRustIdent(name: String): String = {
    val raw = name.flatMap {
      case c if c.isUpper =>
        "_" + c.toLower
      case c if c.isLetterOrDigit =>
        c.toString
      case _ => "_"
    }.stripPrefix("_").replaceAll("_+", "_")
    val ident = if (raw.headOption.exists(_.isLetter) || raw.headOption.contains('_')) raw else s"x_$raw"
    if (RustKeywords.contains(ident)) s"${ident}_" else ident
  }

  private val RustKeywords: Set[String] = Set(
    "as", "break", "const", "continue", "crate", "else", "enum", "extern", "false", "fn",
    "for", "if", "impl", "in", "let", "loop", "match", "mod", "move", "mut", "pub", "ref",
    "return", "self", "Self", "static", "struct", "super", "trait", "true", "type", "unsafe",
    "use", "where", "while", "async", "await", "dyn"
  )

  private def abiFingerprint(tpe: ExportAbi.AbiType): String = tpe match {
    case ExportAbi.AbiType.Unit => "unit"
    case ExportAbi.AbiType.Bool => "bool"
    case ExportAbi.AbiType.Int8 => "int8"
    case ExportAbi.AbiType.Int16 => "int16"
    case ExportAbi.AbiType.Int32 => "int32"
    case ExportAbi.AbiType.Int64 => "int64"
    case ExportAbi.AbiType.Float32 => "float32"
    case ExportAbi.AbiType.Float64 => "float64"
    case ExportAbi.AbiType.String => "string"
    case ExportAbi.AbiType.Bytes => "bytes"
    case ExportAbi.AbiType.List(elm) => s"list(${abiFingerprint(elm)})"
    case ExportAbi.AbiType.Array(elm) => s"array(${abiFingerprint(elm)})"
    case ExportAbi.AbiType.Tuple(elms) => s"tuple(${elms.map(abiFingerprint).mkString(",")})"
    case ExportAbi.AbiType.Option(elm) => s"option(${abiFingerprint(elm)})"
    case ExportAbi.AbiType.Result(ok, err) => s"result(${abiFingerprint(ok)},${abiFingerprint(err)})"
    case ExportAbi.AbiType.Record(fields) =>
      s"record(${fields.map { case (label, fieldTpe) => s"$label:${abiFingerprint(fieldTpe)}" }.mkString(",")})"
  }

  private def abiFingerprintFromDef(record: RustRecordDef): String =
    s"record(${record.fields.map { case (label, _, fieldTpe) => s"$label:${publicAbiFingerprint(fieldTpe)}" }.mkString(",")})"

  private def publicAbiFingerprintFromDef(record: RustRecordDef): String =
    s"record(${record.fields.map { case (label, _, fieldTpe) => s"$label:${publicAbiFingerprint(fieldTpe)}" }.mkString(",")})"

  private def publicRecordKey(tpe: JsonAbiType): String = {
    val shape = publicAbiFingerprint(tpe)
    tpe.name match {
      case Some(name) => s"named(${toLowerIdent(name)}|$shape)"
      case None => shape
    }
  }

  private def publicRecordKeyFromDef(record: RustRecordDef): String = {
    val shape = publicAbiFingerprintFromDef(record)
    record.witName match {
      case Some(name) => s"named(${toLowerIdent(name)}|$shape)"
      case None => shape
    }
  }

  private def publicAbiFingerprint(tpe: JsonAbiType): String = tpe.kind match {
    case "unit" | "bool" | "int8" | "int16" | "int32" | "int64" | "float32" | "float64" | "string" | "bytes" =>
      tpe.kind
    case "resource" =>
      s"resource(${tpe.resource.getOrElse("?")},${tpe.ownership.getOrElse("?")})"
    case "list" =>
      s"list(${publicAbiFingerprint(tpe.element.get)})"
    case "array" =>
      s"array(${publicAbiFingerprint(tpe.element.get)})"
    case "tuple" =>
      s"tuple(${tpe.elements.map(publicAbiFingerprint).mkString(",")})"
    case "option" =>
      s"option(${publicAbiFingerprint(tpe.element.get)})"
    case "result" =>
      s"result(${publicAbiFingerprint(tpe.ok.getOrElse(JsonAbiType("unit")))},${publicAbiFingerprint(tpe.err.getOrElse(JsonAbiType("unit")))})"
    case "record" =>
      s"record(${tpe.fields.sortBy(_.label).map(field => s"${field.label}:${publicAbiFingerprint(field.tpe)}").mkString(",")})"
    case other =>
      s"unknown($other)"
  }

  private def rustRecordName(tpe: ExportAbi.AbiType, records: List[RustRecordDef]): String = {
    val key = abiFingerprint(tpe)
    records.find(rd => abiFingerprintFromDef(rd) == key).map(_.name).getOrElse {
      throw InternalCompilerException(s"Missing generated Rust record for ABI shape $key.", SourceLocation.Unknown)
    }
  }

  private def rustPublicRecordName(tpe: JsonAbiType, records: List[RustRecordDef], effectName: String = ""): String = {
    val key = s"$effectName:${publicRecordKey(tpe)}"
    val publicKey = publicRecordKey(tpe)
    records.find(rd => rd.effectName == effectName && publicRecordKeyFromDef(rd) == publicKey)
      .orElse(records.find(rd => publicRecordKeyFromDef(rd) == publicKey && !containsResource(tpe)))
      .map(_.name)
      .getOrElse {
        throw InternalCompilerException(s"Missing generated Rust record for public ABI shape $key.", SourceLocation.Unknown)
      }
  }

  private def toAbiType(tpe: JsonAbiType, context: String): Result[ExportAbi.AbiType, String] = tpe.kind match {
    case "unit" => Result.Ok(ExportAbi.AbiType.Unit)
    case "bool" => Result.Ok(ExportAbi.AbiType.Bool)
    case "int8" => Result.Ok(ExportAbi.AbiType.Int8)
    case "int16" => Result.Ok(ExportAbi.AbiType.Int16)
    case "int32" => Result.Ok(ExportAbi.AbiType.Int32)
    case "int64" => Result.Ok(ExportAbi.AbiType.Int64)
    case "float32" => Result.Ok(ExportAbi.AbiType.Float32)
    case "float64" => Result.Ok(ExportAbi.AbiType.Float64)
    case "string" => Result.Ok(ExportAbi.AbiType.String)
    case "bytes" => Result.Ok(ExportAbi.AbiType.Bytes)
    case "list" =>
      Result.traverseOpt(tpe.element)(elm => toAbiType(elm, context)).flatMap {
        case Some(elm) => Result.Ok(ExportAbi.AbiType.List(elm))
        case None => Result.Err(s"Malformed WIT list type in $context.")
      }
    case "tuple" =>
      Result.traverse(tpe.elements)(elm => toAbiType(elm, context)).map(ts => ExportAbi.AbiType.Tuple(ts))
    case "option" =>
      Result.traverseOpt(tpe.element)(elm => toAbiType(elm, context)).flatMap {
        case Some(elm) => Result.Ok(ExportAbi.AbiType.Option(elm))
        case None => Result.Err(s"Malformed WIT option type in $context.")
      }
    case "result" =>
      Result.mapN(
        Result.traverseOpt(tpe.ok)(elm => toAbiType(elm, context)),
        Result.traverseOpt(tpe.err)(elm => toAbiType(elm, context))
      ) {
        case (ok, err) => ExportAbi.AbiType.Result(ok.getOrElse(ExportAbi.AbiType.Unit), err.getOrElse(ExportAbi.AbiType.Unit))
      }
    case "record" =>
      Result.traverse(tpe.fields.sortBy(_.label)) { field =>
        toAbiType(field.tpe, s"$context field '${field.label}'").map(t => toLowerIdent(field.label) -> t)
      }.map(fields => ExportAbi.AbiType.Record(fields))
    case other =>
      Result.Err(s"Unsupported WIT type '$other' in $context.")
  }

  private def toRawAbiTypeOrThrow(tpe: JsonAbiType, context: String): ExportAbi.AbiType = toRawAbiType(tpe, context) match {
    case Result.Ok(v) => v
    case Result.Err(msg) => throw InternalCompilerException(msg, SourceLocation.Unknown)
  }

  private def flixTypeOf(tpe: ExportAbi.AbiType): String = tpe match {
    case ExportAbi.AbiType.Unit => "Unit"
    case ExportAbi.AbiType.Bool => "Bool"
    case ExportAbi.AbiType.Int8 => "Int8"
    case ExportAbi.AbiType.Int16 => "Int16"
    case ExportAbi.AbiType.Int32 => "Int32"
    case ExportAbi.AbiType.Int64 => "Int64"
    case ExportAbi.AbiType.Float32 => "Float32"
    case ExportAbi.AbiType.Float64 => "Float64"
    case ExportAbi.AbiType.String => "String"
    case ExportAbi.AbiType.Bytes => "Array[Int8, Static]"
    case ExportAbi.AbiType.List(elm) => s"List[${flixTypeOf(elm)}]"
    case ExportAbi.AbiType.Array(elm) => s"Array[${flixTypeOf(elm)}, Static]"
    case ExportAbi.AbiType.Tuple(elms) => s"(${elms.map(flixTypeOf).mkString(", ")})"
    case ExportAbi.AbiType.Option(elm) => s"Option[${flixTypeOf(elm)}]"
    case ExportAbi.AbiType.Result(ok, err) => s"Result[${flixTypeOf(err)}, ${flixTypeOf(ok)}]"
    case ExportAbi.AbiType.Record(fields) => s"{${fields.map { case (label, fieldTpe) => s"$label = ${flixTypeOf(fieldTpe)}" }.mkString(", ")}}"
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
    case ExportAbi.AbiType.List(elm) => s"""{"kind":"list","element":${renderAbiType(elm)},"repr":{"nilTagId":1,"consTagId":0}}"""
    case ExportAbi.AbiType.Array(elm) => s"""{"kind":"array","element":${renderAbiType(elm)}}"""
    case ExportAbi.AbiType.Tuple(elms) => s"""{"kind":"tuple","elements":[${elms.map(renderAbiType).mkString(",")}]}"""
    case ExportAbi.AbiType.Option(elm) => s"""{"kind":"option","element":${renderAbiType(elm)},"repr":{"noneTagId":0,"someTagId":1}}"""
    case ExportAbi.AbiType.Result(ok, err) => s"""{"kind":"result","ok":${renderAbiType(ok)},"err":${renderAbiType(err)},"repr":{"errTagId":0,"okTagId":1}}"""
    case ExportAbi.AbiType.Record(fields) =>
      s"""{"kind":"record","fields":[${fields.map { case (label, fieldTpe) => s"""{"label":"${escapeJson(label)}","type":${renderAbiType(fieldTpe)}}""" }.mkString(",")}]}"""
  }

  private def toUpperIdent(name: String): String = {
    val parts = name.split("[^A-Za-z0-9]+").toList.filter(_.nonEmpty)
    val base = parts.map(p => s"${p.head.toUpper}${p.tail}").mkString
    keywordSafe(if (base.nonEmpty && base.head.isLetter) base else s"W${base}")
  }

  private def toLowerIdent(name: String): String = {
    val parts = name.split("[^A-Za-z0-9]+").toList.filter(_.nonEmpty)
    val base = parts match {
      case Nil => "x"
      case head :: tail => head.toLowerCase + tail.map(p => s"${p.head.toUpper}${p.tail}").mkString
    }
    val cleaned = if (base.headOption.exists(_.isLetter)) base else s"x$base"
    keywordSafe(cleaned)
  }

  private def keywordSafe(s: String): String = {
    if (FlixKeywords.contains(s)) s"${s}_" else s
  }

  private var flixFreshCounter: Int = 0

  private def freshFlixName(prefix: String): String = {
    flixFreshCounter += 1
    s"${prefix}${flixFreshCounter}"
  }

  private val FlixKeywords: Set[String] = Set(
    "alias", "and", "as", "case", "catch", "choose", "def", "discard", "eff", "else", "enum",
    "extern", "false", "fix", "forall", "force", "foreach", "from", "handler", "if", "import",
    "inject", "instance", "into", "law", "lawful", "lazy", "let", "match", "mod", "mut",
    "new", "not", "null", "or", "override", "par", "pub", "query", "region", "restrictable",
    "run", "sealed", "select", "solve", "spawn", "static", "struct", "throw", "trait", "true",
    "try", "type", "unsafe", "use", "where", "with", "without", "yield"
  )

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

  private def escapeJsString(s: String): String = {
    val b = new StringBuilder(s.length + 8)
    s.foreach {
      case '"' => b.append("\\\"")
      case '\\' => b.append("\\\\")
      case '\n' => b.append("\\n")
      case '\r' => b.append("\\r")
      case '\t' => b.append("\\t")
      case c => b.append(c)
    }
    b.toString()
  }

  private def escapeRustString(s: String): String = {
    val b = new StringBuilder(s.length + 8)
    s.foreach {
      case '"' => b.append("\\\"")
      case '\\' => b.append("\\\\")
      case '\n' => b.append("\\n")
      case '\r' => b.append("\\r")
      case '\t' => b.append("\\t")
      case c => b.append(c)
    }
    b.toString()
  }

  private def copyBundledResource(resource: String, dest: Path): Unit = {
    val is = Option(getClass.getResourceAsStream(resource)).getOrElse {
      throw InternalCompilerException(s"Missing bundled resource '$resource'.", SourceLocation.Unknown)
    }
    try {
      Files.createDirectories(dest.getParent)
      Files.copy(is, dest, StandardCopyOption.REPLACE_EXISTING)
    } finally {
      is.close()
    }
  }

  private def exec(cmd: List[String], cwd: Path): (Int, String) = {
    val pb = new ProcessBuilder(cmd*)
    pb.directory(cwd.toFile)
    pb.redirectErrorStream(true)
    val proc = pb.start()
    val output = new String(proc.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
    val exit = proc.waitFor()
    (exit, output)
  }
}
