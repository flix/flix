/*
 * Copyright 2023 Magnus Madsen
 * Copyright 2025 Jakob Schneider Villumsen
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
package ca.uwaterloo.flix.tools.pkg

import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.tools.pkg.github.GitHub
import ca.uwaterloo.flix.util.{BindingsConfig, CompilationTarget, EmitKind, NativeBindingConfig, NativeCompileConfig, NativeLinkConfig, RunnerKind, WasmBindingConfig}

case class Manifest(name: String,
                    description: String,
                    version: SemVer,
                    repository: Option[GitHub.Project],
                    modules: PackageModules,
                    flix: SemVer,
                    license: Option[String],
                    authors: List[String],
                    dependencies: List[Dependency],
                    buildConfig: Manifest.BuildConfig = Manifest.BuildConfig(),
                    bindings: BindingsConfig = BindingsConfig(),
                    targetConfigs: Manifest.TargetConfigs = Manifest.TargetConfigs(),
                    runConfig: Manifest.RunConfig = Manifest.RunConfig(),
                    testConfig: Manifest.TestConfig = Manifest.TestConfig()) {
  def flixDependencies: List[Dependency.FlixPackageDependency] = dependencies.collect { case dep: Dependency.FlixPackageDependency => dep }

  def mavenDependencies: List[Dependency.MavenDependency] = dependencies.collect { case dep: Dependency.MavenDependency => dep }

  def jarDependencies: List[Dependency.JarDependency] = dependencies.collect { case dep: Dependency.JarDependency => dep }
}

object Manifest {

  case class BuildConfig(targets: List[CompilationTarget] = List(CompilationTarget.Jvm))

  case class TargetConfig(emits: Option[List[EmitKind]] = None)

  case class NativeTargetConfig(emits: Option[List[EmitKind]] = None,
                                link: NativeLinkConfig = NativeLinkConfig(),
                                compile: NativeCompileConfig = NativeCompileConfig())

  case class TargetConfigs(jvm: TargetConfig = TargetConfig(),
                           native: NativeTargetConfig = NativeTargetConfig(),
                           wasm: TargetConfig = TargetConfig()) {
    def emitFor(target: CompilationTarget): Option[List[EmitKind]] = target match {
      case CompilationTarget.Jvm => jvm.emits
      case CompilationTarget.LlvmNative => native.emits
      case CompilationTarget.LlvmWasm => wasm.emits
    }
  }

  case class RunConfig(target: Option[CompilationTarget] = None,
                       runner: Option[RunnerKind] = None)

  case class TestConfig(target: Option[CompilationTarget] = None,
                        runner: Option[RunnerKind] = None)

  /**
    * Formats `manifest` as a string / a valid `.toml` file.
    * Parsing the output yields the original manifest, i.e., `manifest`.
    */
  def format(manifest: Manifest): String = {
    val packageSection = mkPackageSection(manifest)
    val buildSection = mkBuildSection(manifest)
    val nativeBindingsSections = mkNativeBindingSections(manifest)
    val wasmBindingsSections = mkWasmBindingSections(manifest)
    val targetJvmSection = mkTargetSection("jvm", manifest.targetConfigs.jvm)
    val targetNativeSection = mkNativeTargetSection(manifest.targetConfigs.native)
    val targetWasmSection = mkTargetSection("wasm", manifest.targetConfigs.wasm)
    val runSection = mkRunSection(manifest)
    val testSection = mkTestSection(manifest)
    val flixDepSection = mkFlixDependencySection(manifest)
    val mvnDepSection = mkMavenDependencySection(manifest)
    val jarDepSection = mkJarDependencySection(manifest)
    (
      List(Some(packageSection), buildSection, targetJvmSection, targetNativeSection, targetWasmSection, runSection, testSection, Some(flixDepSection), Some(mvnDepSection), Some(jarDepSection))
        .flatten
        .map(formatTomlSection) :::
        nativeBindingsSections.map(formatTomlArraySection) :::
        wasmBindingsSections.map(formatTomlArraySection)
    ).mkString(System.lineSeparator())
  }

  private def mkPackageSection(manifest: Manifest): TomlSection = {
    val repository = manifest.repository.map(proj => TomlEntry.Present(TomlKey("repository"), TomlExp.TomlValue(s"github:$proj")))
      .getOrElse(TomlEntry.Absent)
    val modules = manifest.modules match {
      case PackageModules.All => TomlEntry.Absent
      case PackageModules.Selected(included) =>
        TomlEntry.Present(TomlKey("modules"), TomlExp.TomlArray(included.toList.map(TomlExp.TomlValue.apply)))
    }
    val license = manifest.license.map(license => TomlEntry.Present(TomlKey("license"), TomlExp.TomlValue(license)))
      .getOrElse(TomlEntry.Absent)
    val name = TomlEntry.Present(TomlKey("name"), TomlExp.TomlValue(manifest.name))
    val description = TomlEntry.Present(TomlKey("description"), TomlExp.TomlValue(manifest.description))
    val version = TomlEntry.Present(TomlKey("version"), TomlExp.TomlValue(manifest.version))
    val flixVersion = TomlEntry.Present(TomlKey("flix"), TomlExp.TomlValue(manifest.flix))
    val authors = TomlEntry.Present(TomlKey("authors"), TomlExp.TomlArray(manifest.authors.map(TomlExp.TomlValue.apply)))

    TomlSection("package",
      List(
        name,
        description,
        version,
        repository,
        modules,
        flixVersion,
        license,
        authors,
      )
    )
  }

  private def mkFlixDependencySection(manifest: Manifest): TomlSection = {
    TomlSection("dependencies", manifest.flixDependencies.map(mkFlixDependency))
  }

  private def mkBuildSection(manifest: Manifest): Option[TomlSection] =
    if (manifest.buildConfig.targets == List(CompilationTarget.Jvm)) None
    else {
      val targets = TomlEntry.Present(TomlKey("targets"), TomlExp.TomlArray(manifest.buildConfig.targets.map(formatTarget).map(TomlExp.TomlValue.apply)))
      Some(TomlSection("build", List(targets)))
    }

  private def mkRunSection(manifest: Manifest): Option[TomlSection] =
    if (manifest.runConfig == RunConfig()) None
    else {
      val entries = List(
        manifest.runConfig.target.map(target => TomlEntry.Present(TomlKey("target"), TomlExp.TomlValue(formatTarget(target)))).getOrElse(TomlEntry.Absent),
        manifest.runConfig.runner.map(runner => TomlEntry.Present(TomlKey("runner"), TomlExp.TomlValue(formatRunner(runner)))).getOrElse(TomlEntry.Absent)
      )
      Some(TomlSection("run", entries))
    }

  private def mkTestSection(manifest: Manifest): Option[TomlSection] =
    if (manifest.testConfig == TestConfig()) None
    else {
      val entries = List(
        manifest.testConfig.target.map(target => TomlEntry.Present(TomlKey("target"), TomlExp.TomlValue(formatTarget(target)))).getOrElse(TomlEntry.Absent),
        manifest.testConfig.runner.map(runner => TomlEntry.Present(TomlKey("runner"), TomlExp.TomlValue(formatRunner(runner)))).getOrElse(TomlEntry.Absent)
      )
      Some(TomlSection("test", entries))
    }

  private def mkTargetSection(name: String, config: TargetConfig): Option[TomlSection] =
    config.emits match {
      case None => None
      case Some(emits) =>
        val entry = TomlEntry.Present(TomlKey("emit"), TomlExp.TomlArray(emits.map(formatEmit).map(TomlExp.TomlValue.apply)))
        Some(TomlSection(s"target.$name", List(entry)))
    }

  private def mkNativeTargetSection(config: NativeTargetConfig): Option[TomlSection] = {
    val entries = List(
      config.emits.map(emits => TomlEntry.Present(TomlKey("emit"), TomlExp.TomlArray(emits.map(formatEmit).map(TomlExp.TomlValue.apply)))).getOrElse(TomlEntry.Absent),
      if (config.link.libraries.isEmpty) TomlEntry.Absent else TomlEntry.Present(TomlKey("link-libs"), TomlExp.TomlArray(config.link.libraries.map(TomlExp.TomlValue.apply))),
      if (config.link.searchPaths.isEmpty) TomlEntry.Absent else TomlEntry.Present(TomlKey("link-search"), TomlExp.TomlArray(config.link.searchPaths.map(p => TomlExp.TomlValue(p.toString)))),
      if (config.link.pkgConfigPackages.isEmpty) TomlEntry.Absent else TomlEntry.Present(TomlKey("pkg-config"), TomlExp.TomlArray(config.link.pkgConfigPackages.map(TomlExp.TomlValue.apply))),
      if (config.link.frameworks.isEmpty) TomlEntry.Absent else TomlEntry.Present(TomlKey("frameworks"), TomlExp.TomlArray(config.link.frameworks.map(TomlExp.TomlValue.apply))),
      if (config.link.frameworkSearchPaths.isEmpty) TomlEntry.Absent else TomlEntry.Present(TomlKey("framework-search"), TomlExp.TomlArray(config.link.frameworkSearchPaths.map(p => TomlExp.TomlValue(p.toString)))),
      if (config.link.flags.isEmpty) TomlEntry.Absent else TomlEntry.Present(TomlKey("link-flags"), TomlExp.TomlArray(config.link.flags.map(TomlExp.TomlValue.apply))),
      if (config.compile.sources.isEmpty) TomlEntry.Absent else TomlEntry.Present(TomlKey("compile-sources"), TomlExp.TomlArray(config.compile.sources.map(p => TomlExp.TomlValue(p.toString)))),
      if (config.compile.includePaths.isEmpty) TomlEntry.Absent else TomlEntry.Present(TomlKey("compile-include"), TomlExp.TomlArray(config.compile.includePaths.map(p => TomlExp.TomlValue(p.toString)))),
      if (config.compile.cflags.isEmpty) TomlEntry.Absent else TomlEntry.Present(TomlKey("compile-cflags"), TomlExp.TomlArray(config.compile.cflags.map(TomlExp.TomlValue.apply))),
    )
    if (entries.exists(_.isInstanceOf[TomlEntry.Present])) Some(TomlSection("target.native", entries)) else None
  }

  private def mkNativeBindingSections(manifest: Manifest): List[TomlSection] =
    manifest.bindings.native.map(mkNativeBindingSection)

  private def mkNativeBindingSection(config: NativeBindingConfig): TomlSection = {
    val entries = List(
      TomlEntry.Present(TomlKey("header"), TomlExp.TomlValue(config.header.toString)),
      if (config.module == "Native") TomlEntry.Absent else TomlEntry.Present(TomlKey("module"), TomlExp.TomlValue(config.module)),
      config.spec.map(path => TomlEntry.Present(TomlKey("spec"), TomlExp.TomlValue(path.toString))).getOrElse(TomlEntry.Absent),
      if (config.includePaths.isEmpty) TomlEntry.Absent else TomlEntry.Present(TomlKey("include"), TomlExp.TomlArray(config.includePaths.map(p => TomlExp.TomlValue(p.toString)))),
      if (config.defines.isEmpty) TomlEntry.Absent else TomlEntry.Present(TomlKey("define"), TomlExp.TomlArray(config.defines.map(TomlExp.TomlValue.apply))),
      if (config.cflags.isEmpty) TomlEntry.Absent else TomlEntry.Present(TomlKey("cflag"), TomlExp.TomlArray(config.cflags.map(TomlExp.TomlValue.apply))),
    )
    TomlSection("bindings.native", entries)
  }

  private def mkWasmBindingSections(manifest: Manifest): List[TomlSection] =
    manifest.bindings.wasm.map(mkWasmBindingSection)

  private def mkWasmBindingSection(config: WasmBindingConfig): TomlSection = {
    val entries = List(
      TomlEntry.Present(TomlKey("wit"), TomlExp.TomlValue(config.witDir.toString)),
      TomlEntry.Present(TomlKey("world"), TomlExp.TomlValue(config.world)),
      if (config.module == "Wit") TomlEntry.Absent else TomlEntry.Present(TomlKey("module"), TomlExp.TomlValue(config.module)),
    )
    TomlSection("bindings.wasm", entries)
  }

  private def mkMavenDependencySection(manifest: Manifest) = {
    TomlSection("mvn-dependencies", manifest.mavenDependencies.map(mkMavenDependency))
  }

  private def mkJarDependencySection(manifest: Manifest) = {
    TomlSection("jar-dependencies", manifest.jarDependencies.map(mkJarDependency))
  }

  private def mkFlixDependency(dep: Dependency.FlixPackageDependency): TomlEntry = dep match {
    case dep: Dependency.FlixDependency =>
      val key = TomlKey(dep.identifier)
      val values = dep.sctx match {
        case SecurityContext.Default =>
          // If sctx is default value, don't render the record
          TomlExp.TomlValue(dep.version)

        case sctx => TomlExp.TomlRecord(List(
          TomlEntry.Present(TomlKey("version"), TomlExp.TomlValue(dep.version)),
          TomlEntry.Present(TomlKey("security"), TomlExp.TomlValue(sctx)),
        ))
      }
      TomlEntry.Present(key, values)

    case dep: Dependency.PathDependency =>
      val key = TomlKey(dep.identifier)
      val entries = List(
        TomlEntry.Present(TomlKey("path"), TomlExp.TomlValue(dep.path.toString)),
        TomlEntry.Present(TomlKey("security"), TomlExp.TomlValue(dep.sctx)),
      )
      TomlEntry.Present(key, TomlExp.TomlRecord(entries))
  }

  private def mkMavenDependency(dep: Dependency.MavenDependency): TomlEntry = {
    val key = TomlKey(dep.identifier)
    val value = TomlExp.TomlValue(dep.versionTag)
    TomlEntry.Present(key, value)
  }

  private def mkJarDependency(dep: Dependency.JarDependency): TomlEntry = {
    val key = TomlKey(dep.identifier)
    val value = TomlExp.TomlValue(s"url:${dep.url}")
    TomlEntry.Present(key, value)
  }

  private def formatTomlSection(section0: TomlSection): String = {
    s"""[${section0.section}]
       |${padKeys(section0.entries.collect { case e: TomlEntry.Present => e }).map(formatTomlEntry).mkString(System.lineSeparator())}
       |""".stripMargin
  }

  private def formatTomlArraySection(section0: TomlSection): String = {
    s"""[[${section0.section}]]
       |${padKeys(section0.entries.collect { case e: TomlEntry.Present => e }).map(formatTomlEntry).mkString(System.lineSeparator())}
       |""".stripMargin
  }

  private def formatTomlEntry(entry: TomlEntry): String = entry match {
    case TomlEntry.Absent => ""
    case TomlEntry.Present(key, texp) => s"${formatTomlKey(key)} = ${formatTomlExp(texp)}"
  }

  private def formatTomlExp(exp0: TomlExp): String = exp0 match {
    case TomlExp.TomlValue(v) =>
      val escaped = escape(v.toString)
      s"\"$escaped\""

    case TomlExp.TomlArray(v) =>
      v.map(formatTomlExp).mkString("[", ", ", "]")

    case TomlExp.TomlRecord(List(TomlEntry.Present(_, texp))) =>
      // Special case for record with only one key-value pair: just render the value.
      formatTomlExp(texp)

    case TomlExp.TomlRecord(v) =>
      v.map(formatTomlEntry).mkString("{ ", ", ", " }")
  }

  private def formatTomlKey(key0: TomlKey): String = {
    val padding = List.range(0, key0.padding).map(_ => " ").mkString
    s"\"${key0.k}\"$padding"
  }

  /** Returns the list of entries, where the padding has been adjusted to account for the longest key. */
  private def padKeys(entries: List[TomlEntry.Present]): List[TomlEntry.Present] = {
    val optLongestKey = entries.map(_.key.k.length).maxOption
    optLongestKey match {
      case Some(longestKey) => entries.map {
        case TomlEntry.Present(TomlKey(key, _), texp) => TomlEntry.Present(TomlKey(key, longestKey - key.length), texp)
      }
      case None => entries
    }
  }

  /** Escapes `\` and `"` characters to `\\` and `\"`, respectively. */
  private def escape(str: String): String = {
    str.replace("\\", "\\\\")
      .replace("\"", "\\\"")
  }

  private def formatTarget(target: CompilationTarget): String = target match {
    case CompilationTarget.Jvm => "jvm"
    case CompilationTarget.LlvmNative => "native"
    case CompilationTarget.LlvmWasm => "wasm"
  }

  private def formatEmit(emit: EmitKind): String = emit match {
    case EmitKind.Classes => "classes"
    case EmitKind.Jar => "jar"
    case EmitKind.FatJar => "fatjar"
    case EmitKind.Exe => "exe"
    case EmitKind.StaticLib => "staticlib"
    case EmitKind.SharedLib => "sharedlib"
    case EmitKind.Component => "component"
    case EmitKind.Js => "js"
  }

  private def formatRunner(runner: RunnerKind): String = runner match {
    case RunnerKind.Jvm => "jvm"
    case RunnerKind.Native => "native"
    case RunnerKind.Node => "node"
    case RunnerKind.Browser => "browser"
    case RunnerKind.Wasmtime => "wasmtime"
  }

  private case class TomlSection(section: String, entries: List[TomlEntry])

  private sealed trait TomlEntry

  private object TomlEntry {

    case object Absent extends TomlEntry

    case class Present(key: TomlKey, value: TomlExp) extends TomlEntry

  }

  private case class TomlKey(k: String, padding: Int = 0)

  private sealed trait TomlExp

  private object TomlExp {

    case class TomlValue(v: Any) extends TomlExp

    case class TomlArray(v: List[TomlExp]) extends TomlExp

    case class TomlRecord(v: List[TomlEntry]) extends TomlExp

  }

}
