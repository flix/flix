/*
 * Copyright 2015-2016 Magnus Madsen
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

import ca.uwaterloo.flix.language.ast.Symbol

import java.nio.file.Path

object Options {
  /**
    * Default options.
    */
  val Default: Options = Options(
    lib = LibLevel.All,
    stdlibProfile = StdlibProfile.Jvm,
    target = CompilationTarget.Jvm,
    artifactName = ArtifactNames.DefaultBaseName,
    emits = Set.empty,
    nativeLinkConfig = NativeLinkConfig(),
    nativeCompileConfig = NativeCompileConfig(),
    build = Build.Development,
    entryPoint = None,
    githubToken = None,
    installDeps = false,
    incremental = true,
    json = false,
    outputJvm = false,
    outputPath = Path.of("./build/"),
    progress = false,
    threads = Runtime.getRuntime.availableProcessors(),
    loadClassFiles = true,
    assumeYes = false,
    xprintphases = false,
    xnodeprecated = false,
    xsummary = false,
    xsubeffecting = Set.empty,
    XPerfN = None,
    XPerfFrontend = false,
    XPerfPar = false,
    xchaosMonkey = false
  )

  /**
    * Default test options.
    */
  val DefaultTest: Options = Default.copy(lib = LibLevel.All, progress = false, xnodeprecated = true, xchaosMonkey = true)

  /**
    * Default test options with the standard library.
    */
  val TestWithLibAll: Options = DefaultTest

  /**
    * Default test options with the minimal library.
    */
  val TestWithLibMin: Options = DefaultTest.copy(lib = LibLevel.Min)

  /**
    * Default test options without any library.
    */
  val TestWithLibNix: Options = DefaultTest.copy(lib = LibLevel.Nix)
}

/**
  * General Flix options.
  *
  * @param lib            selects the level of libraries to include.
  * @param build          selects development or production mode.
  * @param entryPoint     specifies the main entry point.
  * @param githubToken    the API key to use for GitHub dependency resolution.
  * @param incremental    enables incremental compilation.
  * @param installDeps    enables automatic installation of dependencies.
  * @param json           enable json output.
  * @param outputJvm      Enable JVM bytecode output.
  * @param outputPath     The path to the output folder.
  * @param progress       print progress during compilation.
  * @param threads        selects the number of threads to use.
  * @param loadClassFiles loads the generated class files into the JVM.
  * @param assumeYes      run non-interactively and assume answer to all prompts is yes.
  */
case class Options(lib: LibLevel,
                   stdlibProfile: StdlibProfile,
                   target: CompilationTarget,
                   artifactName: String,
                   emits: Set[EmitKind] = Set.empty,
                   nativeLinkConfig: NativeLinkConfig = NativeLinkConfig(),
                   nativeCompileConfig: NativeCompileConfig = NativeCompileConfig(),
                   build: Build,
                   entryPoint: Option[Symbol.DefnSym],
                   githubToken: Option[String],
                   incremental: Boolean,
                   installDeps: Boolean,
                   json: Boolean,
                   progress: Boolean,
                   outputJvm: Boolean,
                   outputPath: Path,
                   threads: Int,
                   loadClassFiles: Boolean,
                   assumeYes: Boolean,
                   xprintphases: Boolean,
                   xnodeprecated: Boolean,
                   xsummary: Boolean,
                   xsubeffecting: Set[Subeffecting],
                   XPerfFrontend: Boolean,
                   XPerfPar: Boolean,
                   XPerfN: Option[Int],
                   xchaosMonkey: Boolean
                  )

/**
  * An option to control whether to run in development or production mode.
  */
sealed trait Build

object Build {
  /**
    * Run in development mode.
    */
  case object Development extends Build

  /**
    * Run in production mode.
    *
    * Running the compiler in production mode disables certain features that are allowed during development.
    */
  case object Production extends Build
}

sealed trait LibLevel

object LibLevel {

  /**
    * Do not include any libraries, even those essential for basic functionality.
    */
  case object Nix extends LibLevel

  /**
    * Only include essential libraries.
    */
  case object Min extends LibLevel

  /**
    * Include the full standard library.
    */
  case object All extends LibLevel

}

/**
  * An option to control which stdlib profile is used.
  */
sealed trait StdlibProfile

object StdlibProfile {
  /**
    * The current JVM stdlib profile (may include Java interop and JVM-only overlays).
    */
  case object Jvm extends StdlibProfile

  /**
    * The portable stdlib profile intended for LLVM targets (no Java interop).
    *
    * Note: this profile can also be used on the JVM backend to run the portable conformance suite.
    */
  case object Portable extends StdlibProfile
}

/**
  * An option to control which compilation target is used.
  */
sealed trait CompilationTarget

object CompilationTarget {
  /**
    * Compile to JVM bytecode and execute on the JVM.
    */
  case object Jvm extends CompilationTarget

  /**
    * Compile to LLVM IR intended for native targets (x86_64/aarch64).
    */
  case object LlvmNative extends CompilationTarget

  /**
    * Compile to LLVM IR intended for WebAssembly.
    */
  case object LlvmWasm extends CompilationTarget
}

sealed trait EmitKind

object EmitKind {
  case object Classes extends EmitKind
  case object Jar extends EmitKind
  case object FatJar extends EmitKind
  case object Exe extends EmitKind
  case object StaticLib extends EmitKind
  case object SharedLib extends EmitKind
  case object Component extends EmitKind
  case object Js extends EmitKind
}

sealed trait RunnerKind

object RunnerKind {
  case object Jvm extends RunnerKind
  case object Native extends RunnerKind
  case object Node extends RunnerKind
  case object Browser extends RunnerKind
  case object Wasmtime extends RunnerKind
}

sealed trait Subeffecting

object Subeffecting {

  /**
    * Enable sub-effecting for module-level definitions.
    */
  case object ModDefs extends Subeffecting

  /**
    * Enable sub-effecting for instance-level defs.
    */
  case object InsDefs extends Subeffecting

  /**
    * Enable sub-effecting for lambda expressions.
    */
  case object Lambdas extends Subeffecting

}
