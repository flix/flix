/*
 * Copyright 2023 Magnus Madsen
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

import ca.uwaterloo.flix.api.Bootstrap.{EXT_CLASS, EXT_FPKG, EXT_JAR, FLIX_TOML, LICENSE, README}
import ca.uwaterloo.flix.api.effectlock.{EffectLock, EffectUpgrade, UseGraph}
import ca.uwaterloo.flix.language.ast.{Scheme, SourceLocation, Symbol, TypedAst}
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.language.phase.HtmlDocumentor
import ca.uwaterloo.flix.runtime.CompilationResult
import ca.uwaterloo.flix.tools.{NativeBindingsTool, ProjectTestDriver, Tester, WasmEffectBindingsTool}
import ca.uwaterloo.flix.tools.pkg.FlixPackageManager.findFlixDependencies
import ca.uwaterloo.flix.tools.pkg.Dependency.{FlixDependency, PathDependency}
import ca.uwaterloo.flix.tools.pkg.github.GitHub
import ca.uwaterloo.flix.tools.pkg.{FlixPackageManager, JarPackageManager, Manifest, ManifestParser, MavenPackageManager, PackageModules, ReleaseError}
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.collection.ListMap
import ca.uwaterloo.flix.util.{ArtifactNames, BindingsConfig, Build, CompilationTarget, EmitKind, FileOps, Formatter, NativeBindingConfig, NativeCompileConfig, NativeLinkConfig, PkgConfig, Result, RunnerKind, Validation, WasmBindingConfig}
import ca.uwaterloo.flix.api.lsp.Formatter as LspFormatter
import ca.uwaterloo.flix.language.CompilationMessage

import java.io.PrintStream
import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.*
import java.util.zip.{ZipInputStream, ZipOutputStream}
import scala.collection.mutable
import scala.io.StdIn.readLine
import scala.jdk.CollectionConverters.{IterableHasAsScala, IteratorHasAsScala}
import scala.util.{Failure, Success, Using}


object Bootstrap {

  /**
    * Initializes a new flix project at the given path `p`.
    *
    * The project must not already exist.
    */
  def init(p: Path)(implicit out: PrintStream): Result[Unit, BootstrapError] = {
    //
    // Check that the current working directory is usable.
    //
    if (!Files.isDirectory(p) || !Files.isReadable(p) || !Files.isWritable(p)) {
      return Result.Err(BootstrapError.FileError(s"The directory: '$p' is not accessible. Aborting."))
    }

    //
    // Compute the name of the package based on the directory name.
    //
    val packageName = getPackageName(p)

    //
    // Compute all the directories and files we intend to create.
    //
    val sourceDirectory = getSourceDirectory(p)
    val testDirectory = getTestDirectory(p)

    val manifestFile = getManifestFile(p)
    val gitignoreFile = getGitIgnoreFile(p)
    val licenseFile = getLicenseFile(p)
    val readmeFile = getReadmeFile(p)
    val mainSourceFile = getMainSourceFile(p)
    val mainTestFile = getMainTestFile(p)

    //
    // Create the project directories and files.
    //
    FileOps.newDirectoryIfAbsent(sourceDirectory)
    FileOps.newDirectoryIfAbsent(testDirectory)

    FileOps.newFileIfAbsent(manifestFile) {
      s"""[package]
         |name        = "$packageName"
         |description = "test"
         |version     = "0.1.0"
         |flix        = "${Version.CurrentVersion}"
         |authors     = ["John Doe <john@example.com>"]
         |
         |[build]
         |targets = ["jvm"]
         |
         |[run]
         |target = "jvm"
         |
         |[test]
         |target = "jvm"
         |""".stripMargin
    }

    FileOps.newFileIfAbsent(gitignoreFile) {
      s"""*.fpkg
         |*.jar
         |.GITHUB_TOKEN
         |$artifactDirectoryRaw
         |$buildDirectoryRaw
         |$libDirectoryRaw
         |crash_report_*.txt
         |""".stripMargin
    }

    FileOps.newFileIfAbsent(licenseFile) {
      """Enter license information here.
        |""".stripMargin
    }

    FileOps.newFileIfAbsent(readmeFile) {
      s"""# $packageName
         |
         |Enter some useful information.
         |
         |""".stripMargin
    }

    FileOps.newFileIfAbsent(mainSourceFile) {
      """// The main entry point.
        |def main(): Unit \ IO =
        |    println("Hello World!")
        |""".stripMargin
    }

    FileOps.newFileIfAbsent(mainTestFile) {
      """@Test
        |def test01(): Unit =
        |    if (1 + 1 == 2) ()
        |    else bug!("unexpected arithmetic failure")
        |""".stripMargin
    }
    Result.Ok(())
  }

  /** The class file extension. Does not contain leading '.' */
  private val EXT_CLASS: String = "class"

  /** The flix file extension. Does not contain leading '.' */
  private val EXT_FLIX: String = "flix"

  /** The flix package file extension. Does not contain leading '.' */
  private val EXT_FPKG: String = "fpkg"

  /** The jar file extension. Does not contain leading '.' */
  private val EXT_JAR: String = "jar"

  /** The manifest / flix toml file name. */
  private val FLIX_TOML: String = "flix.toml"

  /** The license file name. */
  private val LICENSE: String = "LICENSE.md"

  /** The readme file name. */
  private val README: String = "README.md"

  /**
    * Returns the path to the artifact directory relative to the given path `p`.
    */
  private def getArtifactDirectory(p: Path): Path = p.resolve(s"./$artifactDirectoryRaw").normalize()

  /**
    * The relative path to the artifact directory as a string.
    *
    * N.B.: Use [[getArtifactDirectory]] if possible.
    */
  private val artifactDirectoryRaw: String = "artifact/"

  /**
    * Returns the path to the library directory relative to the given path `p`.
    */
  def getLibraryDirectory(p: Path): Path = p.resolve(s"./$libDirectoryRaw").normalize()

  /**
    * The relative path to the library directory as a string.
    *
    * N.B.: Use [[getLibraryDirectory]] if possible.
    */
  private val libDirectoryRaw: String = "lib/"

  /**
    * Returns the path to the source directory relative to the given path `p`.
    */
  private def getSourceDirectory(p: Path): Path = p.resolve("./src/").normalize()

  /**
    * Returns the path to the test directory relative to the given path `p`.
    */
  private def getTestDirectory(p: Path): Path = p.resolve("./test/").normalize()

  /**
    * Returns the path to the build directory relative to the given path `p`.
    */
  private def getBuildDirectory(p: Path): Path = p.resolve(s"./$buildDirectoryRaw").normalize()

  def getBuildTargetDirectory(p: Path, target: CompilationTarget): Path = target match {
    case CompilationTarget.Jvm => getBuildDirectory(p)
    case CompilationTarget.LlvmNative => getBuildDirectory(p).resolve("./native/").normalize()
    case CompilationTarget.LlvmWasm => getBuildDirectory(p).resolve("./wasm/").normalize()
  }

  /**
    * The relative path to the build directory as a string.
    *
    * N.B.: Use [[getBuildDirectory]] if possible.
    */
  private val buildDirectoryRaw: String = "build/"

  /**
    * Returns the directory of the output .class-files relative to the given path `p`.
    */
  private def getClassDirectory(p: Path): Path = getBuildDirectory(p).resolve("./class/").normalize()

  /**
    * Returns the directory of the generated documentation files relative to the given path `p`.
    */
  private def getDocumentationDirectory(p: Path): Path = getBuildDirectory(p).resolve("./doc/").normalize()

  /**
    * Returns the path to the artifact directory relative to the given path `p`.
    */
  private def getResourcesDirectory(p: Path): Path = p.resolve("./resources/").normalize()

  /**
    * Returns the path to the `effects.lock` relative to the given path `p`.
    */
  private def getEffectLockFile(p: Path): Path = p.resolve("effects.lock").normalize()

  /**
    * Returns the path to the LICENSE file relative to the given path `p`.
    */
  private def getLicenseFile(p: Path): Path = p.resolve(s"./$LICENSE").normalize()

  /**
    * Returns the path to the README file relative to the given path `p`.
    */
  private def getReadmeFile(p: Path): Path = p.resolve(s"./$README").normalize()

  /**
    * Returns the path to the main source file relative to the given path `p`.
    */
  private def getMainSourceFile(p: Path): Path = getSourceDirectory(p).resolve("./Main.flix").normalize()

  /**
    * Returns the path to the main test file relative to the given path `p`.
    */
  private def getMainTestFile(p: Path): Path = getTestDirectory(p).resolve("./TestMain.flix").normalize()

  /**
    * Returns the path to the Manifest file relative to the given path `p`.
    */
  private def getManifestFile(p: Path): Path = p.resolve(s"./$FLIX_TOML").normalize()

  /**
    * Returns the path to the .gitignore file relative to the given path `p`.
    */
  private def getGitIgnoreFile(p: Path): Path = p.resolve("./.gitignore").normalize()

  /**
    * Returns the path to the jar file based on the given path `p`.
    */
  private def getJarFile(p: Path): Path = getArtifactDirectory(p).resolve(getPackageName(p) + s".$EXT_JAR").normalize()

  /**
    * Returns the package name based on the given path `p`.
    */
  private def getPackageName(p: Path): String = p.toAbsolutePath.normalize().getFileName.toString

  /**
    * Returns the path to the pkg file based on the given path `p`.
    */
  private def getPkgFile(p: Path): Path = getArtifactDirectory(p).resolve(getPackageName(p) + s".$EXT_FPKG").normalize()

  /**
    * Returns `true` if the given path `p` is a jar-file.
    */
  private def isJarFile(p: Path): Boolean = p.normalize().getFileName.toString.endsWith(s".$EXT_JAR") && FileOps.isZipArchive(p)

  /**
    * Returns `true` if the given path `p` is a fpkg-file.
    */
  private def isPkgFile(p: Path): Boolean = p.normalize().getFileName.toString.endsWith(s".$EXT_FPKG") && FileOps.isZipArchive(p)

  /**
    * Creates a new Bootstrap object and initializes it.
    * If a `flix.toml` file exists, parses that to a Manifest and
    * downloads all required files. Otherwise, checks the /lib directory
    * to see what dependencies are already downloaded. Also finds
    * all .flix source files.
    * Then returns the initialized Bootstrap object or an error.
    */
  def bootstrap(path: Path, apiKey: Option[String])(implicit formatter: Formatter, out: PrintStream): Result[Bootstrap, BootstrapError] = {
    //
    // Determine the mode: If `path/flix.toml` exists then "project" mode else "directory mode".
    //
    val bootstrap = new Bootstrap(path, apiKey)
    val tomlPath = getManifestFile(path)
    if (Files.exists(tomlPath)) {
      out.println(s"Found '${formatter.blue(FLIX_TOML)}'. Checking dependencies...")
      bootstrap.projectMode().map(_ => bootstrap)
    } else {
      out.println(s"""No '${formatter.blue(FLIX_TOML)}'. Will load source files from '${formatter.blue(s"*.$EXT_FLIX")}', '${formatter.blue("src/**")}', and '${formatter.blue("test/**")}'.""")
      bootstrap.directoryMode().map(_ => bootstrap)
    }
  }
}

class Bootstrap(val projectPath: Path, apiKey: Option[String]) {

  // The `flix.toml` manifest if in project mode, otherwise `None`
  private var optManifest: Option[Manifest] = None

  // Timestamps at the point the sources were loaded
  private var timestamps: Map[Path, Long] = Map.empty

  // Lists of paths to the source files, flix packages and .jar files used
  private var sourcePaths: List[Path] = List.empty
  private var generatedSourcePaths: Map[CompilationTarget, List[Path]] = Map.empty
  private var flixPackagePaths: List[Path] = List.empty
  private var mavenPackagePaths: List[Path] = List.empty
  private var jarPackagePaths: List[Path] = List.empty
  private var generatedNativeCompileConfig: NativeCompileConfig = NativeCompileConfig()
  private var installedFlixDependencies: List[InstalledFlixDependency] = List.empty

  private var securityLevels: Map[Path, SecurityContext] = Map.empty

  private case class GeneratedBindingState(sourcePaths: List[Path],
                                           nativeCompileConfig: NativeCompileConfig = NativeCompileConfig())

  private case class InstalledFlixDependency(manifest: Manifest,
                                             security: SecurityContext,
                                             packagePath: Option[Path],
                                             extractedRoot: Path)

  def buildTargets: List[CompilationTarget] =
    optManifest.map(_.buildConfig.targets).getOrElse(List(CompilationTarget.Jvm))

  def runTarget: Option[CompilationTarget] =
    optManifest.flatMap(_.runConfig.target)

  def testTarget: Option[CompilationTarget] =
    optManifest.flatMap(_.testConfig.target)

  def runRunner: Option[RunnerKind] =
    optManifest.flatMap(_.runConfig.runner)

  def testRunner: Option[RunnerKind] =
    optManifest.flatMap(_.testConfig.runner)

  def buildEmits(target: CompilationTarget): Option[List[EmitKind]] =
    optManifest.flatMap(_.targetConfigs.emitFor(target))

  def artifactName: String =
    optManifest.map(_.name).getOrElse(projectPath.toAbsolutePath.normalize().getFileName.toString)

  def nativeLinkConfig: NativeLinkConfig =
    optManifest.map(_.targetConfigs.native.link).map(resolveNativeLinkConfig).getOrElse(NativeLinkConfig()) ++ dependencyNativeLinkConfig

  def nativeCompileConfig: NativeCompileConfig =
    optManifest.map(_.targetConfigs.native.compile).map(resolveNativeCompileConfig).getOrElse(NativeCompileConfig()) ++ dependencyNativeCompileConfig ++ generatedNativeCompileConfig

  def bindingsConfig: BindingsConfig =
    optManifest.map(_.bindings).map(resolveBindingsConfig).getOrElse(BindingsConfig())

  private lazy val nativePkgConfigResolution: Result[PkgConfig.Resolution, BootstrapError] =
    PkgConfig.resolve(nativeLinkConfig.pkgConfigPackages, projectPath)
      .mapErr(BootstrapError.GeneralError.apply)

  /**
    * Parses `flix.toml` to a Manifest and downloads all required files.
    * Then makes a list of all flix source files, flix packages
    * and .jar files that this project uses.
    */
  private def projectMode()(implicit formatter: Formatter, out: PrintStream): Result[Unit, BootstrapError] = {
    val tomlPath = Bootstrap.getManifestFile(projectPath)
    for {
      manifest <- Steps.parseManifest(tomlPath)
      deps <- Steps.resolveFlixDependencies(manifest)
      _ <- Steps.installDependencies(deps)
      _ = Steps.addLocalFlixFiles()
    } yield {
      ()
    }
  }

  /**
    * Checks the /lib directory to find existing flix packages and .jar files.
    * Then makes a list of all flix source files, flix packages
    * and .jar files that this project uses.
    */
  private def directoryMode(): Result[Unit, BootstrapError] = {
    Steps.addLocalFlixFiles()
    Steps.addLocalLibs()
    Result.Ok(())
  }

  private def resolvePathAgainst(root: Path, path: Path): Path =
    if (path.isAbsolute) path.normalize()
    else root.resolve(path).normalize()

  private def resolveProjectPath(path: Path): Path =
    resolvePathAgainst(projectPath, path)

  private def resolveDependencyPath(root: Path, path: Path): Path =
    resolvePathAgainst(root, path)

  private def dependencyNativeLinkConfig: NativeLinkConfig =
    installedFlixDependencies.foldLeft(NativeLinkConfig()) {
      case (acc, dep) => acc ++ resolveDependencyNativeLinkConfig(dep)
    }

  private def dependencyNativeCompileConfig: NativeCompileConfig =
    installedFlixDependencies.foldLeft(NativeCompileConfig()) {
      case (acc, dep) => acc ++ resolveDependencyNativeCompileConfig(dep)
    }

  private def resolveNativeLinkConfig(config: NativeLinkConfig): NativeLinkConfig =
    resolveNativeLinkConfigAt(projectPath, config)

  private def resolveNativeCompileConfig(config: NativeCompileConfig): NativeCompileConfig =
    resolveNativeCompileConfigAt(projectPath, config)

  private def resolveBindingsConfig(config: BindingsConfig): BindingsConfig =
    resolveBindingsConfigAt(projectPath, config)

  private def resolveNativeBindingConfig(config: NativeBindingConfig): NativeBindingConfig =
    resolveNativeBindingConfigAt(projectPath, config)

  private def resolveWasmBindingConfig(config: WasmBindingConfig): WasmBindingConfig =
    resolveWasmBindingConfigAt(projectPath, config)

  private def resolveNativeLinkConfigAt(root: Path, config: NativeLinkConfig): NativeLinkConfig =
    config.copy(
      searchPaths = config.searchPaths.map(resolvePathAgainst(root, _)),
      frameworkSearchPaths = config.frameworkSearchPaths.map(resolvePathAgainst(root, _))
    )

  private def resolveNativeCompileConfigAt(root: Path, config: NativeCompileConfig): NativeCompileConfig =
    config.copy(
      sources = config.sources.map(resolvePathAgainst(root, _)),
      includePaths = config.includePaths.map(resolvePathAgainst(root, _))
    )

  private def resolveBindingsConfigAt(root: Path, config: BindingsConfig): BindingsConfig =
    config.copy(
      native = config.native.map(resolveNativeBindingConfigAt(root, _)),
      wasm = config.wasm.map(resolveWasmBindingConfigAt(root, _))
    )

  private def resolveNativeBindingConfigAt(root: Path, config: NativeBindingConfig): NativeBindingConfig =
    config.copy(
      header = resolvePathAgainst(root, config.header),
      spec = config.spec.map(resolvePathAgainst(root, _)),
      includePaths = config.includePaths.map(resolvePathAgainst(root, _))
    )

  private def resolveWasmBindingConfigAt(root: Path, config: WasmBindingConfig): WasmBindingConfig =
    config.copy(
      witDir = resolvePathAgainst(root, config.witDir)
    )

  private def resolveDependencyNativeLinkConfig(dep: InstalledFlixDependency): NativeLinkConfig =
    dep.manifest.targetConfigs.native.link.copy(
      searchPaths = dep.manifest.targetConfigs.native.link.searchPaths.map(resolveDependencyPath(dep.extractedRoot, _)),
      frameworkSearchPaths = dep.manifest.targetConfigs.native.link.frameworkSearchPaths.map(resolveDependencyPath(dep.extractedRoot, _)),
    )

  private def resolveDependencyNativeCompileConfig(dep: InstalledFlixDependency): NativeCompileConfig =
    dep.manifest.targetConfigs.native.compile.copy(
      sources = dep.manifest.targetConfigs.native.compile.sources.map(resolveDependencyPath(dep.extractedRoot, _)),
      includePaths = dep.manifest.targetConfigs.native.compile.includePaths.map(resolveDependencyPath(dep.extractedRoot, _)),
    ) ++ packagedDependencyBindingCompileConfig(dep)

  private def packagedDependencyBindingCompileConfig(dep: InstalledFlixDependency): NativeCompileConfig =
    dep.manifest.bindings.native.zipWithIndex.foldLeft(NativeCompileConfig()) {
      case (acc, (config, index)) =>
        acc ++ packagedDependencyBindingCompileState(dep.extractedRoot, config, index)
    }

  private def packagedDependencyBindingCompileState(extractedRoot: Path, config: NativeBindingConfig, index: Int): NativeCompileConfig = {
    val outDir = packagedDependencyNativeBindingOutDir(extractedRoot, config, index)
    val shimFile = outDir.resolve("native").resolve(s"${config.module}_shim.c").normalize()
    val shimHeaderFile = outDir.resolve("native").resolve("include").resolve(config.header.getFileName.toString).normalize()
    if (!Files.isRegularFile(shimFile)) {
      NativeCompileConfig()
    } else {
      NativeCompileConfig(
        sources = List(shimFile),
        includePaths = (shimHeaderFile.getParent :: resolveDependencyPath(extractedRoot, config.header).getParent :: config.includePaths.map(resolveDependencyPath(extractedRoot, _))).distinct,
        cflags = config.defines.map(d => s"-D$d") ::: config.cflags,
      )
    }
  }

  private def packagedDependencyNativeBindingOutDir(extractedRoot: Path, config: NativeBindingConfig, index: Int): Path =
    extractedRoot.resolve("build").resolve("native").resolve("generated").resolve("bindings").resolve("native").resolve(f"${index}%02d-${sanitizeGeneratedSegment(config.module)}").normalize()

  private def sanitizeGeneratedSegment(segment: String): String =
    segment.replaceAll("[^A-Za-z0-9._-]", "_")

  private def applyProjectTargetConfig(options: ca.uwaterloo.flix.util.Options): ca.uwaterloo.flix.util.Options = {
    val (nativeLinks, nativeCompile) = options.target match {
      case CompilationTarget.LlvmNative => (nativeLinkConfig, nativeCompileConfig)
      case _ => (NativeLinkConfig(), NativeCompileConfig())
    }
    options.copy(
      artifactName = artifactName,
      nativeLinkConfig = nativeLinks,
      nativeCompileConfig = nativeCompile
    )
  }

  /**
    * Builds (compiles) the source files for the project.
    */
  def build(flix: Flix, build: Build = Build.Development, includeTests: Boolean = false): Result[CompilationResult, BootstrapError] = {
    // We disable incremental compilation to ensure a clean compile.
    val newOptions = applyProjectTargetConfig(flix.options.copy(
      build = build,
      incremental = false,
      outputJvm = true,
      outputPath = Bootstrap.getBuildTargetDirectory(projectPath, flix.options.target),
    ))
    flix.setOptions(newOptions)

    // We also clear any cached ASTs.
    flix.clearCaches()

    for {
      _ <- Steps.updateStaleSources(flix, includeTests = includeTests, forceReload = true)
      result <- Steps.compile(flix)
    } yield result
  }

  /**
    * Builds a jar package for the project.
    */
  def buildJar(flix: Flix): Result[Unit, BootstrapError] = {
    val jarFile = Bootstrap.getJarFile(projectPath)
    flix.setOptions(applyProjectTargetConfig(flix.options))
    for {
      _ <- Steps.updateStaleSources(flix, forceReload = true)
      _ <- Steps.configureJarOutput(flix)
      _ <- Steps.compile(flix)
      _ <- Steps.validateJarFile(jarFile)
      contents = (zip: ZipOutputStream) => {
        Steps.addClassFilesFromDirToZip(Bootstrap.getClassDirectory(projectPath), zip)
        Steps.addResourcesFromDirToZip(Bootstrap.getResourcesDirectory(projectPath), zip)
      }
      _ <- Steps.createJar(jarFile, contents)
    } yield {
      ()
    }
  }

  /**
    * Builds a fatjar package for the project.
    */
  def buildFatJar(flix: Flix): Result[Unit, BootstrapError] = {
    val jarFile = Bootstrap.getJarFile(projectPath)
    val libDir = Bootstrap.getLibraryDirectory(projectPath)
    flix.setOptions(applyProjectTargetConfig(flix.options))
    for {
      _ <- Steps.updateStaleSources(flix, forceReload = true)
      _ <- Steps.configureJarOutput(flix)
      _ <- Steps.compile(flix)
      _ <- Steps.validateJarFile(jarFile)
      _ <- Steps.validateDirectory(libDir)
      _ <- Steps.validateJarFilesIn(libDir)
      contents = (zip: ZipOutputStream) => {
        Steps.addClassFilesFromDirToZip(Bootstrap.getClassDirectory(projectPath), zip)
        Steps.addResourcesFromDirToZip(Bootstrap.getResourcesDirectory(projectPath), zip)
        Steps.addJarsFromDirToZip(libDir, zip)
      }
      _ <- Steps.createJar(jarFile, contents)
    } yield {
      ()
    }
  }

  /**
    * Builds a flix package for the project.
    */
  def buildPkg()(implicit formatter: Formatter): Result[Unit, BootstrapError] = {

    // Check that there is a `flix.toml` file.
    if (!Files.exists(Bootstrap.getManifestFile(projectPath))) {
      return Result.Err(BootstrapError.FileError(s"Cannot create a Flix package without a `${formatter.red(FLIX_TOML)}` file."))
    }

    // Create the artifact directory, if it does not exist.
    Files.createDirectories(Bootstrap.getArtifactDirectory(projectPath))

    // The path to the fpkg file.
    val pkgFile = Bootstrap.getPkgFile(projectPath)

    // Check whether it is safe to write to the file.
    if (Files.exists(pkgFile) && !Bootstrap.isPkgFile(pkgFile)) {
      return Result.Err(BootstrapError.FileError(s"The path '${formatter.red(pkgFile.toString)}' exists and is not a $EXT_FPKG-file. Refusing to overwrite."))
    }

    // Copy the `flix.toml` to the artifact directory.
    Files.copy(Bootstrap.getManifestFile(projectPath), Bootstrap.getArtifactDirectory(projectPath).resolve(FLIX_TOML), StandardCopyOption.REPLACE_EXISTING)

    val manifest = optManifest.getOrElse {
      return Result.Err(BootstrapError.FileError(s"Cannot create a Flix package without a parsed `${formatter.red(FLIX_TOML)}` manifest."))
    }

    if (manifest.flixDependencies.exists(_.isInstanceOf[PathDependency])) {
      return Result.Err(BootstrapError.FileError("Cannot create a Flix package with local path dependencies. Publishable packages must use released dependencies."))
    }

    val generatedBindingFiles = Steps.packageGeneratedBindingFiles() match {
      case Ok(files) => files
      case Err(e) => return Err(e)
    }

    val srcFiles = FileOps.getFlixFilesIn(Bootstrap.getSourceDirectory(projectPath), Int.MaxValue)
    val interopAssetFiles = packagedInteropAssetFiles(manifest) match {
      case Ok(files) => files
      case Err(e) => return Err(e)
    }
    val packagedFiles = (srcFiles ::: interopAssetFiles ::: generatedBindingFiles).distinct.filter(Files.isRegularFile(_))

    // Construct a new zip file.
    Using(new ZipOutputStream(Files.newOutputStream(pkgFile))) { zip =>
      // Add required resources.
      FileOps.addToZip(zip, FLIX_TOML, Bootstrap.getManifestFile(projectPath))
      FileOps.addToZip(zip, LICENSE, Bootstrap.getLicenseFile(projectPath))
      FileOps.addToZip(zip, README, Bootstrap.getReadmeFile(projectPath))

      // Add all package files deterministically.
      for ((file, fileNameWithSlashes) <- FileOps.sortPlatformIndependently(projectPath, packagedFiles)) {
        FileOps.addToZip(zip, fileNameWithSlashes, file)
      }
    } match {
      case Success(()) => Result.Ok(())
      case Failure(e) => Result.Err(BootstrapError.FileError(e.getMessage))
    }
  }

  private def packagedInteropAssetFiles(manifest: Manifest): Result[List[Path], BootstrapError] = {
    val resolvedNativeCompile = resolveNativeCompileConfig(manifest.targetConfigs.native.compile)
    val resolvedBindings = resolveBindingsConfig(manifest.bindings)

    val filePaths =
      resolvedNativeCompile.sources :::
        resolvedBindings.native.flatMap(config => config.header :: config.spec.toList) :::
        resolvedBindings.native.flatMap(_.includePaths) :::
        resolvedBindings.wasm.map(_.witDir) :::
        resolvedNativeCompile.includePaths

    collectPackagedFiles(filePaths.distinct)
  }

  private def collectPackagedFiles(paths: List[Path]): Result[List[Path], BootstrapError] =
    Result.traverse(paths) { path =>
      val normalized = path.normalize()
      if (!Files.exists(normalized)) {
        Err(BootstrapError.FileError(s"Cannot package interop asset '$normalized': the path does not exist."))
      } else if (!normalized.startsWith(projectPath.normalize())) {
        Err(BootstrapError.FileError(s"Cannot package interop asset '$normalized': the path is outside the project root '$projectPath'."))
      } else if (Files.isDirectory(normalized)) {
        Ok(FileOps.getFilesIn(normalized, Int.MaxValue))
      } else {
        Ok(List(normalized))
      }
    }.map(_.flatten.distinct)

  /**
    * Returns `Ok(())` if the dependencies are consistent with the `effects.lock` file.
    * Returns `Err(e)` if an error `e` occurred or if the dependencies are inconsistent with the `effect.lock` file.
    */
  def checkEffects(flix: Flix): Result[Unit, BootstrapError] = {
    if (!isProjectMode) {
      return Err(BootstrapError.FileError("No 'flix.toml' found. Refusing to run 'eff-check'"))
    }

    FileOps.exists(Bootstrap.getEffectLockFile(projectPath)) match {
      case Err(e) => return Err(BootstrapError.FileError(s"IO error: ${e.getMessage}"))
      case Ok(false) => return Err(BootstrapError.FileError("No 'effects.lock' file found. Unable to run 'eff-check'."))
      case Ok(true) => ()
    }

    for {
      _ <- Steps.updateStaleSources(flix, forceReload = true)
      json <- FileOps.readString(Bootstrap.getEffectLockFile(projectPath)).mapErr(e => BootstrapError.FileError(s"IO error: ${e.getMessage}"))
      (lockedDefs, lockedSigs) <- EffectLock.deserialize(json).mapErr(BootstrapError.FileError.apply)
      root <- Steps.check(flix)
      errors <- reportEffectUpgradeErrors(lockedDefs, lockedSigs, root)(flix)
    } yield {
      errors
    }
  }

  /**
    * Helper function for [[checkEffects]] to be used in for comprehension.
    *
    * Returns `Ok(())` if no effect upgrade errors are found.
    * Returns `Err(BootstrapError.EffectUpgradeError(errors))` otherwise.
    */
  private def reportEffectUpgradeErrors(lockedDefs: Map[Symbol.DefnSym, Scheme], lockedSigs: Map[Symbol.SigSym, Scheme], root: TypedAst.Root)(implicit flix: Flix): Result[Unit, BootstrapError] = {
    // Compute the inverted use graph to get `f -> g` if `f` is used in `g`.
    val useGraph = ListMap.from(UseGraph.computeGraph(root).invert.map {
      case (UseGraph.UsedSym.DefnSym(f), UseGraph.UsedSym.DefnSym(g)) => f.toString -> g.loc
      case (UseGraph.UsedSym.DefnSym(f), UseGraph.UsedSym.SigSym(g)) => f.toString -> g.loc
      case (UseGraph.UsedSym.SigSym(f), UseGraph.UsedSym.DefnSym(g)) => f.toString -> g.loc
      case (UseGraph.UsedSym.SigSym(f), UseGraph.UsedSym.SigSym(g)) => f.toString -> g.loc
    })

    // N.B.: We erase the keys of the maps to strings, since maps are invariant in the key
    val erasedLockedDefs = lockedDefs.map { case (sym, scheme) => sym.toString -> scheme }
    val erasedUpgradedDefs = root.defs.map { case (sym, defn) => sym.toString -> defn.spec.declaredScheme }
    val erasedLockedSigs = lockedSigs.map { case (sym, scheme) => sym.toString -> scheme }
    val erasedUpgradedSigs = root.sigs.map { case (sym, sig) => sym.toString -> sig.spec.declaredScheme }
    val defnErrors = collectUpgradeErrors(erasedLockedDefs, erasedUpgradedDefs, useGraph)
    val sigErrors = collectUpgradeErrors(erasedLockedSigs, erasedUpgradedSigs, useGraph)
    val allErrors = defnErrors ::: sigErrors

    if (allErrors.isEmpty) {
      Ok(())
    } else {
      Err(BootstrapError.EffectUpgradeError(allErrors))
    }
  }

  /**
    * Collects a list of tuples `(sym, scheme, uses)` if function represented by `sym` is not an effect safe upgrade.
    */
  private def collectUpgradeErrors(lockedFunctions: Map[String, Scheme], upgradeFunctions: Map[String, Scheme], useGraph: ListMap[String, SourceLocation])(implicit flix: Flix): List[(String, Scheme, List[SourceLocation])] = {
    val errors = mutable.ArrayBuffer.empty[(String, Scheme, List[SourceLocation])]
    for ((sym, lockedScheme) <- lockedFunctions) {
      if (upgradeFunctions.contains(sym)) {
        val upgradedScheme = upgradeFunctions(sym)
        val uses = useGraph.get(sym)
        if (!(uses.isEmpty || EffectUpgrade.isEffSafeUpgrade(lockedScheme, upgradedScheme)(flix))) {
          errors.addOne((sym, upgradedScheme, uses))
        }
      }
    }
    errors.toList
  }

  /**
    * Type checks the program and performs effect locking, overwriting the current 'effects.lock' file if it exists.
    * If the program does not type check, then effect locking is aborted without touching the file system.
    */
  def lockEffects(flix: Flix): Result[Unit, BootstrapError] = {
    if (!isProjectMode) {
      return Err(BootstrapError.FileError("No 'flix.toml' found. Refusing to run 'eff-lock'"))
    }
    for {
      _ <- Steps.updateStaleSources(flix, forceReload = true)
      root <- Steps.check(flix)
    } yield {
      EffectLock.lock(root) match {
        case Err(e) => return Err(BootstrapError.GeneralError(s"Unexpected serialization error: $e"))
        case Ok(json) =>
          val path = Bootstrap.getEffectLockFile(projectPath)
          // N.B.: Do not use FileOps.writeJSON, since we use custom serialization formats.
          FileOps.writeString(path, json)
      }
    }
  }

  /** Returns `true` if in project mode. This is the case when a `flix.toml` file is present. */
  private def isProjectMode: Boolean = optManifest.isDefined

  /**
    * Deletes all compiled `.class` files under the project's build directory and removes any now-empty
    * directories (including the `build` directory itself). Performs safety checks to ensure:
    *  - the current directory is a Flix project (manifest present),
    *  - no root or home directories are targeted,
    *  - no ancestor of the project directory is targeted,
    *  - every file in the build directory has a `.class` extension and is a valid class file.
    *
    * Returns `Ok(())` on success or `Err(BootstrapError.FileError(...))` on validation or IO failures.
    */
  def clean(): Result[Unit, BootstrapError] = {
    // Ensure project mode
    if (optManifest.isEmpty) {
      return Err(BootstrapError.FileError("No manifest found (flix.toml). Refusing to run 'clean' in a non-project directory."))
    }

    // Ensure `cwd` is not dangerous
    val cwd = Path.of(System.getProperty("user.dir"))
    checkForSystemPath(cwd) match {
      case Err(e) => return Err(e)
      case Ok(()) => ()
    }

    // Ensure `projectPath` is not dangerous
    checkForSystemPath(projectPath) match {
      case Err(e) => return Err(e)
      case Ok(()) => ()
    }

    val buildDir = Bootstrap.getBuildDirectory(projectPath)
    val classDir = Bootstrap.getClassDirectory(projectPath)
    val docDir = Bootstrap.getDocumentationDirectory(projectPath)
    val nativeDir = Bootstrap.getBuildTargetDirectory(projectPath, CompilationTarget.LlvmNative)
    val wasmDir = Bootstrap.getBuildTargetDirectory(projectPath, CompilationTarget.LlvmWasm)

    // Ensure `buildDir` is not dangerous
    checkForDangerousPath(buildDir) match {
      case Err(e) => return Err(e)
      case Ok(()) => ()
    }

    // Ensure all files in `buildDir` are valid class files.
    val files = FileOps.getFilesIn(buildDir, Int.MaxValue).map(_.normalize())
    for (file <- files) {
      if (file.startsWith(classDir)) {
        if (!FileOps.checkExt(file, "class")) {
          return Err(BootstrapError.FileError(s"Unexpected file extension in build directory (only '.class' files are allowed): '${projectPath.relativize(file)}'"))
        }

        if (!FileOps.isClassFile(file)) {
          return Err(BootstrapError.FileError(s"Invalid class file in build directory: '${projectPath.relativize(file)}'"))
        }
      } else if (file.startsWith(docDir)) {
        isValidDocumentFile(file) match {
          case Err(e) => return Err(e)
          case Ok(()) => ()
        }
      } else if (file.startsWith(nativeDir) || file.startsWith(wasmDir)) {
        ()
      } else {
        return Err(BootstrapError.FileError(s"Unexpected directory in build directory: '${projectPath.relativize(file)}'"))
      }

      checkForDangerousPath(file) match {
        case Err(e) => return Err(e)
        case Ok(()) => ()
      }
    }

    // Delete files
    for (file <- files) {
      FileOps.delete(file) match {
        case Err(e) => return Err(BootstrapError.FileError(s"Failed to delete file '$file': $e"))
        case Ok(_) => ()
      }
    }

    // Delete empty directories
    // Visit in reverse order to delete the innermost directories first
    val directories = FileOps.getDirectoriesIn(buildDir, Int.MaxValue).map(_.normalize())
    for (dir <- directories.reverse) {
      checkForDangerousPath(dir) match {
        case Err(e) => return Err(e)
        case Ok(()) => ()
      }

      FileOps.delete(dir) match {
        case Err(e) => return Err(BootstrapError.FileError(s"Failed to delete directory '$dir': $e"))
        case Ok(_) => ()
      }
    }

    Ok(())
  }

  /**
    * Returns `Err` if `path` is one of the following:
    *   - A root directory of the system
    *   - The user's home directory (`"user.home"` system property, using [[System.getProperty]])
    *   - Any ancestor of [[projectPath]]
    *
    * Returns `Ok(())` otherwise.
    */
  private def checkForDangerousPath(path: Path): Result[Unit, BootstrapError] = {
    checkForSystemPath(path) match {
      case Err(e) => return Err(e)
      case Ok(()) => ()
    }
    checkForAncestor(path) match {
      case Err(e) => return Err(e)
      case Ok(()) => ()
    }
    Ok(())
  }

  /** Returns `Err` if `path` is either a root directory or the user's home directory.
    *
    * @see [[checkForRootDir]]
    * @see [[checkForHomeDir]]
    */
  private def checkForSystemPath(path: Path): Result[Unit, BootstrapError] = {
    checkForRootDir(path) match {
      case Err(e) => return Err(e)
      case Ok(()) => ()
    }
    checkForHomeDir(path) match {
      case Err(e) => return Err(e)
      case Ok(()) => ()
    }
    Ok(())
  }

  /** Returns `Err` if `path` is the user's home directory. */
  private def checkForHomeDir(path: Path): Result[Unit, BootstrapError] = {
    val home = Path.of(System.getProperty("user.home"))
    if (home.normalize() == path.normalize()) {
      return Err(BootstrapError.FileError("Refusing to run 'clean' in home directory."))
    }
    Ok(())
  }

  /** Returns `Err` if `path` is a root directory. */
  private def checkForRootDir(path: Path): Result[Unit, BootstrapError] = {
    val roots = FileSystems.getDefault.getRootDirectories.asScala.toList.map(_.normalize())
    if (roots.contains(path.normalize())) {
      return Err(BootstrapError.FileError("Refusing to run 'clean' in root directory."))
    }
    Ok(())
  }

  /** Returns `Err` if `path` is an ancestor of `projectPath`. */
  private def checkForAncestor(path: Path): Result[Unit, BootstrapError] = {
    if (projectPath.normalize().startsWith(path.normalize())) {
      return Err(BootstrapError.FileError(s"Refusing to run clean in ancestor of project directory: '${path.normalize()}"))
    }
    Ok(())
  }

  /** Returns `Err` if `path` is not a file that could be produced by [[HtmlDocumentor]]. */
  private def isValidDocumentFile(path: Path): Result[Unit, BootstrapError] = {
    val knownFiles = List("favicon.png", "index.js", "styles.css")
    if (knownFiles.contains(path.getFileName.toString)) {
      return Ok(())
    }
    if (FileOps.checkExt(path, "html")) {
      return Ok(())
    }
    val iconsDir = Bootstrap.getDocumentationDirectory(projectPath).resolve("./icons/").normalize()
    if (path.startsWith(iconsDir) && FileOps.checkExt(path, "svg")) {
      return Ok(())
    }

    Err(BootstrapError.FileError(s"Unexpected file '${projectPath.relativize(path)}'. Refusing to run 'clean'."))
  }

  /**
    * Type checks the source files for the project.
    */
  def check(flix: Flix): Result[Unit, BootstrapError] = {
    flix.setOptions(applyProjectTargetConfig(flix.options))
    Steps.updateStaleSources(flix, forceReload = true).flatMap(_ => Steps.check(flix).map(_ => ()))
  }

  /**
    * Checks to see if any source files or packages have been changed.
    * If they have, they are added to flix. Then updates the timestamps
    * map to reflect the current source files and packages.
    */
  def reconfigureFlix(flix: Flix): Result[Unit, BootstrapError] = {
    // TODO: Figure out if this function can be removed somehow (maybe by removing shell depending on bootstrap)
    // TODO: Can be removed by moving `updateStaleSources` into all step functions that require updating stale sources (almost all). This also remove responsibility from the caller.
    flix.setOptions(applyProjectTargetConfig(flix.options))
    Steps.updateStaleSources(flix, forceReload = true)
  }

  /**
    * Generates API documentation.
    */
  def doc(flix: Flix): Result[Unit, BootstrapError] = {
    flix.setOptions(applyProjectTargetConfig(flix.options))
    Steps.updateStaleSources(flix, forceReload = true).flatMap(_ => Steps.check(flix).map(HtmlDocumentor.run(_, getPackageModules)(flix)))
  }

  /**
    * Formats all source files in the project.
    */
  def format(flix: Flix): Result[Unit, BootstrapError] = {
    flix.setOptions(applyProjectTargetConfig(flix.options))
    Steps.updateStaleSources(flix, forceReload = true).flatMap(_ => Steps.check(flix).map {
      case _ =>
        val syntaxTree = flix.getParsedAst
        LspFormatter.formatFiles(syntaxTree, sourcePaths)(flix)
    })
  }

  /**
    * Runs the main function in flix package for the project.
    */
  def run(flix: Flix, args: Array[String]): Result[Unit, BootstrapError] = {
    for {
      compilationResult <- build(flix, includeTests = false)
      main <- compilationResult.getMain match {
        case None => Result.Err(BootstrapError.GeneralError("Project has no main entry point."))
        case Some(main) => Result.Ok(main)
      }
    } yield main(args)
  }

  /**
    * Runs all tests in the flix package for the project.
    */
  def test(flix: Flix, runner: Option[RunnerKind] = None): Result[Unit, BootstrapError] = flix.options.target match {
    case CompilationTarget.Jvm =>
      for {
        compilationResult <- build(flix, includeTests = true)
        res <- Tester.run(Nil, compilationResult)(flix).mapErr(_ => BootstrapError.GeneralError("Tester Error"))
      } yield res

    case CompilationTarget.LlvmNative | CompilationTarget.LlvmWasm =>
      for {
        compilationResult <- buildPortableTestDriver(flix)
        _ <- executePortableTests(compilationResult, flix, runner)
      } yield ()
  }

  /**
    * Package the current project and release it on GitHub.
    */
  def release(flix: Flix)(implicit out: PrintStream): Result[Unit, BootstrapError] = {
    implicit val formatter: Formatter = flix.getFormatter

    // Ensure that we have a manifest
    val manifest = optManifest match {
      case Some(m) => m
      case None => return Result.Err(BootstrapError.ReleaseError(ReleaseError.MissingManifest))
    }

    // Check if `github` option is present
    val githubRepo = manifest.repository match {
      case Some(r) => r
      case None => return Result.Err(BootstrapError.ReleaseError(ReleaseError.MissingRepository))
    }

    // Check if `--github-token` option is present
    val githubToken = flix.options.githubToken match {
      case Some(k) => k
      case None => return Result.Err(BootstrapError.ReleaseError(ReleaseError.MissingApiKey))
    }

    if (!flix.options.assumeYes) {
      // Ask for confirmation
      out.print(s"Release ${formatter.blue(s"github:$githubRepo")} ${formatter.yellow(s"v${manifest.version}")}? [y/N]: ")
      val response = readLine()
      response.toLowerCase match {
        case "y" => // Continue
        case "yes" => // Continue
        case _ => return Result.Err(BootstrapError.ReleaseError(ReleaseError.Cancelled))
      }
    }

    // Build artifacts
    out.println("Building project...")
    buildPkg() match {
      case Ok(_) => // Continue
      case Err(e) => return Result.Err(e)
    }

    // Publish to GitHub
    out.println("Publishing a new release...")
    val artifacts = List(Bootstrap.getPkgFile(projectPath), Bootstrap.getManifestFile(projectPath))
    val publishResult = GitHub.publishRelease(githubRepo, manifest.version, artifacts, githubToken)
    publishResult match {
      case Ok(()) => // Continue
      case Err(e) => return Result.Err(BootstrapError.ReleaseError(e))
    }

    out.println(formatter.green(
      s"""
         |Successfully released v${manifest.version}
         |${formatter.underline(s"https://github.com/${githubRepo.owner}/${githubRepo.repo}/releases/tag/v${manifest.version}")}
         |""".stripMargin
    ))

    Result.Ok(())
  }

  /**
    * Show dependencies which have newer versions available.
    *
    * @return `true` if any outdated dependencies were found, `false` if everything is up to date.
    */
  def outdated(flix: Flix)(implicit out: PrintStream): Result[Boolean, BootstrapError] = {
    implicit val formatter: Formatter = flix.getFormatter

    val flixDeps = optManifest.map(findFlixDependencies).getOrElse(Nil).collect { case dep: FlixDependency => dep }

    val rows = flixDeps.flatMap { dep =>
      val updates = FlixPackageManager.findAvailableUpdates(dep, flix.options.githubToken) match {
        case Ok(u) => u
        case Err(e) => return Result.Err(BootstrapError.FlixPackageError(e))
      }

      if (updates.isEmpty)
        None
      else
        Some(List(
          s"${dep.username}/${dep.projectName}",
          dep.version.toString,
          updates.major.map(v => v.toString).getOrElse(""),
          updates.minor.map(v => v.toString).getOrElse(""),
          updates.patch.map(v => v.toString).getOrElse(""),
        ))
    }

    if (rows.isEmpty) {
      out.println(formatter.green(
        """
          |All dependencies are up to date
          |""".stripMargin
      ))
      Result.Ok(false)
    } else {
      out.println("")
      out.println(formatter.table(
        List("package", "current", "major", "minor", "patch"),
        List(formatter.blue, formatter.cyan, formatter.yellow, formatter.yellow, formatter.yellow),
        rows
      ))
      out.println("")
      Result.Ok(true)
    }
  }

  /**
    * Returns the modules of the package if manifest is present.
    * Returns [[PackageModules.All]] if manifest is not present.
    */
  private def getPackageModules: PackageModules = {
    optManifest match {
      case None => PackageModules.All
      case Some(manifest) => manifest.modules
    }
  }

  private object Steps {

    /**
      * Adds all class files from `dir` to `zip`.
      */
    def addClassFilesFromDirToZip(dir: Path, zip: ZipOutputStream): Unit = {
      // Add all class files.
      // Here we sort entries by relative file name to apply https://reproducible-builds.org/
      val classFiles = FileOps.getFilesWithExtIn(dir, EXT_CLASS, Int.MaxValue)
      for ((buildFile, fileNameWithSlashes) <- FileOps.sortPlatformIndependently(dir, classFiles)) {
        FileOps.addToZip(zip, fileNameWithSlashes, buildFile)
      }
    }

    /**
      * Adds all jars in `dir` to `zip`.
      * Ignores non-jar files and does nothing if `dir` does not exist.
      */
    def addJarsFromDirToZip(dir: Path, zip: ZipOutputStream): Unit = {
      // First, we get all jar files inside the lib folder.
      // If the lib folder doesn't exist, we suppose there is simply no dependency and trigger no error.
      if (!Files.exists(dir)) {
        return
      }
      val jarDependencies = FileOps.getFilesWithExtIn(dir, EXT_JAR, Int.MaxValue)
      // Add jar dependencies.
      jarDependencies.foreach(dep => {
        // Extract the content of the classes to the jar file.
        Using(new ZipInputStream(Files.newInputStream(dep))) {
          zipIn =>
            var entry = zipIn.getNextEntry
            while (entry != null) {
              // Get the class files except module-info and META-INF classes which are specific to each library.
              if (entry.getName.endsWith(s".$EXT_CLASS") && !entry.getName.equals(s"module-info.$EXT_CLASS") && !entry.getName.contains("META-INF/")) {
                // Write extracted class files to zip.
                val classContent = zipIn.readAllBytes()
                FileOps.addToZip(zip, entry.getName, classContent)
              }
              entry = zipIn.getNextEntry
            }
        }
      })
    }

    /**
      * Returns and caches all `.flix` files from `src/` and `test/`.
      */
    def addLocalFlixFiles(): List[Path] = {
      val filesHere = FileOps.getFlixFilesIn(projectPath, 1)
      val filesSrc = FileOps.getFlixFilesIn(Bootstrap.getSourceDirectory(projectPath), Int.MaxValue)
      val filesTest = FileOps.getFlixFilesIn(Bootstrap.getTestDirectory(projectPath), Int.MaxValue)
      val result = filesHere ::: filesSrc ::: filesTest
      sourcePaths = result
      result
    }

    private def refreshGeneratedBindings(target: CompilationTarget): Result[Unit, BootstrapError] = {
      val manifest = optManifest.getOrElse {
        return Result.Ok(())
      }
      val result = refreshGeneratedBindingsFor(projectPath, manifest, target)

      result.map { state =>
        generatedSourcePaths = generatedSourcePaths.updated(target, state.sourcePaths.distinct)
        if (target == CompilationTarget.LlvmNative) {
          generatedNativeCompileConfig = state.nativeCompileConfig
        }
        ()
      }
    }

    def packageGeneratedBindingFiles(): Result[List[Path], BootstrapError] = {
      for {
        nativeFiles <- Result.traverse(bindingsConfig.native.zipWithIndex.toList) {
          case (config, index) =>
            ensureNativeBinding(projectPath, nativePkgConfigResolution, config, index).map(_ => packagedGeneratedFiles(nativeBindingOutDir(projectPath, config, index)))
        }
        wasmFiles <- Result.traverse(bindingsConfig.wasm.zipWithIndex.toList) {
          case (config, index) =>
            ensureWasmBinding(projectPath, config, index).map(_ => packagedGeneratedFiles(wasmBindingOutDir(projectPath, config, index)))
        }
      } yield (nativeFiles.flatten ::: wasmFiles.flatten).distinct
    }

    private def packagedGeneratedFiles(outDir: Path): List[Path] =
      if (!Files.exists(outDir)) Nil
      else FileOps.getFilesIn(outDir, Int.MaxValue).filterNot(_.getFileName.toString == ".flix-bindings.stamp")

    private def localDependencySourceEntries(target: CompilationTarget): Result[List[(Path, SecurityContext)], BootstrapError] =
      Result.traverse(installedFlixDependencies.filter(_.packagePath.isEmpty)) { dep =>
        refreshGeneratedBindingsFor(dep.extractedRoot, dep.manifest, target).map { state =>
          val filesHere = FileOps.getFlixFilesIn(dep.extractedRoot, 1)
          val filesSrc = FileOps.getFlixFilesIn(Bootstrap.getSourceDirectory(dep.extractedRoot), Int.MaxValue)
          val allFiles = (filesHere ::: filesSrc ::: state.sourcePaths).distinct
          allFiles.map(_ -> dep.security)
        }
      }.map(_.flatten)

    private def refreshGeneratedBindingsFor(root: Path, manifest: Manifest, target: CompilationTarget): Result[GeneratedBindingState, BootstrapError] = {
      val resolvedBindings = resolveBindingsConfigAt(root, manifest.bindings)
      val pkgConfigResolution = PkgConfig.resolve(resolveNativeLinkConfigAt(root, manifest.targetConfigs.native.link).pkgConfigPackages, root)
        .mapErr(BootstrapError.GeneralError.apply)

      target match {
        case CompilationTarget.Jvm =>
          Result.Ok(GeneratedBindingState(Nil))

        case CompilationTarget.LlvmNative =>
          Result.traverse(resolvedBindings.native.zipWithIndex.toList) {
            case (config, index) => ensureNativeBinding(root, pkgConfigResolution, config, index)
          }.map { states =>
            states.foldLeft(GeneratedBindingState(Nil)) {
              case (acc, state) =>
                GeneratedBindingState(
                  sourcePaths = acc.sourcePaths ::: state.sourcePaths,
                  nativeCompileConfig = acc.nativeCompileConfig ++ state.nativeCompileConfig
                )
            }
          }

        case CompilationTarget.LlvmWasm =>
          Result.traverse(resolvedBindings.wasm.zipWithIndex.toList) {
            case (config, index) => ensureWasmBinding(root, config, index)
          }.map { states =>
            GeneratedBindingState(states.flatMap(_.sourcePaths))
          }
      }
    }

    private def ensureNativeBinding(root: Path, pkgConfigResolution: Result[PkgConfig.Resolution, BootstrapError], config: NativeBindingConfig, index: Int): Result[GeneratedBindingState, BootstrapError] = {
      pkgConfigResolution.flatMap { pkgConfig =>
        val effectiveConfig = config.copy(
          cflags = (config.cflags ::: pkgConfig.compile.cflags).distinct
        )

        val outDir = nativeBindingOutDir(root, config, index)
        val stamp = outDir.resolve(".flix-bindings.stamp").normalize()
        val flixFile = outDir.resolve("flix").resolve(s"${config.module}.flix").normalize()
        val shimFile = outDir.resolve("native").resolve(s"${config.module}_shim.c").normalize()
        val shimHeaderFile = outDir.resolve("native").resolve("include").resolve(config.header.getFileName.toString).normalize()
        val fingerprint = nativeBindingFingerprint(config, pkgConfig)
        val hasShimOutputs = Files.exists(shimFile) || Files.exists(shimHeaderFile)
        val expectedOutputs = flixFile :: (if (hasShimOutputs) List(shimFile, shimHeaderFile) else Nil)

        val generated =
          if (bindingOutputsFresh(stamp, fingerprint, nativeBindingInputs(root, config), expectedOutputs)) {
            Result.Ok(())
          } else {
            Files.createDirectories(outDir)
            NativeBindingsTool.run(NativeBindingsTool.Config(
              header = config.header,
              outDir = outDir,
              rootModule = config.module,
              spec = config.spec,
              includePaths = config.includePaths,
              defines = config.defines,
              cflags = effectiveConfig.cflags
            )).mapErr(BootstrapError.GeneralError.apply).map { _ =>
              FileOps.writeString(stamp, fingerprint)
              ()
            }
          }

        generated.map { _ =>
          GeneratedBindingState(
            sourcePaths = List(flixFile),
            nativeCompileConfig = generatedNativeCompileState(root, effectiveConfig, shimFile, shimHeaderFile),
          )
        }
      }
    }

    private def ensureWasmBinding(root: Path, config: WasmBindingConfig, index: Int): Result[GeneratedBindingState, BootstrapError] = {
      val outDir = wasmBindingOutDir(root, config, index)
      val stamp = outDir.resolve(".flix-bindings.stamp").normalize()
      val flixFile = outDir.resolve("flix").resolve(s"${config.module}.flix").normalize()
      val bindingsFile = outDir.resolve("manifest").resolve("wit-effect-bindings.json").normalize()
      val jsFile = outDir.resolve("js").resolve("index.mjs").normalize()
      val dtsFile = outDir.resolve("js").resolve("index.d.ts").normalize()
      val jsBrowserHostStubFile = outDir.resolve("js").resolve("browser-host.stub.mjs").normalize()
      val jsPackageFile = outDir.resolve("js").resolve("package.json").normalize()
      val rustFile = outDir.resolve("rust").resolve("src").resolve("wit_effect_bindings.rs").normalize()
      val rustLibFile = outDir.resolve("rust").resolve("src").resolve("lib.rs").normalize()
      val rustHostStubFile = outDir.resolve("rust").resolve("examples").resolve("host_stub.rs").normalize()
      val rustCargoTomlFile = outDir.resolve("rust").resolve("Cargo.toml").normalize()
      val readmeFile = outDir.resolve("README.md").normalize()
      val fingerprint = wasmBindingFingerprint(config)
      val expectedOutputs = List(
        flixFile,
        bindingsFile,
        jsFile,
        dtsFile,
        jsBrowserHostStubFile,
        jsPackageFile,
        rustFile,
        rustLibFile,
        rustHostStubFile,
        rustCargoTomlFile,
        readmeFile,
      )

      val generated =
        if (bindingOutputsFresh(stamp, fingerprint, List(config.witDir), expectedOutputs)) {
          Result.Ok(())
        } else {
          Files.createDirectories(outDir)
          WasmEffectBindingsTool.run(WasmEffectBindingsTool.Config(
            witDir = config.witDir,
            world = config.world,
            outDir = outDir,
            rootModule = config.module,
          )).mapErr(BootstrapError.GeneralError.apply).map { _ =>
            FileOps.writeString(stamp, fingerprint)
            ()
          }
        }

      generated.map(_ => GeneratedBindingState(List(flixFile)))
    }

    private def generatedNativeCompileState(root: Path, config: NativeBindingConfig, shimFile: Path, shimHeaderFile: Path): NativeCompileConfig = {
      if (!Files.isRegularFile(shimFile)) {
        NativeCompileConfig()
      } else {
        NativeCompileConfig(
          sources = List(shimFile),
          includePaths = (shimHeaderFile.getParent :: config.header.getParent :: config.includePaths.map(resolvePathAgainst(root, _))).distinct,
          cflags = config.defines.map(d => s"-D$d") ::: config.cflags,
        )
      }
    }

    private def nativeBindingInputs(root: Path, config: NativeBindingConfig): List[Path] =
      (config.header :: config.spec.toList ::: config.includePaths.filter(path => path.normalize().startsWith(root.normalize()))).distinct

    private def nativeBindingOutDir(root: Path, config: NativeBindingConfig, index: Int): Path =
      generatedBindingsRoot(root, CompilationTarget.LlvmNative).resolve("native").resolve(f"${index}%02d-${sanitizeGeneratedSegment(config.module)}").normalize()

    private def wasmBindingOutDir(root: Path, config: WasmBindingConfig, index: Int): Path =
      generatedBindingsRoot(root, CompilationTarget.LlvmWasm).resolve("wasm").resolve(f"${index}%02d-${sanitizeGeneratedSegment(config.module)}").normalize()

    private def generatedBindingsRoot(root: Path, target: CompilationTarget): Path =
      Bootstrap.getBuildTargetDirectory(root, target).resolve("generated").resolve("bindings").normalize()

    private def sanitizeGeneratedSegment(segment: String): String =
      segment.replaceAll("[^A-Za-z0-9._-]", "_")

    private def nativeBindingFingerprint(config: NativeBindingConfig, pkgConfig: PkgConfig.Resolution): String =
      List(
        s"header=${config.header.toAbsolutePath.normalize()}",
        s"module=${config.module}",
        s"spec=${config.spec.map(_.toAbsolutePath.normalize()).getOrElse("<none>")}",
        s"include=${config.includePaths.map(_.toAbsolutePath.normalize()).mkString(";")}",
        s"define=${config.defines.mkString(";")}",
        s"cflag=${config.cflags.mkString(";")}",
        s"pkg-cflags=${pkgConfig.compile.cflags.mkString(";")}",
        s"pkg-libs=${pkgConfig.link.flags.mkString(";")}",
      ).mkString("\n")

    private def wasmBindingFingerprint(config: WasmBindingConfig): String =
      List(
        s"schema=${WasmEffectBindingsTool.OutputSchemaVersion}",
        s"wit=${config.witDir.toAbsolutePath.normalize()}",
        s"world=${config.world}",
        s"module=${config.module}",
      ).mkString("\n")

    private def bindingOutputsFresh(stamp: Path, fingerprint: String, inputs: List[Path], outputs: List[Path]): Boolean = {
      if (!Files.isRegularFile(stamp) || outputs.exists(path => !Files.exists(path))) {
        return false
      }

      val recordedFingerprint = Files.readString(stamp, StandardCharsets.UTF_8)
      if (recordedFingerprint != fingerprint) {
        return false
      }

      val stampTime = Files.getLastModifiedTime(stamp).toMillis
      maxInputTimestamp(inputs) <= stampTime
    }

    private def maxInputTimestamp(paths: List[Path]): Long =
      paths.filter(Files.exists(_)).map(pathTimestamp).maxOption.getOrElse(0L)

    private def pathTimestamp(path: Path): Long = {
      if (Files.isDirectory(path)) {
        Using(Files.walk(path)) { stream =>
          stream.iterator().asScala.map(p => Files.getLastModifiedTime(p).toMillis).maxOption.getOrElse(0L)
        }.getOrElse(0L)
      } else {
        Files.getLastModifiedTime(path).toMillis
      }
    }

    /**
      * Returns and caches all `.fpkg` files from `lib/`.
      * The cached result is stored in [[flixPackagePaths]].
      */
    private def addLocalFlixLibs(): List[Path] = {
      val flixFilesLib = FileOps.getFilesWithExtIn(Bootstrap.getLibraryDirectory(projectPath), EXT_FPKG, Int.MaxValue)
      flixPackagePaths = flixFilesLib
      flixFilesLib
    }

    /**
      * Returns and caches all `.jar` files from `lib/external/`.
      * The cached result is stored in [[jarPackagePaths]].
      */
    private def addLocalJars(): List[Path] = {
      val jarFilesLib = FileOps.getFilesWithExtIn(Bootstrap.getLibraryDirectory(projectPath).resolve(JarPackageManager.DirName), EXT_JAR, Int.MaxValue)
      jarPackagePaths = jarFilesLib
      jarFilesLib
    }

    /**
      * Returns a list of 3 lists of paths.
      * The lists contain the following paths in the following order:
      *   1. All `.jar` files from `lib/cache/`.
      *   1. All `.jar` files from `lib/external/`.
      *   1. All `.fpkg` files from `lib/`.
      *
      * All results are cached in [[mavenPackagePaths]], [[jarPackagePaths]], and [[flixPackagePaths]], respectively.
      */
    def addLocalLibs(): List[List[Path]] = {
      addLocalMavenJars() :: addLocalJars() :: addLocalFlixLibs() :: Nil
    }

    /**
      * Returns and caches all `.jar` files from `lib/cache/`.
      * The cached result is stored in [[mavenPackagePaths]].
      */
    private def addLocalMavenJars(): List[Path] = {
      val mavenFilesLib = FileOps.getFilesWithExtIn(Bootstrap.getLibraryDirectory(projectPath).resolve(MavenPackageManager.DirName), EXT_JAR, Int.MaxValue)
      mavenPackagePaths = mavenFilesLib
      mavenFilesLib
    }

    /**
      * Adds a `META-INF/MANIFEST.MF` file to `zip`.
      */
    private def addManifestToZip(zip: ZipOutputStream): Unit = {
      val manifest =
        """Manifest-Version: 1.0
          |Main-Class: Main
          |""".stripMargin

      FileOps.addToZip(zip, "META-INF/MANIFEST.MF", manifest.getBytes)
    }

    /**
      * Adds all files in `dir` to `zip`.
      */
    def addResourcesFromDirToZip(dir: Path, zip: ZipOutputStream): Unit = {
      // Add all resources, again sorting by relative file name
      val resources = FileOps.getFilesIn(dir, Int.MaxValue)
      for ((resource, fileNameWithSlashes) <- FileOps.sortPlatformIndependently(dir, resources)) {
        FileOps.addToZip(zip, fileNameWithSlashes, resource)
      }
    }

    /**
      * Type checks the source files for the project.
      */
    def check(flix: Flix): Result[TypedAst.Root, BootstrapError] = {
      val (optRoot, errors) = flix.check()
      if (errors.isEmpty) {
        Ok(optRoot.get)
      } else {
        Err(BootstrapError.GeneralError(CompilationMessage.formatAll(errors)(flix.getFormatter, optRoot)))
      }
    }

    /**
      * Runs the compile function on the `flix` object.
      * It is up to the caller to set the appropriate options on `flix`.
      * It is often the case that `outputJvm` and `loadClassFiles` must be toggled on or off.
      */
    def compile(flix: Flix): Result[CompilationResult, BootstrapError] = {
      val (optRoot, errors) = flix.check()
      if (errors.isEmpty) {
        Ok(flix.codeGen(optRoot.get))
      } else {
        Err(BootstrapError.GeneralError(CompilationMessage.formatAll(errors)(flix.getFormatter, optRoot)))
      }
    }

    /**
      * Configures `flix` to emit class files to the build directory (on the file system)
      * in production mode.
      *
      * @see [[Bootstrap.getBuildDirectory]]
      * @see [[Build.Production]]
      */
    def configureJarOutput(flix: Flix): Result[Unit, BootstrapError] = {
      val buildDir = Bootstrap.getBuildDirectory(projectPath)
      for {
        _ <- validateDirectory(buildDir)
      } yield {
        val newOptions = flix.options.copy(build = Build.Production, outputJvm = true, outputPath = buildDir)
        flix.setOptions(newOptions)
        ()
      }
    }

    /**
      * Writes `contents` to the jar file located at `jar`.
      *
      * This function also adds a manifest to the jar file.
      *
      * Creates the jar file if it does not exist, and truncates it if it already exists.
      *
      * @see [[Steps.addManifestToZip]]
      */
    def createJar(jar: Path, contents: ZipOutputStream => Unit): Result[Unit, BootstrapError.FileError] = {
      Files.createDirectories(jar.getParent.normalize())
      val contentsWithManifest = (zip: ZipOutputStream) => {
        Steps.addManifestToZip(zip)
        contents(zip)
      }
      Result.fromTry(Using(new ZipOutputStream(Files.newOutputStream(jar)))(contentsWithManifest))
        .mapErr(e => BootstrapError.FileError(e.getMessage))
    }

    /**
      * Returns true if the timestamp of the given source file has changed since the last reload.
      */
    private def hasChanged(file: Path) = {
      !timestamps.contains(file) || (timestamps(file) != file.toFile.lastModified())
    }

    /**
      * Downloads and installs all `.fpkg` and `.jar` (maven and urls) dependencies defined by `dependencyManifests`
      * into the `lib/`, `lib/cache`, and `lib/external` directories, respectively.
      * Requires network access.
      * Returns a list of 3 lists of paths containing (in the following order):
      *   1. Paths to `.fpkg` dependencies in `lib/`.
      *   1. Paths to `.jar` dependencies in `lib/cache` (maven).
      *   1. Paths to `.jar` dependencies in `lib/external` (urls).
      */
    def installDependencies(resolution: FlixPackageManager.SecureResolution)(implicit formatter: Formatter, out: PrintStream): Result[List[List[Path]], BootstrapError] = {
      for {
        flixPaths <- installFlixDependencies(resolution)
        mavenPaths <- installMavenDependencies(resolution.manifests)
        jarPaths <- installJarDependencies(resolution.manifests)
      } yield {
        out.println("Dependency resolution completed.")
        List(flixPaths, mavenPaths, jarPaths)
      }
    }

    /**
      * Downloads and installs all `.fpkg` dependencies defined by `dependencyManifests` into the `lib/` directory.
      * Requires network access.
      * Returns the paths to the installed dependencies.
      */
    private def installFlixDependencies(resolution: FlixPackageManager.SecureResolution)(implicit formatter: Formatter, out: PrintStream): Result[List[Path], BootstrapError] = {
      FlixPackageManager.installAll(resolution, projectPath, apiKey) match {
        case Ok(result: List[(Path, SecurityContext)]) =>
          securityLevels = result.toMap
          flixPackagePaths = result.map { case (path, _) => path }
          collectInstalledFlixDependencies(resolution) match {
            case Ok(deps) =>
              installedFlixDependencies = deps
              Ok(flixPackagePaths)
            case Err(e) => Err(e)
          }
        case Err(e) =>
          Err(BootstrapError.FlixPackageError(e))
      }
    }

    private def collectInstalledFlixDependencies(resolution: FlixPackageManager.SecureResolution): Result[List[InstalledFlixDependency], BootstrapError] =
      Result.traverse(resolution.manifestToFlixDeps.map(identity).toList) {
        case (manifest, dep: FlixDependency) =>
          val packagePath = installedFlixPackagePath(dep)
          val extractedRoot = installedFlixDependencyRoot(dep)
          ensureExtractedPackage(packagePath, extractedRoot).map(_ =>
            InstalledFlixDependency(
              manifest = manifest,
              security = resolution.security(manifest),
              packagePath = Some(packagePath),
              extractedRoot = extractedRoot,
            )
          )

        case (manifest, _: PathDependency) =>
          resolution.manifestRoots.get(manifest) match {
            case Some(root) =>
              Ok(InstalledFlixDependency(
                manifest = manifest,
                security = resolution.security(manifest),
                packagePath = None,
                extractedRoot = root,
              ))
            case None =>
              Err(BootstrapError.GeneralError(s"Missing local dependency root for manifest '${manifest.name}'."))
          }
      }

    private def installedFlixPackagePath(dep: ca.uwaterloo.flix.tools.pkg.Dependency.FlixDependency): Path =
      Bootstrap.getLibraryDirectory(projectPath)
        .resolve("github")
        .resolve(dep.username)
        .resolve(dep.projectName)
        .resolve(dep.version.toString)
        .resolve(s"${dep.projectName}-${dep.version}.fpkg")
        .normalize()

    private def installedFlixDependencyRoot(dep: ca.uwaterloo.flix.tools.pkg.Dependency.FlixDependency): Path =
      Bootstrap.getBuildDirectory(projectPath)
        .resolve("dependency-packages")
        .resolve("github")
        .resolve(dep.username)
        .resolve(dep.projectName)
        .resolve(dep.version.toString)
        .normalize()

    private def ensureExtractedPackage(packagePath: Path, extractedRoot: Path): Result[Unit, BootstrapError] = {
      val stamp = extractedRoot.resolve(".flix-package-extract.stamp").normalize()
      val fingerprint = s"${Files.getLastModifiedTime(packagePath).toMillis}:${Files.size(packagePath)}"
      if (Files.isRegularFile(stamp) && Files.readString(stamp, StandardCharsets.UTF_8) == fingerprint) {
        return Ok(())
      }

      deleteDirectoryIfExists(extractedRoot)
      Files.createDirectories(extractedRoot)

      Result.fromTry(Using(new ZipInputStream(Files.newInputStream(packagePath))) { zipIn =>
        var entry = zipIn.getNextEntry
        while (entry != null) {
          if (!entry.isDirectory) {
            val target = extractedRoot.resolve(entry.getName).normalize()
            if (!target.startsWith(extractedRoot)) {
              throw new IllegalStateException(s"Refusing to extract package entry outside destination root: '${entry.getName}'.")
            }
            Files.createDirectories(target.getParent)
            Files.write(target, zipIn.readAllBytes())
          }
          entry = zipIn.getNextEntry
        }
      }).mapErr(e => BootstrapError.FileError(e.getMessage)).map { _ =>
        FileOps.writeString(stamp, fingerprint)
      }
    }

    private def deleteDirectoryIfExists(path: Path): Unit = {
      if (!Files.exists(path)) return
      Using(Files.walk(path)) { stream =>
        stream.iterator().asScala.toList
          .sortBy(_.getNameCount)(Ordering.Int.reverse)
          .foreach(Files.deleteIfExists(_))
      }.get
    }

    /**
      * Downloads and installs all `.jar` dependencies defined by `dependencyManifests` into the `lib/external/` directory.
      * Requires network access.
      * Returns the paths to the installed dependencies.
      */
    private def installJarDependencies(dependencyManifests: List[Manifest])(implicit out: PrintStream): Result[List[Path], BootstrapError] = {
      JarPackageManager.installAll(dependencyManifests, projectPath) match {
        case Ok(paths) =>
          jarPackagePaths = paths
          Ok(paths)
        case Err(e) =>
          Err(BootstrapError.JarPackageError(e))
      }
    }

    /**
      * Downloads and installs all `.jar` dependencies defined by `dependencyManifests` into the `lib/cache/` directory.
      * Requires network access.
      * Returns the paths to the installed dependencies.
      */
    private def installMavenDependencies(dependencyManifests: List[Manifest])(implicit formatter: Formatter, out: PrintStream): Result[List[Path], BootstrapError] = {
      MavenPackageManager.installAll(dependencyManifests, projectPath) match {
        case Ok(paths) =>
          mavenPackagePaths = paths
          Ok(paths)
        case Err(e) =>
          Err(BootstrapError.MavenPackageError(e))
      }
    }

    /**
      * Parses and returns the manifest at `tomlPath`.
      */
    def parseManifest(tomlPath: Path): Result[Manifest, BootstrapError] = {
      ManifestParser.parse(tomlPath) match {
        case Ok(manifest) =>
          optManifest = Some(manifest)
          Ok(manifest)
        case Err(e) =>
          Err(BootstrapError.ManifestParseError(e))
      }
    }

    /**
      * Returns flix manifests of all dependencies of `manifest`. This includes transitive dependencies.
      * Requires network access.
      */
    def resolveFlixDependencies(manifest: Manifest)(implicit formatter: Formatter, out: PrintStream): Result[FlixPackageManager.SecureResolution, BootstrapError] = {
      FlixPackageManager.findTransitiveDependencies(manifest, projectPath, apiKey).map(FlixPackageManager.resolveSecurityLevels) match {
        case Err(e) => Err(BootstrapError.FlixPackageError(e))
        case Ok(securityMap) =>
          val securityResolutionErrors = FlixPackageManager.checkSecurity(securityMap)
          if (securityResolutionErrors.isEmpty) {
            Ok(securityMap)
          } else {
            Err(BootstrapError.GeneralError(securityResolutionErrors.map(_.message(formatter)).mkString(System.lineSeparator())))
          }
      }
    }

    /**
      * Checks to see if any source files or packages have been changed.
      * If they have, they are added to flix. Then updates the timestamps
      * map to reflect the current source files and packages.
      */
    def updateStaleSources(flix: Flix, includeTests: Boolean = true, forceReload: Boolean = false): Result[Unit, BootstrapError] = {
      for {
        _ <- refreshGeneratedBindings(flix.options.target)
        dependencySourceEntries <- localDependencySourceEntries(flix.options.target)
      } yield {
        flix.setOptions(applyProjectTargetConfig(flix.options))

        val localSourcePaths =
          if (includeTests) sourcePaths
          else sourcePaths.filterNot(isTestSourcePath)
        val selectedSourceEntries =
          localSourcePaths.map(_ -> SecurityContext.Unrestricted) :::
            generatedSourcePaths.getOrElse(flix.options.target, Nil).map(_ -> SecurityContext.Unrestricted) :::
            dependencySourceEntries

        val previousSources = timestamps.keySet

        for ((path, sctx) <- selectedSourceEntries if forceReload || hasChanged(path)) {
          flix.addFile(path)(sctx)
        }

        for (path <- flixPackagePaths if forceReload || hasChanged(path)) {
          flix.addPkg(path)(securityLevels.getOrElse(path, SecurityContext.Plain))
        }

        for (path <- mavenPackagePaths if forceReload || hasChanged(path)) {
          flix.addJar(path)
        }

        for (path <- jarPackagePaths if forceReload || hasChanged(path)) {
          flix.addJar(path)
        }

        val currentSources = (selectedSourceEntries.map(_._1) ::: flixPackagePaths ::: mavenPackagePaths ::: jarPackagePaths).filter(p => Files.exists(p))

        val deletedSources = previousSources -- currentSources
        for (path <- deletedSources) {
          flix.remFile(path)(securityLevels.getOrElse(path, SecurityContext.Unrestricted))
        }

        securityLevels = securityLevels ++ dependencySourceEntries.toMap
        timestamps = currentSources.map(f => f -> f.toFile.lastModified).toMap
      }
    }

    private def isTestSourcePath(path: Path): Boolean =
      path.normalize().startsWith(Bootstrap.getTestDirectory(projectPath))

    /**
      * Returns `OK(())` if `dir` exists and is a readable directory.
      * If `dir` does not exist, it returns `Ok(())` too.
      */
    def validateDirectory(dir: Path): Result[Unit, BootstrapError] = {
      if (Files.exists(dir)) {
        if (!Files.isDirectory(dir)) {
          return Err(BootstrapError.FileError(s"The path '${dir.toString}' is not a directory."))
        }
        if (!Files.isReadable(dir)) {
          return Err(BootstrapError.FileError(s"The path '${dir.toString}' is not readable."))
        }
      }
      Ok(())
    }

    /**
      * Returns `Ok(())` if `jarFile` exists and is a readable jar file (a zip archive).
      * If `jarFile` does not exist, it also returns `Ok(())`.
      *
      * @see [[Bootstrap.isJarFile]]
      */
    def validateJarFile(jarFile: Path): Result[Unit, BootstrapError] = {
      if (Files.exists(jarFile) && !Bootstrap.isJarFile(jarFile)) {
        return Err(BootstrapError.FileError(s"The path '${jarFile.toString}' exists and is not a jar-file."))
      }
      Ok(())
    }

    /**
      * Returns `Ok(())` if all files ending with `.jar` in `dir` are valid jar files.
      *
      * @see [[Steps.validateJarFile]]
      */
    def validateJarFilesIn(dir: Path): Result[Unit, BootstrapError] = {
      Result.traverse(FileOps.getFilesWithExtIn(dir, EXT_JAR, Int.MaxValue))(Steps.validateJarFile).map(_ => ())
    }

  }

  private def defaultRunnerKind(target: CompilationTarget): RunnerKind = target match {
    case CompilationTarget.Jvm => RunnerKind.Jvm
    case CompilationTarget.LlvmNative => RunnerKind.Native
    case CompilationTarget.LlvmWasm => RunnerKind.Node
  }

  private def buildPortableTestDriver(flix: Flix): Result[CompilationResult, BootstrapError] = {
    val newOptions = applyProjectTargetConfig(flix.options.copy(
      build = Build.Development,
      entryPoint = None,
      incremental = false,
      outputJvm = true,
      outputPath = Bootstrap.getBuildTargetDirectory(projectPath, flix.options.target),
    ))
    flix.setOptions(newOptions)
    flix.clearCaches()

    implicit val sctx: SecurityContext = SecurityContext.Unrestricted

    for {
      _ <- Steps.updateStaleSources(flix, includeTests = true, forceReload = true)
      root <- Steps.check(flix)
      driver = ProjectTestDriver.mkDriverSource(ProjectTestDriver.collectProjectTests(root))(flix)
      _ = {
        val driverPath = Bootstrap.getBuildTargetDirectory(projectPath, flix.options.target).resolve("__FlixProjectTestDriver.flix").normalize()
        flix.remVirtualPath(driverPath)
        flix.addVirtualPath(driverPath, driver)
        flix.setOptions(flix.options.copy(entryPoint = Some(ProjectTestDriver.EntryPointSym)))
      }
      compilationResult <- Steps.compile(flix)
    } yield compilationResult
  }

  private def executePortableTests(compilationResult: CompilationResult,
                                   flix: Flix,
                                   runner: Option[RunnerKind]): Result[Unit, BootstrapError] = {
    val selectedRunner = runner.getOrElse(defaultRunnerKind(flix.options.target))

    flix.options.target match {
      case CompilationTarget.LlvmWasm if selectedRunner == RunnerKind.Wasmtime =>
        WasmRunSupport.runWasmtime(flix.options, projectPath, Array.empty) match {
          case Right(_) => Result.Ok(())
          case Left(msg) => Result.Err(BootstrapError.GeneralError(msg))
        }

      case CompilationTarget.LlvmWasm if selectedRunner == RunnerKind.Browser =>
        BrowserRunSupport.runBrowser(flix.options, projectPath, Array.empty, headless = true) match {
          case Right(_) => Result.Ok(())
          case Left(msg) => Result.Err(BootstrapError.GeneralError(msg))
        }

      case _ =>
        compilationResult.getMain match {
          case Some(main) =>
            try {
              main(Array.empty)
              Result.Ok(())
            } catch {
              case ex: Throwable =>
                Result.Err(BootstrapError.GeneralError(Option(ex.getMessage).getOrElse(ex.toString)))
            }
          case None =>
            Result.Err(BootstrapError.GeneralError("Generated test driver has no main entry point."))
        }
    }
  }
}
