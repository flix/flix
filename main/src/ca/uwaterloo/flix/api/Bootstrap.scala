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

import ca.uwaterloo.flix.api.Bootstrap.{EFFECT_LOCK_FILE, EXT_CLASS, EXT_FLIX, EXT_FPKG, EXT_JAR, EXT_MANIFEST, FLIX_TOML, LICENSE, README, getExternalJarDirectory, getFlixPackageDir, getFlixPackageFile, getFlixPackageManifestFile, getLibraryDirectory, getManifestFile, getMavenDirectory, getUpgradeBackupDir, libDirectoryRaw}
import ca.uwaterloo.flix.api.effectlock.{EffectLock, EffectUpgrade, UseGraph}
import ca.uwaterloo.flix.api.lsp.FormatterLsp as LspFormatter
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.language.ast.{Scheme, SourceLocation, Symbol, TypedAst}
import ca.uwaterloo.flix.language.phase.HtmlDocumentor
import ca.uwaterloo.flix.runtime.CompilationResult
import ca.uwaterloo.flix.runtime.shell.FileWatcher
import ca.uwaterloo.flix.tools.Tester
import ca.uwaterloo.flix.tools.pkg.github.GitHub
import ca.uwaterloo.flix.tools.pkg.{AvailableUpdates, Dependency, FlixPackageManager, JarPackageManager, Manifest, ManifestParser, MavenPackageManager, PackageModules, ReleaseError, SemVer}
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.collection.ListMap
import ca.uwaterloo.flix.util.{Build, FileOps, Formatter, Result}

import java.io.{BufferedReader, InputStream, InputStreamReader, PrintStream}
import java.nio.file.{FileSystems, Files, Path, StandardCopyOption}
import java.util.zip.{ZipInputStream, ZipOutputStream}
import scala.collection.mutable
import scala.io.Source
import scala.io.StdIn.readLine
import scala.jdk.CollectionConverters.IterableHasAsScala
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
    val workflowsDirectory = getWorkflowsDirectory(p)

    val manifestFile = getManifestFile(p)
    val gitignoreFile = getGitIgnoreFile(p)
    val licenseFile = getLicenseFile(p)
    val readmeFile = getReadmeFile(p)
    val mainSourceFile = getMainSourceFile(p)
    val mainTestFile = getMainTestFile(p)
    val buildAndTestWorkflowFile = getBuildAndTestWorkflowFile(p)

    //
    // Create the project directories and files.
    //
    FileOps.newDirectoryIfAbsent(sourceDirectory)
    FileOps.newDirectoryIfAbsent(testDirectory)
    FileOps.newDirectoryIfAbsent(workflowsDirectory)

    FileOps.newFileIfAbsent(manifestFile) {
      s"""[package]
         |name        = "$packageName"
         |description = "test"
         |version     = "0.1.0"
         |flix        = "${Version.CurrentVersion}"
         |authors     = ["John Doe <john@example.com>"]
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
        |def test01(): Unit \ Assert = Assert.assertEq(expected = 2, 1 + 1)
        |""".stripMargin
    }

    FileOps.newFileIfAbsent(buildAndTestWorkflowFile) {
      """name: Build and Test
        |
        |on:
        |  pull_request:
        |  push:
        |    branches: [ main, master ]
        |
        |jobs:
        |  build-and-test:
        |    runs-on: ubuntu-latest
        |    steps:
        |      - name: Check out
        |        uses: actions/checkout@v5
        |
        |      - name: Install JDK 21
        |        uses: actions/setup-java@v4
        |        with:
        |          distribution: 'temurin'
        |          java-version: '21'
        |
        |      - name: Read Flix version from flix.toml
        |        id: flix
        |        run: |
        |          version=$(grep -E '^"?flix"?[[:space:]]*=' flix.toml \
        |            | head -n1 \
        |            | sed -E 's/.*"([^"]+)"[[:space:]]*$/\1/')
        |          echo "version=$version" >> "$GITHUB_OUTPUT"
        |
        |      - name: Download Flix
        |        run: |
        |          curl -fsSL -o flix.jar \
        |            "https://github.com/flix/flix/releases/download/v${{ steps.flix.outputs.version }}/flix.jar"
        |
        |      - name: Check
        |        run: java -jar flix.jar check
        |
        |      - name: Test
        |        run: java -jar flix.jar test
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

  /** The flix package file extension. Does not contain leading '.' */
  private val EXT_MANIFEST: String = "toml"

  /** The jar file extension. Does not contain leading '.' */
  private val EXT_JAR: String = "jar"

  /** The manifest / flix toml file name. */
  private val FLIX_TOML: String = s"flix.$EXT_MANIFEST"

  private val EFFECT_LOCK_FILE: String = "effects.lock"

  /** The license file name. */
  private val LICENSE: String = "LICENSE.md"

  /** The readme file name. */
  private val README: String = "README.md"

  /** The build-and-test GitHub Actions workflow file name. */
  private val BUILD_AND_TEST_WORKFLOW: String = "build-and-test.yaml"

  /**
    * The relative path to the GitHub Actions workflows directory as a string.
    *
    * N.B.: Use [[getWorkflowsDirectory]] if possible.
    */
  private val workflowsDirectoryRaw: String = ".github/workflows/"

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

  private def getUpgradeBackupDir(p: Path): Path = p.resolve(".flix_upgrade_backup/").normalize()

  private def getFlixPackageDir(p: Path, flixDep: Dependency.FlixDependency): Path =
    p.resolve(flixDep.repo.toString.toLowerCase)
      .resolve(flixDep.username)
      .resolve(flixDep.projectName)
      .resolve(flixDep.version.toString)
      .normalize()

  private def getFlixPackageResource(p: Path, flixDep: Dependency.FlixDependency, fileExtension: String): Path =
    getFlixPackageDir(p, flixDep)
      .resolve(s"${flixDep.projectName}-${flixDep.version}.$fileExtension")
      .normalize()

  private def getFlixPackageFile(p: Path, flixDep: Dependency.FlixDependency): Path =
    getFlixPackageResource(p, flixDep, EXT_FPKG)

  private def getFlixPackageManifestFile(p: Path, flixDep: Dependency.FlixDependency): Path =
    getFlixPackageResource(p, flixDep, EXT_MANIFEST)

  private def getMavenDirectory(p: Path): Path =
    getLibraryDirectory(p).resolve("cache").normalize()

  private def getExternalJarDirectory(p: Path): Path =
    getLibraryDirectory(p).resolve("external").normalize()

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
  private def getEffectLockFile(p: Path): Path = p.resolve(EFFECT_LOCK_FILE).normalize()

  /**
    * Returns the path to the LICENSE file relative to the given path `p`.
    */
  private def getLicenseFile(p: Path): Path = p.resolve(s"./$LICENSE").normalize()

  /**
    * Returns the path to the README file relative to the given path `p`.
    */
  private def getReadmeFile(p: Path): Path = p.resolve(s"./$README").normalize()

  /**
    * Returns the path to the GitHub Actions workflows directory relative to the given path `p`.
    */
  private def getWorkflowsDirectory(p: Path): Path = p.resolve(s"./$workflowsDirectoryRaw").normalize()

  /**
    * Returns the path to the build-and-test workflow file relative to the given path `p`.
    */
  private def getBuildAndTestWorkflowFile(p: Path): Path = getWorkflowsDirectory(p).resolve(s"./$BUILD_AND_TEST_WORKFLOW").normalize()

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
  private var flixPackagePaths: List[Path] = List.empty
  private var mavenPackagePaths: List[Path] = List.empty
  private var jarPackagePaths: List[Path] = List.empty

  private var securityLevels: Map[Path, SecurityContext] = Map.empty

  // The file watcher, if active (used by the REPL shell).
  private var fileWatcher: Option[FileWatcher] = None

  /**
    * Starts a file system watcher that monitors the project directories for changes.
    * When active, `updateStaleSources` will drain watcher events instead of polling timestamps.
    */
  def startWatching(): Unit = {
    val fw = new FileWatcher()
    fw.watchShallow(projectPath)
    // Register these as recursive roots even if they don't exist yet.
    // The watcher will automatically pick them up when they are created.
    fw.watchRecursively(Bootstrap.getSourceDirectory(projectPath))
    fw.watchRecursively(Bootstrap.getTestDirectory(projectPath))
    fw.watchRecursively(Bootstrap.getLibraryDirectory(projectPath))
    fw.start()
    fileWatcher = Some(fw)
  }

  /**
    * Stops the file system watcher, if active.
    */
  def stopWatching(): Unit = {
    fileWatcher.foreach(_.stop())
    fileWatcher = None
  }

  /**
    * Applies any pending file changes to the Flix instance.
    * When the file watcher is active, drains watcher events.
    * Otherwise, falls back to timestamp-based change detection.
    */
  def applyFileChanges(flix: Flix): Unit = {
    Steps.updateStaleSources(flix)
  }

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

  /**
    * Builds (compiles) the source files for the project.
    */
  def build(flix: Flix, build: Build = Build.Development): Result[CompilationResult, BootstrapError] = {
    // We disable incremental compilation to ensure a clean compile.
    val newOptions = flix.options.copy(build = build, incremental = false, outputJvm = true, outputPath = Bootstrap.getBuildDirectory(projectPath))
    flix.setOptions(newOptions)

    // We also clear any cached ASTs.
    flix.clearCaches()

    Steps.updateStaleSources(flix)
    Steps.compile(flix)
  }

  /**
    * Builds a jar package for the project.
    */
  def buildJar(flix: Flix): Result[Unit, BootstrapError] = {
    val jarFile = Bootstrap.getJarFile(projectPath)
    Steps.updateStaleSources(flix)
    for {
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
    * Upgrades `pkgName` to the latest minor version and/or patch.
    *
    * // TODO: Rewrite this documentation. It now takes the packageName and version as argument. If 'latest' then it finds the latest version.
    * Example 1 - Latest minor version if one is available:
    * If package is version 0.1.0, and versions `1.0.0`, `0.3.1`, `0.3.0`, `0.2.0`, and `0.1.1`, then
    * version `0.3.1` is applied.
    * In particular, the major version is ignored.
    *
    * Example 2 - Latest patch version if no greater minor upgrade available:
    * If package is version `0.2.0` and versions `0.2.3`, `0.2.2`, `0.2.1`, `0.2.0`, and `0.1.0`, then
    * version `0.2.3` is applied.
    *
    * Assumes that `this` [[Bootstrap]] instance implies that the manifest was successfully parsed and is up to date.
    *
    * @param pkgName the package identifier to upgrade. The identifier is the key as it appears in the manifest file,
    *                e.g., `github:flix/museum`
    */
  def upgrade(flix: Flix, pkgName: String, semVer: Option[SemVer])(implicit formatter: Formatter, in: InputStream, out: PrintStream): Result[Unit, BootstrapError] = {
    // 1. Ensure project mode
    if (!isProjectMode) {
      return Err(BootstrapError.GeneralError("Refusing to run 'upgrade'. Not in project mode."))
    }

    val oldManifest = optManifest match {
      case None => return Err(BootstrapError.FileError("Refusing to run 'upgrade'. No manifest 'flix.toml' found."))
      case Some(m) => m
    }

    // 2. Ensure project path is not a system directory
    checkForSystemPath(projectPath) match {
      case Err(e) => return Err(e)
      case Ok(()) => ()
    }

    // 3. Ensure cwd is not dangerous (system dir or outside project path)
    val cwd = Path.of(System.getProperty("user.dir"))
    checkForDangerousPath(cwd) match {
      case Err(e) => return Err(e)
      case Ok(()) => ()
    }

    // 4. Check effect lock file exists
    FileOps.exists(Bootstrap.getEffectLockFile(projectPath)) match {
      case Err(e) => return Err(BootstrapError.FileError(s"IO error: ${e.getMessage}"))
      case Ok(false) => return Err(BootstrapError.FileError(s"Refusing to run 'upgrade'. No effect lock file '$EFFECT_LOCK_FILE' found. Run 'eff-lock' to generate one."))
      case Ok(true) => () // Continue
    }

    // 5. Check backup directory from previously aborted upgrade does not exist
    FileOps.exists(Bootstrap.getUpgradeBackupDir(projectPath)) match {
      case Err(e) => return Err(BootstrapError.FileError(s"IO error: ${e.getMessage}"))
      case Ok(true) => return Err(BootstrapError.FileError( // TODO: Refactor this into an error
        "Refusing to run 'upgrade'. " +
          "Backup from earlier aborted 'upgrade' exists." +
          System.lineSeparator() +
          System.lineSeparator() +
          s"Please resolve the issue by manually moving needed files into the '$libDirectoryRaw' directory and removing the backup directory. " +
          System.lineSeparator() +
          s"Backup directory located at: ${Bootstrap.getUpgradeBackupDir(projectPath)}" +
          System.lineSeparator() +
          s"$libDirectoryRaw located at: ${Bootstrap.getLibraryDirectory(projectPath)}"
      ))
      case Ok(false) => () // Continue
    }

    // 4. Check that 'pkgName' occurs as a key in the dependencies declared in the manifest
    val dependency = oldManifest.flixDependencies.find(_.identifier == pkgName) match {
      case None =>
        // TODO: Maybe introduce 'UpgradeError' instead of 'GeneralError'
        return Err(BootstrapError.GeneralError("Refusing to run 'upgrade'. The package is not a declared dependency of the project."))
      case Some(d) => d
    }

    // 5. Check for upgrades
    val update = semVer match {
      case Some(version) if dependency.version == version =>
        // 5(a). The version was specified by the user.
        // Special case: Check `version` is current version before going to network.
        // Perform early return. There is nothing to do.
        out.println(s"Already at version '$version'. Nothing to do.")
        return Ok(())

      case Some(version) =>
        // 5(a). The version was specified by the user.
        // The specified version is different from current version so check network.
        FlixPackageManager.checkForSpecificVersion(dependency, apiKey, version) match {
          case Err(e) => return Err(BootstrapError.FlixPackageError(e))
          case Ok(existingVersion) => existingVersion
        }

      case None =>
        // 5(b). The version was 'latest' so we must check for the latest version.
        val availableUpdates = FlixPackageManager.findAvailableUpdates(dependency, apiKey) match {
          case Err(e) => return Err(BootstrapError.FlixPackageError(e))
          case Ok(u) => u
        }

        findLatestVersion(availableUpdates) match {
          case None =>
            // There was no version higher than the current version. Early return.
            out.println("No upgrade available. Nothing to do.")
            return Ok(())

          case Some(version) if dependency.version == version =>
            // This should be an unreachable case but for sanity, this should be an early return.
            out.println(s"Already at version '$version'. Nothing to do.")
            return Ok(())

          case Some(version) => version
        }
    }

    // 6. Ask for confirmation
    val confirmationMessage =
      s"""A new version was found: ${dependency.version} -> $update
         |Do you want to proceed? [y/N]""".stripMargin

    askForConfirmation(confirmationMessage) match {
      case Err(e) => return Err(e)
      case Ok(false) => return Err(BootstrapError.GeneralError("Refusing to run 'upgrade'. The user declined the upgrade."))
      case Ok(true) => ()
    }

    // 8. Resolve old manifest to obtain dependency graph that we can diff with a new graph
    val oldResolution = Steps.resolveFlixDependencies(oldManifest) match {
      case Err(e) => return Err(e)
      case Ok(r) => r
    }

    // N.B.: Obtain list of old paths (dependency snapshot) before resolving new dependencies since doing so has the side effect of downloading tomls
    // And also snapshot before moving old dependencies away so the list of lib files is not empty
    val oldPaths = FileOps.getFilesIn(getLibraryDirectory(projectPath).resolve("github").normalize(), Int.MaxValue)

    // 9. Update manifest
    val newManifest = oldManifest.copy(
      dependencies = oldManifest.dependencies.map {
        case dep: Dependency.FlixDependency if dep == dependency => dep.copy(version = update)
        case dep => dep
      }
    )

    // 10. Resolve new manifest
    val newResolution = Steps.resolveFlixDependencies(newManifest) match {
      case Err(e) => return Err(e)
      case Ok(r) => r
    }

    // 11. Diff manifests, first checking for added dependencies and then removed dependencies
    // N.B.: Diffing manifests directly is sensitive to tiny changes since the identity of a manifest is all its contents.
    //       Perhaps an identifier that also includes its version or a hash of certain fields would be more resilient.

    // N.B.: Subtract old from new, so if the new set of transitive dependencies is fully contained in the old
    //       set of dependencies, then there is nothing to do.
    val addedManifests = newResolution.manifests.toSet -- oldResolution.manifests

    // N.B.: Subtract new from old, so we know which dependencies to back up.
    val removedManifests = oldResolution.manifests.toSet -- newResolution.manifests

    // Early return in case of no new dependencies
    if (addedManifests.isEmpty) {
      // TODO: Report message saying no new dependencies to download but suggest pruning removed dependencies.
      //       Doing so requires 'removedManifests'.
      return Ok(())
    }

    out.println(s"addedManifests = $addedManifests")

    // 12. Back up removed dependencies
    // Move old flix dependencies
    val removedDependencies = removedManifests.flatMap(_.flixDependencies)
    removedDependencies.foreach { dep =>
      val fpkgSourcePath = getFlixPackageDir(projectPath, dep)
      val fpkgTargetPath = getFlixPackageDir(getUpgradeBackupDir(projectPath), dep)
      FileOps.moveDir(fpkgSourcePath, fpkgTargetPath)
    }

    // Move all maven jars
    val mvnDir = getMavenDirectory(projectPath)
    val tmpMvnDir = getMavenDirectory(getUpgradeBackupDir(projectPath))
    FileOps.exists(mvnDir) match {
      case Err(e) => return Err(BootstrapError.FileError(s"Unexpected error checking existence of maven directory: ${e.getMessage}"))
      case Ok(true) => FileOps.moveDir(mvnDir, tmpMvnDir) match {
        case Err(e) => return Err(BootstrapError.FileError(s"Unable to Maven directory: ${e.getMessage}"))
        case Ok(()) => ()
      }
      case Ok(false) => () // Tolerate missing maven directory and continue
    }

    // Move all external jars
    val extDir = getExternalJarDirectory(projectPath)
    val tmpExtDir = getExternalJarDirectory(getUpgradeBackupDir(projectPath))
    FileOps.exists(extDir) match {
      case Err(e) => return Err(BootstrapError.FileError(s"Unexpected error checking existence of external jar directory: ${e.getMessage}"))
      case Ok(true) => FileOps.moveDir(extDir, tmpExtDir) match {
        case Err(e) => return Err(BootstrapError.FileError(s"Unable to move external jar directory: ${e.getMessage}"))
        case Ok(()) => ()
      }
      case Ok(false) => () // Tolerate missing maven directory and continue
    }

    // 13. Download dependencies
    val newInstalledDeps = Steps.installDependencies(newResolution) match {
      case Err(e) => return Err(e)
      case Ok(fpkgs :: _ :: _ :: Nil) =>
        out.println(s"oldPaths = $oldPaths")
        out.println(s"fpkgs = ${fpkgs.toSet}")
        out.println(s"fpkgs.toSet -- oldPaths = ${fpkgs.toSet -- oldPaths}")
        fpkgs.toSet -- oldPaths
      case Ok(paths) =>
        return Err(BootstrapError.GeneralError(s"Internal Error: Installing dependencies returned unexpected number of paths: ${paths.length}"))
    }

    // 14. Check for effect-safe upgrade
    checkEffects(flix) match {
      case Err(effectError) =>

        val errorMessage = effectError.message(formatter)
        val effectUpgradeConfirmationMessage =
          s"""$errorMessage
             |
             |Do trust the package to use these new effects [y/N]?""".stripMargin

        askForConfirmation(effectUpgradeConfirmationMessage) match {
          // TODO: Refactor Err(_) case into own case and perform same cleanup logic
          case Err(_) | Ok(false) =>
            // Restore old files
            val fpkgRollBacks = List(
              Result.traverse(newInstalledDeps.map(_.getParent))(FileOps.deleteDir),
              Result.traverse(removedDependencies) { dep =>
                // Move fpkg
                val fpkgSourcePath = getFlixPackageDir(getUpgradeBackupDir(projectPath), dep)
                val fpkgTargetPath = getFlixPackageDir(projectPath, dep)
                FileOps.move(fpkgSourcePath, fpkgTargetPath)
              })

            // Tolerate missing directories
            val mvnRollBacks = List(
              FileOps.exists(mvnDir).flatMap {
                case true => FileOps.deleteDir(mvnDir)
                case false => Result.Ok(())
              },
              FileOps.exists(tmpMvnDir).flatMap {
                case true => FileOps.moveDir(tmpMvnDir, mvnDir)
                case false => Result.Ok(())
              }
            )

            // Tolerate missing directories
            val extJarRollBacks = List(
              FileOps.exists(extDir).flatMap {
                case true => FileOps.deleteDir(extDir)
                case false => Result.Ok(())
              },
              FileOps.exists(tmpExtDir).flatMap {
                case true => FileOps.moveDir(tmpExtDir, extDir)
                case false => Result.Ok(())
              }
            )

            Result.sequence(fpkgRollBacks ::: mvnRollBacks ::: extJarRollBacks).map(_ => ()) match {
              case Err(e) => return Err(BootstrapError.FileError( // TODO: Refactor to error
                "FATAL UPGRADE ERROR: Unable to restore files. " +
                  "Please remove newly installed dependencies and restore old dependencies." +
                  System.lineSeparator() +
                  "Alternatively, remove all dependencies and redownload them." +
                  System.lineSeparator() +
                  System.lineSeparator() +
                  s"Files to restore located in: ${Bootstrap.getUpgradeBackupDir(projectPath)}" +
                  System.lineSeparator() +
                  s"Files to remove located in:" +
                  s"  ${Bootstrap.getLibraryDirectory(projectPath)}" +
                  System.lineSeparator() +
                  s"  ${Bootstrap.getMavenDirectory(projectPath)}" +
                  System.lineSeparator() +
                  s"  ${Bootstrap.getExternalJarDirectory(projectPath)}" +
                  System.lineSeparator() +
                  System.lineSeparator() +
                  s"Root cause: ${e.getMessage}"
              ))
              case Ok(()) => // Continue
            }
            return Err(BootstrapError.GeneralError("Upgrade aborted. Restored previous package version."))

          case Ok(true) => () // Continue
        }

      case Ok(()) => () // Continue
    }


    // 17. Write new manifest to file
    FileOps.writeString(getManifestFile(projectPath), Manifest.format(newManifest))

    // 17. Remove files from old dependency graph (from backup directory)
    FileOps.deleteDir(Bootstrap.getUpgradeBackupDir(projectPath))

    // Lock effects
    lockEffects(flix).map {
      v =>
        out.println("Upgrade successful.")
        v
    }
  }


  /**
    * // TODO: Better docs
    * Assumes 'no', does not close any of the streams.
    *
    * @param confirmationMessage
    * @param in
    * @param out
    * @return
    */
  private def askForConfirmation(confirmationMessage: String)(implicit in: InputStream, out: PrintStream): Result[Boolean, BootstrapError] = {
    out.print(confirmationMessage)
    val reader = new BufferedReader(new InputStreamReader(in))
    try {
      val input = reader.readLine()
      if (input == null) {
        return Err(BootstrapError.GeneralError("Refusing to run 'upgrade'. Confirmation input was null."))
      }
      out.println()
      Ok(input.toLowerCase == "y")
    } catch {
      case e: Exception => Err(BootstrapError.GeneralError(s"Refusing to run 'upgrade'. Failed to read input: ${e.getMessage}"))
    }
  }

  /**
    * Builds a fatjar package for the project.
    */
  def buildFatJar(flix: Flix): Result[Unit, BootstrapError] = {
    val jarFile = Bootstrap.getJarFile(projectPath)
    val libDir = Bootstrap.getLibraryDirectory(projectPath)
    Steps.updateStaleSources(flix)
    for {
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

  private def findLatestVersion(updates: AvailableUpdates): Option[SemVer] = updates match {
    case AvailableUpdates(Some(major), _, _) => Some(major)
    case AvailableUpdates(_, Some(minor), _) => Some(minor)
    case AvailableUpdates(_, _, Some(patch)) => Some(patch)
    case AvailableUpdates(_, _, _) => None
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

    // Construct a new zip file.
    Using(new ZipOutputStream(Files.newOutputStream(pkgFile))) { zip =>
      // Add required resources.
      FileOps.addToZip(zip, FLIX_TOML, Bootstrap.getManifestFile(projectPath))
      FileOps.addToZip(zip, LICENSE, Bootstrap.getLicenseFile(projectPath))
      FileOps.addToZip(zip, README, Bootstrap.getReadmeFile(projectPath))

      // Add all source files.
      // Here we sort entries by relative file name to apply https://reproducible-builds.org/
      val srcFiles = FileOps.getFlixFilesIn(Bootstrap.getSourceDirectory(projectPath), Int.MaxValue)
      for ((sourceFile, fileNameWithSlashes) <- FileOps.sortPlatformIndependently(projectPath, srcFiles)) {
        FileOps.addToZip(zip, fileNameWithSlashes, sourceFile)
      }
    } match {
      case Success(()) => Result.Ok(())
      case Failure(e) => Result.Err(BootstrapError.FileError(e.getMessage))
    }
  }

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
      case Ok(false) => return Err(BootstrapError.FileError(s"No '$EFFECT_LOCK_FILE' file found. Unable to run 'eff-check'."))
      case Ok(true) => ()
    }

    Steps.updateStaleSources(flix)
    for {
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
    Steps.updateStaleSources(flix)
    for {
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
      return Err(BootstrapError.FileError("No manifest found ('flix.toml'). Refusing to run 'clean' in a non-project directory."))
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
      return Err(BootstrapError.FileError("Refusing to delete file in home directory."))
    }
    Ok(())
  }

  /** Returns `Err` if `path` is a root directory. */
  private def checkForRootDir(path: Path): Result[Unit, BootstrapError] = {
    val roots = FileSystems.getDefault.getRootDirectories.asScala.toList.map(_.normalize())
    if (roots.contains(path.normalize())) {
      return Err(BootstrapError.FileError("Refusing to delete file in root directory."))
    }
    Ok(())
  }

  /** Returns `Err` if `path` is an ancestor of `projectPath`. */
  private def checkForAncestor(path: Path): Result[Unit, BootstrapError] = {
    if (projectPath.normalize().startsWith(path.normalize())) {
      return Err(BootstrapError.FileError(s"Refusing to delete file in ancestor of project directory: '${path.normalize()}"))
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
    Steps.updateStaleSources(flix)
    Steps.check(flix).map(_ => ())
  }

  /**
    * Generates API documentation.
    */
  def doc(flix: Flix): Result[Unit, BootstrapError] = {
    Steps.updateStaleSources(flix)
    Steps.check(flix).map(HtmlDocumentor.run(_, getPackageModules)(flix))
  }

  /**
    * Formats all source files in the project.
    */
  def format(flix: Flix): Result[Unit, BootstrapError] = {
    Steps.updateStaleSources(flix)
    Steps.check(flix).map {
      case _ =>
        val syntaxTree = flix.getParsedAst
        LspFormatter.formatFiles(syntaxTree, sourcePaths)(flix)
    }
  }

  /**
    * Runs the main function in flix package for the project.
    */
  def run(flix: Flix, args: Array[String]): Result[Unit, BootstrapError] = {
    for {
      compilationResult <- build(flix)
    } yield {
      compilationResult.getMain match {
        case None => ()
        case Some(main) => main(args)
      }
    }
  }

  /**
    * Runs all tests in the flix package for the project.
    */
  def test(flix: Flix): Result[Unit, BootstrapError] = {
    for {
      compilationResult <- build(flix)
      res <- Tester.run(Nil, compilationResult)(flix).mapErr(_ => BootstrapError.GeneralError("Tester Error"))
    } yield {
      res
    }
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

    val flixDeps = optManifest.map(FlixPackageManager.findFlixDependencies).getOrElse(Nil)

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
      *
      * Most of each dependency jar is copied verbatim — class files, ordinary resources
      * (native libraries, capability files, `.properties` files, ...), and library-specific
      * `META-INF/` resources such as JLine's `META-INF/jline/providers/` registry — with two
      * exceptions:
      *   - `META-INF/services/` service-provider files are *merged* across jars (rather than
      *     letting one jar's copy overwrite another's) so that `java.util.ServiceLoader` still
      *     finds every provider.
      *   - A small set of entries is dropped because copying them would be unsafe or useless:
      *       - `META-INF/MANIFEST.MF` would collide with (and clobber) the fat jar's own manifest.
      *       - Signature files (`.SF`, `.RSA`, `.DSA`, `.EC`, `SIG-` files) would no longer match
      *         the repacked contents, making the JVM reject the jar with a `SecurityException`.
      *       - `META-INF/INDEX.LIST` would reference jars that no longer exist.
      *       - `META-INF/versions/` multi-release classes would be inert (the fat jar manifest
      *         does not declare `Multi-Release: true`) and risk shadowing the base classes.
      *       - `module-info.class` cannot be merged: only one may live at the jar root.
      *
      * Duplicate entry paths across jars are de-duplicated (first jar wins) so that the build
      * does not abort with a `ZipException: duplicate entry`.
      */
    def addJarsFromDirToZip(dir: Path, zip: ZipOutputStream): Unit = {
      // First, we get all jar files inside the lib folder.
      // If the lib folder doesn't exist, we suppose there is simply no dependency and trigger no error.
      if (!Files.exists(dir)) {
        return
      }
      val servicesPrefix = "META-INF/services/"
      val metaInfPrefix = "META-INF/"
      val jarDependencies = FileOps.getFilesWithExtIn(dir, EXT_JAR, Int.MaxValue)

      // Tracks entry names already written to `zip` so that an entry present in more than one
      // dependency jar is written only once (first jar wins) instead of throwing.
      val seen = mutable.Set.empty[String]

      // Accumulates merged `META-INF/services/*` files: service name -> ordered, de-duplicated provider lines.
      val services = mutable.LinkedHashMap.empty[String, List[String]]

      // Returns `true` for entries that must not be copied into the fat jar (see method doc).
      def isUnsafeEntry(name: String): Boolean = {
        // Signature files sit directly under META-INF/ (a single path segment after it).
        val rest = if (name.startsWith(metaInfPrefix)) name.substring(metaInfPrefix.length) else name
        val isSignatureFile = name.startsWith(metaInfPrefix) && !rest.contains("/") &&
          (rest.startsWith("SIG-") || rest.endsWith(".SF") || rest.endsWith(".RSA") ||
            rest.endsWith(".DSA") || rest.endsWith(".EC"))
        name.equals(s"module-info.$EXT_CLASS") ||
          name.equals("META-INF/MANIFEST.MF") ||
          name.equals("META-INF/INDEX.LIST") ||
          name.startsWith("META-INF/versions/") ||
          isSignatureFile
      }

      // Add jar dependencies.
      jarDependencies.foreach(dep => {
        // Extract the runtime contents of the dependency into the fat jar.
        Using(new ZipInputStream(Files.newInputStream(dep))) {
          zipIn =>
            var entry = zipIn.getNextEntry
            while (entry != null) {
              val name = entry.getName
              if (entry.isDirectory) {
                // Directory entries carry no content; the zip records them implicitly.
              } else if (name.startsWith(servicesPrefix)) {
                // Merge service-provider files rather than overwriting, so every provider survives.
                val lines = new String(zipIn.readAllBytes()).linesIterator
                  .map(_.trim).filter(l => l.nonEmpty && !l.startsWith("#")).toList
                services(name) = (services.getOrElse(name, List.empty) ++ lines).distinct
              } else if (isUnsafeEntry(name)) {
                // Drop entries that are unsafe or useless in a fat jar (see method doc).
              } else if (seen.add(name)) {
                // Copy everything else — classes, resources, and library-specific META-INF
                // entries such as META-INF/jline/ — skipping paths taken by an earlier jar.
                FileOps.addToZip(zip, name, zipIn.readAllBytes())
              }
              entry = zipIn.getNextEntry
            }
        }
      })

      // Write the merged service-provider files.
      for ((name, providers) <- services) {
        FileOps.addToZip(zip, name, providers.mkString("\n").getBytes)
      }
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
          Ok(flixPackagePaths)
        case Err(e) =>
          Err(BootstrapError.FlixPackageError(e))
      }
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
      *
      * When a file watcher is active (REPL mode), drains watcher events instead of polling timestamps.
      */
    def updateStaleSources(flix: Flix): Unit = fileWatcher match {
      case Some(fw) => applyWatcherEvents(fw.drain(), flix)
      case None => updateStaleSourcesByTimestamp(flix)
    }

    /**
      * Applies file watcher events to the Flix instance and updates the cached path lists.
      * On overflow, falls back to a full re-scan.
      */
    private def applyWatcherEvents(events: List[FileWatcher.WatchEvent], flix: Flix): Unit = {
      import FileWatcher.WatchEvent.*

      if (events.exists(_ == Overflow)) {
        // Overflow occurred: fall back to a full re-scan.
        rescanAndUpdate(flix)
        return
      }

      for (event <- events) event match {
        case Created(path) =>
          if (FileOps.checkExt(path, EXT_FLIX)) {
            sourcePaths = path :: sourcePaths
            flix.addFile(path)(SecurityContext.Unrestricted)
          } else if (FileOps.checkExt(path, EXT_FPKG)) {
            flixPackagePaths = path :: flixPackagePaths
            flix.addPkg(path)(securityLevels.getOrElse(path, SecurityContext.Plain))
          } else if (FileOps.checkExt(path, EXT_JAR)) {
            val libDir = Bootstrap.getLibraryDirectory(projectPath)
            val mavenDir = libDir.resolve(MavenPackageManager.DirName)
            val jarDir = libDir.resolve(JarPackageManager.DirName)
            if (path.startsWith(mavenDir)) {
              mavenPackagePaths = path :: mavenPackagePaths
            } else if (path.startsWith(jarDir)) {
              jarPackagePaths = path :: jarPackagePaths
            }
            flix.addJar(path)
          }

        case Modified(path) =>
          if (FileOps.checkExt(path, EXT_FLIX)) {
            flix.addFile(path)(SecurityContext.Unrestricted)
          } else if (FileOps.checkExt(path, EXT_FPKG)) {
            flix.addPkg(path)(securityLevels.getOrElse(path, SecurityContext.Plain))
          } else if (FileOps.checkExt(path, EXT_JAR)) {
            flix.addJar(path)
          }

        case Deleted(path) =>
          if (path.toString.endsWith(s".$EXT_FLIX")) {
            sourcePaths = sourcePaths.filterNot(_ == path)
            flix.remFile(path)(SecurityContext.Unrestricted)
          } else if (path.toString.endsWith(s".$EXT_FPKG")) {
            flixPackagePaths = flixPackagePaths.filterNot(_ == path)
            flix.remFpkg(path)(SecurityContext.Unrestricted)
          } else if (path.toString.endsWith(s".$EXT_JAR")) {
            mavenPackagePaths = mavenPackagePaths.filterNot(_ == path)
            jarPackagePaths = jarPackagePaths.filterNot(_ == path)
          } else {
            // No recognized file extension — likely a directory deletion.
            // Remove all tracked files that were children of this path.
            val deletedFlix = sourcePaths.filter(_.startsWith(path))
            val deletedFpkg = flixPackagePaths.filter(_.startsWith(path))
            sourcePaths = sourcePaths.filterNot(_.startsWith(path))
            flixPackagePaths = flixPackagePaths.filterNot(_.startsWith(path))
            mavenPackagePaths = mavenPackagePaths.filterNot(_.startsWith(path))
            jarPackagePaths = jarPackagePaths.filterNot(_.startsWith(path))
            for (p <- deletedFlix) flix.remFile(p)(SecurityContext.Unrestricted)
            for (p <- deletedFpkg) flix.remFpkg(p)(SecurityContext.Unrestricted)
          }

        case Overflow => // already handled above
      }
    }

    /**
      * Falls back to a full directory re-scan and updates the Flix instance with any changes.
      * Used when the watcher reports an overflow event.
      */
    private def rescanAndUpdate(flix: Flix): Unit = {
      val previousSources = (sourcePaths ::: flixPackagePaths ::: mavenPackagePaths ::: jarPackagePaths).toSet

      // Re-scan directories to discover current files.
      addLocalFlixFiles()
      addLocalLibs()

      val currentSources = (sourcePaths ::: flixPackagePaths ::: mavenPackagePaths ::: jarPackagePaths).toSet

      // Add new or re-add all current sources.
      for (path <- currentSources) {
        if (FileOps.checkExt(path, EXT_FLIX)) {
          flix.addFile(path)(SecurityContext.Unrestricted)
        } else if (FileOps.checkExt(path, EXT_FPKG)) {
          flix.addPkg(path)(securityLevels.getOrElse(path, SecurityContext.Plain))
        } else if (FileOps.checkExt(path, EXT_JAR)) {
          flix.addJar(path)
        }
      }

      // Remove deleted sources.
      for (path <- previousSources -- currentSources) {
        flix.remFile(path)(SecurityContext.Unrestricted)
      }
    }

    /**
      * Timestamp-based stale source detection (used when no file watcher is active).
      */
    private def updateStaleSourcesByTimestamp(flix: Flix): Unit = {
      val previousSources = timestamps.keySet
      println(s"Timestamps = ${timestamps.keySet}")

      for (path <- sourcePaths if hasChanged(path)) {
        flix.addFile(path)(SecurityContext.Unrestricted)
      }

      for (path <- flixPackagePaths if hasChanged(path)) {
        flix.addPkg(path)(securityLevels.getOrElse(path, SecurityContext.Plain))
      }

      for (path <- mavenPackagePaths if hasChanged(path)) {
        flix.addJar(path)
      }

      for (path <- jarPackagePaths if hasChanged(path)) {
        flix.addJar(path)
      }

      val currentSources = (sourcePaths ::: flixPackagePaths ::: mavenPackagePaths ::: jarPackagePaths).filter(p => Files.exists(p))

      val deletedSources = previousSources -- currentSources
      println(s"deletedSource = $deletedSources")
      for (path <- deletedSources) {
        //
        if (path.toAbsolutePath.normalize().startsWith(getLibraryDirectory(projectPath).toAbsolutePath.normalize())) {
          flix.remFpkg(path)(SecurityContext.Paranoid)
        } else {
          flix.remFile(path)(SecurityContext.Unrestricted)
        }
      }

      timestamps = currentSources.map(f => f -> f.toFile.lastModified).toMap
    }

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
}
