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
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.language.phase.HtmlDocumentor
import ca.uwaterloo.flix.runtime.CompilationResult
import ca.uwaterloo.flix.tools.Tester
import ca.uwaterloo.flix.tools.pkg.FlixPackageManager.findFlixDependencies
import ca.uwaterloo.flix.tools.pkg.github.GitHub
import ca.uwaterloo.flix.tools.pkg.{FlixPackageManager, JarPackageManager, Manifest, ManifestParser, MavenPackageManager, PackageModules, ReleaseError}
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.{Build, FileOps, Formatter, Result, Validation}

import java.io.PrintStream
import java.nio.file.*
import java.util.zip.{ZipInputStream, ZipOutputStream}
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
        |def test01(): Bool = 1 + 1 == 2
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
    * Returns the path to the artifact directory relative to the given path `p`.
    */
  private def getResourcesDirectory(p: Path): Path = p.resolve("./resources/").normalize()

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
  private var flixPackagePaths: List[Path] = List.empty
  private var mavenPackagePaths: List[Path] = List.empty
  private var jarPackagePaths: List[Path] = List.empty

  private var securityLevels: Map[Path, SecurityContext] = Map.empty

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
  def buildJar(flix: Flix)(implicit formatter: Formatter): Result[Unit, BootstrapError] = {
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
    * Builds a fatjar package for the project.
    */
  def buildFatJar(flix: Flix)(implicit formatter: Formatter): Result[Unit, BootstrapError] = {
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

    // Ensure `buildDir` is not dangerous
    checkForDangerousPath(buildDir) match {
      case Err(e) => return Err(e)
      case Ok(()) => ()
    }

    // Ensure all files in `buildDir` are valid class files.
    val files = FileOps.getFilesIn(buildDir, Int.MaxValue)
    for (file <- files) {
      if (!FileOps.checkExt(file, "class")) {
        return Err(BootstrapError.FileError(s"Unexpected file extension in build directory (only '.class' files are allowed): '${projectPath.relativize(file)}'"))
      }

      if (!FileOps.isClassFile(file)) {
        return Err(BootstrapError.FileError(s"Invalid class file in build directory: '${projectPath.relativize(file)}'"))
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
    val directories = FileOps.getDirectoriesIn(buildDir, Int.MaxValue)
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
    if (home == path) {
      return Err(BootstrapError.FileError("Refusing to run 'clean' in home directory."))
    }
    Ok(())
  }

  /** Returns `Err` if `path` is a root directory. */
  private def checkForRootDir(path: Path): Result[Unit, BootstrapError] = {
    val roots = FileSystems.getDefault.getRootDirectories.asScala.toList
    if (roots.contains(path)) {
      return Err(BootstrapError.FileError("Refusing to run 'clean' in root directory."))
    }
    Ok(())
  }

  /** Returns `Err` if `path` is an ancestor of `projectPath`. */
  private def checkForAncestor(path: Path): Result[Unit, BootstrapError] = {
    if (projectPath.startsWith(path)) {
      return Err(BootstrapError.FileError(s"Refusing to run clean in ancestor of project directory: '${path.normalize()}"))
    }
    Ok(())
  }

  /**
    * Type checks the source files for the project.
    */
  def check(flix: Flix): Result[Unit, BootstrapError] = {
    Steps.updateStaleSources(flix)
    Steps.check(flix).map(_ => ())
  }

  /**
    * Checks to see if any source files or packages have been changed.
    * If they have, they are added to flix. Then updates the timestamps
    * map to reflect the current source files and packages.
    */
  def reconfigureFlix(flix: Flix): Unit = {
    // TODO: Figure out if this function can be removed somehow (maybe by removing shell depending on bootstrap)
    // TODO: Can be removed by moving `updateStaleSources` into all step functions that require updating stale sources (almost all). This also remove responsibility from the caller.
    Steps.updateStaleSources(flix)
  }

  /**
    * Generates API documentation.
    */
  def doc(flix: Flix): Result[Unit, BootstrapError] = {
    Steps.updateStaleSources(flix)
    Steps.check(flix).map(HtmlDocumentor.run(_, getPackageModules)(flix))
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
      res <- Tester.run(Nil, compilationResult)(flix).mapErr(_ => BootstrapError.GeneralError(List("Tester Error")))
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

    val flixDeps = optManifest.map(findFlixDependencies).getOrElse(Nil)

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
        Err(BootstrapError.GeneralError(flix.mkMessages(errors)))
      }
    }

    /**
      * Runs the compile function on the `flix` object.
      * It is up to the caller to set the appropriate options on `flix`.
      * It is often the case that `outputJvm` and `loadClassFiles` must be toggled on or off.
      */
    def compile(flix: Flix): Result[CompilationResult, BootstrapError] = {
      flix.compile() match {
        case Validation.Success(result: CompilationResult) => Ok(result)
        case Validation.Failure(errors) => Err(BootstrapError.GeneralError(flix.mkMessages(errors.toList)))
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
            Err(BootstrapError.GeneralError(securityResolutionErrors.map(_.toString)))
          }
      }
    }

    /**
      * Checks to see if any source files or packages have been changed.
      * If they have, they are added to flix. Then updates the timestamps
      * map to reflect the current source files and packages.
      */
    def updateStaleSources(flix: Flix): Unit = {
      val previousSources = timestamps.keySet

      for (path <- sourcePaths if hasChanged(path)) {
        flix.addFlix(path)(SecurityContext.Unrestricted)
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
      for (path <- deletedSources) {
        flix.remSourceCode(path.toString)
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
