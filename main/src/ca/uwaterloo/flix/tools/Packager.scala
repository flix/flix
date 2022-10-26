/*
 * Copyright 2021 Magnus Madsen, Matthew Lutze
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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.Ast
import ca.uwaterloo.flix.language.ast.Ast.Source
import ca.uwaterloo.flix.runtime.CompilationResult
import ca.uwaterloo.flix.tools.github.GitHub
import ca.uwaterloo.flix.util.Formatter.AnsiTerminalFormatter
import ca.uwaterloo.flix.util.Result.{ToErr, ToOk}
import ca.uwaterloo.flix.util.Validation.{ToFailure, ToSuccess}
import ca.uwaterloo.flix.util._

import java.io.{File, PrintWriter}
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes
import java.util.{Calendar, GregorianCalendar}
import java.util.zip.{ZipEntry, ZipFile, ZipOutputStream}
import scala.collection.mutable
import scala.util.{Failure, Success, Using}

/**
  * An interface to manage flix packages.
  */
object Packager {

  /**
    * Installs a flix package from the Github `project`.
    *
    * `project` must be of the form `<owner>/<repo>`
    *
    * The package is installed at `lib/<owner>/<repo>`
    */
  def install(project: String, p: Path, o: Options): Result[Unit, Int] = {
    val proj = GitHub.parseProject(project)
    val release = GitHub.getLatestRelease(proj)
    val assets = release.assets.filter(_.name.endsWith(".fpkg"))
    val lib = getLibraryDirectory(p)
    val assetFolder = lib.resolve(proj.owner).resolve(proj.repo)

    // create the asset directory if it doesn't exist
    Files.createDirectories(assetFolder)

    // clear the asset folder
    assetFolder.toFile.listFiles.foreach(deletePackage)

    // download each asset to the folder
    for (asset <- assets) {
      val path = assetFolder.resolve(asset.name)
      Using(GitHub.downloadAsset(asset)) {
        stream => Files.copy(stream, path, StandardCopyOption.REPLACE_EXISTING)
      }
    }
    ().toOk
  }

  /**
    * Initializes a new flix project at the given path `p`.
    *
    * The project must not already exist.
    */
  def init(p: Path, o: Options): Result[Unit, Int] = {
    //
    // Check that the current working directory is usable.
    //
    if (!Files.isDirectory(p) || !Files.isReadable(p) || !Files.isWritable(p)) {
      throw new RuntimeException(s"The directory: '$p' is not accessible. Aborting.")
    }

    //
    // Compute the name of the package based on the directory name.
    //
    val packageName = getPackageName(p)

    //
    // Compute all the directories and files we intend to create.
    //
    val buildDirectory = getBuildDirectory(p)
    val libraryDirectory = getLibraryDirectory(p)
    val sourceDirectory = getSourceDirectory(p)
    val testDirectory = getTestDirectory(p)

    val licenseFile = getLicenseFile(p)
    val readmeFile = getReadmeFile(p)
    val mainSourceFile = getMainSourceFile(p)
    val mainTestFile = getMainTestFile(p)

    //
    // Check that the project directories and files do not already exist.
    //
    val dirPaths = List(
      buildDirectory, libraryDirectory, sourceDirectory, testDirectory
    )

    // blocked directories are those that already exist and are not directories
    val blockedDirs = dirPaths.filter(p => Files.exists(p) && !Files.isDirectory(p))

    if (blockedDirs.nonEmpty) {
      throw new RuntimeException(s"The following files already exist and are not directories: ${blockedDirs.mkString(", ")}. Aborting.")
    }

    //
    // Create the project directories and files.
    //
    newDirectoryIfAbsent(buildDirectory)
    newDirectoryIfAbsent(libraryDirectory)
    newDirectoryIfAbsent(sourceDirectory)
    newDirectoryIfAbsent(testDirectory)

    newFileIfAbsent(licenseFile) {
      """Enter license information here.
        |""".stripMargin
    }

    newFileIfAbsent(readmeFile) {
      s"""# $packageName
         |
         |Enter some useful information.
         |
         |""".stripMargin
    }

    newFileIfAbsent(mainSourceFile) {
      """// The main entry point.
        |def main(): Unit \ IO =
        |    println("Hello World!")
        |""".stripMargin
    }

    newFileIfAbsent(mainTestFile) {
      """@test
        |def test01(): Bool = 1 + 1 == 2
        |""".stripMargin
    }
    ().toOk
  }

  /**
    * Type checks the source files for the given project path `p`.
    */
  def check(p: Path, o: Options): Result[Unit, Int] = {
    // Configure a new Flix object.
    implicit val flix: Flix = new Flix()
    flix.setOptions(o)

    // Add sources and packages.
    addSourcesAndPackages(p, o)

    flix.check() match {
      case Validation.Success(_) => ().toOk
      case Validation.Failure(errors) =>
        flix.mkMessages(errors).foreach(println)
        Result.Err(1)
    }
  }

  /**
    * Builds (compiles) the source files for the given project path `p`.
    */
  def build(p: Path, o: Options, loadClasses: Boolean = true)(implicit flix: Flix = new Flix()): Result[CompilationResult, Int] = {
    // Configure a new Flix object.
    val newOptions = o.copy(
      output = Some(getBuildDirectory(p)),
      loadClassFiles = loadClasses
    )
    flix.setOptions(newOptions)

    // Add sources and packages.
    addSourcesAndPackages(p, o)

    flix.compile() match {
      case Validation.Success(r) => Result.Ok(r)
      case Validation.Failure(errors) =>
        flix.mkMessages(errors).foreach(println)
        1.toErr
    }
  }

  /**
    * Adds all source files and packages to the given `flix` object.
    */
  private def addSourcesAndPackages(p: Path, o: Options)(implicit flix: Flix): Result[Unit, Int] = {
    // Add all files *directly* in `p` (non-recursively).
    for (sourceFile <- p.toFile.listFiles().map(_.toPath)) {
      if (sourceFile.getFileName.toString.endsWith(".flix")) {
        flix.addSourcePath(sourceFile)
      }
    }

    // Add all files in `p/src` (recursively).
    for (sourceFile <- getAllFiles(getSourceDirectory(p))) {
      if (sourceFile.getFileName.toString.endsWith(".flix")) {
        flix.addSourcePath(sourceFile)
      }
    }

    // Add all files in `p/test` (recursively).
    for (testFile <- getAllFiles(getTestDirectory(p))) {
      if (testFile.getFileName.toString.endsWith(".flix")) {
        flix.addSourcePath(testFile)
      }
    }

    // Add all library packages.
    val lib = getLibraryDirectory(p)
    if (lib.toFile.isDirectory) {
      for (file <- getAllFiles(lib)) {
        if (file.getFileName.toString.endsWith(".fpkg")) {
          // Case 1: It's a Flix package.
          flix.addSourcePath(file)
        } else if (file.getFileName.toString.endsWith(".jar")) {
          // Case 2: It's a JAR.
          flix.addJar(file)
        }
      }
    }
    ().toOk
  }

  /**
    * Builds a jar package for the given project path `p`.
    */
  def buildJar(p: Path, o: Options): Result[Unit, Int] = {
    // The path to the jar file.
    val jarFile = getJarFile(p)

    // Check whether it is safe to write to the file.
    if (Files.exists(jarFile) && !isJarFile(jarFile)) {
      throw new RuntimeException(s"The path '$jarFile' exists and is not a jar-file. Refusing to overwrite.")
    }

    // Construct a new zip file.
    Using(new ZipOutputStream(Files.newOutputStream(jarFile))) { zip =>
      // META-INF/MANIFEST.MF
      val manifest =
        """Manifest-Version: 1.0
          |Main-Class: Main
          |""".stripMargin

      // Add manifest file.
      addToZip(zip, "META-INF/MANIFEST.MF", manifest.getBytes)

      // Add all class files.
      // Here we sort entries by relative file name to apply https://reproducible-builds.org/
      for ((buildFile, fileNameWithSlashes) <- getAllFiles(getBuildDirectory(p))
        .map { path => (path, convertPathToRelativeFileName(getBuildDirectory(p), path)) }
        .sortBy(_._2)) {
        addToZip(zip, fileNameWithSlashes, buildFile)
      }
    } match {
      case Success(()) => ().toOk
      case Failure(e) =>
        println(e.getMessage)
        1.toErr
    }
  }

  /**
    * Builds a flix package for the given project path `p`.
    */
  def buildPkg(p: Path, o: Options): Result[Unit, Int] = {
    //
    // We require that a Flix package has a specific structure.
    //
    checkProjectPath(p) match {
      case Validation.Success(_) => ()
      case Validation.Failure(missingPaths) => throw new RuntimeException(s"Missing files or directories: ${missingPaths.mkString(", ")}")
    }

    // The path to the fpkg file.
    val pkgFile = getPkgFile(p)

    // Check whether it is safe to write to the file.
    if (Files.exists(pkgFile) && !isPkgFile(pkgFile)) {
      throw new RuntimeException(s"The path '$pkgFile' exists and is not a fpkg-file. Refusing to overwrite.")
    }

    // Construct a new zip file.
    Using(new ZipOutputStream(Files.newOutputStream(pkgFile))) { zip =>
      // Add required resources.
      addToZip(zip, "LICENSE.md", getLicenseFile(p))
      addToZip(zip, "README.md", getReadmeFile(p))

      // Add all source files.
      // Here we sort entries by relative file name to apply https://reproducible-builds.org/
      for ((sourceFile, fileNameWithSlashes) <- getAllFiles(getSourceDirectory(p))
        .map { path => (path, convertPathToRelativeFileName(p, path)) }
        .sortBy(_._2)) {
        addToZip(zip, fileNameWithSlashes, sourceFile)
      }
    } match {
      case Success(()) => ().toOk
      case Failure(e) =>
        println(e.getMessage)
        1.toErr
    }
  }

  /**
    * Runs the main function in flix package for the given project path `p`.
    */
  def run(p: Path, o: Options): Result[Unit, Int] = {
    val res = for {
      compilationResult <- build(p, o).toOption
      main <- compilationResult.getMain
    } yield {
      main(Array.empty)
      ().toOk[Unit, Int]
    }
    res.getOrElse(1.toErr)
  }

  /**
    * Runs all benchmarks in the flix package for the given project path `p`.
    */
  def benchmark(p: Path, o: Options): Result[Unit, Int] = {
    build(p, o) map {
      compilationResult =>
        Benchmarker.benchmark(compilationResult, new PrintWriter(System.out, true))(o)
    }
  }

  /**
    * Runs all tests in the flix package for the given project path `p`.
    */
  def test(p: Path, o: Options): Result[Unit, Int] = {
    implicit val flix: Flix = new Flix().setFormatter(AnsiTerminalFormatter)
    build(p, o) flatMap {
      compilationResult =>
        Tester.run(Nil, compilationResult)
        ().toOk
    }
  }

  /**
    * Returns a list of sources extracted from the given flix package at path `p`.
    */
  def unpack(p: Path)(implicit flix: Flix): List[Source] = {
    // Check that the path is a flix package.
    if (!isPkgFile(p))
      throw new RuntimeException(s"The path '$p' is not a flix package.")

    // Open the zip file.
    Using(new ZipFile(p.toFile)) { zip =>
      // Collect all source and test files.
      val result = mutable.ListBuffer.empty[Source]
      val iterator = zip.entries()
      while (iterator.hasMoreElements) {
        val entry = iterator.nextElement()
        val name = entry.getName
        if (name.endsWith(".flix")) {
          val bytes = StreamOps.readAllBytes(zip.getInputStream(entry))
          val str = new String(bytes, flix.defaultCharset)
          val arr = str.toCharArray
          result += Source(Ast.Input.Text(name, str, stable = false), arr, stable = false)
        }
      }
      result.toList
    }.get // TODO Return a Result instead, see https://github.com/flix/flix/issues/3132
  }

  /**
    * Returns a Validation containing the list of missing paths in case the path is not a project path.
    */
  private def checkProjectPath(p: Path): Validation[Unit, Path] = {
    val required = List(
      getSourceDirectory(p),
      getTestDirectory(p),
      getLicenseFile(p),
      getReadmeFile(p)
    )

    Validation.traverseX(required) {
      case path =>
        if (Files.exists(path)) {
          ().toSuccess
        } else {
          path.toFailure
        }
    }
  }

  /**
    * Returns the package name based on the given path `p`.
    */
  private def getPackageName(p: Path): String = p.toAbsolutePath.normalize().getFileName.toString

  /**
    * Returns the path to the pkg file based on the given path `p`.
    */
  private def getPkgFile(p: Path): Path = p.resolve(getPackageName(p) + ".fpkg").normalize()

  /**
    * Returns the path to the jar file based on the given path `p`.
    */
  private def getJarFile(p: Path): Path = p.resolve(getPackageName(p) + ".jar").normalize()

  /**
    * Returns the path to the build directory relative to the given path `p`.
    */
  private def getBuildDirectory(p: Path): Path = p.resolve("./build/").normalize()

  /**
    * Returns the path to the library directory relative to the given path `p`.
    */
  def getLibraryDirectory(p: Path): Path = p.resolve("./lib/").normalize()

  /**
    * Returns the path to the source directory relative to the given path `p`.
    */
  def getSourceDirectory(p: Path): Path = p.resolve("./src/").normalize()

  /**
    * Returns the path to the test directory relative to the given path `p`.
    */
  def getTestDirectory(p: Path): Path = p.resolve("./test/").normalize()

  /**
    * Returns the path to the LICENSE file relative to the given path `p`.
    */
  private def getLicenseFile(p: Path): Path = p.resolve("./LICENSE.md").normalize()

  /**
    * Returns the path to the README file relative to the given path `p`.
    */
  private def getReadmeFile(p: Path): Path = p.resolve("./README.md").normalize()

  /**
    * Returns the path to the main source file relative to the given path `p`.
    */
  private def getMainSourceFile(p: Path): Path = getSourceDirectory(p).resolve("./Main.flix").normalize()

  /**
    * Returns the path to the main test file relative to the given path `p`.
    */
  private def getMainTestFile(p: Path): Path = getTestDirectory(p).resolve("./TestMain.flix").normalize()

  /**
    * Creates a new directory at the given path `p`.
    * The path must not already contain a non-directory.
    */
  private def newDirectoryIfAbsent(p: Path): Unit = {
    Files.createDirectories(p)
  }

  /**
    * Creates a new text file at the given path `p` with the given content `s`,
    * if the file does not already exist.
    */
  private def newFileIfAbsent(p: Path)(s: String): Unit = {
    Files.writeString(p, s, StandardOpenOption.CREATE)
  }

  /**
    * Adds an entry to the given zip file.
    */
  private def addToZip(zip: ZipOutputStream, name: String, p: Path): Unit = {
    addToZip(zip, name, Files.readAllBytes(p))
  }

  /**
    * To support DOS time, Java 8+ treats dates before the 1980 January in special way.
    * Here we use 2014-06-27 (the date of the first commit to Flix) to avoid the complexity introduced by this hack.
    *
    * @see <a href="https://bugs.openjdk.java.net/browse/JDK-4759491">JDK-4759491 that introduced the hack around 1980 January from Java 8+</a>
    * @see <a href="https://bugs.openjdk.java.net/browse/JDK-6303183">JDK-6303183 that explains why the second should be even to create ZIP files in platform-independent way</a>
    * @see <a href="https://github.com/gradle/gradle/blob/445deb9aa988e506120b7918bf91acb421e429ba/subprojects/core/src/main/java/org/gradle/api/internal/file/archive/ZipCopyAction.java#L42-L57">A similar case from Gradle</a>
    */
  private val ENOUGH_OLD_CONSTANT_TIME: Long = new GregorianCalendar(2014, Calendar.JUNE, 27, 0, 0, 0).getTimeInMillis

  /**
    * Adds an entry to the given zip file.
    */
  private def addToZip(zip: ZipOutputStream, name: String, d: Array[Byte]): Unit = {
    val entry = new ZipEntry(name)
    entry.setTime(ENOUGH_OLD_CONSTANT_TIME)
    zip.putNextEntry(entry)
    zip.write(d)
    zip.closeEntry()
  }

  /**
    * Returns all files in the given path `p`.
    */
  def getAllFiles(p: Path): List[Path] = {
    if (Files.isReadable(p) && Files.isDirectory(p)) {
      val visitor = new FileVisitor
      Files.walkFileTree(p, visitor)
      visitor.result.toList
    } else {
      Nil
    }
  }

  /**
    * Deletes the file if it is a Flix package.
    */
  private def deletePackage(file: File): Unit = {
    if (isPkgFile(file.toPath)) {
      file.delete()
    } else {
      throw new RuntimeException(s"Refusing to delete non-Flix package file: ${file.getAbsolutePath}")
    }
  }

  /**
    * Returns `true` if the given path `p` is a jar-file.
    */
  private def isJarFile(p: Path): Boolean = p.getFileName.toString.endsWith(".jar") && isZipArchive(p)

  /**
    * Returns `true` if the given path `p` is a fpkg-file.
    */
  private def isPkgFile(p: Path): Boolean = p.getFileName.toString.endsWith(".fpkg") && isZipArchive(p)

  /**
    * Returns `true` if the given path `p` is a zip-archive.
    */
  private def isZipArchive(p: Path): Boolean = {
    if (Files.exists(p) && Files.isReadable(p) && Files.isRegularFile(p)) {
      // Read the first four bytes of the file.
      return Using(Files.newInputStream(p)) { is =>
        val b1 = is.read()
        val b2 = is.read()
        val b3 = is.read()
        val b4 = is.read()
        // Check if the four first bytes match 0x50, 0x4b, 0x03, 0x04
        return b1 == 0x50 && b2 == 0x4b && b3 == 0x03 && b4 == 0x04
      }.get
    }
    false
  }

  /**
    * @param root the root directory to compute a relative path from the given path
    * @param path the path to be converted to a relative path based on the given root directory
    * @return relative file name separated by slashes, like `path/to/file.ext`
    */
  private def convertPathToRelativeFileName(root: Path, path: Path): String =
    root.relativize(path).toString.replace('\\', '/')

  private class FileVisitor extends SimpleFileVisitor[Path] {
    val result: mutable.ListBuffer[Path] = mutable.ListBuffer.empty

    override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
      result += file
      FileVisitResult.CONTINUE
    }
  }
}
