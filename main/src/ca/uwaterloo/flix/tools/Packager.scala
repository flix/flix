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

import ca.uwaterloo.flix.api.{Bootstrap, Flix, Version}
import ca.uwaterloo.flix.language.ast.Ast
import ca.uwaterloo.flix.language.ast.Ast.Source
import ca.uwaterloo.flix.runtime.CompilationResult
import ca.uwaterloo.flix.util.Formatter.AnsiTerminalFormatter
import ca.uwaterloo.flix.util.Result.{ToErr, ToOk}
import ca.uwaterloo.flix.util.Validation.{ToFailure, ToSuccess}
import ca.uwaterloo.flix.util._

import java.io.PrintWriter
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes
import java.util.zip.{ZipEntry, ZipFile, ZipOutputStream}
import java.util.{Calendar, GregorianCalendar}
import scala.collection.mutable
import scala.util.{Failure, Success, Using}

/**
  * An interface to manage flix packages.
  */
object Packager {

  //TODO: fix comments

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
    val buildDirectory = Bootstrap.getBuildDirectory(p)
    val libraryDirectory = Bootstrap.getLibraryDirectory(p)
    val sourceDirectory = Bootstrap.getSourceDirectory(p)
    val testDirectory = Bootstrap.getTestDirectory(p)

    val manifestFile = Bootstrap.getManifestFile(p)
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

    newFileIfAbsent(manifestFile) {
      s"""[package]
        |name    = "$packageName"
        |description = "test"
        |version = "0.1.0"
        |flix    = "${Version.CurrentVersion}"
        |authors = ["Tester"]
        |""".stripMargin
    }

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
  def check(b: Bootstrap, o: Options): Result[Unit, Int] = {
    // Configure a new Flix object.
    implicit val flix: Flix = new Flix()
    flix.setOptions(o)

    // Add sources and packages.
    b.reconfigureFlix(flix)

    flix.check() match {
      case Validation.Success(_) => ().toOk
      case failure =>
        flix.mkMessages(failure.errors).foreach(println)
        Result.Err(1)
    }
  }

  /**
    * Builds (compiles) the source files for the given project path `p`.
    */
  def build(b: Bootstrap, o: Options, loadClasses: Boolean = true)(implicit flix: Flix = new Flix()): Result[CompilationResult, Int] = {
    // Configure a new Flix object.
    val newOptions = o.copy(
      output = Some(Bootstrap.getBuildDirectory(b.path)),
      loadClassFiles = loadClasses
    )
    flix.setOptions(newOptions)

    // Add sources and packages.
    b.reconfigureFlix(flix)

    flix.compile() match {
      case Validation.Success(r) => Result.Ok(r)
      case failure =>
        flix.mkMessages(failure.errors).foreach(println)
        1.toErr
    }
  }

  /**
    * Builds a jar package for the given project path `p`.
    */
  def buildJar(b: Bootstrap, o: Options): Result[Unit, Int] = {
    // The path to the jar file.
    val jarFile = getJarFile(b.path)

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
      for ((buildFile, fileNameWithSlashes) <- Bootstrap.getAllFiles(Bootstrap.getBuildDirectory(b.path))
        .map { path => (path, convertPathToRelativeFileName(Bootstrap.getBuildDirectory(b.path), path)) }
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
    * Returns a Validation containing the list of missing paths in case the path is not a project path.
    */
  private def checkProjectPath(p: Path): Validation[Unit, Path] = {
    val required = List(
      Bootstrap.getSourceDirectory(p),
      Bootstrap.getTestDirectory(p),
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
    * Builds a flix package for the given project path `p`.
    */
  def buildPkg(b: Bootstrap, o: Options): Result[Unit, Int] = {
    //
    // We require that a Flix package has a specific structure.
    //
    checkProjectPath(b.path) match {
      case Validation.Success(_) => ()
      case failure => throw new RuntimeException(s"Missing files or directories: ${failure.errors.mkString(", ")}")
    }

    // The path to the fpkg file.
    val pkgFile = getPkgFile(b.path)

    // Check whether it is safe to write to the file.
    if (Files.exists(pkgFile) && !isPkgFile(pkgFile)) {
      throw new RuntimeException(s"The path '$pkgFile' exists and is not a fpkg-file. Refusing to overwrite.")
    }

    // Construct a new zip file.
    Using(new ZipOutputStream(Files.newOutputStream(pkgFile))) { zip =>
      // Add required resources.
      addToZip(zip, "LICENSE.md", getLicenseFile(b.path))
      addToZip(zip, "README.md", getReadmeFile(b.path))

      // Add all source files.
      // Here we sort entries by relative file name to apply https://reproducible-builds.org/
      for ((sourceFile, fileNameWithSlashes) <- Bootstrap.getAllFiles(Bootstrap.getSourceDirectory(b.path))
        .map { path => (path, convertPathToRelativeFileName(b.path, path)) }
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
  def run(b: Bootstrap, o: Options): Result[Unit, Int] = {
    val res = for {
      compilationResult <- build(b, o).toOption
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
  def benchmark(b: Bootstrap, o: Options): Result[Unit, Int] = {
    build(b, o) map {
      compilationResult =>
        Benchmarker.benchmark(compilationResult, new PrintWriter(System.out, true))(o)
    }
  }

  /**
    * Runs all tests in the flix package for the given project path `p`.
    */
  def test(b: Bootstrap, o: Options): Result[Unit, Int] = {
    implicit val flix: Flix = new Flix().setFormatter(AnsiTerminalFormatter)
    build(b, o) flatMap {
      compilationResult =>
        Tester.run(Nil, compilationResult)
        ().toOk
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
  private def getMainSourceFile(p: Path): Path = Bootstrap.getSourceDirectory(p).resolve("./Main.flix").normalize()

  /**
    * Returns the path to the main test file relative to the given path `p`.
    */
  private def getMainTestFile(p: Path): Path = Bootstrap.getTestDirectory(p).resolve("./TestMain.flix").normalize()

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
    * Returns `true` if the given path `p` is a jar-file.
    */
  private def isJarFile(p: Path): Boolean = p.getFileName.toString.endsWith(".jar") && isZipArchive(p)

  /**
    * Returns `true` if the given path `p` is a fpkg-file.
    */
  def isPkgFile(p: Path): Boolean = p.getFileName.toString.endsWith(".fpkg") && isZipArchive(p)

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

}
