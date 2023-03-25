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

import ca.uwaterloo.flix.tools.pkg.{FlixPackageManager, ManifestParser, MavenPackageManager, Manifest}
import ca.uwaterloo.flix.util.Result
import ca.uwaterloo.flix.util.Result.{Err, Ok, ToOk}

import java.io.PrintStream
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, Paths, SimpleFileVisitor}
import scala.collection.mutable

object Bootstrap {

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
    * Returns the path to the Manifest file relative to the given path `p`.
    */
  def getManifestFile(p: Path): Path = p.resolve("./flix.toml").normalize()

  /**
    * Returns all files in the given path `p` ending with .`ext`.
    */
  private def getAllFilesWithExt(p: Path, ext: String): List[Path] =
    getAllFiles(p).filter(p => p.getFileName.toString.endsWith(s".$ext"))

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
    * Returns all .flix files directly in the directory given by `p`.
    */
  private def getAllFlixFilesHere(path: Path): List[Path] = {
    val files = path.toFile.listFiles()
    if (files == null) {
      List.empty
    } else {
      files.toList.map(f => f.toPath).filter(p => p.getFileName.toString.endsWith(s".flix"))
    }
  }

  private class FileVisitor extends SimpleFileVisitor[Path] {
    val result: mutable.ListBuffer[Path] = mutable.ListBuffer.empty

    override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
      result += file
      FileVisitResult.CONTINUE
    }
  }

  /**
    * Creates a new Bootstrap object and initializes it.
    * If a `flix.toml` file exists, parses that to a Manifest and
    * downloads all required files. Otherwise checks the /lib folder
    * to see what dependencies are already downloadet. Also finds
    * all .flix source files.
    * Then returns the initialized Bootstrap object or an error.
    */
  def bootstrap(path: Path)(implicit out: PrintStream): Result[Bootstrap, BootstrapError] = {
    //
    // Determine the mode: If `path/flix.toml` exists then "project" mode else "folder mode".
    //
    val bootstrap = new Bootstrap()
    val tomlPath = Bootstrap.getManifestFile(path)
    if (Files.exists(tomlPath)) {
      out.println("Found `flix.toml'. Checking dependencies...")
      bootstrap.projectMode(path).map(_ => bootstrap)
    } else {
      out.println("No `flix.toml'. Will load source files from `*.flix`, `src/**`, and `test/**`.")
      bootstrap.folderMode(path).map(_ => bootstrap)
    }
  }
}

class Bootstrap {

  // Timestamps at the point the sources were loaded
  private var timestamps: Map[Path, Long] = Map.empty

  // Lists of paths to the source files, flix packages and .jar files used
  private var sourcePaths: List[Path] = List.empty
  private var flixPackagePaths: List[Path] = List.empty
  private var mavenPackagePaths: List[Path] = List.empty

  /**
    * Parses `flix.toml` to a Manifest and downloads all required files.
    * Then makes a list of all flix source files, flix packages
    * and .jar files that this project uses.
    */
  private def projectMode(path: Path)(implicit out: PrintStream): Result[Unit, BootstrapError] = {
    // 1. Read, parse, and validate flix.toml.
    val tomlPath = Bootstrap.getManifestFile(path)
    val manifest = ManifestParser.parse(tomlPath) match {
      case Ok(m) => m
      case Err(e) => return Err(BootstrapError.ManifestParseError(e))
    }

    // 2. Check each dependency is available or download it.
    val manifests: List[Manifest] = FlixPackageManager.findTransitiveDependencies(manifest, path) match {
      case Ok(l) => l
      case Err(e) => return Err(BootstrapError.FlixPackageError(e))
    }
    FlixPackageManager.installAll(manifests, path) match {
      case Ok(l) => flixPackagePaths = l
      case Err(e) => return Err(BootstrapError.FlixPackageError(e))
    }
    MavenPackageManager.installAll(manifests, path) match {
      case Ok(l) => mavenPackagePaths = l
      case Err(e) => return Err(BootstrapError.MavenPackageError(e))
    }
    out.println("Dependency resolution completed.")

    // 3. Add *.flix, src/**.flix and test/**.flix
    val filesHere = Bootstrap.getAllFlixFilesHere(path)
    val filesSrc = Bootstrap.getAllFilesWithExt(Bootstrap.getSourceDirectory(path), "flix")
    val filesTest = Bootstrap.getAllFilesWithExt(Bootstrap.getTestDirectory(path), "flix")
    sourcePaths = filesHere ++ filesSrc ++ filesTest
    ().toOk
  }

  /**
    * Checks the /lib folder to find existing flix packages and .jar files.
    * Then makes a list of all flix source files, flix packages
    * and .jar files that this project uses.
    */
  private def folderMode(path: Path): Result[Unit, BootstrapError] = {
    // 1. Add *.flix, src/**.flix and test/**.flix
    val filesHere = Bootstrap.getAllFlixFilesHere(path)
    val filesSrc = Bootstrap.getAllFilesWithExt(Bootstrap.getSourceDirectory(path), "flix")
    val filesTest = Bootstrap.getAllFilesWithExt(Bootstrap.getTestDirectory(path), "flix")
    sourcePaths = filesHere ++ filesSrc ++ filesTest

    // 2. Grab all jars in lib/
    val jarFilesLib = Bootstrap.getAllFilesWithExt(Bootstrap.getLibraryDirectory(path), "jar")
    mavenPackagePaths = jarFilesLib

    // 3. Grab all flix packages in lib/
    val flixFilesLib = Bootstrap.getAllFilesWithExt(Bootstrap.getLibraryDirectory(path), "fpkg")
    flixPackagePaths = flixFilesLib

    ().toOk
  }

  /**
    * Checks to see if any source files or packages have been changed.
    * If they have, they are added to flix. Then updates the timestamps
    * map to reflect the current source files and packages.
    */
  def reconfigureFlix(flix: Flix): Unit = {
    val previousSources = timestamps.keySet

    for (path <- sourcePaths if hasChanged(path)) {
      flix.addFlix(path)
    }

    for (path <- flixPackagePaths if hasChanged(path)) {
      flix.addPkg(path)
    }

    for (path <- mavenPackagePaths if hasChanged(path)) {
      flix.addJar(path)
    }

    val currentSources = (sourcePaths ++ flixPackagePaths ++ mavenPackagePaths).filter(p => Files.exists(p))

    val deletedSources = previousSources -- currentSources
    for (path <- deletedSources) {
      flix.remSourceCode(path.toString)
    }

    timestamps = currentSources.map(f => f -> f.toFile.lastModified).toMap
  }

  /**
    * Returns true if the timestamp of the given source file has changed since the last reload.
    */
  private def hasChanged(file: Path) = {
    !(timestamps contains file) || (timestamps(file) != file.toFile.lastModified())
  }
}
