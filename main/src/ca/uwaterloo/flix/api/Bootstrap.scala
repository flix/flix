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

import ca.uwaterloo.flix.tools.pkg.{FlixPackageManager, Manifest, ManifestParser, MavenPackageManager}
import ca.uwaterloo.flix.util.Result
import ca.uwaterloo.flix.util.Result.{Err, Ok}

import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, Paths, SimpleFileVisitor}
import scala.collection.mutable

// TODO: This class is ultimately to replace functionality in:
// SourceProvider, SourceFiles, Packager, etc.
// Feel free to look there for functionality that can be reused.
// The idea is to implement everything in here so it works 100% and then remove the old code.
// Hence its OK to copy paste some methods from the aforementioned classes in here.

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
  def getAllFilesWithExt(p: Path, ext: String): List[Path] =
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
  def getAllFlixFilesHere(path: Path): List[Path] = {
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

}

class Bootstrap {

  // Timestamps at the point the sources were loaded
  private var timestamps: Map[Path, Long] = Map.empty

  // Lists of paths to the source files, flix packages and .jar files used
  private var sourcePaths: List[Path] = List.empty
  private var flixPackagePaths: List[Path] = List.empty
  private var mavenPackagePaths: List[Path] = List.empty

  /**
    * If a `flix.toml` file exists, parses that to a Manifest and
    * downloads all required files. Otherwise checks the /lib folder
    * to see what dependencies are already downloadet.
    * Then returns a list of all flix source files, flix packages
    * and .jar files that this project uses.
    */
  def bootstrap(pathString: String): Result[List[Path], BootstrapError] = {
    //
    // Determine the mode: If `path/flix.toml` exists then "project" mode else "folder mode".
    //
    val path = Paths.get(pathString)
    val tomlPath = Bootstrap.getManifestFile(path)
    if (Files.exists(tomlPath)) {
      projectMode(path)
    } else {
      folderMode(path)
    }
  }

  /**
    * Parses `flix.toml` to a Manifest and downloads all required files.
    * Then makes a list of all flix source files, flix packages
    * and .jar files that this project uses.
    */
  private def projectMode(path: Path): Result[List[Path], BootstrapError] = {
    // 1. Read, parse, and validate flix.toml.
    val tomlPath = Bootstrap.getManifestFile(path)
    val manifest = ManifestParser.parse(tomlPath) match {
      case Ok(m) => m
      case Err(e) => return Err(BootstrapError.ManifestParseError(e))
    }

    // 2. Check each dependency is available or download it.
    FlixPackageManager.installAll(manifest, path)(System.out) match {
      case Ok(l) => flixPackagePaths = l
      case Err(e) => return Err(BootstrapError.FlixPackageError(e))
    }
    MavenPackageManager.installAll(manifest, path)(System.out) match {
      case Ok(l) => mavenPackagePaths = l
      case Err(e) => return Err(BootstrapError.MavenPackageError(e))
    }

    // 3. Add *.flix, src/**.flix and test/**.flix
    val filesHere = Bootstrap.getAllFlixFilesHere(path)
    val filesSrc = Bootstrap.getAllFilesWithExt(Bootstrap.getSourceDirectory(path), "flix")
    val filesTest = Bootstrap.getAllFilesWithExt(Bootstrap.getTestDirectory(path), "flix")
    sourcePaths = filesHere ++ filesSrc ++ filesTest

    timestamps = timestamps ++ (flixPackagePaths ++ mavenPackagePaths ++ sourcePaths).map(f => f -> f.toFile.lastModified).toMap

    Ok(sourcePaths ++ flixPackagePaths ++ mavenPackagePaths)
  }

  /**
    * Checks the /lib folder to find existing flix packages and .jar files.
    * Then makes a list of all flix source files, flix packages
    * and .jar files that this project uses.
    */
  private def folderMode(path: Path): Result[List[Path], BootstrapError] = {
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

    Ok(sourcePaths ++ flixPackagePaths ++ mavenPackagePaths)
  }

  /**
    * Checks to see if any source files or packages have been changed.
    * If they have, they are added to flix. Then updates the timestamps
    * map to reflect the current source files and packages.
    */
  def reconfigureFlix(flix: Flix): Unit = {
    val previousSources = timestamps.keySet

    for (path <- sourcePaths if hasChanged(path)) {
      flix.addSourcePath(path)
    }

    for (path <- flixPackagePaths if hasChanged(path)) {
      flix.addSourcePath(path)
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
