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
  def main(args: Array[String]): Unit = {
    // TODO: Do ad-hoc testing here.
    val b = new Bootstrap
    b.bootstrap(".")
  }

}

class Bootstrap {

  private val sourcePaths: mutable.ListBuffer[Path] = mutable.ListBuffer.empty

  def bootstrap(pathString: String): Result[List[Path], BootstrapError] = {
    //
    // Determine the mode: If `path/flix.toml` exists then "project" mode else "folder mode".
    //
    val path = Paths.get(pathString)
    val tomlPath = getManifestFile(path)
    if (Files.exists(tomlPath)) {
      projectMode(path)
    } else {
      folderMode(path)
    }
  }

  private def projectMode(path: Path): Result[List[Path], BootstrapError] = {
    // 1. Read, parse, and validate flix.toml.
    val tomlPath = getManifestFile(path)
    val manifest = ManifestParser.parse(tomlPath) match {
      case Ok(m) => println(m); m
      case Err(e) => return Err(BootstrapError.ManifestParseError(e))
    }

    // 2. Check each dependency is available or download it.
    FlixPackageManager.installAll(manifest, path)(System.out) match {
      case Ok(_) => // do nothing TODO: return list of paths
      case Err(e) => return Err(BootstrapError.FlixPackageError(e))
    }
    MavenPackageManager.installAll(manifest)(System.out) match {
      case Ok(_) => // do nothing
      case Err(e) => return Err(BootstrapError.MavenPackageError(e))
    }

    // 3. Compute the set of JAR paths and Flix fpkg paths.
    val filesFlix = manifest.getFlixPackages //TODO: implement - why use manifest?
    val filesMaven = manifest.getMavenPackages //TODO: implement - where are Coursier dependencies?

    // 4. Add *.flix, src/**.flix and test/**.flix
    val filesHere = getAllFlixFilesHere(path)
    val filesSrc = getAllFilesWithExt(getSourceDirectory(path), "flix")
    val filesTest = getAllFilesWithExt(getTestDirectory(path), "flix")

    val res = filesFlix ++ filesMaven ++ filesHere ++ filesSrc ++ filesTest
    println(res)
    Ok(res)
  }

  private def folderMode(path: Path): Result[List[Path], BootstrapError] = {
    // 1. Add *.flix, src/**.flix and test/**.flix
    val filesHere = getAllFlixFilesHere(path)
    val filesSrc = getAllFilesWithExt(getSourceDirectory(path), "flix")
    val filesTest = getAllFilesWithExt(getTestDirectory(path), "flix")

    // 2. Grab all jars in lib/
    val jarFilesLib = getAllFilesWithExt(getLibraryDirectory(path), "jar")

    // 3. Grab all flix packages in lib/
    val flixFilesLib = getAllFilesWithExt(getLibraryDirectory(path), "fpkg")

    val res = filesHere ++ filesSrc ++ filesTest ++ jarFilesLib ++ flixFilesLib
    println(res)
    Ok(res)
  }

  def reconfigureFlix(flix: Flix): Unit = { // TODO: Probably return Result or Validation

    val manifest: Manifest = ??? // TODO: Probably from a field.

    // TODO: Add logic to check if the file has changed.
    for (path <- sourcePaths) {
      flix.addSourcePath(path)
    }

    for (flixPackagePath <- manifest.getFlixPackages) {
      flix.addSourcePath(flixPackagePath)
    }

    for (mavenJarPath <- manifest.getMavenPackages) {
      flix.addSourcePath(mavenJarPath)
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

  //Below: copied from Packager.scala

  /**
    * Returns the path to the library directory relative to the given path `p`.
    */
  private def getLibraryDirectory(p: Path): Path = p.resolve("./lib/").normalize()

  /**
    * Returns the path to the source directory relative to the given path `p`.
    */
  private def getSourceDirectory(p: Path): Path = p.resolve("./src/").normalize()

  /**
    * Returns the path to the test directory relative to the given path `p`.
    */
  private def getTestDirectory(p: Path): Path = p.resolve("./test/").normalize()

  /**
    * Returns the path to the Manifest file relative to the given path `p`.
    */
  private def getManifestFile(p: Path): Path = p.resolve("./flix.toml").normalize()

  /**
    * Returns all files in the given path `p` ending with .`ext`.
    */
  private def getAllFilesWithExt(p: Path, ext: String): List[Path] =
    getAllFiles(p).filter(p => p.getFileName.toString.endsWith(s".$ext"))

  /**
    * Returns all files in the given path `p`.
    */
  private def getAllFiles(p: Path): List[Path] = {
    if (Files.isReadable(p) && Files.isDirectory(p)) {
      val visitor = new FileVisitor
      Files.walkFileTree(p, visitor)
      visitor.result.toList
    } else {
      Nil
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
