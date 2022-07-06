/*
 * Copyright 2022 Paul Butcher
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
 
package ca.uwaterloo.flix.runtime.shell

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.tools.Packager

import java.nio.file.{Files, Path}
import java.io.File
import scala.collection.mutable

/**
  * The location from which source files should be loaded
  */
sealed trait SourceProvider

case object SourceProvider {
  case class ProjectPath(path: Path) extends SourceProvider
  case class SourceFileList(files: Seq[File]) extends SourceProvider
}

/**
  * Represents the source files loaded by the REPL
  *
  * @param source Either a path representing the current directory (if the REPL is started with no arguments)
  *               or a seqence of files (if the REPL is started with a list of file to load)
  */
class SourceFiles(sourceProvider: SourceProvider) {

  // The sources and libraries currently loaded
  var currentSources: Set[Path] = Set.empty
  var currentLibs: Set[Path] = Set.empty

  // Timestamps at the point the sources were loaded
  var timestamps: Map[Path, Long] = Map.empty

  /**
    * Scan the disk for changes, and reload anything that's changed
    */
  def addSourcesAndPackages(flix: Flix) = {
    val previousSources = currentSources

    sourceProvider match {
      case SourceProvider.ProjectPath(path) =>
        if (Packager.isProjectPath(path)) {
          val sourceFiles = getSourceFiles(path)
          val (packages, libraries) = getPackagesAndLibraries(path)

          currentSources = sourceFiles ++ packages
          currentLibs = libraries
        }
  
      case SourceProvider.SourceFileList(files) =>
        currentSources = files.map(_.toPath).toSet
    }

    for (file <- currentSources
         if hasChanged(file)) {
      val bytes = Files.readAllBytes(file)
      val str = new String(bytes, flix.defaultCharset)
      flix.addSourceCode(file.toString(), str)
    }

    for (file <- currentLibs
         if hasChanged(file))
      flix.addJar(file)

    val deletedSources = previousSources -- currentSources
    for (file <- deletedSources)
      flix.remSourceCode(file.toString)

    timestamps = (currentSources ++ currentLibs).map(f => f -> f.toFile.lastModified).toMap
  }

  /**
    * Returns true if the timestamp of the given source file has changed since the last reload
    */
  private def hasChanged(file: Path) = {
    !(timestamps contains file) || (timestamps(file) != file.toFile.lastModified())
  }

  /**
    * Filters a sequence of files by extension
    */
  private def filterByExt(files: Seq[Path], ext: String): Set[Path] = {
    files.filter(_.getFileName.toString.endsWith(ext)).toSet
  }

  /**
    * Return a set of all .flix files within a project
    */
  private def getSourceFiles(path: Path): Set[Path] = {
    val sourceFiles = Packager.getAllFiles(Packager.getSourceDirectory(path))
    val testFiles = Packager.getAllFiles(Packager.getTestDirectory(path))
    filterByExt(sourceFiles ++ testFiles, ".flix").toSet
  }

  /**
    * Returns a set of all the .fpkg and a set of all the .jar files within a project
    */  
  private def getPackagesAndLibraries(path: Path): (Set[Path], Set[Path]) = {
    val libraryDirectory = Packager.getLibraryDirectory(path)
    if (libraryDirectory.toFile.isDirectory) {
      val files = Packager.getAllFiles(libraryDirectory)
      val packages = filterByExt(files, ".fpkg")
      val libraries = filterByExt(files, ".jar")
      (packages, libraries)
    } else {
      (Set.empty, Set.empty)
    }
  }
}
