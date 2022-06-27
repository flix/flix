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

import java.nio.file.Path
import java.io.File

class SourceFiles(source: Either[Path, Seq[File]]) {

  var currentSources: Set[Path] = Set.empty
  var currentLibs: Set[Path] = Set.empty

  var timestamps: Map[Path, Long] = Map.empty

  def addSourcesAndPackages(flix: Flix) = {
    val previousSources = currentSources

    source match {
      case Left(path) =>
        val sourceFiles = getSourceFiles(path)
        val (packages, libraries) = getPackagesAndLibraries(path)

        currentSources = sourceFiles ++ packages
        currentLibs = libraries
  
      case Right(files) =>
        currentSources = files.map(_.toPath).toSet
    }

    for (file <- currentSources
         if hasChanged(file))
      flix.addSourcePath(file)

    for (file <- currentLibs
         if hasChanged(file))
      flix.addJar(file)

    val deletedSources = previousSources -- currentSources
    for (file <- deletedSources)
      flix.remSourcePath(file)

    timestamps = (currentSources ++ currentLibs).map(f => f -> f.toFile.lastModified).toMap
  }

  private def hasChanged(file: Path) = {
    !(timestamps contains file) || (timestamps(file) != file.toFile.lastModified())
  }

  private def filterByExt(files: Seq[Path], ext: String): Set[Path] = {
    files.filter(_.getFileName.toString.endsWith(ext)).toSet
  }

  private def getSourceFiles(path: Path): Set[Path] = {
    val sourceFiles = Packager.getAllFiles(Packager.getSourceDirectory(path))
    val testFiles = Packager.getAllFiles(Packager.getTestDirectory(path))
    filterByExt(sourceFiles ++ testFiles, ".flix").toSet
  }
  
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
