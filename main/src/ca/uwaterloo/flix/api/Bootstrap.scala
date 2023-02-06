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

import ca.uwaterloo.flix.tools.pkg.Manifest

import java.nio.file.Path
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
    b.bootstrap("path-to-flix-project")
  }

}

class Bootstrap {

  private val sourcePaths: mutable.ListBuffer[Path] = mutable.ListBuffer.empty

  def bootstrap(path: String): Unit = { // TODO: Probably return Result or Validation
    //
    // Determine the mode: If `path/flix.toml` exists then "project" mode else "folder mode".
    //
    if (???) {
      projectMode()
    } else {
      folderMode()
    }
  }

  private def projectMode(): Unit = { // TODO: Probably return Result or Validation
    // 1. Read, parse, and validate flix.toml.
    // 2. Check each dependency is available or download it.
    // 3. Compute the set of JAR paths and Flix fpkg paths.
    // 4. Add *.flix, src/**.flix and test/**.flix
  }

  private def folderMode(): Unit = { // TODO: Probably return Result or Validation
    // 1. Add *.flix, src/**.flix and test/**.flix
    // 2. Grab all jars in lib/
    // 3. Grab all flix packages in lib/
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


}
