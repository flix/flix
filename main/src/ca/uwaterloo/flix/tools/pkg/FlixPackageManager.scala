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
package ca.uwaterloo.flix.tools.pkg

import ca.uwaterloo.flix.tools.Packager
import ca.uwaterloo.flix.tools.Packager.getLibraryDirectory
import ca.uwaterloo.flix.tools.pkg.github.GitHub
import ca.uwaterloo.flix.util.Result.ToOk
import ca.uwaterloo.flix.util.{Options, Result}

import java.io.File
import java.nio.file.{Files, Path, StandardCopyOption}
import scala.util.Using

object FlixPackageManager {

  // TODO: Move functionality from "Packager" in here.

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
    * Deletes the file if it is a Flix package.
    */
  private def deletePackage(file: File): Unit = {
    if (Packager.isPkgFile(file.toPath)) {
      file.delete()
    } else {
      throw new RuntimeException(s"Refusing to delete non-Flix package file: ${file.getAbsolutePath}")
    }
  }

}
