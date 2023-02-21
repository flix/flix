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

import ca.uwaterloo.flix.tools.Packager.getLibraryDirectory
import ca.uwaterloo.flix.tools.pkg.Dependency.FlixDependency
import ca.uwaterloo.flix.tools.pkg.github.GitHub
import ca.uwaterloo.flix.util.Result.{Err, Ok, ToOk}
import ca.uwaterloo.flix.util.Result

import java.io.{IOException, PrintStream}
import java.nio.file.{Files, Path, StandardCopyOption}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Using

object FlixPackageManager {

  // TODO: Move functionality from "Packager" in here.

  /**
    * Installs all the Flix dependencies for a Manifest.
    */
  def installAll(manifest: Manifest, path: Path)(implicit out: PrintStream): Result[Unit, PackageError] = {
    val flixDeps = findFlixDependencies(manifest)

    for(dep <- flixDeps) {
      val depName: String = s"${dep.username}/${dep.projectName}"
      install(depName, Some(dep.version), path) match {
        case Ok(_) => //do nothing
        case Err(e) => out.println(s"Installation of $depName failed"); return Err(e)
      }
    }
    ().toOk
  }

  /**
    *  Installs a flix package from the Github `project`.
    *
    * `project` must be of the form `<owner>/<repo>`
    *
    * The package is installed at `lib/<owner>/<repo>`
    */
  def install(project: String, version: Option[SemVer], p: Path)(implicit out: PrintStream): Result[Unit, PackageError] = {
    GitHub.parseProject(project).flatMap {
      proj =>
        (version match {
          case None => GitHub.getLatestRelease(proj)
          case Some(ver) => GitHub.getSpecificRelease(proj, ver)
        }).flatMap {
          release =>
            val assets = release.assets.filter(_.name.endsWith(".fpkg"))
            val lib = getLibraryDirectory(p)
            val assetFolder = createAssetFolderPath(proj, release, lib)

            // create the asset directory if it doesn't exist
            Files.createDirectories(assetFolder)

            // download each asset to the folder
            for (asset <- assets) {
              val assetName = asset.name
              val path = assetFolder.resolve(assetName)
              val newDownload = !Files.exists(path)
              if(newDownload) {
                out.println(s"Installing $assetName")
                try {
                  Using(GitHub.downloadAsset(asset)) {
                    stream => Files.copy(stream, path, StandardCopyOption.REPLACE_EXISTING)
                  }
                } catch {
                  case _: IOException => return Err(PackageError.DownloadError(s"Error occurred while downloading $assetName"))
                }
                out.println(s"Installation of $assetName completed")
              } else {
                out.println(s"$assetName already exists")
              }
            }
            ().toOk
        }
    }
  }

  /**
    * Creates a path from the `lib` folder to where assets should be stored.
    * The path will look like this: `lib`/owner/repo/verX.X.X.
    */
  private def createAssetFolderPath(proj: GitHub.Project, release: GitHub.Release, lib: Path): Path = {
    lib.resolve(proj.owner).resolve(proj.repo).resolve(s"ver${release.version.toString()}")
  }

  /**
    * Finds the Flix dependencies in a Manifest.
    */
  private def findFlixDependencies(manifest: Manifest): List[FlixDependency] = {
    manifest.dependencies.collect{ case dep: FlixDependency => dep }
  }

}
