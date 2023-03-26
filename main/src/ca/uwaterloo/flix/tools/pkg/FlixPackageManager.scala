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

import ca.uwaterloo.flix.api.Bootstrap
import ca.uwaterloo.flix.tools.pkg.Dependency.FlixDependency
import ca.uwaterloo.flix.tools.pkg.github.GitHub
import ca.uwaterloo.flix.util.Result.{Err, Ok, ToOk, flatTraverse, traverse}
import ca.uwaterloo.flix.util.Result

import java.io.{IOException, PrintStream}
import java.nio.file.{Files, Path, StandardCopyOption}
import scala.util.Using

object FlixPackageManager {

  /**
    * Finds all the transitive dependencies for `manifest` and
    * returns their manifests. The toml files for the manifests
    * will be put at `path`/lib.
    */
  def findTransitiveDependencies(manifest: Manifest, path: Path)(implicit out: PrintStream): Result[List[Manifest], PackageError] = {
    out.println("Resolving Flix dependencies...")

    findTransitiveDependenciesRec(manifest, path, List(manifest))
  }

  /**
    * Installs all the Flix dependencies for a list of Manifests at the /lib folder
    * of `path` and returns a list of paths to all the dependencies.
    */
  def installAll(manifests: List[Manifest], path: Path)(implicit out: PrintStream): Result[List[Path], PackageError] = {
    out.println("Downloading Flix dependencies...")

    val allFlixDeps: List[FlixDependency] = manifests.foldLeft(List.empty[FlixDependency])((l, m) => l ++ findFlixDependencies(m))

    val flixPaths = allFlixDeps.flatMap(dep => {
      val depName: String = s"${dep.username}/${dep.projectName}"
      install(depName, dep.version, "fpkg", path) match {
        case Ok(l) => l
        case Err(e) => out.println(s"ERROR: Installation of `$depName' failed."); return Err(e)
      }
    })

    Ok(flixPaths)
  }

  /**
    * Installs a flix package from the Github `project`.
    *
    * `project` must be of the form `<owner>/<repo>`
    *
    * The package is installed at `lib/<owner>/<repo>`
    *
    * Returns a list of paths to the downloaded files.
    */
  private def install(project: String, version: SemVer, extension: String, p: Path)(implicit out: PrintStream): Result[List[Path], PackageError] = {
    GitHub.parseProject(project).flatMap {
      proj =>
        GitHub.getSpecificRelease(proj, version).flatMap {
          release =>
            val assets = release.assets.filter(_.name.endsWith(s".$extension"))
            val lib = Bootstrap.getLibraryDirectory(p)
            val assetFolder = createAssetFolderPath(proj, release, lib)

            // create the asset directory if it doesn't exist
            Files.createDirectories(assetFolder)

            // download each asset to the folder
            for (asset <- assets) {
              val assetName = asset.name
              val path = assetFolder.resolve(assetName)
              val newDownload = !Files.exists(path)
              if (newDownload) {
                out.print(s"  Downloading `$project/$assetName' (v$version)... ")
                out.flush()
                try {
                  Using(GitHub.downloadAsset(asset)) {
                    stream => Files.copy(stream, path, StandardCopyOption.REPLACE_EXISTING)
                  }
                } catch {
                  case _: IOException => return Err(PackageError.DownloadError(asset))
                }
                out.println(s"OK.")
              } else {
                out.println(s"  Cached `$project/$assetName' (v$version).")
              }
            }
            assets.map(asset => assetFolder.resolve(asset.name)).toOk
        }
    }
  }

  /**
    * Recursively finds all transitive dependencies of `manifest`.
    * Downloads any missing toml files for found dependencies and
    * parses them to manifests. Returns the list of manifests.
    * `res` is the list of Manifests found so far to avoid duplicates.
    */
  private def findTransitiveDependenciesRec(manifest: Manifest, path: Path, res: List[Manifest])(implicit out: PrintStream): Result[List[Manifest], PackageError] = {
    //find Flix dependencies of the current manifest
    val flixDeps = findFlixDependencies(manifest)

    for (
      //download toml files
      tomlPaths <- flatTraverse(flixDeps)(dep => {
        val depName = s"${dep.username}/${dep.projectName}"
        install(depName, dep.version, "toml", path)
      });

      //parse the manifests
      transManifests <- traverse(tomlPaths)(p => parseManifest(p))

    ) yield {
      //remove duplicates
      val newManifests = transManifests.filter(m => !res.contains(m))
      var newRes = res ++ newManifests

      //do recursive calls for all dependencies
      for (m <- newManifests) {
        findTransitiveDependenciesRec(m, path, newRes) match {
          case Ok(t) => newRes = newRes ++ t.filter(a => !newRes.contains(a))
          case Err(e) => return Err(e)
        }
      }
      newRes
    }
  }

  /**
    * Parses the toml file at `path` into a Manifest,
    * and converts any error to a PackageError.
    */
  private def parseManifest(path: Path): Result[Manifest, PackageError] = {
    ManifestParser.parse(path) match {
      case Ok(t) => Ok(t)
      case Err(e) => Err(PackageError.ManifestParseError(e))
    }
  }

  /**
    * Creates a path from the `lib` folder to where assets should be stored.
    * The path will look like this: `lib`/owner/repo/vX.X.X.
    */
  private def createAssetFolderPath(proj: GitHub.Project, release: GitHub.Release, lib: Path): Path = {
    lib.resolve(proj.owner).resolve(proj.repo).resolve(release.version.toString)
  }

  /**
    * Finds the Flix dependencies in a Manifest.
    */
  private def findFlixDependencies(manifest: Manifest): List[FlixDependency] = {
    manifest.dependencies.collect { case dep: FlixDependency => dep }
  }

}
