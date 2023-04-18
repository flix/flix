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

    val flixPaths = allFlixDeps.map(dep => {
      val depName: String = s"${dep.username}/${dep.projectName}"
      install(depName, dep.version, "fpkg", path) match {
        case Ok(p) => p
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
    * There should be only one file with the given extension.
    *
    * Returns the path to the downloaded file.
    */
  private def install(project: String, version: SemVer, extension: String, p: Path)(implicit out: PrintStream): Result[Path, PackageError] = {
    GitHub.parseProject(project).flatMap { proj =>
      val lib = Bootstrap.getLibraryDirectory(p)
      val assetName = s"${proj.repo}-$version.$extension"
      val folderPath = lib.resolve("github").resolve(proj.owner).resolve(proj.repo).resolve(version.toString)
      //create the folder if it does not exist
      Files.createDirectories(folderPath)
      val assetPath = folderPath.resolve(assetName)

      if (Files.exists(assetPath)) {
        out.println(s"  Cached `${proj.owner}/${proj.repo}.$extension` (v$version).")
        Ok(assetPath)
      } else {
        GitHub.getSpecificRelease(proj, version).flatMap { release =>
          val assets = release.assets.filter(_.name.endsWith(s".$extension"))
          if (assets.isEmpty) {
            Err(PackageError.NoSuchFile(project, extension))
          } else if (assets.length != 1) {
            Err(PackageError.TooManyFiles(project, extension))
          } else {
            // download asset to the folder
            val asset = assets.head
            out.print(s"  Downloading `${proj.owner}/${proj.repo}.$extension` (v$version)... ")
            out.flush()
            try {
              Using(GitHub.downloadAsset(asset)) {
                stream => Files.copy(stream, assetPath, StandardCopyOption.REPLACE_EXISTING)
              }
            } catch {
              case e: IOException => out.println(s"ERROR."); return Err(PackageError.DownloadError(asset, e.getMessage))
            }
            if(Files.exists(assetPath)) {
              out.println(s"OK.")
              Ok(assetPath)
            } else {
              out.println(s"ERROR.")
              Err(PackageError.DownloadError(asset, ""))
            }
          }
        }
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
      tomlPaths <- traverse(flixDeps)(dep => {
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
    * Finds the Flix dependencies in a Manifest.
    */
  private def findFlixDependencies(manifest: Manifest): List[FlixDependency] = {
    manifest.dependencies.collect { case dep: FlixDependency => dep }
  }

}
