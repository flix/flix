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
import ca.uwaterloo.flix.tools.pkg.FlixPackageManager.downloadFromGitHub
import ca.uwaterloo.flix.tools.pkg.github.GitHub
import ca.uwaterloo.flix.util.Result.{Err, Ok, ToOk, traverse}
import ca.uwaterloo.flix.util.Result
import org.apache.commons.io.FileUtils

import java.io.{IOException, PrintStream}
import java.nio.file.{Files, Path, StandardCopyOption}
import scala.util.Using

object FlixPackageManager {

  //All Maven dependencies found transitively from Flix dependencies
  val mavenSet = collection.mutable.Set.empty[String]

  //The total set of Flix dependencies found
  private val flixSet = collection.mutable.Set.empty[FlixDependency]

  //The set of Flix dependencies where transitive dependencies have not yet been found
  //TODO: how to make this prettier?
  private var workSet = collection.mutable.Set.empty[FlixDependency]

  /**
    * Installs all the Flix dependencies for a Manifest at the /lib folder
    * of `path` and returns a list of paths to all the dependencies.
    */
  def installAll(manifest: Manifest, path: Path)(implicit out: PrintStream): Result[List[Path], PackageError] = {
    out.println("Resolving Flix dependencies...")

    val flixDepsManifest = findFlixDependencies(manifest)

    findTransitiveDependencies(flixDepsManifest, path) match {
      case Ok(_) =>
      case Err(e) => return Err(e)
    }

    flixSet.toList.flatMap(dep => {
      val depName: String = s"${dep.username}/${dep.projectName}"
      install(depName, dep.version, path) match {
        case Ok(l) => l
        case Err(e) => out.println(s"ERROR: Installation of `$depName' failed."); return Err(e)
      }
    }).toOk
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
  private def install(project: String, version: SemVer, p: Path)(implicit out: PrintStream): Result[List[Path], PackageError] = {
    GitHub.parseProject(project).flatMap {
      proj =>
        GitHub.getSpecificRelease(proj, version).flatMap {
          release =>
            val assets = release.assets.filter(_.name.endsWith(".fpkg"))
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
                downloadFromGitHub(asset, path) match {
                  case Ok(_) =>
                  case Err(e) => return Err(e)
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

  //TODO: test!
  /**
    * Finds all transitive dependencies for a list of Flix dependencies,
    * and puts the list in `flixSet`. Also finds any Maven dependencies,
    * and puts those in `mavenSet`. Returns an error if anything goes wrong.
    */
  private def findTransitiveDependencies(flixDepsManifest: List[FlixDependency], p: Path): Result[Unit, PackageError] = {
    workSet = collection.mutable.Set.empty[FlixDependency]

    flixDepsManifest.foreach(dep => {workSet.addOne(dep); flixSet.addOne(dep)})
    val tempDir = Files.createTempDirectory(p, "")

    while(workSet.nonEmpty) {
      val dep = workSet.head
      workSet.remove(dep)

      for(
        proj <- GitHub.parseProject(s"${dep.username}/${dep.projectName}");
        release <- GitHub.getSpecificRelease(proj, dep.version)
      ) yield {
        val tomlFiles = release.assets.filter(_.name.endsWith(".toml"))
        traverse(tomlFiles)(tomlFile => {
          val assetName = tomlFile.name
          val tomlPath = tempDir.resolve(s"${dep.username}-${dep.projectName}-$assetName")
          processTomlFile(tomlFile, tomlPath)
        }) match {
          case Ok(_) =>
          case Err(e) => return Err(e)
        }
      }
    }

    //TODO: should we save the toml files?
    FileUtils.deleteDirectory(tempDir.toFile)

    ().toOk
  }

  /**
    * Downloads `tomlFile` from GitHub and puts it at `path`.
    * Then finds new transitive Flix dependencies and
    * Maven dependencies and adds them to the respective sets.
    */
  private def processTomlFile(tomlFile: GitHub.Asset, path: Path): Result[Unit, PackageError] = {
    for (
      _ <- downloadFromGitHub(tomlFile, path);
      manifest <- parseManifest(path)
    ) yield {
      val newFlixDeps = findFlixDependencies(manifest).filter(d => !flixSet.contains(d))
      workSet.addAll(newFlixDeps)
      flixSet.addAll(newFlixDeps)

      val newMavenDeps = MavenPackageManager.getMavenDependencyStrings(manifest)
      mavenSet.addAll(newMavenDeps)
    }
  }

  /**
    * Downloads `asset` from GitHub and puts it at `path`.
    * Returns an error if something goes wrong.
    */
  private def downloadFromGitHub(asset: GitHub.Asset, path: Path): Result[Unit, PackageError] = {
    val assetName: String = asset.name
    try {
      Using(GitHub.downloadAsset(asset)) {
        stream => Files.copy(stream, path, StandardCopyOption.REPLACE_EXISTING)
      }
    } catch {
      case _: IOException => return Err(PackageError.DownloadError(s"Error occurred while downloading $assetName"))
    }
    ().toOk
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
    * The path will look like this: `lib`/owner/repo/verX.X.X.
    */
  private def createAssetFolderPath(proj: GitHub.Project, release: GitHub.Release, lib: Path): Path = {
    lib.resolve(proj.owner).resolve(proj.repo).resolve(s"ver${release.version.toString()}")
  }

  /**
    * Finds the Flix dependencies in a Manifest.
    */
  private def findFlixDependencies(manifest: Manifest): List[FlixDependency] = {
    manifest.dependencies.collect { case dep: FlixDependency => dep }
  }

}
