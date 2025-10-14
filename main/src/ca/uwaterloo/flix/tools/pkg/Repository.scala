/*
 * Copyright 2024 Magnus Madsen
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

import ca.uwaterloo.flix.tools.pkg.Repository.{Asset, Project, Release}
import ca.uwaterloo.flix.tools.pkg.github.GitHub
import ca.uwaterloo.flix.util.Result
import ca.uwaterloo.flix.util.Result.{Err, Ok}

import java.io.InputStream
import java.net.URL
import java.nio.file.Path

trait Repository {

  /**
    * Returns the name of the repository
    */
  def name: String

  /**
    * Lists the project's releases.
    */
  def getReleases(project: Project, apiKey: Option[String]): Result[List[Release], PackageError]

  /**
    * Publish a new release the given project.
    */
  def publishRelease(project: Project, version: SemVer, artifacts: Iterable[Path], apiKey: String): Result[Unit, ReleaseError]

  /**
    * Parses a project from a string.
    */
  def parseProject(string: String): Result[Project, PackageError]

  /**
    * Gets the project release with the relevant semantic version.
    */
  def getSpecificRelease(project: Project, version: SemVer, apiKey: Option[String]): Result[Release, PackageError]

  /**
    * Downloads the given asset.
    */
  def downloadAsset(asset: Asset): InputStream

}

object Repository {

  /**
    * A project.
    */
  case class Project(owner: String, repo: String) {
    override def toString: String = s"$owner/$repo"
  }

  /**
    * A release of a project.
    */
  case class Release(version: SemVer, assets: List[Asset])

  /**
    * An asset from a project release.
    *
    * `url` is the link to download the asset.
    */
  case class Asset(name: String, url: URL)

  /**
    * Convert a [[String]] into a [[Repository]].
    */
  def mkRepository(s: String): Result[Repository, RepositoryError] = s match {
    case "github" => Ok(GitHub)
    case _ => Err(RepositoryError.UnsupportedRepositoryError(s))
  }

}
