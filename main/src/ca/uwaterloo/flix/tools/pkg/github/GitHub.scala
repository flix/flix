/*
 * Copyright 2021 Matthew Lutze
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
package ca.uwaterloo.flix.tools.pkg.github

import ca.uwaterloo.flix.tools.pkg.{PackageError, SemVer}
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.{Result, StreamOps}
import org.json4s.JsonAST.{JArray, JValue}
import org.json4s.native.JsonMethods.parse

import java.io.{IOException, InputStream}
import java.net.URL

/**
  * An interface for the GitHub API.
  */
object GitHub {

  /**
    * A GitHub project.
    */
  case class Project(owner: String, repo: String) {
    override def toString: String = s"$owner/$repo"
  }

  /**
    * A release of a GitHub project.
    */
  case class Release(version: SemVer, assets: List[Asset])

  /**
    * An asset from a GitHub project release.
    *
    * `url` is the link to download the asset.
    */
  case class Asset(name: String, url: URL)

  /**
    * Lists the project's releases.
    */
  def getReleases(project: Project): Result[List[Release], PackageError] = {
    val url = releasesUrl(project)
    val releaseJsons = try {
      val stream = url.openStream()
      val json = StreamOps.readAll(stream)
      parse(json).asInstanceOf[JArray]
    } catch {
      case _: IOException => return Err(PackageError.ProjectNotFound(s"Could not open stream to $url"))
      case _: ClassCastException => return Err(PackageError.JsonError(s"Could not parse $url as JSON array"))
    }
    Ok(releaseJsons.arr.map(parseRelease))
  }

  /**
    * Parses a GitHub project from an `<owner>/<repo>` string.
    */
  def parseProject(string: String): Result[Project, PackageError] = string.split('/') match {
    case Array(owner, repo) => Ok(Project(owner, repo))
    case _ => Err(PackageError.InvalidProjectName(string))
  }

  /**
    * Gets the project release with the highest semantic version.
    */
  def getLatestRelease(project: Project): Result[Release, PackageError] = {
    getReleases(project).flatMap {
      releases =>
        releases.maxByOption(_.version) match {
          case None => Err(PackageError.NoReleasesFound(s"No releases available for project ${project}"))
          case Some(latest) => Ok(latest)
        }
    }
  }

  /**
    * Gets the project release with the relevant semantic version.
    */
  def getSpecificRelease(project: Project, version: SemVer): Result[Release, PackageError] = {
    getReleases(project).flatMap {
      releases =>
        releases.find(r => r.version == version) match {
          case None => Err(PackageError.VersionDoesNotExist(s"Version ${version.toString} of project ${project.toString} does not exist"))
          case Some(release) => Ok(release)
        }
    }
  }

  /**
    * Downloads the given asset.
    */
  def downloadAsset(asset: Asset): InputStream =
    asset.url.openStream()

  /**
    * Returns the URL that returns data related to the project's releases.
    */
  private def releasesUrl(project: Project): URL = {
    new URL(s"https://api.github.com/repos/${project.owner}/${project.repo}/releases")
  }

  /**
    * Parses a Release JSON.
    */
  private def parseRelease(json: JValue): Release = {
    val version = parseSemVer((json \ "tag_name").values.toString)
    val assetJsons = (json \ "assets").asInstanceOf[JArray]
    val assets = assetJsons.arr.map(parseAsset)
    Release(version, assets)
  }

  /**
    * Parses an Asset JSON.
    */
  private def parseAsset(asset: JValue): Asset = {
    val url = asset \ "browser_download_url"
    val name = asset \ "name"
    Asset(name.values.toString, new URL(url.values.toString))
  }

  /**
    * Parses a semantic version, starting with v, e.g.
    *
    * * `v2.3.4`
    */
  private def parseSemVer(string: String): SemVer = {
    val semVer = """v(\d+)\.(\d+)\.(\d+)""".r
    string match {
      case semVer(major, minor, patch) => SemVer(major.toInt, minor.toInt, Some(patch.toInt), None)
      case _ => throw new RuntimeException(s"Invalid semantic version: $string")
    }
  }
}
