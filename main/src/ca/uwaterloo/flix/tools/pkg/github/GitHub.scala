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

import ca.uwaterloo.flix.tools.pkg.{PackageError, ReleaseError, SemVer}
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.{Result, StreamOps}
import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.JsonAST.{JArray, JValue}
import org.json4s.native.JsonMethods.{compact, parse, render}

import java.io.{IOException, InputStream}
import java.net.{URI, URL}
import javax.net.ssl.HttpsURLConnection

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
  def getReleases(project: Project, apiKey: Option[String]): Result[List[Release], PackageError] = {
    val url = releasesUrl(project)
    val json = try {
      val conn = url.openConnection()
      // add the API key as bearer if needed
      apiKey.foreach(key => conn.addRequestProperty("Authorization", "Bearer " + key))
      val stream = conn.getInputStream
      StreamOps.readAll(stream)
    } catch {
      case _: IOException => return Err(PackageError.ProjectNotFound(url, project))
    }
    val releaseJsons = try {
      parse(json).asInstanceOf[JArray]
    } catch {

      case _: ClassCastException => return Err(PackageError.JsonError(json, project))
    }
    Ok(releaseJsons.arr.map(parseRelease))
  }

  /**
    * Publish a new release the given project.
    */
  def publishRelease(project: Project, version: SemVer, apiKey: String): Result[Unit, ReleaseError] = {
    val content: JValue = ("tag_name" -> s"v$version") ~ ("name" -> s"v$version")
    val jsonCompact = compact(render(content))

    val url = releasesUrl(project)
    try {
      val conn = url.openConnection().asInstanceOf[HttpsURLConnection]
      conn.setRequestMethod("POST")
      conn.addRequestProperty("Authorization", "Bearer " + apiKey)
      conn.setDoOutput(true)

      // Send request
      val outStream = conn.getOutputStream
      outStream.write(jsonCompact.getBytes("utf-8"))

      // Process response
      conn.getResponseCode match {
        case x if 200 <= x && x <= 299 => Ok(())
        case 401 => Err(ReleaseError.InvalidApiKeyError)
        case 404 => Err(ReleaseError.InvalidProject(project))
        case 422 => Err(ReleaseError.AlreadyExists(project, version))
        case code => Err(ReleaseError.UnknownResponse(code, conn.getResponseMessage))
      }
    } catch {
      case _: IOException => Err(ReleaseError.NetworkError)
    }
  }

  /**
    * Parses a GitHub project from an `<owner>/<repo>` string.
    */
  def parseProject(string: String): Result[Project, PackageError] = string.split('/') match {
    case Array(owner, repo) if owner.nonEmpty && repo.nonEmpty => Ok(Project(owner, repo))
    case _ => Err(PackageError.InvalidProjectName(string))
  }

  /**
    * Gets the project release with the highest semantic version.
    */
  def getLatestRelease(project: Project, apiKey: Option[String]): Result[Release, PackageError] = {
    getReleases(project, apiKey).flatMap {
      releases =>
        releases.maxByOption(_.version) match {
          case None => Err(PackageError.NoReleasesFound(project))
          case Some(latest) => Ok(latest)
        }
    }
  }

  /**
    * Gets the project release with the relevant semantic version.
    */
  def getSpecificRelease(project: Project, version: SemVer, apiKey: Option[String]): Result[Release, PackageError] = {
    getReleases(project, apiKey).flatMap {
      releases =>
        releases.find(r => r.version == version) match {
          case None => Err(PackageError.VersionDoesNotExist(version, project))
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
    new URI(s"https://api.github.com/repos/${project.owner}/${project.repo}/releases").toURL
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
    Asset(name.values.toString, new URI(url.values.toString).toURL)
  }

  /**
    * Parses a semantic version, starting with v, e.g.
    *
    * * `v2.3.4`
    */
  private def parseSemVer(string: String): SemVer = {
    val semVer = """v(\d+)\.(\d+)\.(\d+)""".r
    string match {
      case semVer(major, minor, patch) => SemVer(major.toInt, minor.toInt, Some(patch.toInt), None, None)
      case _ => throw new RuntimeException(s"Invalid semantic version: $string")
    }
  }
}
