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

import java.io.{File, IOException, InputStream}
import java.net.{URI, URL}
import java.nio.file.{Files, Path}
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
  def publishRelease(project: Project, version: SemVer, artifacts: Iterable[Path], apiKey: String): Result[Unit, ReleaseError] = {
    for (
      _ <- verifyRelease(project, version, apiKey);
      id <- createDraftRelease(project, version, apiKey);
      _ <- Result.traverse(artifacts)(p => uploadAsset(p, project, id, apiKey));
      _ <- markReleaseReady(project, version, id, apiKey)
    ) yield Ok(())
  }

  /**
    * Verifies that the release does not already exist.
    */
  private def verifyRelease(project: Project, version: SemVer, apiKey: String): Result[Unit, ReleaseError] = {
    val url = releaseVersionUrl(project, version)

    try {
      val conn = url.openConnection().asInstanceOf[HttpsURLConnection]
      conn.addRequestProperty("Authorization", "Bearer " + apiKey)

      val code = conn.getResponseCode
      code match {
        case 200 => Err(ReleaseError.ReleaseAlreadyExists(project, version))
        case _ => Ok(())
      }

    } catch {
      case _: IOException => Err(ReleaseError.NetworkError)
    }
  }

  /**
    * Create a new release marked as a draft, meaning that it is not publicly visible.
    * The release will not contain any assets (apart from the default zips of the source code).
    *
    * Returns the ID of the release if successful.
    */
  private def createDraftRelease(project: Project, version: SemVer, apiKey: String): Result[String, ReleaseError] = {
    val content: JValue =
      ("tag_name" -> s"v$version") ~
        ("name" -> s"v$version") ~
        ("generate_release_notes" -> true) ~
        ("draft" -> true)

    val jsonCompact = compact(render(content))

    val url = releasesUrl(project)
    val json = try {
      val conn = url.openConnection().asInstanceOf[HttpsURLConnection]
      conn.setRequestMethod("POST")
      conn.addRequestProperty("Authorization", "Bearer " + apiKey)
      conn.setDoOutput(true)

      // Send request
      val outStream = conn.getOutputStream
      outStream.write(jsonCompact.getBytes("utf-8"))

      // Process response errors
      val code = conn.getResponseCode
      code match {
        case 201 =>
          val inStream = conn.getInputStream
          StreamOps.readAll(inStream)
        case 401 => return Err(ReleaseError.InvalidApiKeyError)
        case 404 => return Err(ReleaseError.RepositoryNotFound(project))
        case _ => return Err(ReleaseError.UnexpectedResponseCode(code, conn.getResponseMessage))
      }

    } catch {
      case _: IOException => return Err(ReleaseError.NetworkError)
    }

    // Extract URL from returned JSON
    val id = try {
      val obj = parse(json).asInstanceOf[JObject]
      val jsonId = (obj \ "id").asInstanceOf[JInt]
      jsonId.values.toString
    } catch {
      case _: ClassCastException => return Err(ReleaseError.UnexpectedResponseJson(json))
    }

    Ok(id)
  }

  /**
    * Uploads a single asset.
    */
  private def uploadAsset(assetPath: Path, project: Project, releaseId: String, apiKey: String): Result[Unit, ReleaseError] = {
    val assetName = assetPath.getFileName.toString
    val assetData = Files.readAllBytes(assetPath)

    val url = releaseAssetUploadUrl(project, releaseId, assetName)

    try {
      val conn = url.openConnection().asInstanceOf[HttpsURLConnection]

      conn.setRequestMethod("POST")
      conn.addRequestProperty("Authorization", "Bearer " + apiKey)
      conn.addRequestProperty("Content-Type", "application/octet-stream")
      conn.setDoOutput(true)

      // Send request
      val outStream = conn.getOutputStream
      outStream.write(assetData)

      // Process response errors
      val code = conn.getResponseCode
      code match {
        case 201 => Ok(())
        case 401 => Err(ReleaseError.InvalidApiKeyError)
        case _ => Err(ReleaseError.UnexpectedResponseCode(code, conn.getResponseMessage))
      }

    } catch {
      case _: IOException => Err(ReleaseError.NetworkError)
    }
  }

  /**
    * Mark the given release as no longer being a draft, making it publicly available.
    */
  private def markReleaseReady(project: Project, version: SemVer, releaseId: String, apiKey: String): Result[Unit, ReleaseError] = {
    val content: JValue = "draft" -> false
    val jsonCompact = compact(render(content))

    val url = releaseIdUrl(project, releaseId)

    try {
      val conn = url.openConnection().asInstanceOf[HttpsURLConnection]

      // Java doesn't recognize "PATCH" as a valid request method (???)
      conn.setRequestMethod("POST")
      conn.setRequestProperty("X-HTTP-Method-Override", "PATCH")

      conn.addRequestProperty("Authorization", "Bearer " + apiKey)
      conn.setDoOutput(true)

      // Send request
      val outStream = conn.getOutputStream
      outStream.write(jsonCompact.getBytes("utf-8"))

      // Process response errors
      val code = conn.getResponseCode
      code match {
        case 200 => Ok(())
        case 401 => Err(ReleaseError.InvalidApiKeyError)
        case 404 => Err(ReleaseError.RepositoryNotFound(project))
        case 422 => Err(ReleaseError.ReleaseAlreadyExists(project, version))
        case _ => Err(ReleaseError.UnexpectedResponseCode(code, conn.getResponseMessage))
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
    * Returns the URL for updating information about this specific release.
    */
  private def releaseIdUrl(project: Project, releaseId: String): URL = {
    new URL(s"${releasesUrl(project).toString}/$releaseId")
  }

  /**
    * Returns the URL for viewing basic information about this specific release.
    */
  private def releaseVersionUrl(project: Project, version: SemVer): URL = {
    new URL(s"${releasesUrl(project).toString}/tags/v$version")
  }

  /**
    * Returns the URL that release assets can be uploaded to.
    */
  private def releaseAssetUploadUrl(project: Project, releaseId: String, assetName: String): URL = {
    new URI(s"https://uploads.github.com/repos/${project.owner}/${project.repo}/releases/$releaseId/assets?name=$assetName").toURL
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
