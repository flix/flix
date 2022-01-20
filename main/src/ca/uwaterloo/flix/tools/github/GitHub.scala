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
package ca.uwaterloo.flix.tools.github

import ca.uwaterloo.flix.util.StreamOps
import org.json4s.JsonAST.{JArray, JValue}
import org.json4s.native.JsonMethods.parse

import java.io.InputStream
import java.net.URL

/**
  * An interface for the GitHub API.
  */
object GitHub {

  /**
    * A semantic version number.
    */
  // TODO support prereleases
  case class SemVer(major: Int, minor: Int, patch: Int)

  /**
    * An ordering on Semantic Versions
    */
  implicit def semVerOrdering: Ordering[SemVer] =
    Ordering.by((_: SemVer).major)
      .orElseBy(_.minor)
      .orElseBy(_.patch)

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
  def getReleases(project: Project): List[Release] = {
    val url = releasesUrl(project)
    val stream = url.openStream()
    val json = StreamOps.readAll(stream)
    val releaseJsons = parse(json).asInstanceOf[JArray]
    releaseJsons.arr.map(parseRelease)
  }

  /**
    * Parses a GitHub project from an `<owner>/<repo>` string.
    */
  def parseProject(string: String): Project = string.split('/') match {
    case Array(owner, repo) => Project(owner, repo)
    case _ => throw new RuntimeException(s"Invalid project name: ${string}")
  }

  /**
    * Gets the project release with the highest semantic version.
    */
  def getLatestRelease(project: Project): Release = {
    getReleases(project)
      .maxByOption(_.version)
      .getOrElse(throw new RuntimeException(s"No releases available for project ${project}"))
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
      case semVer(major, minor, patch) => SemVer(major.toInt, minor.toInt, patch.toInt)
      case _ => throw new RuntimeException(s"Invalid semantic version: $string")
    }
  }
}
