package ca.uwaterloo.flix.tools.github

import ca.uwaterloo.flix.util.StreamOps
import org.json4s.JsonAST.{JArray, JValue}
import org.json4s.native.JsonMethods.parse

import java.io.InputStream
import java.net.URL

object GitHub {

  case class SemVer(major: Int, minor: Int, patch: Int)
  implicit def ordering: Ordering[SemVer] =
    Ordering.by((_: SemVer).major)
      .orElseBy(_.minor)
      .orElseBy(_.patch)

  case class Project(owner: String, repo: String)

  case class Release(version: SemVer, assets: List[Asset])

  case class Asset(name: String, url: URL)

  private def releasesUrl(project: Project): URL = {
    new URL(s"https://api.github.com/repos/${project.owner}/${project.repo}/releases")
  }

  def getReleases(project: Project): List[Release] = {
    val url = releasesUrl(project)
    val stream = url.openStream()
    val json = StreamOps.readAll(stream)
    val releaseJsons = parse(json).asInstanceOf[JArray]
    releaseJsons.arr.map(parseRelease)
  }

  private def parseRelease(json: JValue): Release = {
    val version = parseSemVer((json \\ "tag_name").toString)
    val assetJsons = (json \\ "assets").asInstanceOf[JArray]
    val assets = assetJsons.arr.map(parseAsset)
    Release(version, assets)
  }

  private def parseAsset(asset: JValue): Asset = {
    val url = asset \\ "url"
    val name = asset \\ "name"
    Asset(name.toString, new URL(url.toString))
  }

  private def parseSemVer(string: String): SemVer = string.split('.') match {
    case Array(major, minor, patch) =>
      SemVer(major.toInt, minor.toInt, patch.toInt)
    case _ => throw new RuntimeException(s"bad semver: $string") // MATT use monadic stuff
  }

  def getLatestRelease(project: Project): Option[Release] = {
    getReleases(project).maxByOption(_.version)
  }

  def downloadAsset(asset: Asset): InputStream =
    asset.url.openStream()

  trait GitHubError


}
