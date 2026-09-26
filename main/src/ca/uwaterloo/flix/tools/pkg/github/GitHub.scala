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
import ca.uwaterloo.flix.util.Result
import org.json4s.*
import org.json4s.JsonAST.{JArray, JValue}
import org.json4s.JsonDSL.*
import org.json4s.native.JsonMethods.{compact, parse, render}

import java.io.{IOException, InputStream}
import java.net.http.HttpRequest.BodyPublishers
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.net.{URI, URL, URLEncoder}
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.util.Locale

/**
  * An interface for the GitHub API.
  */
object GitHub {

  /**
    * The hosts a token may be sent to.
    *
    * These are matched in full rather than by suffix: a host that merely ends in one of them, as
    * `github.com.example.com` does, is a different host and is not one of them.
    */
  private val TokenHosts: Set[String] = Set("api.github.com", "github.com", "uploads.github.com")

  /**
    * How many releases to ask for at once.
    *
    * The API answers 30 unless asked otherwise, and 100 is the most it will give.
    */
  private val ReleasesPerPage: Int = 100

  /**
    * The media type the REST API is asked to answer in.
    */
  private val ApiMediaType: String = "application/vnd.github+json"

  /**
    * The version of the REST API to speak.
    *
    * GitHub dates its API and asks every request to name the one it was written against. A
    * request that names none is served by whichever version is current, so a breaking change to
    * the API arrives as a build that stopped working; naming one means it arrives as a version
    * to move off when we choose to.
    */
  private val ApiVersion: String = "2022-11-28"

  /**
    * A GitHub project.
    */
  case class Project(owner: String, repo: String) {
    override def toString: String = s"$owner/$repo"
  }

  /**
    * Lists the versions the project has released.
    *
    * A release asset is addressed by name (see [[downloadReleaseAsset]]), so the listing is read
    * only to learn which versions exist, and never to find a file within one.
    *
    * The status is read before the body is: a project that does not exist, and a request that is
    * refused, both answer with a body that is not a listing, and reporting either as a body that
    * could not be parsed says nothing about what went wrong. Kept apart, as in [[download]]: no
    * such project (404), a refusal (403/429, usually a rate limit), any other unexpected status,
    * and never reaching a server at all.
    */
  def getReleaseVersions(project: Project, token: Option[String]): Result[List[SemVer], PackageError] = {
    val url = releaseListingUrl(project)
    val req = newApiRequest(url, token).GET().build()
    val response = try {
      Client.sendRequest(req)
    } catch {
      case ex: IOException => return Err(PackageError.ProjectUnreachable(url, project, ex))
    }

    val status = response.statusCode()
    if (status < 200 || status >= 300) {
      return status match {
        case 401 if isAuthorized(url, token) => Err(PackageError.TokenRejected(url))
        case 403 => Err(PackageError.DownloadRefused(url, status, retryAfter(response), isAuthorized(url, token)))
        case 404 => Err(PackageError.ProjectDoesNotExist(project, url))
        case 429 => Err(PackageError.DownloadRefused(url, status, retryAfter(response), isAuthorized(url, token)))
        case _ => Err(PackageError.DownloadFailed(url, status))
      }
    }

    val json = response.body()
    val releaseJsons = try {
      parse(json).asInstanceOf[JArray]
    } catch {

      case _: ClassCastException => return Err(PackageError.JsonError(json, project))
    }
    Ok(releaseJsons.arr.flatMap(parseRelease))
  }

  /**
    * Publish a new release the given project.
    */
  def publishRelease(project: Project, version: SemVer, artifacts: Iterable[Path], token: String): Result[Unit, ReleaseError] = {
    for (
      _ <- verifyRelease(project, version, token);
      id <- createDraftRelease(project, version, token);
      _ <- Result.traverse(artifacts)(p => uploadAsset(p, project, id, token));
      _ <- markReleaseReady(project, version, id, token)
    ) yield Ok(())
  }

  /**
    * Verifies that the release does not already exist.
    */
  private def verifyRelease(project: Project, version: SemVer, token: String): Result[Unit, ReleaseError] = {
    val url = releaseVersionUrl(project, version)
    val req = newApiRequest(url, Some(token)).GET().build()

    try {
      // Send request
      val resp = Client.sendRequest(req)

      // Process response errors
      val code = resp.statusCode()
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
  private def createDraftRelease(project: Project, version: SemVer, token: String): Result[String, ReleaseError] = {
    val content: JValue =
      ("tag_name" -> s"v$version") ~
        ("name" -> s"v$version") ~
        ("generate_release_notes" -> true) ~
        ("draft" -> true)

    val jsonCompact = compact(render(content))

    val url = releasesUrl(project)
    val req = newApiRequest(url, Some(token))
      .header("Content-Type", "application/json")
      .POST(BodyPublishers.ofByteArray(jsonCompact.getBytes("utf-8")))
      .build()

    val json = try {
      // Send request
      val resp = Client.sendRequest(req)

      // Process response errors
      val code = resp.statusCode()
      code match {
        case 201 => resp.body()
        case 401 => return Err(ReleaseError.InvalidToken)
        case 404 => return Err(ReleaseError.RepositoryNotFound(project))
        case _ => return Err(ReleaseError.UnexpectedResponseCode(code, resp.body()))
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
  private def uploadAsset(assetPath: Path, project: Project, releaseId: String, token: String): Result[Unit, ReleaseError] = {
    val assetName = assetPath.getFileName.toString

    val url = releaseAssetUploadUrl(project, releaseId, assetName)
    val req = newApiRequest(url, Some(token))
      .header("Content-Type", "application/octet-stream")
      .POST(BodyPublishers.ofFile(assetPath))
      .build()

    try {
      // Send request
      val resp = Client.sendRequest(req)

      // Process response errors
      val code = resp.statusCode()
      code match {
        case 201 => Ok(())
        case 401 => Err(ReleaseError.InvalidToken)
        case _ => Err(ReleaseError.UnexpectedResponseCode(code, resp.body()))
      }

    } catch {
      case _: IOException => Err(ReleaseError.NetworkError)
    }
  }

  /**
    * Mark the given release as no longer being a draft, making it publicly available.
    */
  private def markReleaseReady(project: Project, version: SemVer, releaseId: String, token: String): Result[Unit, ReleaseError] = {
    val content: JValue = "draft" -> false
    val jsonCompact = compact(render(content))

    val url = releaseIdUrl(project, releaseId)
    val req = newApiRequest(url, Some(token))
      .header("Content-Type", "application/json")
      .method("PATCH", BodyPublishers.ofByteArray(jsonCompact.getBytes("utf-8")))
      .build()

    try {
      // Send request
      val resp = Client.sendRequest(req)

      // Process response errors
      val code = resp.statusCode()
      code match {
        case 200 => Ok(())
        case 401 => Err(ReleaseError.InvalidToken)
        case 404 => Err(ReleaseError.RepositoryNotFound(project))
        case 422 => Err(ReleaseError.ReleaseAlreadyExists(project, version))
        case _ => Err(ReleaseError.UnexpectedResponseCode(code, resp.body()))
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
    * Opens a stream over `url`, following redirects, carrying `token` if `url` is an address it
    * may be sent to. The caller closes the stream.
    *
    * A release asset redirects to the storage it is served from, which is not GitHub and
    * authorizes requests its own way. The JDK drops the `Authorization` header across a redirect,
    * so the token reaches GitHub and nothing past it; following redirects by hand would have to
    * do the same.
    *
    * Kept apart: a refusal (403/429, usually a rate limit), any other unexpected status, and never
    * reaching a server at all.
    */
  def download(url: URL, token: Option[String]): Result[InputStream, PackageError] = {
    val request = newRequest(url, token).GET().build()

    val response = try {
      Client.sendStreamingRequest(request)
    } catch {
      case ex: IOException => return Err(PackageError.DownloadUnreachable(url, ex.getMessage))
    }

    response.statusCode() match {
      case status if status >= 200 && status < 300 =>
        Ok(response.body())
      case status =>
        // A close failure must not shadow the status being reported.
        try response.body().close() catch { case _: IOException => () }
        status match {
          case 401 if isAuthorized(url, token) => Err(PackageError.TokenRejected(url))
          case 403 => Err(PackageError.DownloadRefused(url, status, retryAfter(response), isAuthorized(url, token)))
          case 429 => Err(PackageError.DownloadRefused(url, status, retryAfter(response), isAuthorized(url, token)))
          case _ => Err(PackageError.DownloadFailed(url, status))
        }
    }
  }

  /**
    * Returns `response`'s `Retry-After` header, if it has one.
    */
  private def retryAfter(response: HttpResponse[?]): Option[String] = {
    val header = response.headers().firstValue("Retry-After")
    if (header.isPresent) Some(header.get()) else None
  }

  /**
    * Opens a stream over the `assetName` asset of `project`'s `version` release, without consulting
    * the REST API -- a release asset's address is fully predictable from owner/repo/tag/name.
    * The caller closes the stream.
    */
  def downloadReleaseAsset(project: Project, version: SemVer, assetName: String, token: Option[String]): Result[InputStream, PackageError] = {
    val url = releaseAssetUrl(project, version, assetName)
    download(url, token) match {
      case Err(PackageError.DownloadFailed(_, 404)) =>
        Err(PackageError.ReleaseAssetNotFound(project, version, assetName, url))
      case other => other
    }
  }

  /**
    * The permanent, non-REST address of a release asset.
    */
  private def releaseAssetUrl(project: Project, version: SemVer, assetName: String): URL = {
    // The 4-arg constructor percent-encodes the path, so a name with a space or "#" (legal in a
    // manifest's declared name, which this can be built from) can't produce a malformed URL.
    val path = s"/${project.owner}/${project.repo}/releases/download/v$version/$assetName"
    new URI("https", "github.com", path, null).toURL
  }

  /**
    * Returns the URL that returns data related to the project's releases.
    */
  private def releasesUrl(project: Project): URL = {
    new URI(s"https://api.github.com/repos/${project.owner}/${project.repo}/releases").toURL
  }

  /**
    * Returns the URL of the project's releases, asking for as many at once as the API will give.
    *
    * A project with more than that is read short -- `upgrade` and `outdated` can be wrong about
    * it -- and the answer names the rest in a `Link` header.
    *
    * Kept apart from [[releasesUrl]], which a page size cannot go on: a release is created
    * there, and a single release's address is built by appending to it.
    */
  private def releaseListingUrl(project: Project): URL = {
    new URI(s"${releasesUrl(project).toString}?per_page=$ReleasesPerPage").toURL
  }

  /**
    * Returns the URL for updating information about this specific release.
    */
  private def releaseIdUrl(project: Project, releaseId: String): URL = {
    new URI(s"${releasesUrl(project).toString}/$releaseId").toURL
  }

  /**
    * Returns the URL for viewing basic information about this specific release.
    */
  private def releaseVersionUrl(project: Project, version: SemVer): URL = {
    new URI(s"${releasesUrl(project).toString}/tags/v$version").toURL
  }

  /**
    * Returns the URL that release assets can be uploaded to.
    */
  private def releaseAssetUploadUrl(project: Project, releaseId: String, assetName: String): URL = {
    // "&" and "=" are legal query characters, so the URI constructor won't escape them -- an
    // assetName containing one could inject an extra query parameter. URLEncoder escapes them.
    val path = s"/repos/${project.owner}/${project.repo}/releases/$releaseId/assets"
    val base = new URI("https", "uploads.github.com", path, null, null)
    val encodedName = URLEncoder.encode(assetName, StandardCharsets.UTF_8)
    new URI(s"$base?name=$encodedName").toURL
  }

  /**
    * Parses the version a release JSON is tagged with, if it is a release of the package.
    *
    * A repository's releases are its own to tag, and only the ones tagged as a version are
    * versions of the package: a repository may release something that is not a Flix package at
    * all, or may have released one before it was one. Such a release is passed over rather than
    * read as a version, and rather than -- as it once was -- thrown out of the listing as an
    * exception, which took down every build that read a repository holding one.
    */
  private def parseRelease(json: JValue): Option[SemVer] = json \ "tag_name" match {
    case JString(tag) => parseSemVer(tag)
    case _ => None
  }

  /**
    * Parses a semantic version that starts with `v`, e.g. `v2.3.4`, if `str` is one.
    */
  private def parseSemVer(str: String): Option[SemVer] = {
    val (v, num) = str.splitAt(1)
    if (v == "v") SemVer.ofString(num) else None
  }

  /**
    * Returns `true` if `url` is an address a token may be sent to.
    *
    * A token authorizes a request to GitHub, and is offered to nothing else. Not every address
    * that is requested is GitHub's own: a jar is downloaded from wherever the manifest that
    * declares it says, and a release asset is served from the storage it lives on rather than
    * from GitHub itself. The scheme is part of the question, since a token that is sent in the
    * clear is a token that has been given away.
    */
  def mayReceiveToken(url: URL): Boolean = {
    val host = url.getHost
    url.getProtocol == "https" && host != null && TokenHosts.contains(host.toLowerCase(Locale.ROOT))
  }

  /**
    * Returns `true` if a request to `url` carries `token`.
    *
    * What to say about a refusal turns on it: a client that carries no token can be told to set
    * one, and a token that was never sent cannot be what a request was refused over.
    */
  private def isAuthorized(url: URL, token: Option[String]): Boolean =
    token.nonEmpty && mayReceiveToken(url)

  /**
    * Returns a builder for a REST API request to `url`, as [[newRequest]] does, naming the media
    * type and the API version the answer is expected to follow.
    *
    * Only the API is asked these: a release asset and a jar are files served from an address,
    * and what they are is not GitHub's to say.
    */
  def newApiRequest(url: URL, token: Option[String]): HttpRequest.Builder =
    newRequest(url, token)
      .header("Accept", ApiMediaType)
      .header("X-GitHub-Api-Version", ApiVersion)

  /**
    * Returns a builder for a request to `url`, carrying `token` if there is one to carry and
    * `url` is an address it may be sent to.
    *
    * Every request is built here, so that whether it carries the token is decided in one place
    * rather than separately at each call. Deciding it at each call is what left the download of
    * a release asset anonymous while the listing that found it was authorized.
    */
  def newRequest(url: URL, token: Option[String]): HttpRequest.Builder = {
    val builder = HttpRequest.newBuilder(url.toURI)
    if (mayReceiveToken(url)) {
      token.foreach(t => builder.header("Authorization", s"Bearer $t"))
    }
    builder
  }

  /** A thread-safe HTTP Client. */
  private object Client {

    /**
      * Internally re-used Http Client.
      *
      * Reusing the instance yields better performance since it can
      * keep connections open.
      *
      * The client is immutable once built and manages its own connection pool,
      * so it can be shared across threads without external locking.
      */
    private val HTTP_CLIENT: HttpClient =
      // Follows redirects: a release download address redirects to the storage the asset lives on.
      HttpClient.newBuilder().followRedirects(HttpClient.Redirect.NORMAL).build()

    /**
      * Sends the HTTP request, `request`, and returns the response.
      *
      * Is blocking and thread-safe.
      *
      * May throw [[IOException]].
      */
    def sendRequest(request: HttpRequest): HttpResponse[String] =
      HTTP_CLIENT.send(request, HttpResponse.BodyHandlers.ofString())

    /**
      * As [[sendRequest]], but with a streamed body. May throw [[IOException]].
      */
    def sendStreamingRequest(request: HttpRequest): HttpResponse[InputStream] =
      HTTP_CLIENT.send(request, HttpResponse.BodyHandlers.ofInputStream())

  }
}
