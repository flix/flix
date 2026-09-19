/*
 * Copyright 2023 Anna Blume Jakobsen
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

import ca.uwaterloo.flix.language.ast.shared.{PackageId, SecurityContext}
import ca.uwaterloo.flix.tools.pkg.Dependency.FlixDependency
import ca.uwaterloo.flix.tools.pkg.github.GitHub.Project
import ca.uwaterloo.flix.util.{Formatter, Sha256}

import java.io.IOException
import java.net.URL
import java.nio.file.Path

sealed trait PackageError {
  /**
    * Returns a human-readable and formatted string representation of this error.
    */
  def message(f: Formatter): String
}

object PackageError {
  case class VersionDoesNotExist(version: SemVer, project: Project) extends PackageError {
    override def message(f: Formatter): String =
      s"Version ${f.bold(version.toString)} does not exist for project ${f.bold(project.toString)}"
  }

  case class InvalidProjectName(projectString: String) extends PackageError {
    override def message(f: Formatter): String =
      s"""A GitHub project should be formatted like so: 'owner/repository'.
         |Instead found: ${f.red(projectString)}.
         |""".stripMargin
  }

  /**
    * A request about a project never reached a server at all.
    */
  case class ProjectUnreachable(url: URL, project: Project, exception: IOException) extends PackageError {
    override def message(f: Formatter): String =
      s"""An I/O error occurred while trying to read the following url:
         |${f.cyan(url.toString)}
         |Project: ${f.bold(project.toString)}
         |Error: ${f.red(exception.getMessage)}
         |""".stripMargin
  }

  /**
    * A project that GitHub answers 404 for.
    *
    * A private project answers the same way to whoever cannot see it, so the message names both
    * possibilities: GitHub does not say which of the two it is.
    */
  case class ProjectDoesNotExist(project: Project, url: URL) extends PackageError {
    override def message(f: Formatter): String =
      s"""There is no project ${f.red(project.toString)} to read releases from.
         |Either it does not exist, or it is private and the API token in use cannot see it.
         |Looked at ${f.cyan(url.toString)}.
         |""".stripMargin
  }

  case class JsonError(json: String, project: Project) extends PackageError {
    override def message(f: Formatter): String =
      s"""An error occurred while trying to parse the following as JSON:
         |${f.cyan(json)}
         |Project: ${f.bold(project.toString)}
         |""".stripMargin
  }

  /**
    * A release asset isn't where its address says: either the release doesn't exist, or it exists
    * without that asset -- a 404 can't say which, so the message names both possibilities.
    */
  case class ReleaseAssetNotFound(project: Project, version: SemVer, assetName: String, url: URL)
    extends PackageError {
    override def message(f: Formatter): String =
      s"""Could not find ${f.bold(assetName)} in release ${f.bold(s"v$version")}
         |of ${f.bold(project.toString)}.
         |Either the release does not exist, or it does not publish that file.
         |Looked at ${f.cyan(url.toString)}.
         |""".stripMargin
  }

  /**
    * A download refused (403/429), which for an anonymous request usually means a rate limit.
    */
  case class DownloadRefused(url: URL, status: Int, retryAfter: Option[String])
    extends PackageError {
    override def message(f: Formatter): String = {
      // Retry-After is delta-seconds per RFC 9110, but may also be an HTTP-date.
      val when = retryAfter match {
        case Some(s) if s.forall(_.isDigit) => s"Retry after $s seconds."
        case Some(s) => s"Retry after $s."
        case None => "This is usually a rate limit."
      }
      s"""Refused (HTTP ${f.red(status.toString)}) by ${f.cyan(url.toString)}.
         |$when
         |""".stripMargin
    }
  }

  /**
    * A download answered with a status that is neither success nor a refusal.
    */
  case class DownloadFailed(url: URL, status: Int) extends PackageError {
    override def message(f: Formatter): String = {
      val detail =
        if (status >= 300 && status < 400)
          "The address redirected somewhere that could not be followed."
        else "Unexpected response."
      s"""Could not download ${f.cyan(url.toString)}: HTTP ${f.red(status.toString)}.
         |$detail
         |""".stripMargin
    }
  }

  /**
    * A download never reached a server at all.
    */
  case class DownloadUnreachable(url: URL, message: String) extends PackageError {
    override def message(f: Formatter): String =
      s"""Could not reach ${f.cyan(url.toString)}.
         |$message
         |""".stripMargin
  }

  case class DownloadError(name: String, message: Option[String]) extends PackageError {
    override def message(f: Formatter): String =
      s"""A download error occurred while downloading ${f.bold(name)}
         |${
        message match {
          case Some(e) => e
          case None => ""
        }
      }
         |""".stripMargin
  }

  case class DownloadErrorJar(url: String, fileName: String, message: Option[String]) extends PackageError {
    override def message(f: Formatter): String =
      s"""A download error occurred while downloading ${f.bold(fileName)} from $url
         |${
        message match {
          case Some(e) => e
          case None => ""
        }
      }
         |""".stripMargin
  }

  /**
    * A file that was already in `lib/` is not the one `packages.lock` records for it.
    *
    * Nothing downloaded it during this build, so the file has changed on disk since the build
    * that did. Deleting it is enough to recover: the next build downloads it again.
    *
    * @param identifier the identifier of the package, e.g. `github:flix/museum`.
    * @param version    the version of the package.
    * @param extension  the file of the package that does not match, i.e. `toml` or `fpkg`.
    * @param path       the path of the file in `lib/`.
    * @param expected   the digest that `packages.lock` records.
    * @param actual     the digest of the file that is there.
    */
  case class MismatchedCachedDigest(identifier: PackageId, version: SemVer, extension: String, path: Path, expected: Sha256, actual: Sha256) extends PackageError {
    override def message(f: Formatter): String =
      s"""The ${f.bold(extension)} of ${f.bold(identifier.toString)} ${f.bold(version.toString)} is not the one ${f.bold("packages.lock")} records.
         |   expected: ${f.cyan(expected.toString)}
         |  but found: ${f.red(actual.toString)}
         |
         |The file at ${f.cyan(path.toString)} has changed since it was downloaded.
         |Delete it and build again to download it afresh, or update ${f.bold("packages.lock")} if the
         |change was intended.
         |""".stripMargin
  }

  /**
    * A file that was just downloaded is not the one `packages.lock` records for it.
    *
    * The published release itself has changed, which GitHub permits: a release asset can be
    * deleted and uploaded again at the same version. Deleting the file does not help, because
    * downloading it again produces the same bytes.
    *
    * @param identifier the identifier of the package, e.g. `github:flix/museum`.
    * @param version    the version of the package.
    * @param extension  the file of the package that does not match, i.e. `toml` or `fpkg`.
    * @param path       the path the file was downloaded to.
    * @param expected   the digest that `packages.lock` records.
    * @param actual     the digest of the file that was downloaded.
    */
  case class MismatchedDownloadedDigest(identifier: PackageId, version: SemVer, extension: String, path: Path, expected: Sha256, actual: Sha256) extends PackageError {
    override def message(f: Formatter): String =
      s"""The ${f.bold(extension)} of ${f.bold(identifier.toString)} ${f.bold(version.toString)} is not the one ${f.bold("packages.lock")} records.
         |        expected: ${f.cyan(expected.toString)}
         |  but downloaded: ${f.red(actual.toString)}
         |
         |The published release has changed since ${f.bold("packages.lock")} was written. A release asset
         |can be replaced at the same version, so this may be a supply chain attack.
         |The file was written to ${f.cyan(path.toString)}.
         |Update ${f.bold("packages.lock")} only if you know the change was intended.
         |""".stripMargin
  }

  /**
    * A file in `lib/` could not be read to compute its digest. The file was there a moment ago,
    * so this means the filesystem is in an unexpected state rather than that a download failed.
    */
  case class DigestError(path: Path, message: String) extends PackageError {
    override def message(f: Formatter): String =
      s"""An I/O error occurred while reading ${f.cyan(path.toString)}.
         |Error: ${f.red(message)}
         |""".stripMargin
  }

  case class CoursierError(errorMsg: String) extends PackageError {
    override def message(f: Formatter): String =
      s"""An error occurred with Coursier:
         |$errorMsg
         |""".stripMargin
  }

  case class NoSuchFile(project: String, extension: String) extends PackageError {
    override def message(f: Formatter): String =
      s"""There are no files in project '${f.bold(project)}' with extension '${f.bold(s".$extension")}'.
         |""".stripMargin
  }

  case class TooManyFiles(project: String, extension: String) extends PackageError {
    override def message(f: Formatter): String =
      s"""There are too many files in project '${f.bold(project)}' with extension '${f.bold(s".$extension")}'.
         |There should only be one $extension file in each project.
         |""".stripMargin
  }

  case class ManifestParseError(e: ManifestError) extends PackageError {
    override def message(f: Formatter): String = e.message(f)
  }

  /**
    * An error raised when the dependency graph itself is inconsistent w.r.t. security contexts.
    *
    * @param manifest   the manifest that declares [[dependency]].
    * @param dependency the dependency that requires a higher security level than what is allowed by the declaring manifest.
    * @param sctx       the maximum allowed security context.
    */
  case class DepGraphSecurityError(manifest: Manifest, dependency: FlixDependency, sctx: SecurityContext) extends PackageError {
    // TODO: Show the offending original dependency/-ies (from origin manifest)
    // TODO: Maybe collect list of errors that can all be displayed in a single error message.
    override def message(f: Formatter): String = {
      s"""${f.underline("Found security violation in the dependency graph:")}
         |  Dependency '$dependency' of package ${manifest.displayName} requires security context '${dependency.sctx}' but context '$sctx' was given.
         |
         |  There are several possible actions:
         |    - Remove the offending dependency
         |    - Use a different dependency.
         |    - Increase security level. ${f.yellow("WARNING")}: This can be dangerous and may expose you to supply chain attacks.
         |""".stripMargin
    }
  }

  /**
    * An error raised when the dependency graph itself is inconsistent w.r.t. security contexts.
    *
    * @param manifest the dependency that requires a stronger security context than what is allowed by the declaring manifest.
    * @param sctx     the maximum allowed security level.
    */
  case class IllegalJavaDependencyForSctx(manifest: Manifest, dependency: Dependency, sctx: SecurityContext) extends PackageError {
    // TODO: Show the offending original dependency/-ies (from origin manifest)
    // TODO: Maybe collect list of errors that can all be displayed in a single error message.
    override def message(f: Formatter): String = {
      s"""${f.underline("Found security violation in the dependency graph:")}
         |  Project '${manifest.displayName}' declares Java dependency '$dependency' which requires security context '${SecurityContext.Unrestricted}' but only $sctx was given.
         |
         |  There are several possible actions:
         |    - Remove the offending dependency
         |    - Use a different dependency.
         |    - Increase security level. ${f.yellow("WARNING")}: This can be dangerous and may expose you to supply chain attacks.
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that some dependents of the package `identifier` mount it and
    * others do not.
    *
    * @param identifier the package the dependents disagree about.
    * @param mounted    the dependents that mount it.
    * @param unmounted  the dependents that do not.
    */
  case class InconsistentMounts(identifier: PackageId, mounted: List[String], unmounted: List[String]) extends PackageError {
    override def message(f: Formatter): String = {
      s"""${f.underline("Found a package that is mounted by some of its dependents and not by others:")}
         |  The package '${f.red(identifier.toString)}' is mounted by: ${mounted.mkString(", ")}
         |  but not by: ${unmounted.mkString(", ")}
         |
         |  A mounted package is reachable only under its mount, so the dependents that do not
         |  mount it cannot reach it at all. Until a mount is required, every dependent of a
         |  package must either mount it or leave it unmounted.
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that the package `identifier` is required at versions that do
    * not share a major version, so that no version satisfies every dependent.
    *
    * @param identifier   the package that is required at incompatible versions.
    * @param requirements every dependent that requires the package, paired with the
    *                     dependency declaration that states the required version.
    */
  case class IncompatibleVersions(identifier: PackageId, requirements: List[(Manifest, FlixDependency)]) extends PackageError {
    override def message(f: Formatter): String = {
      val lines = requirements.map {
        case (dependent, dep) => s"    ${f.bold(dep.version.toString)} required by '${dependent.displayName}'"
      }
      s"""${f.underline("Found incompatible versions of the same package in the dependency graph:")}
         |  The package '${f.red(identifier.toString)}' is required at versions that do not share a major version:
         |
         |${lines.mkString(System.lineSeparator())}
         |
         |  A package is built at one version, which must satisfy every dependent: it must
         |  be at or above the version the dependent requires, and have the same major version.
         |  No version satisfies these, so one of the dependents must move across a major version.
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that a release of the package `identifier` contains a manifest
    * that declares another version than the one it is released as.
    *
    * @param identifier the package.
    * @param release    the version the release is published as.
    * @param declared   the version the manifest in the release declares.
    */
  case class MismatchedVersions(identifier: PackageId, release: SemVer, declared: SemVer) extends PackageError {
    override def message(f: Formatter): String = {
      s"""Mismatched versions:
         |  The release ${f.bold(s"v$release")} of the package '${f.red(identifier.toString)}'
         |  contains a manifest that declares version ${f.bold(declared.toString)}.
         |
         |  Released as: $release
         |  Declared:    $declared
         |
         |  This is a mistake in how the package was released, which its author must fix.
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that the package `identifier` requires a newer version of Flix
    * than the one that is running.
    *
    * @param identifier the package.
    * @param version    the version the package is built at.
    * @param required   the oldest version of Flix the package can be built with.
    * @param current    the version of Flix that is running.
    */
  case class FlixVersionTooOld(identifier: PackageId, version: SemVer, required: SemVer, current: SemVer) extends PackageError {
    override def message(f: Formatter): String =
      s"""The package '${f.red(identifier.toString)}' ${f.bold(version.toString)} requires Flix version ${f.bold(required.toString)}, but the current version is ${f.red(current.toString)}.
         |Please upgrade to Flix ${f.bold(required.toString)} or newer.
         |""".stripMargin
  }
}
