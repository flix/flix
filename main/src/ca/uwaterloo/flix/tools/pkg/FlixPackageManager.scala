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

import ca.uwaterloo.flix.api.{Bootstrap, InstalledPackage}
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.tools.pkg.Dependency.{FlixDependency, JarDependency, MavenDependency}
import ca.uwaterloo.flix.tools.pkg.github.GitHub
import ca.uwaterloo.flix.util.{Formatter, Result, Sha256}
import ca.uwaterloo.flix.util.Result.{Err, Ok, traverse}
import ca.uwaterloo.flix.util.collection.ListMap

import java.io.{IOException, PrintStream}
import java.nio.file.{Files, Path, StandardCopyOption}
import scala.collection.mutable

object FlixPackageManager {

  /**
    * Represents the dependency resolution of [[origin]].
    * All fields should be considered private except [[origin]] and [[manifests]].
    *
    * @param origin              the manifest that corresponds to the current / local project.
    * @param manifests           all manifests in the resolution.
    * @param immediateDependents all immediate dependents / parents of each manifest.
    * @param manifestToFlixDeps  a mapping from [[Manifest]]s to [[FlixDependency]]s.
    *                            A manifest is the resource a flix dependency resolves to.
    * @param tomlDigests         the digest of the `flix.toml` each manifest was parsed from.
    *                            [[origin]] does not appear: its manifest is the one in the
    *                            project directory, which is not downloaded.
    */
  case class Resolution(origin: Manifest,
                        manifests: List[Manifest],
                        immediateDependents: Map[Manifest, List[Manifest]],
                        manifestToFlixDeps: ListMap[Manifest, FlixDependency],
                        tomlDigests: Map[Manifest, Sha256])

  /**
    * Represents the dependency resolution of [[origin]] where the maximum security level has been computed
    * for each manifest.
    *
    * @param origin             the manifest that corresponds to the current / local project.
    * @param security           the maximum allowed security level of each manifest.
    * @param manifestToFlixDeps a mapping from [[Manifest]]s to [[FlixDependency]]s.
    *                           A manifest is the resource a flix dependency resolves to.
    * @param tomlDigests        the digest of the `flix.toml` each manifest was parsed from.
    */
  case class SecureResolution(origin: Manifest,
                              security: Map[Manifest, SecurityContext],
                              manifestToFlixDeps: ListMap[Manifest, FlixDependency],
                              tomlDigests: Map[Manifest, Sha256]) {
    /**
      * All manifests in the resolution.
      */
    val manifests: List[Manifest] = security.keys.toList
  }

  /**
    * The Flix packages installed for a project.
    *
    * @param packages the installed packages.
    * @param lockfile what each installed package was, to be recorded in `flix.lock`.
    */
  case class Installation(packages: List[InstalledPackage], lockfile: Lockfile)

  /**
    * A file installed in the `lib/` directory.
    *
    * @param path   the path to the file.
    * @param digest the digest of the contents of the file.
    */
  private case class InstalledFile(path: Path, digest: Sha256)

  /**
    * Finds all the transitive dependencies for `manifest` and
    * returns their manifests. The toml files for the manifests
    * will be put at `path/lib`.
    */
  def findTransitiveDependencies(manifest: Manifest, path: Path, apiKey: Option[String], lockfile: Lockfile)(implicit formatter: Formatter, out: PrintStream): Result[Resolution, PackageError] = {
    out.println("Resolving Flix dependencies...")
    implicit val immediateDependents: mutable.Map[Manifest, List[Manifest]] = mutable.Map(manifest -> List.empty)
    implicit val manifestToFlixDeps: mutable.Map[Manifest, List[FlixDependency]] = mutable.Map(manifest -> List.empty)
    implicit val tomlDigests: mutable.Map[Manifest, Sha256] = mutable.Map.empty
    findTransitiveDependenciesRec(manifest, path, List(manifest), apiKey, lockfile).map(manifests => Resolution(manifest, manifests, immediateDependents.toMap, ListMap.from(manifestToFlixDeps.flatMap { case (m, deps) => deps.map(d => (m, d)) }), tomlDigests.toMap))
  }

  /**
    * Resolves the maximal allowed security level for all dependencies in `resolution`.
    */
  def resolveSecurityLevels(resolution: Resolution): SecureResolution = {
    implicit val securityContexts: mutable.Map[Manifest, SecurityContext] = mutable.Map(resolution.origin -> SecurityContext.Unrestricted)
    implicit val res: Resolution = resolution
    val manifests = resolution.manifests.map(m => (m, minSecurityLevel(m))).toMap
    SecureResolution(resolution.origin, manifests, resolution.manifestToFlixDeps, resolution.tomlDigests)
  }

  /**
    * Finds all security errors at the package level if any.
    * A security error exists if at least one of the following holds:
    *   1. A manifest `m0` has allowed security context `t0` and `m0` contains a dependency with security context `t1` where `t1 > t0`. E.g., `unrestricted > plain`.
    *   1. A manifest `m0` has security context `plain` or lower and contains at least one jar or maven dependency.
    */
  def checkSecurity(resolution: SecureResolution): List[PackageError] = {
    resolution.security.flatMap { case (m, t) => findSecurityViolations(m, t) }.toList
  }

  /**
    * Finds every package that is required at more than one version in `manifests`.
    *
    * A package may occur at exactly one version in the dependency graph. Two dependents
    * that require different versions of the same package is an error: both versions would
    * otherwise be installed and compiled together, which duplicates every definition the
    * package declares.
    */
  def checkSingleVersion(manifests: List[Manifest]): List[PackageError] = {
    // Pair every dependency declaration with the manifest that declares it.
    val requirements = manifests.flatMap(m => findFlixDependencies(m).map(dep => (m, dep)))

    // Report every package that is required at more than one version.
    val byPackage = requirements.groupBy { case (_, dep) => dep.id }
    byPackage.toList.sortBy { case (identifier, _) => identifier }.flatMap {
      case (identifier, reqs) =>
        val versions = reqs.map { case (_, dep) => dep.version }.distinct
        if (versions.sizeIs > 1) {
          // Order by version, and then by dependent, so the message is deterministic.
          val sorted = reqs.sortBy { case (dependent, dep) => (dep.version, dependent.name) }
          Some(PackageError.MultipleVersions(identifier, sorted))
        } else {
          None
        }
    }
  }

  /**
    * Finds every package that some of its dependents mount and others do not.
    *
    * A mounted package is named under its own root and is reachable only through its mount, so a
    * dependent that leaves it unmounted cannot reach it at all. Transitional: the question goes
    * away once every dependency must declare a mount.
    */
  def checkConsistentMounts(manifests: List[Manifest]): List[PackageError] = {
    // Pair every dependency declaration with the manifest that declares it.
    val declarations = manifests.flatMap(m => findFlixDependencies(m).map(dep => (m, dep)))

    declarations.groupBy { case (_, dep) => dep.id }.toList.sortBy { case (id, _) => id }.flatMap {
      case (identifier, decls) =>
        val (mounted, unmounted) = decls.partition { case (_, dep) => dep.mount.isDefined }
        if (mounted.nonEmpty && unmounted.nonEmpty) {
          Some(PackageError.InconsistentMounts(
            identifier,
            mounted.map { case (dependent, _) => dependent.name }.sorted,
            unmounted.map { case (dependent, _) => dependent.name }.sorted
          ))
        } else {
          None
        }
    }
  }

  /**
    * Finds the Flix dependencies in a Manifest.
    */
  def findFlixDependencies(manifest: Manifest): List[FlixDependency] = {
    manifest.dependencies.collect { case dep: FlixDependency => dep }
  }

  /**
    * Finds the most relevant available updates for the given dependency.
    */
  def findAvailableUpdates(dep: FlixDependency, apiKey: Option[String]): Result[AvailableUpdates, PackageError] = {
    for {
      githubProject <- GitHub.parseProject(s"${dep.id.owner}/${dep.id.name}")
      releases <- GitHub.getReleases(githubProject, apiKey)
      availableVersions = releases.map(r => r.version)

      ver = dep.version
      major = ver.majorUpdate(availableVersions)
      minor = ver.minorUpdate(availableVersions)
      patch = ver.patchUpdate(availableVersions)
    } yield AvailableUpdates(major, minor, patch)
  }

  /**
    * Installs all the Flix dependencies of `resolution` into the `lib/` directory of `projectRoot`
    * and returns the installed packages together with the lock file that records them.
    *
    * Each package is checked against `lockfile` as it is installed. The lock file that is
    * returned describes the resolution as it is now, so a dependency that has been added since
    * `lockfile` was written gains an entry, and one that has been removed loses its own.
    */
  def installAll(resolution: SecureResolution, projectRoot: Path, apiKey: Option[String], lockfile: Lockfile)(implicit formatter: Formatter, out: PrintStream): Result[Installation, PackageError] = {
    out.println("Downloading Flix dependencies...")

    // Every dependency declaration, paired with the manifest of the package it resolves to.
    // The version installed is the manifest's own, not the one the declaration asks for: a
    // declaration says what its dependent requires, and the manifest is what the resolution
    // settled on. That is also the version recorded in the lock file, since the lock file
    // describes what was installed.
    val installed = resolution.manifestToFlixDeps.map { case (manifest, dep) =>
      val depName: String = s"${dep.id.owner}/${dep.id.name}"
      install(dep, manifest.version, Bootstrap.EXT_FPKG, projectRoot, apiKey, lockfile) match {
        case Ok(fpkg) =>
          val pkg = InstalledPackage(fpkg.path, dep.id, resolution.security(manifest), manifest.mounts)
          val entry = LockEntry(manifest.version, resolution.tomlDigests(manifest), fpkg.digest)
          (pkg, dep.id -> entry)
        case Err(e) =>
          out.println(s"ERROR: Installation of `$depName' failed.")
          return Err(e)
      }
    }.toList

    val (packages, entries) = installed.unzip
    Ok(Installation(packages, Lockfile(entries.toMap)))
  }

  /**
    * Installs the `extension` file of the Github package `dep` depends on, at `version`.
    *
    * `version` is the version to install. It is not read off `dep`, because a declaration says
    * what its dependent requires, which is not in general what the resolution installs.
    *
    * The package is installed at `lib/<owner>/<repo>`
    *
    * There should be only one file with the given extension.
    *
    * Returns the installed file, whether it was downloaded now or was already cached, and an
    * error if it is not the file `lockfile` records.
    *
    * The check happens here, as the file lands, rather than once everything is installed: a
    * `flix.toml` is parsed and an `.fpkg` becomes a source of code as soon as they are installed,
    * and checking afterwards would mean having already acted on bytes that were never verified.
    */
  private def install(dep: FlixDependency, version: SemVer, extension: String, p: Path, apiKey: Option[String], lockfile: Lockfile)(implicit formatter: Formatter, out: PrintStream): Result[InstalledFile, PackageError] = {
    val proj = GitHub.Project(dep.id.owner, dep.id.name)
    val lib = Bootstrap.getLibraryDirectory(p)
    val assetName = s"${proj.repo}-$version.$extension"
    val dirPath = lib.resolve("github").resolve(proj.owner).resolve(proj.repo).resolve(version.toString)
    // create the directory if it does not exist
    Files.createDirectories(dirPath)
    val assetPath = dirPath.resolve(assetName)

    if (Files.exists(assetPath)) {
      out.println(s"  Cached `${formatter.blue(s"${proj.owner}/${proj.repo}.$extension")}` (${formatter.cyan(s"v$version")}).")
      verifyCached(assetPath, dep, version, extension, lockfile)
    } else {
      GitHub.getSpecificRelease(proj, version, apiKey).flatMap { release =>
        val assets = release.assets.filter(_.name.endsWith(s".$extension"))
        if (assets.isEmpty) {
          Err(PackageError.NoSuchFile(proj.toString, extension))
        } else if (assets.length != 1) {
          Err(PackageError.TooManyFiles(proj.toString, extension))
        } else {
          // download asset to the directory
          val asset = assets.head
          out.print(s"  Downloading `${formatter.blue(s"${proj.owner}/${proj.repo}.$extension")}` (${formatter.cyan(s"v$version")})... ")
          out.flush()
          try {
            val stream = GitHub.downloadAsset(asset)
            try {
              Files.copy(stream, assetPath, StandardCopyOption.REPLACE_EXISTING)
            } finally {
              // Best-effort: the stream is already broken if the copy above failed, so a
              // close failure here must not mask that error.
              try stream.close() catch { case _: IOException => () }
            }
          } catch {
            case e: IOException =>
              // Remove a truncated file so the cache check above doesn't trust it next run. A
              // failure here is attached rather than swallowed, since it means the filesystem
              // itself is in an unexpected state -- but it must not prevent the original
              // download error from being reported below.
              try {
                Files.deleteIfExists(assetPath)
              } catch {
                case e2: IOException => e.addSuppressed(e2)
              }
              out.println(s"ERROR: ${e.getMessage}.")
              return Err(PackageError.DownloadError(asset, Some(e.getMessage)))
          }
          if (Files.exists(assetPath)) {
            out.println(s"OK.")
            verifyDownloaded(assetPath, dep, version, extension, lockfile)
          } else {
            out.println(s"ERROR: File was not created.")
            Err(PackageError.DownloadError(asset, None))
          }
        }
      }
    }
  }

  /**
    * Returns the file at `path` together with the digest of its contents.
    *
    * The digest is taken from the file on disk rather than from the bytes as they are downloaded,
    * so that it describes what a later build will actually read. A file that was cached by an
    * earlier build is digested the same way, which is what lets a corrupted or tampered `lib/`
    * be told apart from an intact one.
    */
  private def digest(path: Path): Result[InstalledFile, PackageError] = {
    try {
      Ok(InstalledFile(path, Sha256.ofFile(path)))
    } catch {
      case e: IOException => Err(PackageError.DigestError(path, e.getMessage))
    }
  }

  /**
    * Returns the file at `path`, which was already in `lib/`, together with its digest, and an
    * error if `lockfile` records a different digest for it.
    */
  private def verifyCached(path: Path, dep: FlixDependency, version: SemVer, extension: String, lockfile: Lockfile): Result[InstalledFile, PackageError] = {
    digest(path).flatMap { file =>
      recordedDigest(dep, version, extension, lockfile) match {
        case Some(expected) if expected != file.digest =>
          Err(PackageError.MismatchedCachedDigest(dep.id, version, extension, path, expected, file.digest))
        case _ =>
          Ok(file)
      }
    }
  }

  /**
    * Returns the file at `path`, which was just downloaded, together with its digest, and an
    * error if `lockfile` records a different digest for it.
    */
  private def verifyDownloaded(path: Path, dep: FlixDependency, version: SemVer, extension: String, lockfile: Lockfile): Result[InstalledFile, PackageError] = {
    digest(path).flatMap { file =>
      recordedDigest(dep, version, extension, lockfile) match {
        case Some(expected) if expected != file.digest =>
          Err(PackageError.MismatchedDownloadedDigest(dep.id, version, extension, path, expected, file.digest))
        case _ =>
          Ok(file)
      }
    }
  }

  /**
    * Returns the digest that `lockfile` records for the `extension` file of the package `dep`
    * depends on, if it records one at `version`.
    *
    * A package that `lockfile` does not record, or records at another version, has no digest
    * here and so is not checked. That is a dependency that was added or whose version was
    * changed since the lock file was written, and there is nothing yet to compare it against.
    * It is recorded when the lock file is written again.
    */
  private def recordedDigest(dep: FlixDependency, version: SemVer, extension: String, lockfile: Lockfile): Option[Sha256] = {
    lockfile.packages.get(dep.id).filter(_.version == version).flatMap {
      entry =>
        // A package is installed as exactly these two files, and the lock file holds a digest of
        // each. Anything else is not something a lock file describes.
        extension match {
          case Bootstrap.EXT_TOML => Some(entry.toml)
          case Bootstrap.EXT_FPKG => Some(entry.fpkg)
          case _ => None
        }
    }
  }

  /**
    * Recursively finds all transitive dependencies of `manifest`.
    * Downloads any missing toml files for found dependencies and
    * parses them to manifests. Returns the list of manifests.
    * `res` is the list of Manifests found so far to avoid duplicates.
    */
  private def findTransitiveDependenciesRec(manifest: Manifest, path: Path, res: List[Manifest], apiKey: Option[String], lockfile: Lockfile)(implicit immediateDependents: mutable.Map[Manifest, List[Manifest]], manifestToDep: mutable.Map[Manifest, List[Dependency.FlixDependency]], tomlDigests: mutable.Map[Manifest, Sha256], formatter: Formatter, out: PrintStream): Result[List[Manifest], PackageError] = {
    // find Flix dependencies of the current manifest
    val flixDeps = findFlixDependencies(manifest)

    for {
      // download toml files
      tomlFiles <- traverse(flixDeps) { dep =>
        val depName = s"${dep.id.owner}/${dep.id.name}"
        install(dep, dep.version, Bootstrap.EXT_TOML, path, apiKey, lockfile).map(toml => (toml, dep))
      }

      // parse manifests
      transitiveManifests <- traverse(tomlFiles) { case (toml, d) => validateManifest(toml, d, d.version) }

    } yield {
      for (m <- transitiveManifests) {
        immediateDependents.put(m, manifest :: immediateDependents.getOrElse(m, List.empty))
      }

      // remove duplicates
      val newManifests = transitiveManifests.filter(!res.contains(_))
      var newRes = res ++ newManifests

      // do recursive calls for all dependencies
      for (m <- newManifests) {
        findTransitiveDependenciesRec(m, path, newRes, apiKey, lockfile) match {
          case Ok(t) => newRes = newRes ++ t.filter(!newRes.contains(_))
          case Err(e) => return Err(e)
        }
      }
      newRes
    }
  }

  /** Parses and validates the manifest in `toml` by checking that it declares `version`, the
    * version that was downloaded.
    *
    * `version` is passed rather than read off `flixDep` for the reason given on [[install]].
    *
    * Also mutates `manifestToDep` by adding or updating the mapping `m -> ds` to `m -> d :: ds`,
    * and records the digest of the file `m` was parsed from in `tomlDigests`.
    */
  private def validateManifest(toml: InstalledFile, flixDep: FlixDependency, version: SemVer)(implicit manifestToDep: mutable.Map[Manifest, List[Dependency.FlixDependency]], tomlDigests: mutable.Map[Manifest, Sha256]): Result[Manifest, PackageError] = {
    parseManifest(toml.path).flatMap {
      m =>
        manifestToDep.put(m, flixDep :: manifestToDep.getOrElse(m, List.empty))
        tomlDigests.put(m, toml.digest)
        if (m.version == version) {
          Ok(m)
        } else {
          Err(PackageError.MismatchedVersions(m, flixDep))
        }
    }
  }

  /**
    * Computes the maximum allowed security level for `manifest` which is the minimum / greatest lower bound of both
    *   1. the [[minSecurityLevel]] of all (transitive) dependent / parent manifests and
    *   1. the security levels with which `manifest` is depended upon,
    *      i.e., when `"security" = "..."` occurs in a manifest and that dependency points to `manifest`.
    */
  private def minSecurityLevel(manifest: Manifest)(implicit resolution: Resolution, securityLevels: mutable.Map[Manifest, SecurityContext]): SecurityContext = {
    securityLevels.get(manifest) match {
      case Some(t) => t
      case None =>
        val incomingSctxs = resolution.manifestToFlixDeps(manifest).map(_.sctx)
        val parentSctxs = resolution.immediateDependents(manifest).map(minSecurityLevel)
        val glb = SecurityContext.glb(parentSctxs ::: incomingSctxs)
        securityLevels.put(manifest, glb)
        glb
    }
  }

  /**
    * A security error is present if one of the following holds:
    *   1. A manifest `m0` has allowed security context `t0` and `m0` contains a dependency with security context `t1` where `t1 > t0`. E.g., `unrestricted > plain`.
    *   1. A manifest `m0` has security context `plain` or lower and contains at least one jar or maven dependency.
    *
    * Note that the first condition represents an inconsistency in the security levels of the dependency graph itself.
    * Such an inconsistency exists if for some path `u ~> v` it holds that `security(u) >= security(v)`
    * and an edge `v -> w` exists where `security(w) > security(u)`.
    * E.g., if an edge with security context `unrestricted` is found on a path with max security context `plain`, then an error exists.
    */
  private def findSecurityViolations(m: Manifest, sctx: SecurityContext): List[PackageError] = {
    val graphErrors = checkGraphErrors(m, sctx)
    val dependencyErrors = checkJavaDependencies(m, sctx)
    dependencyErrors ::: graphErrors
  }

  /**
    * Checks condition 1 of [[findSecurityViolations]] and returns all security inconsistencies.
    */
  private def checkGraphErrors(m: Manifest, sctx: SecurityContext): List[PackageError.DepGraphSecurityError] = {
    val flixDeps = m.dependencies.collect { case d: FlixDependency => d }
    flixDeps.filter(d => d.sctx.greaterThan(sctx)).map(d => PackageError.DepGraphSecurityError(m, d, sctx))
  }

  /**
    * Checks condition 2 of [[findSecurityViolations]] and returns all illegal dependencies.
    */
  private def checkJavaDependencies(m: Manifest, sctx: SecurityContext): List[PackageError.IllegalJavaDependencyForSctx] = sctx match {
    case SecurityContext.Unrestricted =>
      List.empty

    case SecurityContext.Plain =>
      // No maven or jar deps allowed
      m.dependencies.collect {
        case d: MavenDependency => d
        case d: JarDependency => d
      }.map(d => PackageError.IllegalJavaDependencyForSctx(m, d, sctx))

    case SecurityContext.Paranoid =>
      // No maven or jar deps allowed
      m.dependencies.collect {
        case d: MavenDependency => d
        case d: JarDependency => d
      }.map(d => PackageError.IllegalJavaDependencyForSctx(m, d, sctx))
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

}
