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
import ca.uwaterloo.flix.language.ast.shared.{PackageId, SecurityContext}
import ca.uwaterloo.flix.tools.pkg.Dependency.{FlixDependency, JarDependency, MavenDependency}
import ca.uwaterloo.flix.tools.pkg.github.GitHub
import ca.uwaterloo.flix.util.{Formatter, Result, Sha256}
import ca.uwaterloo.flix.util.Result.{Err, Ok, traverse}
import ca.uwaterloo.flix.util.collection.ListMap

import java.io.{IOException, InputStream, PrintStream}
import java.nio.file.{Files, Path, StandardCopyOption}
import scala.annotation.tailrec
import scala.collection.mutable

object FlixPackageManager {

  /**
    * Represents the dependency resolution of [[origin]].
    * All fields should be considered private except [[origin]] and [[manifests]].
    *
    * @param origin              the manifest that corresponds to the current / local project.
    * @param manifests           the manifest of [[origin]] and of every package it is built with.
    * @param immediateDependents all immediate dependents / parents of each manifest.
    * @param manifestToFlixDeps  a mapping from [[Manifest]]s to [[FlixDependency]]s.
    *                            A manifest is the resource a flix dependency resolves to. A
    *                            manifest maps to every declaration that resolves to it, which
    *                            is one for each of its dependents.
    * @param tomlDigests         the digest of the `flix.toml` of every package at every version
    *                            that the dependency graph requires, whether or not the package
    *                            is built at that version: each of them is read to resolve the
    *                            graph. [[origin]] does not appear: its manifest is the one in
    *                            the project directory, which is not downloaded.
    */
  case class Resolution(origin: Manifest,
                        manifests: List[Manifest],
                        immediateDependents: Map[Manifest, List[Manifest]],
                        manifestToFlixDeps: ListMap[Manifest, FlixDependency],
                        tomlDigests: Map[(PackageId, SemVer), Sha256])

  /**
    * Represents the dependency resolution of [[origin]] where the maximum security level has been computed
    * for each manifest.
    *
    * @param origin             the manifest that corresponds to the current / local project.
    * @param security           the maximum allowed security level of each manifest.
    * @param manifestToFlixDeps a mapping from [[Manifest]]s to [[FlixDependency]]s.
    *                           A manifest is the resource a flix dependency resolves to.
    * @param tomlDigests        the digest of the `flix.toml` of every package at every version
    *                           that the dependency graph requires, see [[Resolution]].
    */
  case class SecureResolution(origin: Manifest,
                              security: Map[Manifest, SecurityContext],
                              manifestToFlixDeps: ListMap[Manifest, FlixDependency],
                              tomlDigests: Map[(PackageId, SemVer), Sha256]) {
    /**
      * All manifests in the resolution.
      */
    val manifests: List[Manifest] = security.keys.toList
  }

  /**
    * The Flix packages installed for a project.
    *
    * @param packages the installed packages.
    * @param lockfile what each installed package was, to be recorded in `packages.lock`.
    */
  case class Installation(packages: List[InstalledPackage], lockfile: Lockfile)

  /**
    * A dependency declaration that has been followed.
    *
    * @param source    the package and version that `dependent` is the manifest of, or `None` if
    *                  `dependent` is the manifest of the project.
    * @param dependent the manifest that declares `dep`.
    * @param dep       the declaration.
    * @param target    the manifest of the package `dep` names, at the version `dep` declares.
    * @param digest    the digest of the `flix.toml` that `target` was parsed from.
    */
  private case class Edge(source: Option[(PackageId, SemVer)], dependent: Manifest, dep: FlixDependency, target: Manifest, digest: Sha256)

  /**
    * A file installed in the `lib/` directory.
    *
    * @param path   the path to the file.
    * @param digest the digest of the contents of the file.
    */
  private case class InstalledFile(path: Path, digest: Sha256)

  /**
    * Finds the packages that `manifest` is built with and returns their manifests. The
    * `flix.toml` files of the packages are put at `path/lib`.
    *
    * The dependency graph has a node for every package at every version that something
    * requires, and an edge for every dependency declaration. It is resolved in three steps.
    * Each step reads the ones before it, and none feeds back into an earlier one:
    *
    *   1. Reach, see [[reach]]: every node that can be reached from `manifest` is visited, once, and its
    *      `flix.toml` is downloaded. A version that is not selected is visited like any other,
    *      and what it requires counts in the next step. Were it skipped, the result would
    *      depend on whether it was met before or after the version selected in its place.
    *
    *   1. Select, see [[select]]: every package is given the greatest version it is required
    *      at, which is what [[selectVersion]] picks. This is minimal version selection: read as
    *      a lower bound, a requirement is satisfied by any version at or above it, so the
    *      greatest version required is the least one that satisfies every dependent.
    *
    *   1. Live, see [[live]]: a package is built only if it can be reached from `manifest` through the
    *      selected versions. A package that is required only by versions that were not selected
    *      is dropped: it is not installed, not locked, and not compiled.
    *
    * For example, given the declarations:
    *
    * {{{
    *   project  requires  A 1.0.0  and  B 1.0.0
    *   A 1.0.0  requires  C 1.1.1
    *   B 1.0.0  requires  C 1.1.2
    *   C 1.1.1  requires  X 1.0.0
    *   C 1.1.2  requires  nothing
    * }}}
    *
    * Reach visits A 1.0.0, B 1.0.0, C 1.1.1, C 1.1.2, and X 1.0.0. Select gives C the version
    * 1.1.2, the greater of the two it is required at, which satisfies both A and B. Live drops
    * X, since the only node that requires it is C 1.1.1, which was not selected. The project is
    * built with A 1.0.0, B 1.0.0, and C 1.1.2.
    *
    * X is dropped after the selection and not before it, so had X required a package, that
    * requirement would have counted in the selection. A selection can therefore be higher than
    * is strictly needed, but it never depends on which versions were selected.
    *
    * A package that is built at a greater version than one of its dependents declares is
    * reported on `out`, with the dependents that require the greater version.
    *
    * Returns an error if a package is required at versions that do not share a major, since
    * then there is no version to select for it.
    */
  def resolve(manifest: Manifest, path: Path, apiKey: Option[String], lockfile: Lockfile)(implicit formatter: Formatter, out: PrintStream): Result[Resolution, PackageError] = {
    out.println("Resolving Flix dependencies...")
    reach(manifest, path, apiKey, lockfile).flatMap { edges =>
      // Every node, in the order it was found, and the first edge that led to it.
      val nodes = edges.map(e => (e.dep.id, e.dep.version)).distinct
      val edgeTo = edges.reverseIterator.map(e => (e.dep.id, e.dep.version) -> e).toMap

      select(nodes) match {
        case Err(id) =>
          val requirements = edges.collect { case e if e.dep.id == id => (e.dependent, e.dep) }
          Err(mkIncompatibleVersions(id, requirements))

        case Ok(selected) =>
          val roots = edges.collect { case e if e.source.isEmpty => e.dep.id }
          val requires = edges.collect { case Edge(Some(source), _, dep, _, _) => source -> dep.id }.groupMap(_._1)(_._2)
          val livePackages = live(roots, selected, requires)

          // A node is built if its package is live and it is the selected version of it.
          def isBuilt(node: (PackageId, SemVer)): Boolean = node match {
            case (id, version) => livePackages.contains(id) && selected(id) == version
          }

          // Every declaration made by what is built is an edge of the resolution. It leads to the
          // selected version of the package it names, whichever version it declares.
          val immediateDependents: mutable.Map[Manifest, List[Manifest]] = mutable.Map(manifest -> List.empty)
          val manifestToFlixDeps: mutable.Map[Manifest, List[FlixDependency]] = mutable.Map.empty
          for (e <- edges if e.source.forall(isBuilt)) {
            val target = edgeTo((e.dep.id, selected(e.dep.id))).target
            immediateDependents.put(target, e.dependent :: immediateDependents.getOrElse(target, List.empty))
            manifestToFlixDeps.put(target, e.dep :: manifestToFlixDeps.getOrElse(target, List.empty))
          }

          printRaised(edges, edges.filter(e => e.source.forall(isBuilt)), selected)

          val built = nodes.filter(isBuilt).map(edgeTo)
          val manifests = manifest :: built.map(_.target)
          val tomlDigests = nodes.map(n => n -> edgeTo(n).digest).toMap
          // Every declaration is kept, and not one for each package: the security context of a
          // package is the strictest of those it is declared with, so all of them must be seen.
          Ok(Resolution(manifest, manifests, immediateDependents.toMap, ListMap(manifestToFlixDeps.toMap), tomlDigests))
      }
    }
  }

  /**
    * Follows every dependency declaration that can be reached from `origin`, and returns them in
    * the order they were followed.
    *
    * The graph is walked with a worklist of the manifests whose declarations are yet to be
    * followed. A package is visited once for every version it is required at, however many
    * dependents require it, which is also what ends the walk on a cycle.
    */
  private def reach(origin: Manifest, path: Path, apiKey: Option[String], lockfile: Lockfile)(implicit formatter: Formatter, out: PrintStream): Result[List[Edge], PackageError] = {
    val edges: mutable.ListBuffer[Edge] = mutable.ListBuffer.empty

    // Every package version that has been visited. A visit is identified by the package and the
    // version, which is what a dependency declaration states, so whether a declaration leads
    // somewhere new is known without comparing manifests.
    val visited: mutable.Set[(PackageId, SemVer)] = mutable.Set.empty

    // The manifests whose declarations are yet to be followed. What a manifest depends on is
    // followed before the manifests that were found beside it, so the walk is depth first.
    var worklist: List[(Option[(PackageId, SemVer)], Manifest)] = List((None, origin))

    while (worklist.nonEmpty) {
      val (source, dependent) = worklist.head
      follow(source, dependent, path, apiKey, lockfile) match {
        case Err(e) => return Err(e)
        case Ok(found) =>
          // Every declaration is an edge of the graph, whether or not it leads somewhere new.
          edges ++= found
          val unvisited = found.collect {
            case e if visited.add((e.dep.id, e.dep.version)) => (Option((e.dep.id, e.dep.version)), e.target)
          }
          worklist = unvisited ::: worklist.tail
      }
    }

    Ok(edges.toList)
  }

  /**
    * Follows every Flix dependency that `dependent` declares: downloads and parses the manifest
    * of the package at the declared version.
    *
    * Every `flix.toml` is installed before any of them is parsed.
    */
  private def follow(source: Option[(PackageId, SemVer)], dependent: Manifest, path: Path, apiKey: Option[String], lockfile: Lockfile)(implicit formatter: Formatter, out: PrintStream): Result[List[Edge], PackageError] = {
    for {
      // download toml files
      tomlFiles <- traverse(findFlixDependencies(dependent)) { dep =>
        install(dep, dep.version, Bootstrap.EXT_TOML, path, apiKey, lockfile).map(toml => (toml, dep))
      }

      // parse manifests
      edges <- traverse(tomlFiles) {
        case (toml, dep) => validateManifest(toml, dep, dep.version).map(target => Edge(source, dependent, dep, target, toml.digest))
      }
    } yield edges
  }

  /** Parses and validates the manifest in `toml` by checking that it declares `version`, the
    * version of the release it was downloaded from.
    *
    * A release that declares another version than the one it is published as is a mistake in
    * how the package was released, and not something a dependent can resolve.
    */
  private def validateManifest(toml: InstalledFile, flixDep: FlixDependency, version: SemVer): Result[Manifest, PackageError] = {
    parseManifest(toml.path).flatMap {
      m =>
        if (m.version == version) {
          Ok(m)
        } else {
          Err(PackageError.MismatchedVersions(flixDep.id, version, m.version))
        }
    }
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
    * Returns the version that every package in `nodes` is given: the one [[selectVersion]] picks among
    * the versions the package occurs at in `nodes`.
    *
    * Returns the least package, in order of identifier, whose versions do not share a major, if
    * there is one.
    */
  def select(nodes: List[(PackageId, SemVer)]): Result[Map[PackageId, SemVer], PackageId] = {
    val byPackage = nodes.groupMap(_._1)(_._2).toList.sortBy { case (id, _) => id }
    traverse(byPackage) {
      case (id, versions) => selectVersion(versions) match {
        case Some(version) => Ok(id -> version)
        case None => Err(id)
      }
    }.map(_.toMap)
  }

  /**
    * Returns the version minimal version selection picks for a package required at `versions`,
    * if there is one.
    *
    * The pick is the greatest of `versions`. Read as a lower bound, a requirement is satisfied by
    * any version at or above it, so the greatest is the least version that satisfies them all.
    *
    * Versions that do not share a major have no such pick. A major is a compatibility boundary,
    * so the greater of two majors is not a version the other dependent can be given, and there is
    * nothing to select.
    *
    * `versions` must be non-empty.
    */
  def selectVersion(versions: List[SemVer]): Option[SemVer] = {
    if (versions.map(_.major).distinct.sizeIs > 1)
      None
    else
      versions.maxOption
  }

  /**
    * Returns the packages that can be reached from `roots` through the versions in `selected`.
    *
    * `requires` gives the packages that a package at a version requires. A package is reached
    * through what its selected version requires, and through nothing that another version of it
    * requires.
    */
  def live(roots: List[PackageId], selected: Map[PackageId, SemVer], requires: Map[(PackageId, SemVer), List[PackageId]]): Set[PackageId] = {
    val found: mutable.Set[PackageId] = mutable.Set.empty
    var worklist: List[PackageId] = roots
    while (worklist.nonEmpty) {
      val id = worklist.head
      worklist = worklist.tail
      if (found.add(id)) {
        worklist = requires.getOrElse((id, selected(id)), List.empty) ::: worklist
      }
    }
    found.toSet
  }

  /**
    * Prints a line for every package that is built at a greater version than a declaration in
    * `built` asks for, where `built` is the declarations made by what is built and `edges` is
    * every declaration in the graph.
    *
    * The line gives the least version that was asked for, the version that was selected, and who
    * requires the selected version, which is looked for in `edges`: a requirement counts towards
    * the selection whether or not the package that makes it is built.
    *
    * A rise across a minor version of a package whose major version is 0 is printed in yellow,
    * since a minor version is allowed to break compatibility before 1.0.0.
    */
  private def printRaised(edges: List[Edge], built: List[Edge], selected: Map[PackageId, SemVer])(implicit formatter: Formatter, out: PrintStream): Unit = {
    for ((id, declarations) <- built.groupBy(_.dep.id).toList.sortBy { case (id, _) => id }) {
      val from = declarations.map(_.dep.version).min
      val to = selected(id)
      if (from < to) {
        val requiredBy = edges.collect {
          case e if e.dep.id == id && e.dep.version == to => e.source match {
            case Some((dependent, version)) => s"`${formatter.blue(s"${dependent.owner}/${dependent.name}")}` (${formatter.cyan(s"v$version")})"
            case None => s"`${formatter.blue(e.dependent.displayName)}`"
          }
        }.distinct.sorted
        val rise = s"v$from -> v$to"
        val breaking = to.major == 0 && from.minor != to.minor
        out.println(s"  Raised `${formatter.blue(s"${id.owner}/${id.name}")}` (${if (breaking) formatter.yellow(rise) else formatter.cyan(rise)}), required by ${requiredBy.mkString(", ")}.")
      }
    }
  }

  /**
    * Resolves the maximal allowed security level for all dependencies in `resolution`.
    */
  def resolveSecurityLevels(resolution: Resolution): SecureResolution = {
    SecureResolution(resolution.origin, minSecurityLevels(resolution), resolution.manifestToFlixDeps, resolution.tomlDigests)
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
    * Returns the error that the package `id` is required at versions that do not share a major,
    * where `requirements` is every declaration that requires it, paired with the manifest that
    * makes it.
    */
  def mkIncompatibleVersions(id: PackageId, requirements: List[(Manifest, FlixDependency)]): PackageError.IncompatibleVersions = {
    // Order by version, and then by dependent, so the message is deterministic.
    val sorted = requirements.sortBy { case (dependent, dep) => (dep.version, dependent.displayName) }
    PackageError.IncompatibleVersions(id, sorted)
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
            mounted.map { case (dependent, _) => dependent.displayName }.sorted,
            unmounted.map { case (dependent, _) => dependent.displayName }.sorted
          ))
        } else {
          None
        }
    }
  }

  /**
    * Finds every package in `resolution` that requires a newer version of Flix than `current`.
    *
    * Only the packages that are built are checked, since a package that is not built is not
    * compiled. The project itself is not checked here: it is for whoever read its manifest to
    * check, since only they know where it was read from.
    */
  def checkFlixVersions(resolution: Resolution, current: SemVer): List[PackageError] = {
    val errors = for {
      manifest <- resolution.manifests
      if manifest != resolution.origin && current < manifest.flix
      // Any of the declarations that resolve to the package will do, since they all name it.
      dep <- resolution.manifestToFlixDeps(manifest).headOption
    } yield PackageError.FlixVersionTooOld(dep.id, manifest.version, manifest.flix, current)
    errors.sortBy(e => e.identifier)
  }

  /**
    * Finds the Flix dependencies in a Manifest.
    */
  def findFlixDependencies(manifest: Manifest): List[FlixDependency] = {
    manifest.dependencies.collect { case dep: FlixDependency => dep }
  }

  /**
    * Returns the version that every package in `resolution` is built at.
    *
    * It is the version of the manifest that the declarations of the package resolve to, and not
    * a version that any of them declares: a package is built at the greatest version that is
    * required of it, see [[resolve]].
    */
  def builtVersions(resolution: SecureResolution): Map[PackageId, SemVer] = {
    resolution.manifestToFlixDeps.m.collect {
      // Any of the declarations that resolve to the package will do, since they all name it.
      case (manifest, dep :: _) => dep.id -> manifest.version
    }
  }

  /**
    * Finds the most relevant available updates of the package `id`, which are the releases of it
    * that are newer than `version`.
    *
    * `version` is what to compare against. For a package that is built, it should be the version
    * it is built at, see [[builtVersions]], and not the version a dependent declares, which is
    * only the least version that dependent can be built with.
    */
  def findAvailableUpdates(id: PackageId, version: SemVer, apiKey: Option[String]): Result[AvailableUpdates, PackageError] = {
    for {
      githubProject <- GitHub.parseProject(s"${id.owner}/${id.name}")
      releases <- GitHub.getReleases(githubProject, apiKey)
      availableVersions = releases.map(r => r.version)

      major = version.majorUpdate(availableVersions)
      minor = version.minorUpdate(availableVersions)
      patch = version.patchUpdate(availableVersions)
    } yield AvailableUpdates(major, minor, patch)
  }

  /**
    * Installs all the Flix dependencies of `resolution` into the `lib/` directory of `projectRoot`
    * and returns the installed packages together with the lock file that records them.
    *
    * Each package is checked against `lockfile` as it is installed. The lock file that is
    * returned describes the resolution as it is now, so a dependency that has been added since
    * `lockfile` was written gains an entry, and one that has been removed loses its own.
    *
    * It records the `flix.toml` of every package at every version that the graph requires, and
    * the `.fpkg` of the packages that are installed, which are those that are built.
    */
  def installAll(resolution: SecureResolution, projectRoot: Path, apiKey: Option[String], lockfile: Lockfile)(implicit formatter: Formatter, out: PrintStream): Result[Installation, PackageError] = {
    out.println("Downloading Flix dependencies...")

    // Every package, with one of the declarations that resolve to it. A package is installed
    // once, however many dependents declare it, and any of the declarations will do: they all
    // name the same package.
    //
    // The version installed is the manifest's own, not the one the declaration asks for: a
    // declaration says what its dependent requires, and the manifest is what the resolution
    // settled on.
    val installed = resolution.manifestToFlixDeps.m.toList.collect { case (manifest, dep :: _) =>
      val depName: String = s"${dep.id.owner}/${dep.id.name}"
      install(dep, manifest.version, Bootstrap.EXT_FPKG, projectRoot, apiKey, lockfile) match {
        case Ok(fpkg) =>
          val pkg = InstalledPackage(fpkg.path, dep.id, resolution.security(manifest), manifest.mounts)
          (pkg, (dep.id, manifest.version) -> fpkg.digest)
        case Err(e) =>
          out.println(s"ERROR: Installation of `$depName' failed.")
          return Err(e)
      }
    }

    val (packages, fpkgs) = installed.unzip
    val fpkgDigests = fpkgs.toMap
    val entries = resolution.tomlDigests.map {
      case (node, toml) => node -> LockEntry(toml, fpkgDigests.get(node))
    }
    Ok(Installation(packages, Lockfile(entries)))
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
      out.print(s"  Downloading `${formatter.blue(s"${proj.owner}/${proj.repo}.$extension")}` (${formatter.cyan(s"v$version")})... ")
      out.flush()
      openReleaseAsset(proj, version, extension, apiKey) match {
        case Err(e) =>
          out.println("ERROR.")
          Err(e)

        case Ok(stream) =>
          try {
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
              return Err(PackageError.DownloadError(assetName, Some(e.getMessage)))
          }
          if (Files.exists(assetPath)) {
            out.println(s"OK.")
            verifyDownloaded(assetPath, dep, version, extension, lockfile)
          } else {
            out.println(s"ERROR: File was not created.")
            Err(PackageError.DownloadError(assetName, None))
          }
      }
    }
  }

  /**
    * Opens a stream over the `extension` file of `proj`'s `version` release. The caller closes
    * the stream.
    *
    * A release asset's address follows from the repository, the version, and the name, so the
    * names a package is expected to publish are tried first, each at the cost of one request
    * that either finds the file or does not. Only if none of them is there is the release
    * listing read, which costs a request against the API rate limit: 60 an hour for an
    * anonymous client, shared by every package a build resolves.
    *
    * Only a name that is not there is worth another guess. A refusal or an unreachable server
    * says nothing about the name, and reading the listing would not get any further, so it is
    * reported as it is.
    */
  private def openReleaseAsset(proj: GitHub.Project, version: SemVer, extension: String, apiKey: Option[String]): Result[InputStream, PackageError] = {
    def fromListing(): Result[InputStream, PackageError] =
      GitHub.findReleaseAsset(proj, version, extension, apiKey).flatMap(asset => GitHub.download(asset.url))

    @tailrec
    def tryNames(names: List[String]): Result[InputStream, PackageError] = names match {
      case Nil => fromListing()
      case name :: rest =>
        GitHub.downloadReleaseAsset(proj, version, name) match {
          case Err(_: PackageError.ReleaseAssetNotFound) => tryNames(rest)
          case result => result
        }
    }

    tryNames(guessedAssetNames(proj, extension))
  }

  /**
    * Returns the names the `extension` asset of `proj` is guessed to have, in the order they are
    * tried. A package whose asset has none of them is found through the release listing instead.
    */
  private def guessedAssetNames(proj: GitHub.Project, extension: String): List[String] = extension match {
    case Bootstrap.EXT_TOML =>
      List(Bootstrap.FLIX_TOML)

    case Bootstrap.EXT_FPKG =>
      // A release published before the package was given a fixed name carries the name of the
      // repository, or of the directory it was built in, which cannot be guessed at all.
      List(Bootstrap.PACKAGE_FPKG, s"${proj.repo}.$extension")

    case _ =>
      Nil
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
    * depends on, at `version`, if it records one.
    *
    * A package that `lockfile` does not record at `version` has no digest here and so is not
    * checked. That is a dependency that was added, or that is required or built at a version it
    * was not when the lock file was written, and there is nothing yet to compare it against. It
    * is recorded when the lock file is written again.
    */
  private def recordedDigest(dep: FlixDependency, version: SemVer, extension: String, lockfile: Lockfile): Option[Sha256] = {
    lockfile.packages.get((dep.id, version)).flatMap {
      entry =>
        // A package is installed as exactly these two files, and the lock file holds a digest of
        // each. Anything else is not something a lock file describes.
        extension match {
          case Bootstrap.EXT_TOML => Some(entry.toml)
          case Bootstrap.EXT_FPKG => entry.fpkg
          case _ => None
        }
    }
  }

  /**
    * Computes the maximum allowed security level of every manifest in `resolution`: the strictest
    * of the levels its (transitive) dependents are given and the levels it is depended upon with,
    * i.e. the `"security" = "..."` of every declaration that points to it.
    *
    * The graph can hold a cycle — two packages can each require the other, and a package that
    * requires an older version of itself is its own dependent — so the levels are a fixpoint:
    * every manifest starts unrestricted, and a round lowers it to the strictest of what its
    * dependents hold and its declarations ask for. A round never raises a level, so they end.
    *
    * The project is the root of the graph and is not lowered.
    */
  private def minSecurityLevels(resolution: Resolution): Map[Manifest, SecurityContext] = {
    val levels: mutable.Map[Manifest, SecurityContext] =
      mutable.Map.from(resolution.manifests.map(m => m -> SecurityContext.Unrestricted))

    var changed = true
    while (changed) {
      changed = false
      for (manifest <- resolution.manifests) {
        if (manifest != resolution.origin) {
          val incomingSctxs = resolution.manifestToFlixDeps(manifest).map(_.sctx)
          val parentSctxs = resolution.immediateDependents(manifest).map(levels)
          val glb = SecurityContext.glb(parentSctxs ::: incomingSctxs)
          if (glb != levels(manifest)) {
            levels.put(manifest, glb)
            changed = true
          }
        }
      }
    }

    levels.toMap
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

}
