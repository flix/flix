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

import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.language.ast.shared.{Mountpoint, PackageId, Repository, SecurityContext}
import ca.uwaterloo.flix.tools.pkg.Dependency.{FlixDependency, JarDependency, MavenDependency}
import ca.uwaterloo.flix.tools.pkg.github.GitHub
import ca.uwaterloo.flix.util.Result
import ca.uwaterloo.flix.util.Result.{Err, Ok, traverse}
import org.tomlj.*

import java.io.{IOException, StringReader}
import java.net.URI
import java.nio.file.Path
import scala.collection.mutable
import scala.jdk.CollectionConverters.{ListHasAsScala, SetHasAsScala}

object ManifestParser {
  /**
    * Regular expression defining a valid string for the username and project name of a Flix
    * dependency. Concretely, a valid name consists only of alphanumeric characters, `_`, and `-`.
    *
    * A `.` is not allowed: the name becomes part of the package's canonical root, which is a JVM
    * package path, and a `.` is the separator there as well as in a Flix namespace.
    */
  /**
    * Creates a Manifest from the .toml file
    * at path `p` and returns an error if
    * there are parsing errors
    */
  def parse(p: Path): Result[Manifest, ManifestError] = {
    val parser = try {
      Toml.parse(p)
    } catch {
      case e: IOException => return Err(ManifestError.IOError(p, e.getMessage))
    }
    createManifest(parser, p)
  }

  /**
    * Creates a Manifest from the String `s`
    * which should have the .toml format and
    * returns an error if there are parsing
    * errors. The path `p` should be where `s`
    * comes from.
    */
  def parse(s: String, p: Path): Result[Manifest, ManifestError] = {
    val stringReader = new StringReader(s)
    val parser = try {
      Toml.parse(stringReader)
    } catch {
      case e: IOException => return Err(ManifestError.IOError(p, e.getMessage))
    }
    createManifest(parser, p)
  }

  /**
    * Creates a Manifest from the TomlParseResult
    * which should be at path `p` and returns an
    * error if there are parsing errors.
    */
  private def createManifest(parser: TomlParseResult, p: Path): Result[Manifest, ManifestError] = {
    val errors = parser.errors
    if (errors.size() > 0) {
      var errorString = ""
      errors.forEach(error => errorString = errorString + error.toString + ", ")
      return Err(ManifestError.ManifestParseError(p, errorString))
    }

    for (
      _ <- checkKeys(parser, p);

      // For backwards compatibility -- for now -- we still accept the `name` field.

      version <- getRequiredStringProperty("package.version", parser, p);
      versionSemVer <- toFlixVer(version, p);

      repository <- getOptionalStringProperty("package.repository", parser, p);
      githubProject <- Result.traverseOpt(repository)(r => toGithubProject(r, p));

      flix <- getRequiredStringProperty("package.flix", parser, p);
      flixSemVer <- toFlixVer(flix, p);

      deps <- getOptionalTableProperty("dependencies", parser, p);
      depsList <- collectDependencies(deps, flixDep = true, jarDep = false, p);
      _ <- checkDuplicateMounts(depsList, p);

      mvnDeps <- getOptionalTableProperty("mvn-dependencies", parser, p);
      mvnDepsList <- collectDependencies(mvnDeps, flixDep = false, jarDep = false, p);

      jarDeps <- getOptionalTableProperty("jar-dependencies", parser, p);
      jarDepsList <- collectDependencies(jarDeps, flixDep = false, jarDep = true, p)

    ) yield Manifest(versionSemVer, githubProject, flixSemVer, depsList ++ mvnDepsList ++ jarDepsList)
  }

  private def checkKeys(parser: TomlParseResult, p: Path): Result[Unit, ManifestError] = {
    val keySet: Set[String] = parser.keySet().asScala.toSet
    val allowedKeys = Set("package", "dependencies", "mvn-dependencies", "jar-dependencies")
    val illegalKeys = keySet.diff(allowedKeys)

    if (illegalKeys.nonEmpty) {
      return Err(ManifestError.IllegalTableFound(p, illegalKeys.head))
    }

    val dottedKeys = parser.dottedKeySet().asScala.toSet
    val packageKeys = dottedKeys.filter(s => s.startsWith("package."))
    val allowedPackageKeys = Set("package.name", "package.description", "package.version", "package.repository", "package.modules", "package.flix", "package.authors", "package.license")
    val illegalPackageKeys = packageKeys.diff(allowedPackageKeys)
    if (illegalPackageKeys.nonEmpty) {
      return Err(ManifestError.IllegalPackageKeyFound(p, illegalPackageKeys.head))
    }

    Ok(())
  }

  /**
    * Parses a String which should be at `prop`
    * and returns the String or an error if the result
    * cannot be found.
    */
  private def getRequiredStringProperty(prop: String, parser: TomlParseResult, p: Path): Result[String, ManifestError] = {
    try {
      val result = parser.getString(prop)
      if (result == null) {
        return Err(ManifestError.MissingRequiredProperty(p, prop, None))
      }
      Ok(result)
    } catch {
      case e: IllegalArgumentException => Err(ManifestError.MissingRequiredProperty(p, prop, Some(e.getMessage)))
      case e: TomlInvalidTypeException => Err(ManifestError.RequiredPropertyHasWrongType(p, prop, "String", e.getMessage))
    }
  }

  /**
    * Parses a String which might be at `prop`
    * and returns the String as an Option.
    */
  private def getOptionalStringProperty(prop: String, parser: TomlParseResult, p: Path): Result[Option[String], ManifestError] = {
    try {
      val result = parser.getString(prop)
      Ok(Option(result))
    } catch {
      case _: IllegalArgumentException => Ok(None)
      case e: TomlInvalidTypeException => Err(ManifestError.RequiredPropertyHasWrongType(p, prop, "String", e.getMessage))
    }
  }

  /**
    * Parses a Table which should be at `prop`
    * and returns the Table or an error if the result
    * cannot be found.
    */
  private def getOptionalTableProperty(prop: String, parser: TomlParseResult, p: Path): Result[Option[TomlTable], ManifestError] = {
    try {
      val table = parser.getTable(prop)
      Ok(Option(table))
    } catch {
      case _: IllegalArgumentException => Ok(None)
      case e: TomlInvalidTypeException => Err(ManifestError.RequiredPropertyHasWrongType(p, prop, "Table", e.getMessage))
    }
  }

  /**
    * Converts a String `s` to a semantic version and returns
    * an error if the String is not of the correct format.
    * The only allowed format is "x.x.x"
    */
  private def toFlixVer(s: String, p: Path): Result[SemVer, ManifestError] = {
    try {
      s.split('.') match {
        case Array(major, minor, patch) =>
          Ok(SemVer(major.toInt, minor.toInt, patch.toInt))
        case _ => Err(ManifestError.FlixVersionHasWrongLength(p, s))
      }
    } catch {
      case e: NumberFormatException => Err(ManifestError.VersionNumberWrong(p, s, e.getMessage))
    }
  }

  /**
    * Converts a String `s` to a reference to a GitHub project.
    * Returns an error if the string is not in the correct format.
    * The only allowed format is "github:<username>/<repository>".
    */
  private def toGithubProject(s: String, p: Path): Result[GitHub.Project, ManifestError] = {
    s.split(':') match {
      case Array("github", repo) =>
        GitHub.parseProject(repo)
          .mapErr(_ => ManifestError.RepositoryFormatError(p, s))
      case _ => Err(ManifestError.RepositoryFormatError(p, s))
    }
  }

  /**
    * Converts a TomlTable to a list of Dependencies. This requires
    * the value of each entry is a String which can be converted to a
    * semantic version. `flixDep` decides whether the Dependency is a Flix
    * or MavenDependency and `prodDep` decides whether it is for production
    * or development. `jarDep` decides whether it is an external jar. This
    * overrides `flixDep` and `prodDep`.
    * Returns an error if anything is not as expected.
    */
  private def collectDependencies(optDeps: Option[TomlTable], flixDep: Boolean, jarDep: Boolean, p: Path): Result[List[Dependency], ManifestError] = {
    optDeps match {
      case None => Ok(List.empty)
      case Some(deps) =>
        val depsEntries = deps.entrySet().asScala
        traverse(depsEntries)(entry => {
          val depKey = entry.getKey
          val depValue = entry.getValue
          if (jarDep) {
            createJarDep(depKey, depValue, p)
          } else if (flixDep) {
            // Key needs this format to do typed look-ups.
            val dottedDepKey = s"\"$depKey\""
            createFlixDep(deps, dottedDepKey, p)
          } else {
            createMavenDep(depKey, depValue, p)
          }
        })
    }
  }

  /**
    * Creates a MavenDependency.
    * Group id and artifact id are given by `depName`.
    * The version is given by `depVer`.
    * `p` is for reporting errors.
    */
  private def createMavenDep(depName: String, depVer: AnyRef, p: Path): Result[MavenDependency, ManifestError] = {
    for (
      groupId <- getGroupId(depName, p);
      artifactId <- getArtifactId(depName, p);
      version <- getMavenVersion(depVer, p)
    ) yield {
      Dependency.MavenDependency(groupId, artifactId, version)
    }
  }

  /**
    * Retrieves the group id for a Maven dependency
    * and returns an error if it is not formatted correctly
    * or has characters that are not allowed.
    */
  private def getGroupId(depName: String, p: Path): Result[String, ManifestError] = {
    depName.split(':') match {
      case Array(groupId, _) => checkNameCharacters(groupId, p)
      case _ => Err(ManifestError.MavenDependencyFormatError(p, depName))
    }
  }

  /**
    * Retrieves the artifact id for a Maven dependency
    * and returns an error if it is not formatted correctly
    * or has characters that are not allowed.
    */
  private def getArtifactId(depName: String, p: Path): Result[String, ManifestError] = {
    depName.split(':') match {
      case Array(_, artifactId) => checkNameCharacters(artifactId, p)
      case _ => Err(ManifestError.MavenDependencyFormatError(p, depName))
    }
  }

  /**
    * A Maven version number is an uninterpreted tag. Maven (the repository) does not
    * enforce a format for version numbers so we must be liberal about what we accept.
    */
  private def getMavenVersion(depVer: AnyRef, p: Path): Result[String, ManifestError] = {
    try {
      val version = depVer.asInstanceOf[String]
      Ok(version)
    } catch {
      case e: ClassCastException =>
        Err(ManifestError.DependencyFormatError(p, e.getMessage))
    }
  }

  /**
    * Create a [[FlixDependency]].
    *
    * @param deps   [[TomlTable]] of declared Flix dependencies.
    * @param depKey Repository address of the package.
    * @param p      [[Path]] of the project Toml file.
    * @return [[Result]] of the [[FlixDependency]] if succesful, otherwise a [[ManifestError]]
    */
  private def createFlixDep(deps: TomlTable, depKey: String, p: Path): Result[FlixDependency, ManifestError] = {
    // Regex for extracting repository, username, and project name.
    // (.+) is a capturing group, where . matches any character.
    val validPkg = s"^\"(.+):(.+)/(.+)\"$$".r
    depKey match {
      case validPkg(repoStr, username, projectName) =>
        val repo = Repository.mkRepository(repoStr) match {
          case Some(r) => r
          case None => return Err(ManifestError.UnsupportedRepository(p, repoStr))
        }

        // Ensure the username is valid.
        if (!PackageId.isValidName(username))
          return Err(ManifestError.IllegalName(p, depKey))

        // Ensure the project name is valid.
        if (!PackageId.isValidName(projectName))
          return Err(ManifestError.IllegalName(p, depKey))

        val id = PackageId(repo, username, projectName)

        // If the dependency maps to a string, it declares only a version and has no mount.
        if (deps.isString(depKey)) {
          for (
            ver <- getFlixVersion(deps, depKey, p)
          ) yield FlixDependency(id, ver, None, SecurityContext.Default, DependencyStyle.VersionOnly)

          // If the dependency maps to a table, get the version, security, and mount.
        } else if (deps.isTable(depKey)) {
          val depTbl = deps.getTable(depKey)
          val verKey = "version"
          val mountKey = "mount"
          val securityKey = "security"

          for (
            _ <- checkDependencyKeys(depTbl, depKey, Set(verKey, mountKey, securityKey), p);
            ver <- getFlixVersion(depTbl, verKey, p);
            mount <- getMount(depTbl, mountKey, depKey, p);
            security <- getSecurity(depTbl, securityKey, p)
          ) yield FlixDependency(id, ver, mount, security, DependencyStyle.Table)
        } else {
          Err(ManifestError.VersionTypeError(p, depKey, deps.get(depKey)))
        }
      case _ => Err(ManifestError.FlixDependencyFormatError(p, depKey))
    }
  }

  /**
    * Attempt to retrieve a [[SemVer]] at `depKey` from the table `deps`.
    */
  private def getFlixVersion(deps: TomlTable, depKey: String, p: Path): Result[SemVer, ManifestError] = {
    // Ensure the version is a String.
    if (!deps.isString(depKey)) {
      Err(ManifestError.VersionTypeError(p, depKey, deps.get(depKey)))
    } else {
      val depVer = deps.getString(depKey)
      SemVer.ofString(depVer) match {
        case Some(v) => Ok(v)
        case None => Err(ManifestError.FlixVersionFormatError(p, depKey, depVer))
      }
    }
  }

  /**
    * Retrieve the given security context from a [[TomlTable]] `depTbl` at `key`.
    */
  private def getSecurity(depTbl: TomlTable, key: String, path: Path): Result[SecurityContext, ManifestError] = {
    // Ensure the security value is a string.
    if (!depTbl.contains(key)) {
      return Ok(SecurityContext.Default)
    }
    if (!depTbl.isString(key)) {
      val perms = depTbl.get(key)
      Err(ManifestError.FlixDependencySecurityType(path, key, perms))
    } else {
      val value = depTbl.getString(key)
      SecurityContext.fromString(value) match {
        case Some(sctx) => Ok(sctx)
        case None => Err(ManifestError.FlixUnknownSecurityValue(path, key, value))
      }
    }
  }

  /**
    * Returns an error if the dependency table `depTbl` of the dependency `depKey` has a key not in `allowed`.
    */
  private def checkDependencyKeys(depTbl: TomlTable, depKey: String, allowed: Set[String], p: Path): Result[Unit, ManifestError] = {
    val illegalKeys = depTbl.keySet().asScala.toSet.diff(allowed)
    illegalKeys.toList.sorted match {
      case Nil => Ok(())
      case key :: _ => Err(ManifestError.IllegalDependencyKeyFound(p, depKey, key))
    }
  }

  /**
    * Retrieves the mount of the dependency `depKey` from the table `depTbl` at `key`.
    *
    * A dependency that declares no mount has none: its modules are reachable unqualified.
    */
  private def getMount(depTbl: TomlTable, key: String, depKey: String, p: Path): Result[Option[Mountpoint], ManifestError] = {
    if (!depTbl.contains(key)) {
      Ok(None)
    } else if (!depTbl.isString(key)) {
      Err(ManifestError.FlixDependencyMountType(p, depKey, depTbl.get(key)))
    } else {
      val mount = depTbl.getString(key)
      Mountpoint.mkMountpoint(mount) match {
        case Some(m) => Ok(Some(m))
        case None => Err(ManifestError.FlixDependencyIllegalMount(p, depKey, mount))
      }
    }
  }

  /**
    * Returns an error if two Flix dependencies in `deps` share a mount.
    */
  private def checkDuplicateMounts(deps: List[Dependency], p: Path): Result[Unit, ManifestError] = {
    val mountedDeps = deps.collect { case dep: FlixDependency if dep.mount.isDefined => (dep.mount.get, dep) }
    val seen = mutable.Map.empty[Mountpoint, FlixDependency]
    for ((mount, dep) <- mountedDeps) {
      seen.get(mount) match {
        case Some(prev) => return Err(ManifestError.FlixDependencyDuplicateMount(p, mount, prev.id, dep.id))
        case None => seen += mount -> dep
      }
    }
    Ok(())
  }

  /**
    * Creates a JarDependency.
    * URL and website is given by `depUrl`.
    * The file name is given by `depName`.
    * `p` is for reporting errors.
    */
  private def createJarDep(depName: String, depUrl: AnyRef, p: Path): Result[JarDependency, ManifestError] = {
    for (
      url <- getUrl(depUrl, p);
      fileName <- getFileName(depName, p)
    ) yield {
      Dependency.JarDependency(url, fileName)
    }
  }

  /**
    * Converts `depUrl` to a String and retrieves the URL for a jar dependency.
    * Returns an error if it is not formatted correctly.
    */
  private def getUrl(depUrl: AnyRef, p: Path): Result[String, ManifestError] = {
    try {
      val url = depUrl.asInstanceOf[String]
      try {
        if (url.startsWith("url:")) {
          val removeTag = url.substring(4)
          Ok(new URI(removeTag).toURL.toString)
        } else {
          Err(ManifestError.JarUrlFormatError(p, url))
        }
      } catch {
        case e: IllegalArgumentException =>
          Err(ManifestError.WrongUrlFormat(p, url, e.getMessage))
      }
    } catch {
      case e: ClassCastException =>
        Err(ManifestError.JarUrlTypeError(p, e.getMessage))

    }
  }

  /**
    * Retrieves the file name for a jar dependency
    * and returns an error if it is not formatted correctly
    * or has characters that are not allowed.
    */
  private def getFileName(depName: String, p: Path): Result[String, ManifestError] = {
    val split = depName.split('.')
    if (split.length >= 2) {
      val extension = split.apply(split.length - 1)
      if (extension == "jar") {
        checkNameCharacters(depName, p)
      } else {
        Err(ManifestError.JarUrlExtensionError(p, depName, extension))
      }
    } else {
      Err(ManifestError.JarUrlFileNameError(p, depName))
    }
  }

  /**
    * Checks that a package name does not include any illegal characters.
    */
  private def checkNameCharacters(name: String, p: Path): Result[String, ManifestError] = {
    if (name.matches("^[a-zA-Z0-9.:/_-]+$"))
      Ok(name)
    else
      Err(ManifestError.IllegalName(p, name))
  }

}
