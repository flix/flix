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

import ca.uwaterloo.flix.tools.pkg.Dependency.{FlixDependency, JarDependency, MavenDependency}
import ca.uwaterloo.flix.util.Result
import ca.uwaterloo.flix.util.Result.{Err, Ok, ToOk, traverse}
import org.tomlj.{Toml, TomlArray, TomlInvalidTypeException, TomlParseResult, TomlTable}

import java.io.{IOException, StringReader}
import java.net.{MalformedURLException, URI, URL}
import java.nio.file.Path
import scala.collection.mutable
import scala.jdk.CollectionConverters.SetHasAsScala

object ManifestParser {

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

      name <- getRequiredStringProperty("package.name", parser, p);

      description <- getRequiredStringProperty("package.description", parser, p);

      version <- getRequiredStringProperty("package.version", parser, p);
      versionSemVer <- toFlixVer(version, p);

      flix <- getRequiredStringProperty("package.flix", parser, p);
      flixSemVer <- toFlixVer(flix, p);

      license <- getOptionalStringProperty("package.license", parser, p);

      authors <- getRequiredArrayProperty("package.authors", parser, p);
      authorsList <- convertTomlArrayToStringList(authors, p);

      deps <- getOptionalTableProperty("dependencies", parser, p);
      depsList <- collectDependencies(deps, flixDep = true, prodDep = true, jarDep = false, p);

      devDeps <- getOptionalTableProperty("dev-dependencies", parser, p);
      devDepsList <- collectDependencies(devDeps, flixDep = true, prodDep = false, jarDep = false, p);

      mvnDeps <- getOptionalTableProperty("mvn-dependencies", parser, p);
      mvnDepsList <- collectDependencies(mvnDeps, flixDep = false, prodDep = true, jarDep = false, p);

      devMvnDeps <- getOptionalTableProperty("dev-mvn-dependencies", parser, p);
      devMvnDepsList <- collectDependencies(devMvnDeps, flixDep = false, prodDep = false, jarDep = false, p);

      jarDeps <- getOptionalTableProperty("jar-dependencies", parser, p);
      jarDepsList <- collectDependencies(jarDeps, flixDep = false, prodDep = false, jarDep = true, p)

    ) yield Manifest(name, description, versionSemVer, flixSemVer, license, authorsList, depsList ++ devDepsList ++ mvnDepsList ++ devMvnDepsList ++ jarDepsList)
  }

  private def checkKeys(parser: TomlParseResult, p: Path): Result[Unit, ManifestError] = {
    val keySet: Set[String] = parser.keySet().asScala.toSet
    val allowedKeys = Set("package", "dependencies", "dev-dependencies", "mvn-dependencies", "dev-mvn-dependencies", "jar-dependencies")
    val illegalKeys = keySet.diff(allowedKeys)

    if(illegalKeys.nonEmpty) {
      return Err(ManifestError.IllegalTableFound(p, illegalKeys.head))
    }

    val dottedKeys = parser.dottedKeySet().asScala.toSet
    val packageKeys = dottedKeys.filter(s => s.startsWith("package."))
    val allowedPackageKeys = Set("package.name", "package.description", "package.version", "package.flix", "package.authors", "package.license")
    val illegalPackageKeys = packageKeys.diff(allowedPackageKeys)
    if (illegalPackageKeys.nonEmpty) {
      return Err(ManifestError.IllegalPackageKeyFound(p, illegalPackageKeys.head))
    }

    ().toOk
  }

  /**
    * Parses a String which should be at `propString`
    * and returns the String or an error if the result
    * cannot be found.
    */
  private def getRequiredStringProperty(propString: String, parser: TomlParseResult, p: Path): Result[String, ManifestError] = {
    try {
      val prop = parser.getString(propString)
      if (prop == null) {
        return Err(ManifestError.MissingRequiredProperty(p, propString, None))
      }
      Ok(prop)
    } catch {
      case e: IllegalArgumentException => Err(ManifestError.MissingRequiredProperty(p, propString, Some(e.getMessage)))
      case e: TomlInvalidTypeException => Err(ManifestError.RequiredPropertyHasWrongType(p, propString, "String", e.getMessage))
    }
  }

  /**
    * Parses a String which might be at `propString`
    * and returns the String as an Option.
    */
  private def getOptionalStringProperty(propString: String, parser: TomlParseResult, p: Path): Result[Option[String], ManifestError] = {
    try {
      val prop = parser.getString(propString)
      Ok(Option(prop))
    } catch {
      case _: IllegalArgumentException => Ok(None)
      case e: TomlInvalidTypeException => Err(ManifestError.RequiredPropertyHasWrongType(p, propString, "String", e.getMessage))
    }
  }

  /**
    * Parses an Array which should be at `propString`
    * and returns the Array or an error if the result
    * cannot be found.
    */
  private def getRequiredArrayProperty(propString: String, parser: TomlParseResult, p: Path): Result[TomlArray, ManifestError] = {
    try {
      val array = parser.getArray(propString)
      if (array == null) {
        return Err(ManifestError.MissingRequiredProperty(p, propString, None))
      }
      Ok(array)
    } catch {
      case e: IllegalArgumentException => Err(ManifestError.MissingRequiredProperty(p, propString, Some(e.getMessage)))
      case e: TomlInvalidTypeException => Err(ManifestError.RequiredPropertyHasWrongType(p, propString, "Array", e.getMessage))
    }
  }

  /**
    * Parses a Table which should be at `propString`
    * and returns the Table or an error if the result
    * cannot be found.
    */
  private def getOptionalTableProperty(propString: String, parser: TomlParseResult, p: Path): Result[Option[TomlTable], ManifestError] = {
    try {
      val table = parser.getTable(propString)
      Ok(Option(table))
    } catch {
      case _: IllegalArgumentException => Ok(None)
      case e: TomlInvalidTypeException => Err(ManifestError.RequiredPropertyHasWrongType(p, propString, "Table", e.getMessage))
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
          Ok(SemVer(major.toInt, minor.toInt, Some(patch.toInt), None, None))
        case _ => Err(ManifestError.FlixVersionHasWrongLength(p, s))
      }
    } catch {
      case e: NumberFormatException => Err(ManifestError.VersionNumberWrong(p, s, e.getMessage))
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
  private def collectDependencies(deps: Option[TomlTable], flixDep: Boolean, prodDep: Boolean, jarDep: Boolean, p: Path): Result[List[Dependency], ManifestError] = {
    deps match {
      case None => Ok(List.empty)
      case Some(deps) =>
        val depsEntries = deps.entrySet().asScala
        traverse(depsEntries)(entry => {
          val depKey = entry.getKey
          val depValue = entry.getValue
          if (jarDep) {
            createJarDep(depKey, depValue, p)
          } else if (flixDep) {
            createFlixDep(depKey, depValue, prodDep, p)
          } else {
            createMavenDep(depKey, depValue, prodDep, p)
          }
        })
    }
  }

  /**
    * Retrieves the repository for a Flix dependency
    * and returns an error if it is not formatted correctly
    * or has characters that are not allowed.
    */
  private def getRepository(depName: String, p: Path): Result[Repository, ManifestError] = {
    depName.split(':') match {
      case Array(repo, _) =>
        if (repo == "github") Ok(Repository.GitHub)
        else Err(ManifestError.UnsupportedRepository(p, repo))
      case _ => Err(ManifestError.FlixDependencyFormatError(p, depName))
    }
  }

  /**
    * Retrieves the username for a Flix dependency
    * and returns an error if it is not formatted correctly
    * or has characters that are not allowed.
    */
  private def getUsername(depName: String, p: Path): Result[String, ManifestError] = {
    depName.split(':') match {
      case Array(_, rest) => rest.split('/') match {
        case Array(username, _) => checkNameCharacters(username, p)
        case _ => Err(ManifestError.FlixDependencyFormatError(p, depName))
      }
      case _ => Err(ManifestError.FlixDependencyFormatError(p, depName))
    }
  }

  /**
    * Retrieves the project name for a Flix dependency
    * and returns an error if it is not formatted correctly
    * or has characters that are not allowed.
    */
  private def getProjectName(depName: String, p: Path): Result[String, ManifestError] = {
    depName.split('/') match {
      case Array(_, projectName) => checkNameCharacters(projectName, p)
      case _ => Err(ManifestError.FlixDependencyFormatError(p, depName))
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
    * Converts `depUrl` to a String and retrieves the URL for a jar dependency.
    * Returns an error if it is not formatted correctly.
    */
  private def getUrl(depUrl: AnyRef, p: Path): Result[URL, ManifestError] = {
    try {
      val url = depUrl.asInstanceOf[String]
      try {
        if (url.startsWith("url:")) {
          val removeTag = url.substring(4)
          Ok(new URI(removeTag).toURL)
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
    if(split.length >= 2) {
      val extension = split.apply(split.length-1)
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
    * Converts `depVer` to a String and then to a semantic version
    * and returns an error if `depVer` is not of the correct format.
    */
  def getFlixVersion(depVer: AnyRef, p: Path): Result[SemVer, ManifestError] = {
    try {
      toFlixVer(depVer.asInstanceOf[String], p)
    } catch {
      case e: ClassCastException =>
        Err(ManifestError.DependencyFormatError(p, e.getMessage))
    }
  }

  /**
    * Converts `depVer` to a String and then to a semantic version
    * and returns an error if `depVer` is not of the correct format.
    * Allowed formats are "x.x", "x.x.x", "x.x.x.x" and "x.x.x-x"
    */
  def getMavenVersion(depVer: AnyRef, p: Path): Result[SemVer, ManifestError] = {
    try {
      val version = depVer.asInstanceOf[String]
      version.split('.') match {
        case Array(major, minor) => Ok(SemVer(major.toInt, minor.toInt, None, None, None))
        case Array(major, minor, patch) =>
          patch.split('-') match {
            case Array(patch) => Ok(SemVer(major.toInt, minor.toInt, Some(patch.toInt), None, None))
            case Array(patch, build) => Ok(SemVer(major.toInt, minor.toInt, Some(patch.toInt), None, Some(build)))
          }
        case Array(major, minor, patch, build) => Ok(SemVer(major.toInt, minor.toInt, Some(patch.toInt), Some(build.toInt), None))
        case _ => Err(ManifestError.MavenVersionHasWrongLength(p, version))
      }
    } catch {
      case e: ClassCastException =>
        Err(ManifestError.DependencyFormatError(p, e.getMessage))
      case e: NumberFormatException =>
        Err(ManifestError.VersionNumberWrong(p, depVer.asInstanceOf[String], e.getMessage))
    }
  }

  /**
    * Creates a MavenDependency.
    * Group id and artifact id are given by `depName`.
    * The version is given by `depVer`.
    * `prodDep` decides whether it is a production or development dependency.
    * `p` is for reporting errors.
    */
  private def createMavenDep(depName: String, depVer: AnyRef, prodDep: Boolean, p: Path): Result[MavenDependency, ManifestError] = {
    for(
      groupId <- getGroupId(depName, p);
      artifactId <- getArtifactId(depName, p);
      version <- getMavenVersion(depVer, p)
    ) yield {
      if(prodDep) {
        Dependency.MavenDependency(groupId, artifactId, version, DependencyKind.Production)
      } else {
        Dependency.MavenDependency(groupId, artifactId, version, DependencyKind.Development)
      }
    }
  }

  /**
    * Creates a FlixDependency.
    * Repository, username and project name are given by `depName`.
    * The version is given by `depVer`.
    * `prodDep` decides whether it is a production or development dependency.
    * `p` is for reporting errors.
    */
  private def createFlixDep(depName: String, depVer: AnyRef, prodDep: Boolean, p: Path): Result[FlixDependency, ManifestError] = {
    for (
      repository <- getRepository(depName, p);
      username <- getUsername(depName, p);
      projectName <- getProjectName(depName, p);
      version <- getFlixVersion(depVer, p)
    ) yield {
      if (prodDep) {
        Dependency.FlixDependency(repository, username, projectName, version, DependencyKind.Production)
      } else {
        Dependency.FlixDependency(repository, username, projectName, version, DependencyKind.Development)
      }
    }
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
    * Converts a TomlArray to a list of Strings. Returns
    * an error if anything in the array is not a String.
    */
  private def convertTomlArrayToStringList(array: TomlArray, p: Path): Result[List[String], ManifestError] = {
    val stringSet = mutable.Set.empty[String]
    for (i <- 0 until array.size()) {
      try {
        val s = array.getString(i)
        stringSet.add(s)
      } catch {
        case e: TomlInvalidTypeException =>
          return Err(ManifestError.AuthorNameError(p, e.getMessage))
      }
    }
    Ok(stringSet.toList)
  }

  /**
    * Checks that a package name does not include any illegal characters.
    */
  private def checkNameCharacters(name: String, p: Path): Result[String, ManifestError] = {
    if(name.matches("^[a-zA-Z0-9.:/_-]+$"))
      Ok(name)
    else
      Err(ManifestError.IllegalName(p, name))
  }

}
