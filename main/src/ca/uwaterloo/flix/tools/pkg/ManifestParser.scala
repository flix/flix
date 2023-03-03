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

import ca.uwaterloo.flix.util.Result
import ca.uwaterloo.flix.util.Result.{Err, Ok, ToOk}
import org.tomlj.{Toml, TomlArray, TomlInvalidTypeException, TomlParseResult, TomlTable}

import java.io.{IOException, StringReader}
import java.nio.file.Path
import scala.collection.mutable

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
      case _: IOException => return Err(ManifestError.IOError(p))
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
      case _: IOException => return Err(ManifestError.IOError(p))
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
      name <- getRequiredStringProperty("package.name", parser, p);

      description <- getRequiredStringProperty("package.description", parser, p);

      version <- getRequiredStringProperty("package.version", parser, p);
      versionSemVer <- toSemVer(version, p);

      flix <- getRequiredStringProperty("package.flix", parser, p);
      flixSemVer <- toSemVer(flix, p);

      license <- getOptionalStringProperty("package.license", parser, p);

      authors <- getRequiredArrayProperty("package.authors", parser, p);
      authorsList <- convertTomlArrayToStringList(authors, p);

      deps <- getOptionalTableProperty("dependencies", parser, p);
      depsList <- collectDependencies(deps, flixDep = true, prodDep = true, p);

      devDeps <- getOptionalTableProperty("dev-dependencies", parser, p);
      devDepsList <- collectDependencies(devDeps, flixDep = true, prodDep = false, p);

      mvnDeps <- getOptionalTableProperty("mvn-dependencies", parser, p);
      mvnDepsList <- collectDependencies(mvnDeps, flixDep = false, prodDep = true, p);

      devMvnDeps <- getOptionalTableProperty("dev-mvn-dependencies", parser, p);
      devMvnDepsList <- collectDependencies(devMvnDeps, flixDep = false, prodDep = false, p)

    ) yield Manifest(name, description, versionSemVer, flixSemVer, license, authorsList, depsList ++ devDepsList ++ mvnDepsList ++ devMvnDepsList)
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
        return Err(ManifestError.MissingRequiredProperty(p, s"'$propString' is missing"))
      }
      Ok(prop)
    } catch {
      case _: IllegalArgumentException => Err(ManifestError.MissingRequiredProperty(p, s"'$propString' is missing"))
      case _: TomlInvalidTypeException => Err(ManifestError.RequiredPropertyHasWrongType(p, s"'$propString' should have type String"))
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
      case _: TomlInvalidTypeException => Err(ManifestError.RequiredPropertyHasWrongType(p, s"'$propString' should have type String"))
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
        return Err(ManifestError.MissingRequiredProperty(p, s"'$propString' is missing"))
      }
      Ok(array)
    } catch {
      case _: IllegalArgumentException => Err(ManifestError.MissingRequiredProperty(p, s"'$propString' is missing"))
      case _: TomlInvalidTypeException => Err(ManifestError.RequiredPropertyHasWrongType(p, s"'$propString' should have type Array"))
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
      case _: TomlInvalidTypeException => Err(ManifestError.RequiredPropertyHasWrongType(p, s"'$propString' should have type Table"))
    }
  }

  /**
    * Converts a String `s` to a semantic version and returns
    * an error if the String is not of the correct format.
    */
  private def toSemVer(s: String, p: Path): Result[SemVer, ManifestError] = {
    val splitVersion = s.split('.')
    if (splitVersion.length == 3) {
      try {
        val major = splitVersion.apply(0).toInt
        val minor = splitVersion.apply(1).toInt
        val patch = splitVersion.apply(2).toInt
        Ok(SemVer(major, minor, patch))
      } catch {
        case _: NumberFormatException => Err(ManifestError.VersionNumberWrong(p, "Could not parse version as three numbers"))
      }
    } else {
      Err(ManifestError.VersionHasWrongLength(p, "A version should be formatted like so: 'x.x.x'"))
    }
  }

  /**
    * Converts a TomlTable to a list of Dependencies. This requires
    * the value of each entry is a String which can be converted to a
    * semantic version. `flixDep` decides whether the Dependency is a Flix
    * or MavenDependency and `prodDep` decides whether it is for production
    * or development. Returns an error if anything is not as expected.
    */
  private def collectDependencies(deps: Option[TomlTable], flixDep: Boolean, prodDep: Boolean, p: Path): Result[List[Dependency], ManifestError] = {
    deps match {
      case None => Ok(List.empty)
      case Some(deps) =>
        val depsEntries = deps.entrySet()
        val depsSet = mutable.Set.empty[Dependency]
        depsEntries.forEach(entry => {
          val depName = entry.getKey
          val depVer = entry.getValue
          try {
            toSemVer(depVer.asInstanceOf[String], p) match {
              case Ok(version) => if (flixDep) {
                depName.split(':') match {
                  case Array(repo, rest) =>
                    rest.split('/') match {
                      case Array(username, projectName) =>
                        if (repo == "github") {
                          checkNameCharacters(username, p) match {
                            case Ok(_) => //the name is fine, do nothing
                            case Err(e) => return Err(e)
                          }
                          checkNameCharacters(projectName, p) match {
                            case Ok(_) => //the name is fine, do nothing
                            case Err(e) => return Err(e)
                          }
                          if (prodDep) {
                            depsSet.add(Dependency.FlixDependency(Repository.GitHub, username, projectName, version, DependencyKind.Production))
                          } else {
                            depsSet.add(Dependency.FlixDependency(Repository.GitHub, username, projectName, version, DependencyKind.Development))
                          }
                        } else {
                          return Err(ManifestError.UnsupportedRepository(p, s"The repository $repo is not supported"))
                        }
                      case _ =>
                        return Err(ManifestError.FlixDependencyFormatError(p, "A Flix dependency should be formatted like so: 'host:username/projectname'"))
                    }
                  case _ =>
                    return Err(ManifestError.FlixDependencyFormatError(p, "A Flix dependency should be formatted like so: 'host:username/projectname'"))
                }
              } else {
                depName.split(':') match {
                  case Array(groupId, artifactId) =>
                    checkNameCharacters(groupId, p) match {
                      case Ok(_) => //the name is fine, do nothing
                      case Err(e) => return Err(e)
                    }
                    checkNameCharacters(artifactId, p) match {
                      case Ok(_) => //the name is fine, do nothing
                      case Err(e) => return Err(e)
                    }
                    if (prodDep) {
                      depsSet.add(Dependency.MavenDependency(groupId, artifactId, version, DependencyKind.Production))
                    } else {
                      depsSet.add(Dependency.MavenDependency(groupId, artifactId, version, DependencyKind.Development))
                    }
                  case _ =>
                    return Err(ManifestError.MavenDependencyFormatError(p, "A Maven dependency should be formatted like so: 'group:artifact'"))
                }
              }
              case Err(e) => return Err(e)
            }
          } catch {
            case _: ClassCastException =>
              return Err(ManifestError.DependencyFormatError(p, "A value in a dependency table should be of type String"))
          }
        })
        Ok(depsSet.toList)
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
        case _: TomlInvalidTypeException =>
          return Err(ManifestError.AuthorNameError(p, "All author names should be of type String"))
      }
    }
    Ok(stringSet.toList)
  }

  /**
    * Checks that a package name does not include any illegal characters.
    */
  private def checkNameCharacters(name: String, p: Path): Result[Unit, ManifestError] = {
    if(name.matches("^[a-zA-Z0-9.-]+$"))
      ().toOk
    else
      Err(ManifestError.IllegalName(p, s"A dependency name cannot include any special characters: $name"))
  }

}
