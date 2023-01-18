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
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import org.tomlj.{Toml, TomlArray, TomlInvalidTypeException, TomlParseResult, TomlTable}

import java.io.{IOException, PrintStream}
import java.nio.file.{InvalidPathException, Path, Paths}
import scala.collection.mutable

object ManifestParser {

  def parse(p: Path)(implicit out: PrintStream): Result[Manifest, ManifestError] = {
    val parser = try {
      val parser = Toml.parse(p)
      parser
    } catch {
      case _: IOException => return Err(ManifestError.IOError(p))
    }

    val errors = parser.errors
    if(errors.size() > 0) {
      var errorString = ""
      errors.forEach(error => errorString = errorString + error.toString + ", ")
      return Err(ManifestError.ManifestParseError(p, errorString))
    }

    val name = getRequiredStringProperty("package.name", parser, p) match {
      case Ok(s) => s
      case Err(e) => return Err(e)
    }

    val description = getRequiredStringProperty("package.description", parser, p) match {
      case Ok(s) => s
      case Err(e) => return Err(e)
    }

    val version = getRequiredStringProperty("package.version", parser, p) match {
      case Ok(s) => s
      case Err(e) => return Err(e)
    }
    val semanticVersion = toSemVer(version, p) match {
      case Ok(v) => v
      case Err(e) => return Err(e)
    }

    val flix = getRequiredStringProperty("package.flix", parser, p) match {
      case Ok(s) => s
      case Err(e) => return Err(e)
    }
    val semanticFlixVersion = toSemVer(flix, p) match {
      case Ok(v) => v
      case Err(e) => return Err(e)
    }

    val licenseOpt: Option[String] = try {
      val license = parser.getString("package.license")
      Option(license)
    } catch {
      case _: IllegalArgumentException => None
      case _: TomlInvalidTypeException => return Err(ManifestError.RequiredPropertyHasWrongType(p, "'package.license' should have type String"))
    }

    val authors = try {
      val authors = parser.getArray("package.authors")
      if (authors == null) {
        return Err(ManifestError.MissingRequiredProperty(p, "'package.authors' is missing"))
      }
      authors
    } catch {
      case _: IllegalArgumentException => return Err(ManifestError.MissingRequiredProperty(p, "'package.authors' is missing"))
      case _: TomlInvalidTypeException => return Err(ManifestError.RequiredPropertyHasWrongType(p, "'package.authors' should have type Array"))
    }
    val authorsList = convertTomlArrayToStringList(authors, p) match {
      case Ok(l) => l
      case Err(e) => return Err(e)
    }

    val flixDependencies = getRequiredTableProperty("dependencies", parser, p) match {
      case Ok(t) => t
      case Err(e) => return Err(e)
    }
    val flixDepsList = collectDependencies(flixDependencies, flixDep = true, prodDep = true, p) match {
      case Ok(l) => l
      case Err(e) => return Err(e)
    }

    val devFlixDependencies = getRequiredTableProperty("dev-dependencies", parser, p) match {
      case Ok(t) => t
      case Err(e) => return Err(e)
    }
    val devFlixDepsList = collectDependencies(devFlixDependencies, flixDep = true, prodDep = false, p) match {
      case Ok(l) => l
      case Err(e) => return Err(e)
    }

    val mvnDependencies = getRequiredTableProperty("mvn-dependencies", parser, p) match {
      case Ok(t) => t
      case Err(e) => return Err(e)
    }
    val mvnDepsList = collectDependencies(mvnDependencies, flixDep = false, prodDep = true, p) match {
      case Ok(l) => l
      case Err(e) => return Err(e)
    }

    val devMvnDependencies = getRequiredTableProperty("dev-mvn-dependencies", parser, p) match {
      case Ok(t) => t
      case Err(e) => return Err(e)
    }
    val devMvnDepsList = collectDependencies(devMvnDependencies, flixDep = false, prodDep = false, p) match {
      case Ok(l) => l
      case Err(e) => return Err(e)
    }

    val dependencyList = flixDepsList ++ devFlixDepsList ++ mvnDepsList ++ devMvnDepsList

    Ok(Manifest(name, description, semanticVersion, semanticFlixVersion, licenseOpt, authorsList, dependencyList))
  }

  def parse(s: String)(implicit out: PrintStream): Result[Manifest, ManifestError] = {
    try {
      parse(Paths.get(s))
    } catch {
      case _: InvalidPathException => Err(ManifestError.ManifestNotFoundAt(s))
    }
  }

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

  private def getRequiredTableProperty(propString: String, parser: TomlParseResult, p: Path): Result[TomlTable, ManifestError] = {
    try {
      val flixDependencies = parser.getTable(propString)
      if (flixDependencies == null) {
        return Err(ManifestError.MissingRequiredProperty(p, s"'$propString' is missing"))
      }
      Ok(flixDependencies)
    } catch {
      case _: IllegalArgumentException => Err(ManifestError.MissingRequiredProperty(p, s"'$propString' is missing"))
      case _: TomlInvalidTypeException => Err(ManifestError.RequiredPropertyHasWrongType(p, s"'$propString' should have type Table"))
    }
  }

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

  private def collectDependencies(deps: TomlTable, flixDep: Boolean, prodDep: Boolean, p: Path): Result[List[Dependency], ManifestError] = {
    val depsEntries = deps.entrySet()
    val depsSet = mutable.Set.empty[Dependency]
    depsEntries.forEach(entry => {
      val depName = entry.getKey
      val depVer = entry.getValue
      try {
        toSemVer(depVer.asInstanceOf[String], p) match {
          case Ok(version) => if (flixDep) {
            if (prodDep) {
              depsSet.add(Dependency.FlixDependency(depName, version, DependencyKind.Production))
            } else {
              depsSet.add(Dependency.FlixDependency(depName, version, DependencyKind.Development))
            }
          } else {
            val depNameSplit = depName.split(':')
            if (depNameSplit.length == 2) {
              if (prodDep) {
                depsSet.add(Dependency.MavenDependency(depNameSplit.apply(0), depNameSplit.apply(1), version, DependencyKind.Production))
              } else {
                depsSet.add(Dependency.MavenDependency(depNameSplit.apply(0), depNameSplit.apply(1), version, DependencyKind.Development))
              }
            } else {
              return Err(ManifestError.MavenDependencyFormatError(p, "A Maven dependency should be formatted like so: 'group:artifact'"))
            }
          }
          case Err(e) => return Err(e)
        }
      } catch {
        case _: ClassCastException => return Err(ManifestError.DependencyFormatError(p, "A value in a dependency table should be of type String"))
      }
    })
    Ok(depsSet.toList)
  }

  private def convertTomlArrayToStringList(array: TomlArray, p: Path): Result[List[String], ManifestError] = {
    val stringSet = mutable.Set.empty[String]
    for(i <- 0 until array.size()) {
      try {
        val s = array.getString(i)
        stringSet.add(s)
      } catch {
        case _: TomlInvalidTypeException => return Err(ManifestError.AuthorNameError(p, "All author names should be of type String"))
      }
    }
    Ok(stringSet.toList)
  }

}
