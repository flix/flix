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
import org.tomlj.{Toml, TomlArray, TomlInvalidTypeException, TomlTable}

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
      var error_string = ""
      errors.forEach(error => error_string = error_string + error.toString + ", ")
      return Err(ManifestError.ManifestParseError(p, error_string))
    }

    val name = try {
      val name = parser.getString("package.name")
      if(name == null) {
        return Err(ManifestError.RequiredPropertyIsNull(p, "'package.name' is null"))
      }
      name
    } catch {
      case _: IllegalArgumentException => return Err(ManifestError.MissingRequiredProperty(p, "'package.name' is missing"))
      case _: TomlInvalidTypeException => return Err(ManifestError.RequiredPropertyHasWrongType(p, s"'package.name' should have type String"))
    }

    val description = try {
      val description = parser.getString("package.description")
      if (description == null) {
        return Err(ManifestError.RequiredPropertyIsNull(p, "'package.description' is null"))
      }
      description
    } catch {
      case _: IllegalArgumentException => return Err(ManifestError.MissingRequiredProperty(p, "'package.description' is missing"))
      case _: TomlInvalidTypeException => return Err(ManifestError.RequiredPropertyHasWrongType(p, s"'package.description' should have type String"))
    }

    val version = try {
      val version = parser.getString("package.version")
      if (version == null) {
        return Err(ManifestError.RequiredPropertyIsNull(p, "'package.version' is null"))
      }
      version
    } catch {
      case _: IllegalArgumentException => return Err(ManifestError.MissingRequiredProperty(p, "'package.version' is missing"))
      case _: TomlInvalidTypeException => return Err(ManifestError.RequiredPropertyHasWrongType(p, s"'package.version' should have type String"))
    }
    val semanticVersion = toSemVer(version, p) match {
      case Ok(v) => v
      case Err(e) => return Err(e)
    }

    val flix = try {
      val flix = parser.getString("package.flix")
      if (flix == null) {
        return Err(ManifestError.RequiredPropertyIsNull(p, "'package.flix' is null"))
      }
      flix
    } catch {
      case _: IllegalArgumentException => return Err(ManifestError.MissingRequiredProperty(p, "'package.flix' is missing"))
      case _: TomlInvalidTypeException => return Err(ManifestError.RequiredPropertyHasWrongType(p, s"'package.flix' should have type String"))
    }
    val semanticFlixVersion = toSemVer(flix, p) match {
      case Ok(v) => v
      case Err(e) => return Err(e)
    }

    val license_opt: Option[String] = try {
      val license = parser.getString("package.license")
      if (license == null) {
        None
      } else {
        Option(license)
      }
    } catch {
      case _: IllegalArgumentException => None
      case _: TomlInvalidTypeException => return Err(ManifestError.RequiredPropertyHasWrongType(p, s"'package.license' should have type String"))
    }

    val authors = try {
      val authors = parser.getArray("package.authors")
      if (authors == null) {
        return Err(ManifestError.RequiredPropertyIsNull(p, "'package.authors' is null"))
      }
      authors
    } catch {
      case _: IllegalArgumentException => return Err(ManifestError.MissingRequiredProperty(p, "'package.authors' is missing"))
      case _: TomlInvalidTypeException => return Err(ManifestError.RequiredPropertyHasWrongType(p, s"'package.authors' should have type Array"))
    }
    val authors_list = convertTomlArrayToStringList(authors, p) match {
      case Ok(l) => l
      case Err(e) => return Err(e)
    }

    val flix_dependencies = try {
      val flix_dependencies = parser.getTable("dependencies")
      if (flix_dependencies == null) {
        return Err(ManifestError.RequiredPropertyIsNull(p, "'dependencies' is null"))
      }
      flix_dependencies
    } catch {
      case _: IllegalArgumentException => return Err(ManifestError.MissingRequiredProperty(p, "'dependencies' is missing"))
      case _: TomlInvalidTypeException => return Err(ManifestError.RequiredPropertyHasWrongType(p, s"'dependencies' should have type Table"))
    }
    val flix_deps_list = collectDependencies(flix_dependencies, true, true, p) match {
      case Ok(l) => l
      case Err(e) => return Err(e)
    }

    val dev_flix_dependencies = try {
      val dev_flix_dependencies = parser.getTable("dev-dependencies")
      if (dev_flix_dependencies == null) {
        return Err(ManifestError.RequiredPropertyIsNull(p, "'dev-dependencies' is null"))
      }
      dev_flix_dependencies
    } catch {
      case _: IllegalArgumentException => return Err(ManifestError.MissingRequiredProperty(p, "'dev-dependencies' is missing"))
      case _: TomlInvalidTypeException => return Err(ManifestError.RequiredPropertyHasWrongType(p, s"'dev-dependencies' should have type Table"))
    }
    val dev_flix_deps_list = collectDependencies(dev_flix_dependencies, true, false, p) match {
      case Ok(l) => l
      case Err(e) => return Err(e)
    }

    val mvn_dependencies = try {
      val mvn_dependencies = parser.getTable("mvn-dependencies")
      if (mvn_dependencies == null) {
        return Err(ManifestError.RequiredPropertyIsNull(p, "'mvn-dependencies' is null"))
      }
      mvn_dependencies
    } catch {
      case _: IllegalArgumentException => return Err(ManifestError.MissingRequiredProperty(p, "'mvn-dependencies' is missing"))
      case _: TomlInvalidTypeException => return Err(ManifestError.RequiredPropertyHasWrongType(p, s"'mvn-dependencies' should have type Table"))
    }
    val mvn_deps_list = collectDependencies(mvn_dependencies, false, true, p) match {
      case Ok(l) => l
      case Err(e) => return Err(e)
    }

    val dev_mvn_dependencies = try {
      val dev_mvn_dependencies = parser.getTable("dev-mvn-dependencies")
      if (dev_mvn_dependencies == null) {
        return Err(ManifestError.RequiredPropertyIsNull(p, "'dev-mvn-dependencies' is null"))
      }
      dev_mvn_dependencies
    } catch {
      case _: IllegalArgumentException => return Err(ManifestError.MissingRequiredProperty(p, "'dev-mvn-dependencies' is missing"))
      case _: TomlInvalidTypeException => return Err(ManifestError.RequiredPropertyHasWrongType(p, s"'dev-mvn-dependencies' should have type Table"))
    }
    val dev_mvn_deps_list = collectDependencies(dev_mvn_dependencies, false, false, p) match {
      case Ok(l) => l
      case Err(e) => return Err(e)
    }

    val dependency_list = flix_deps_list ++ dev_flix_deps_list ++ mvn_deps_list ++ dev_mvn_deps_list

    Ok(Manifest(name, description, semanticVersion, semanticFlixVersion, license_opt, authors_list, dependency_list))
  }

  def parse(s: String)(implicit out: PrintStream): Result[Manifest, ManifestError] = {
    try {
      parse(Paths.get(s))
    } catch {
      case _: InvalidPathException => Err(ManifestError.ManifestNotFoundAt(s))
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
    val deps_entries = deps.entrySet()
    val deps_set = mutable.Set.empty[Dependency]
    deps_entries.forEach(entry => {
      try {
        toSemVer(entry.getValue.asInstanceOf[String], p) match {
          case Ok(version) => flixDep match {
            case true => prodDep match {
              case true => deps_set.add(Dependency.FlixDependency(entry.getKey, version, DependencyKind.Production))
              case false => deps_set.add(Dependency.FlixDependency(entry.getKey, version, DependencyKind.Development))
            }
            case false =>
              val key_split = entry.getKey.split(':')
              if (key_split.length == 2) {
                prodDep match {
                  case true => deps_set.add(Dependency.MavenDependency(key_split.apply(0), key_split.apply(1), version, DependencyKind.Production))
                  case false => deps_set.add(Dependency.MavenDependency(key_split.apply(0), key_split.apply(1), version, DependencyKind.Development))
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
    Ok(deps_set.toList)
  }

  private def convertTomlArrayToStringList(array: TomlArray, p: Path): Result[List[String], ManifestError] = {
    val string_set = mutable.Set.empty[String]
    for(i <- 0 to array.size()-1) {
      try {
        val s = array.get(i).asInstanceOf[String]
        string_set.add(s)
      } catch {
        case _: ClassCastException => Err(ManifestError.AuthorNameError(p, "All author names should be of type String"))
      }
    }
    Ok(string_set.toList)
  }

}
