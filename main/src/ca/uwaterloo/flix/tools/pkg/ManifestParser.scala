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
import org.tomlj.{Toml, TomlInvalidTypeException, TomlTable}

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
    println(license_opt)

    val authors = try {
      val authors = parser.getArray("package.authors")
      if (authors == null) {
        return Err(ManifestError.RequiredPropertyIsNull(p, "'package.authors' is null"))
      }
      authors
    } catch {
      case _: IllegalArgumentException => return Err(ManifestError.MissingRequiredProperty(p, "'package.authors' is missing"))
      case _: TomlInvalidTypeException => return Err(ManifestError.RequiredPropertyHasWrongType(p, s"'package.authors' should have type Array"))
    } //TODO: convert to List[String]

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
    val flix_deps_list = collectDependencies(flix_dependencies, p) match {
      case Ok(l) => l
      case Err(e) => return Err(e)
    }

    //TODO: parse other dependencies
    //val dev_deps = parser.getTable("dev-dependencies")
    //val mvn_deps = parser.getTable("mvn-dependencies")
    //val dev_mvn_deps = parser.getTable("dev-mvn-dependencies")

    Ok(Manifest(name, description, semanticVersion, semanticFlixVersion, license_opt, List.empty, flix_deps_list))
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

  private def collectDependencies(deps: TomlTable, p: Path): Result[List[Dependency], ManifestError] = {
    //TODO: add error handling
    val deps_set = mutable.Set.empty[Dependency]
    val deps_entries = deps.entrySet()
    deps_entries.forEach(entry => {
      toSemVer(entry.getValue.asInstanceOf[String], p) match {
        //TODO: choose correct dependency kind
        case Ok(version) => deps_set.add(Dependency.FlixDependency(entry.getKey, version, DependencyKind.Production))
        case Err(e) => return Err(e)
      }
    })
    Ok(deps_set.toList)
  }

}
