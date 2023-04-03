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

import ca.uwaterloo.flix.util.Formatter

import java.nio.file.Path

sealed trait ManifestError {
  /**
    * Returns a human-readable and formatted string representation of this error.
    */
  def message(f: Formatter): String
}

object ManifestError {

  case class MissingRequiredProperty(path: Path, property: String) extends ManifestError {
    override def message(f: Formatter): String =
    s"""
      | The toml file does not contain a property called ${f.bold(property)}. This is required.
      | The toml file was found at ${f.cyan(if(path == null) "null" else path.toString)}.
      |""".stripMargin
  }

  case class RequiredPropertyHasWrongType(path: Path, property: String, requiredType: String) extends ManifestError {
    override def message(f: Formatter): String =
      s"""
        | The property ${f.bold(property)} is required to have a value of type ${f.bold(requiredType)}.
        | The toml file was found at ${f.cyan(if(path == null) "null" else path.toString)}.
        |""".stripMargin
  }

  case class FlixVersionHasWrongLength(path: Path, version: String) extends ManifestError {
    override def message(f: Formatter): String = {
      s"""
        | This toml file has a Flix version number of the wrong length: ${f.red(version)}.
        | A version in Flix should be formatted like so: 'x.x.x'.
        | The toml file was found at ${f.cyan(if(path == null) "null" else path.toString)}.
        |""".stripMargin
    }
  }

  case class MavenVersionHasWrongLength(path: Path, version: String) extends ManifestError {
    override def message(f: Formatter): String = {
      s"""
         | This toml file has a Maven version number of the wrong format: ${f.red(version)}.
         | A version in Maven should be formatted like so: 'x.x.x', 'x.x', 'x.x.x.x' or 'x.x.x-x'.
         | The toml file was found at ${f.cyan(if(path == null) "null" else path.toString)}.
         |""".stripMargin
    }
  }

  case class VersionNumberWrong(path: Path, version: String) extends ManifestError {
    override def message(f: Formatter): String =
      s"""
         | This toml file has a version number which includes things that are not numbers: ${f.red(version)}.
         | The toml file was found at ${f.cyan(if(path == null) "null" else path.toString)}.
         |""".stripMargin
  }

  case class MavenDependencyFormatError(path: Path, depName: String) extends ManifestError {
    override def message(f: Formatter): String =
      s"""
         | "A Maven dependency should be formatted like so: 'group:artifact'".
         | Instead found: ${f.red(depName)}.
         | The toml file was found at ${f.cyan(if(path == null) "null" else path.toString)}.
         |""".stripMargin
  }

  case class FlixDependencyFormatError(path: Path, depName: String) extends ManifestError {
    override def message(f: Formatter): String =
      s"""
         | "A Flix dependency should be formatted like so: 'repository:username/projectname'".
         | Instead found: ${f.red(depName)}.
         | The toml file was found at ${f.cyan(if(path == null) "null" else path.toString)}.
         |""".stripMargin
  }

  case class DependencyFormatError(path: Path, depVer: AnyRef) extends ManifestError {
    override def message(f: Formatter): String =
      s"""
         | All versions should be of type String. Instead found: $depVer.
         | The toml file was found at ${f.cyan(if(path == null) "null" else path.toString)}.
         |""".stripMargin
  }

  case class AuthorNameError(path: Path) extends ManifestError {
    override def message(f: Formatter): String =
      s"""
         | There was an author name which was not of type String.
         | The toml file was found at ${f.cyan(if(path == null) "null" else path.toString)}.
         |""".stripMargin
  }

  case class ManifestParseError(path: Path, msg: String) extends ManifestError {
    override def message(f: Formatter): String =
      s"""
         | There was a problem parsing the toml file with the following errors:
         | '$msg'
         | The toml file was found at ${f.cyan(if(path == null) "null" else path.toString)}.
         |""".stripMargin
  }

  case class UnsupportedRepository(path: Path, attemptedRepo: String) extends ManifestError {
    override def message(f: Formatter): String =
      s"""
         | ${f.red(attemptedRepo)} is not supported as a repository to download Flix dependencies from.
         | Supported repositories: ${f.bold("github")}.
         | The toml file was found at ${f.cyan(if(path == null) "null" else path.toString)}.
         |""".stripMargin
  }

  case class IllegalName(path: Path, dependency: String) extends ManifestError {
    override def message(f: Formatter): String =
      s"""
         | A dependency includes a non-supported character: ${f.red(dependency)}
         | The dependencies in a toml file can only include the following characters:
         | a-z, A-Z, 0-9, ., :, -, _, /
         | The toml file was found at ${f.cyan(if(path == null) "null" else path.toString)}.
         |""".stripMargin
  }

  case class IOError(path: Path) extends ManifestError {
    override def message(f: Formatter): String =
      s"""
         | An I/O error occured while parsing the toml file.
         | The toml file was found at ${f.cyan(if(path == null) "null" else path.toString)}.
         |""".stripMargin
  }

  case class IllegalTableFound(path: Path, tableName: String) extends ManifestError {
    override def message(f: Formatter): String =
      s"""
         | The toml file has a table named ${f.red(tableName)}, which is not allowed.
         | Allowed table names:
         |   package, dependencies, dev-dependencies, mvn-dependencies, dev-mvn-dependencies
         | The toml file was found at ${f.cyan(if (path == null) "null" else path.toString)}.
         |""".stripMargin
  }

  case class IllegalPackageKeyFound(path: Path, entryName: String) extends ManifestError {
    override def message(f: Formatter): String =
      s"""
         | The toml file has an entry in the package table named ${f.red(entryName)}, which is not allowed.
         | Allowed entry names in the package table:
         |   name, description, version, flix, authors, license
         | The toml file was found at ${f.cyan(if (path == null) "null" else path.toString)}.
         |""".stripMargin
  }

}
