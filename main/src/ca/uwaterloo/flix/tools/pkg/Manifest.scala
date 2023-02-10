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

import ca.uwaterloo.flix.tools.pkg.Dependency.FlixDependency

import java.nio.file.{Path, Paths}

case class Manifest(name: String, description: String, version: SemVer, flix: SemVer, license: Option[String], authors: List[String], dependencies: List[Dependency]) {

  def getFlixPackages: List[Path] = {
    dependencies.collect{
          //TODO: this is a directory - get all the fpkg file paths from there....
      case d: FlixDependency => Paths.get(".").resolve("lib").resolve(d.username).resolve(d.projectName).resolve(d.version.toString)
    }

    List.empty
  }

  def getMavenPackages: List[Path] = Nil // TODO: Where does Coursier put the files???

}
