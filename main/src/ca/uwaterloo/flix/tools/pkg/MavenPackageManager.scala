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

import ca.uwaterloo.flix.tools.pkg.Dependency.MavenDependency
import ca.uwaterloo.flix.util.Result
import ca.uwaterloo.flix.util.Result.{Err, Ok, ToOk}

import java.io.PrintStream
import coursier._

object MavenPackageManager {

  val scalaVersion = "2.13"

  /**
    * Installs all MavenDependencies for a Manifest,
    * including transitive dependencies using coursier.
    */
  def installAll(manifest: Manifest)(implicit out: PrintStream): Result[List[String], PackageError] = {
    val depStrings = getMavenDependencyStrings(manifest)

    createCoursierDependencies(depStrings).flatMap { deps =>
      val res = deps.foldLeft(Resolve())((res, dep) => res.addDependencies(dep))
      val resolution = res.run()

      val installed = resolution.dependencies.map(dep => dep.module.toString()).toList
      val fetch = resolution.dependencies.foldLeft(Fetch())((f, dep) => {
        out.println(s"Installing ${dep.module.toString()}");
        f.addDependencies(dep)
      })
      try {
        fetch.run()
      } catch {
        //TODO: does not seem to work...
        case e: Exception => return Err(PackageError.CoursierError(s"Error in downloading Maven dependency: ${e.getMessage}"))
      }

      Ok(installed)
    }
  }

  /**
    * Finds the MavenDependencies for a Manifest
    * and converts them to Strings.
    */
  private def getMavenDependencyStrings(manifest: Manifest): List[String] = {
    manifest.dependencies.collect {
      case dep: MavenDependency => dep
    }.map(dep => s"${dep.groupId}:${dep.artifactId}:${dep.version.toString}")
  }

  /**
    * Creates Coursier dependencies from a list of Strings
    */
  private def createCoursierDependencies(depStrings: List[String]): Result[List[Dependency], PackageError] = {
    Result.sequence(
      depStrings.map(depName => coursier.parse.DependencyParser.dependency(depName, scalaVersion))
      .map {
        case Left(error) => Err(PackageError.CoursierError(s"Error in creating Coursier dependency: $error"))
        case Right(cDep) => Ok(cDep)
      }
    )
  }

}
