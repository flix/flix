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

import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.tools.pkg.Dependency.MavenDependency
import ca.uwaterloo.flix.util.{InternalCompilerException, Result}
import ca.uwaterloo.flix.util.Result.{Err, Ok, ToOk}

import java.io.PrintStream
import coursier._

object MavenPackageManager {

  private val scalaVersion = "2.13"

  /**
    * Installs all MavenDependencies for a Manifest,
    * including transitive dependencies using coursier.
    */
  def installAll(manifest: Manifest)(implicit out: PrintStream): Result[Unit, PackageError] = {
    val depStrings = getMavenDependencyStrings(manifest)

    Result.sequence(depStrings.map(createCoursierDependencies)).flatMap { deps =>
      try {
        val res = deps.foldLeft(Resolve())((res, dep) => res.addDependencies(dep))
        val resolution = res.run()

        val fetch = resolution.dependencies.foldLeft(Fetch())((f, dep) => {
          out.println(s"Installing ${dep.module.toString()}");
          f.addDependencies(dep)
        })
        fetch.run()
      } catch {
        case e: Exception =>
          out.println(e.getMessage)
          //Shortens the error message to just give the name of the dependency
          val message = e.getMessage.replaceAll("[^a-zA-Z0-9:. ]", "/").split('/').apply(0)
          return Err(PackageError.CoursierError(s"Error in downloading Maven dependency: $message"))
      }
      ().toOk
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
  private def createCoursierDependencies(depString: String): Result[Dependency, PackageError] =
    coursier.parse.DependencyParser.dependency(depString, scalaVersion) match {
      case Left(error) => throw InternalCompilerException(s"Coursier error: $error", SourceLocation.Unknown)
      case Right(cDep) => Ok(cDep)
    }



}
