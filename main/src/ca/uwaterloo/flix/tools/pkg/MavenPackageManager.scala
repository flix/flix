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

import ca.uwaterloo.flix.api.Bootstrap
import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.tools.pkg.Dependency.MavenDependency
import ca.uwaterloo.flix.util.{Formatter, InternalCompilerException, Result}
import ca.uwaterloo.flix.util.Result.{Err, Ok, ToOk}

import java.io.PrintStream
import coursier.{Dependency as CoursierDependency, Fetch, Resolve}
import coursier.cache.{Cache, FileCache}
import coursier.util.Task

import java.nio.file.{Files, Path, Paths}

object MavenPackageManager {

  val FolderName = "cache"
  private val scalaVersion = "2.13"

  /**
    * Installs all MavenDependencies for a Manifest including transitive
    * dependencies using coursier in the /lib/cache folder of `path`.
    * Returns a list of paths to the downloadet .jars.
    */
  def installAll(manifests: List[Manifest], path: Path)(implicit formatter: Formatter, out: PrintStream): Result[List[Path], PackageError] = {
    out.println("Resolving Maven dependencies...")

    val depStrings = manifests.flatMap(manifest => getMavenDependencyStrings(manifest))

    val libPath = Bootstrap.getLibraryDirectory(path).resolve(FolderName)
    val cacheString = libPath.toString
    Files.createDirectories(Paths.get(cacheString))

    Result.sequence(depStrings.map(createCoursierDependencies)).flatMap { deps =>
      val l = try {
        val cache: Cache[Task] = FileCache().withLocation(cacheString)

        val res = deps.foldLeft(Resolve())((res, dep) => res.addDependencies(dep))
        val resolution = res.withCache(cache).run()

        val fetch = resolution.dependencies.foldLeft(Fetch())(
          (f, dep) => {
            out.println(s"  Adding `${formatter.blue(dep.module.toString)}' (${formatter.cyan(s"v${dep.version}")}).")
            f.addDependencies(dep)
        })
        out.println("  Running Maven dependency resolver.")
        val paths = fetch.withCache(cache).run()
        paths.map(_.toPath).toList
      } catch {
        case e: Exception =>
          out.println(e.getMessage)
          //Shortens the error message to just give the name of the dependency
          val message = e.getMessage.replaceAll("[^a-zA-Z0-9:. ]", "/").split('/').apply(0)
          return Err(PackageError.CoursierError(message))
      }
      l.toOk
    }
  }

  /**
    * Finds the MavenDependencies for a Manifest
    * and converts them to Strings.
    */
  def getMavenDependencyStrings(manifest: Manifest): List[String] = {
    manifest.dependencies.collect {
      case dep: MavenDependency => dep
    }.map(dep => s"${dep.groupId}:${dep.artifactId}:${dep.versionTag}")
  }

  /**
    * Creates Coursier dependencies from a list of Strings
    */
  private def createCoursierDependencies(depString: String): Result[CoursierDependency, PackageError] =
    coursier.parse.DependencyParser.dependency(depString, scalaVersion) match {
      case Left(error) => throw InternalCompilerException(s"Coursier error: $error", SourceLocation.Unknown)
      case Right(cDep) => Ok(cDep)
    }

}
