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
import ca.uwaterloo.flix.util.{InternalCompilerException, Result}
import ca.uwaterloo.flix.util.Result.{Err, Ok, ToOk}

import java.io.PrintStream
import coursier._
import coursier.cache.{Cache, FileCache}
import coursier.util.Task

import java.nio.file.{Files, Path, Paths}

object MavenPackageManager {

  private val scalaVersion = "2.13"

  /**
    * Installs all MavenDependencies for a Manifest including transitive
    * dependencies using coursier in the /lib/cache folder of `path`.
    * Returns a list of paths to the downloadet .jars.
    */
  def installAll(manifests: List[Manifest], path: Path)(implicit out: PrintStream): Result[List[Path], PackageError] = {
    out.println("Resolving Maven dependencies...")

    val depStrings = manifests.flatMap(manifest => getMavenDependencyStrings(manifest))

    val libPath = Bootstrap.getLibraryDirectory(path).resolve("cache")
    val cacheString = libPath.toString
    Files.createDirectories(Paths.get(cacheString))

    Result.sequence(depStrings.map(createCoursierDependencies)).flatMap { deps =>
      val l = try {
        val cache: Cache[Task] = FileCache().withLocation(cacheString)

        val res = deps.foldLeft(Resolve())((res, dep) => res.addDependencies(dep))
        val resolution = res.withCache(cache).run()

        val resList: collection.mutable.ListBuffer[Path] = collection.mutable.ListBuffer.empty
        val fetch = resolution.dependencies.foldLeft(Fetch())(
          (f, dep) => {
            val moduleName = dep.module.toString()
            val moduleNamePath = moduleName.replaceAll("[^a-zA-Z0-9-]", "/")
            val versionString = dep.version
            val fileName = s"${moduleNamePath.split('/').last}-$versionString.jar"
            val filePrefix = "https/repo1.maven.org/maven2"
            val depPath = libPath.resolve(filePrefix).resolve(moduleNamePath).resolve(versionString).resolve(fileName)
            resList.addOne(depPath)

            out.println(s"  Adding `$moduleName' ($versionString).")
            f.addDependencies(dep)
        })
        out.println("  Running Maven dependency resolver.")
        fetch.withCache(cache).run()
        resList.toList
      } catch {
        case e: Exception =>
          out.println(e)
          //Shortens the error message to just give the name of the dependency
          val message = e.getMessage.replaceAll("[^a-zA-Z0-9:. ]", "/").split('/').apply(0)
          return Err(PackageError.CoursierError(s"Error in downloading Maven dependency: $message"))
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
