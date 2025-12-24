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

import ca.uwaterloo.flix.tools.pkg.github.GitHub

case class Manifest(name: String,
                    description: String,
                    version: SemVer,
                    repository: Option[GitHub.Project],
                    modules: PackageModules,
                    flix: SemVer,
                    license: Option[String],
                    authors: List[String],
                    dependencies: List[Dependency]) {
  val flixDependencies: List[Dependency.FlixDependency] = dependencies.collect { case dep: Dependency.FlixDependency => dep }
  val mavenDependencies: List[Dependency.MavenDependency] = dependencies.collect { case dep: Dependency.MavenDependency => dep }
  val jarDependencies: List[Dependency.JarDependency] = dependencies.collect { case dep: Dependency.JarDependency => dep }
}

object Manifest {

  /**
    * Renders `manifest` as its textual representation.
    * Parsing the output yields the original manifest, i.e., `manifest`.
    */
  def render(manifest: Manifest): String = {
    val repo = manifest.repository match {
      case None => ""
      case Some(r) => System.lineSeparator() + s"repository  = $r" + System.lineSeparator()
    }
    val modules = manifest.modules match {
      case PackageModules.All => ""
      case PackageModules.Selected(included) =>
        System.lineSeparator() + s"modules     = ${included.map(_.toString).mkString("[", ", ", "]")}"
    }

    val license = manifest.license match {
      case None => ""
      case Some(l) => System.lineSeparator() + s"license     = $l" + System.lineSeparator()
    }

    val base =
      s"""[package]
         |name        = ${manifest.name}
         |description = ${manifest.description}
         |version     = ${manifest.version}""".stripMargin +
        repo +
        modules +
        s"""flix        = ${manifest.flix}""".stripMargin +
        license +
        s"""
           |authors     = ${manifest.authors.mkString("[", ", ", "]")}
           |""".stripMargin


    val flixDeps = manifest.flixDependencies match {
      case Nil => ""
      case _ :: _ =>
        s"""[dependencies]
           |${manifest.flixDependencies.mkString(System.lineSeparator())}
           |""".stripMargin
    }

    val mvnDeps = manifest.mavenDependencies match {
      case Nil => ""
      case _ :: _ =>
        s"""[mvn-dependencies]
           |${manifest.mavenDependencies.mkString(System.lineSeparator())}
           |""".stripMargin
    }

    val jarDeps = manifest.jarDependencies match {
      case Nil => ""
      case _ :: _ =>
        s"""[mvn-dependencies]
           |${manifest.jarDependencies.mkString(System.lineSeparator())}
           |""".stripMargin
    }

    base + flixDeps + mvnDeps + jarDeps
  }

}
