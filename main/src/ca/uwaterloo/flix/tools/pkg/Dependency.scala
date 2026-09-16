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

import ca.uwaterloo.flix.language.ast.shared.SecurityContext

import java.net.{URI, URL}

sealed trait Dependency

object Dependency {

  /**
    * A dependency on a Flix package.
    *
    * @param mount the name of the top-level module under which the package is visible to the dependent.
    */
  case class FlixDependency(repo: Repository, username: String, projectName: String, version: SemVer, mount: String, sctx: SecurityContext) extends Dependency {
    val identifier: String = {
      val r = repo.toString.toLowerCase
      s"$r:$username/$projectName"
    }

    /** Returns `true` if `mount` is the default mount derived from `projectName`. */
    def hasDefaultMount: Boolean = FlixDependency.defaultMount(projectName).contains(mount)

    override def toString: String = {
      val mountStr = if (hasDefaultMount) "" else s"mount = \"$mount\", "
      s"\"$identifier\" = { version = \"$version\", ${mountStr}security = \"$sctx\" }"
    }
  }

  object FlixDependency {
    /** A valid mount: an uppercase letter followed by letters, digits, and underscores. */
    private val ValidMount = "[A-Z][A-Za-z0-9_]*".r

    /** Returns `true` if `s` can serve as a mount, i.e. as the name of a top-level module. */
    def isValidMount(s: String): Boolean = ValidMount.matches(s)

    /**
      * Returns the default mount for the project `projectName`, if one can be derived.
      *
      * The hyphen-separated words of the project name are joined with their first letters
      * uppercased: `flixball` becomes `Flixball` and `tic-tac-toe` becomes `TicTacToe`.
      * Returns `None` if the result is not a valid mount, e.g. for a name that starts with a digit.
      */
    def defaultMount(projectName: String): Option[String] = {
      val candidate = projectName.split('-').filter(_.nonEmpty).map(_.capitalize).mkString
      Option.when(isValidMount(candidate))(candidate)
    }
  }

  case class MavenDependency(groupId: String, artifactId: String, versionTag: String) extends Dependency {
    val identifier = s"$groupId:$artifactId"

    override def toString: String = {
      s"\"$identifier\" = \"$versionTag\""
    }
  }

  case class JarDependency(url: String, fileName: String) extends Dependency {
    val identifier: String = fileName

    override def toString: String = s"\"$identifier\" = \"url:$url\""

    def getUrl: URL = new URI(url).toURL
  }

}
