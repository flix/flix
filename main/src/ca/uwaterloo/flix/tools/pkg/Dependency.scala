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

  case class FlixDependency(repo: Repository, username: String, projectName: String, version: SemVer, sctx: SecurityContext) extends Dependency {
    val identifier: String = {
      val r = repo.toString.toLowerCase
      s"$r:$username/$projectName"
    }

    override def toString: String = {
      s"\"$identifier\" = { version = \"$version\", security = \"$sctx\" }"
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
