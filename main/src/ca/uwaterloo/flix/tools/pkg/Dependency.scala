/*
 * Copyright 2023 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
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
