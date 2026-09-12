/*
 * Copyright 2023 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.tools.pkg

import ca.uwaterloo.flix.language.ast.shared.{Mountpoint, PackageId, SecurityContext}

import java.net.{URI, URL}

sealed trait Dependency

object Dependency {

  /**
    * A dependency on a Flix package.
    *
    * @param version the least version of the package that the dependent can be built with. The
    *                version it is built with is the greatest one that any dependent requires,
    *                which has the same major version, see [[FlixPackageManager.resolve]].
    * @param mount the name of the top-level module the package is visible under, if the dependency
    *              declares one. A dependency without a mount is reachable unqualified instead, as
    *              it was before mounts existed. Transitional: a mount becomes required.
    * @param style how the dependency is written in `flix.toml`, which is how it is written back,
    *              see [[Manifest.format]].
    */
  case class FlixDependency(id: PackageId, version: SemVer, mount: Option[Mountpoint], sctx: SecurityContext, style: DependencyStyle) extends Dependency {
    override def toString: String = {
      val mountStr = mount.map(m => s"mount = \"$m\", ").getOrElse("")
      s"\"$id\" = { version = \"$version\", ${mountStr}security = \"$sctx\" }"
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
