/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.api

import ca.uwaterloo.flix.language.ast.shared.{Mountpoint, PackageId, SecurityContext}

import java.nio.file.Path

/**
  * A Flix package installed on disk.
  *
  * @param path   the path to the `.fpkg` file.
  * @param id     the package, e.g. `github:flix/museum-clerk`.
  * @param sctx   the security context the package is compiled under.
  * @param mounts the mount table of the package, as its own manifest declares it: the name of
  *               each mount to the dependency it names.
  */
case class InstalledPackage(path: Path, id: PackageId, sctx: SecurityContext, mounts: Map[Mountpoint, PackageId])
