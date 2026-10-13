/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.api

import java.nio.file.Path

/**
  * The files of a Flix project: its source files, its packages with their security contexts, and its JARs.
  *
  * Immutable: a change on disk is represented by a new value, obtained by scanning the project again.
  *
  * @param sources the `.flix` source files.
  * @param pkgs    the installed Flix packages.
  * @param jars    the `.jar` files, Maven dependencies before external JARs.
  */
case class ProjectFiles(sources: List[Path], pkgs: List[InstalledPackage], jars: List[Path]) {

  /**
    * Returns `true` if `path` is one of the packages or JARs.
    */
  def isDependency(path: Path): Boolean =
    pkgs.exists(_.path == path) || jars.contains(path)

}
