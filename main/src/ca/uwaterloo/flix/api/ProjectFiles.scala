/*
 * Copyright 2026 Flix Authors
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
