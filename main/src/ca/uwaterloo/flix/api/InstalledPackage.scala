/*
 * Copyright 2026 Magnus Madsen
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

import ca.uwaterloo.flix.language.ast.shared.SecurityContext

import java.nio.file.Path

/**
  * A Flix package installed on disk.
  *
  * @param path   the path to the `.fpkg` file.
  * @param id     the identifier of the package, e.g. `github:flix/museum-clerk`.
  * @param sctx   the security context the package is compiled under.
  * @param mounts the mount table of the package, as its own manifest declares it: the name of
  *               each mount to the identifier of the dependency it names.
  */
case class InstalledPackage(path: Path, id: String, sctx: SecurityContext, mounts: Map[String, String])
