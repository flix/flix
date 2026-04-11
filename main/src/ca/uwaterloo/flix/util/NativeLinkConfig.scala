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

package ca.uwaterloo.flix.util

import java.nio.file.Path

/**
  * Project-level native link configuration for `llvm-native`.
  *
  * Search paths may be relative when parsed from `flix.toml`; Bootstrap resolves them
  * against the project directory before code generation/linking.
  */
case class NativeLinkConfig(libraries: List[String] = Nil,
                            searchPaths: List[Path] = Nil,
                            pkgConfigPackages: List[String] = Nil,
                            frameworks: List[String] = Nil,
                            frameworkSearchPaths: List[Path] = Nil,
                            flags: List[String] = Nil) {

  def isEmpty: Boolean =
    libraries.isEmpty &&
      searchPaths.isEmpty &&
      pkgConfigPackages.isEmpty &&
      frameworks.isEmpty &&
      frameworkSearchPaths.isEmpty &&
      flags.isEmpty

  def ++(other: NativeLinkConfig): NativeLinkConfig =
    NativeLinkConfig(
      libraries = libraries ::: other.libraries,
      searchPaths = searchPaths ::: other.searchPaths,
      pkgConfigPackages = pkgConfigPackages ::: other.pkgConfigPackages,
      frameworks = frameworks ::: other.frameworks,
      frameworkSearchPaths = frameworkSearchPaths ::: other.frameworkSearchPaths,
      flags = flags ::: other.flags,
    )

}
