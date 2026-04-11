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

case class NativeBindingConfig(header: Path,
                               module: String = "Native",
                               spec: Option[Path] = None,
                               includePaths: List[Path] = Nil,
                               defines: List[String] = Nil,
                               cflags: List[String] = Nil)

case class WasmBindingConfig(witDir: Path,
                             world: String,
                             module: String = "Wit")

case class BindingsConfig(native: List[NativeBindingConfig] = Nil,
                          wasm: List[WasmBindingConfig] = Nil) {
  def isEmpty: Boolean = native.isEmpty && wasm.isEmpty
}

case class NativeCompileConfig(sources: List[Path] = Nil,
                               includePaths: List[Path] = Nil,
                               cflags: List[String] = Nil) {
  def isEmpty: Boolean =
    sources.isEmpty &&
      includePaths.isEmpty &&
      cflags.isEmpty

  def ++(other: NativeCompileConfig): NativeCompileConfig =
    NativeCompileConfig(
      sources = (sources ::: other.sources).distinct,
      includePaths = (includePaths ::: other.includePaths).distinct,
      cflags = (cflags ::: other.cflags).distinct,
    )
}
