/*
 * Copyright 2024 Magnus Madsen
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
package ca.uwaterloo.flix.language.ast.shared

import ca.uwaterloo.flix.language.ast.{Name, SourcePosition}

/**
  * Represents the source position of an enclosing module.
  *
  * If the program is the one below, and we are inside baz:
  *
  * {{{
  * mod Foo {
  *   |mod Bar {
  *     def baz(): Unit = ...
  *   }
  * }
  * }}}
  *
  * then the source position is indicated by the vertical bar.
  *
  * N.B.: Do not construct the case class directly but use [[EnclosingMod.mk]].
  */
case class EnclosingMod(sp: SourcePosition)

object EnclosingMod {
  def mk(name: Name.NName): EnclosingMod = {
    if (name.isRoot) EnclosingMod(name.loc.sp1)
    else EnclosingMod(name.loc.sp1.copy(line = name.loc.sp1.line + 1))
  }
}


