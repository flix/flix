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

package ca.uwaterloo.flix.language.phase.llvm

import ca.uwaterloo.flix.language.ast.{LoweredAst, Symbol}

/**
  * Shared effect/op id assignment used by the LLVM backend and wasm-side metadata emitters.
  *
  * These ids are only required to be stable within a single compilation unit, but they must be
  * computed from one place. Async wasm effect manifests and the runtime `unknown(effId, opId)`
  * request path must agree exactly.
  */
object LlvmEffectIds {

  def effectSymIds(root: LoweredAst.Root): Map[Symbol.EffSym, Long] =
    root.effects.keys.toList.sortBy(_.toString).zipWithIndex.map {
      case (sym, idx) => sym -> (idx.toLong + 1L)
    }.toMap

  def opIndices(root: LoweredAst.Root): Map[Symbol.OpSym, Int] =
    root.effects.values.flatMap { eff =>
      eff.ops.zipWithIndex.map {
        case (op, idx) => op.sym -> idx
      }
    }.toMap
}
