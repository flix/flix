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
import ca.uwaterloo.flix.language.phase.ExportAbi

/**
  * Computes the set of lowered definitions that are invokable via the wasm component runtime
  * (i.e. via `def-id` in the WIT surface).
  *
  * Current scheme: allocate sequential def-ids and emit a manifest for hosts.
  *
  * Invariants:
  *   - If `main` exists, it is always assigned def-id 0.
  *   - `@Export` defs are assigned subsequent def-ids in stable name order.
  *   - `main` (if also `@Export`) appears only once.
  */
object LlvmWasmDefs {

  case class Entry(defId: Long,
                   sym: Symbol.DefnSym,
                   defn: LoweredAst.Def,
                   signature: ExportAbi.Signature,
                   isMain: Boolean,
                   isExport: Boolean)

  def compute(root: LoweredAst.Root): List[Entry] = {
    val defs = root.defs

    val mainSymOpt = root.mainEntryPoint
    val mainList: List[Symbol.DefnSym] = mainSymOpt.toList

    val exportSyms = defs.values
      .iterator
      .filter(_.ann.isExport)
      .map(_.sym)
      .toList
      .sortBy(_.toString)
      .filter(sym => !mainSymOpt.contains(sym))

    val invokableSyms = mainList ::: exportSyms

    invokableSyms.zipWithIndex.map {
      case (sym, idx) =>
        val defn = defs(sym)
        val isMain = mainSymOpt.contains(sym)
        val signature = defn.exportedSignature.orElse {
          if (isMain) ExportAbi.portableExportSignature((defn.cparams ::: defn.fparams).map(_.tpe), defn.unboxedType.tpe) else None
        }.getOrElse {
          val kind = if (isMain) "main entrypoint" else "@Export def"
          throw new IllegalStateException(s"Missing portable wasm invocation signature for '$sym' ($kind).")
        }
        Entry(
          defId = idx.toLong,
          sym = sym,
          defn = defn,
          signature = signature,
          isMain = isMain,
          isExport = defn.ann.isExport
        )
    }
  }
}
