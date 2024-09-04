/*
 * Copyright 2022 Paul Butcher, Lukas Rønn
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
package ca.uwaterloo.flix.api.lsp.provider.completion

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.StructCompletion
import ca.uwaterloo.flix.api.lsp.provider.completion.TypeCompleter.{formatTParams, formatTParamsSnippet, getInternalPriority, priorityBoostForTypes}
import ca.uwaterloo.flix.api.lsp.{Index, TextEdit}
import ca.uwaterloo.flix.language.ast.Symbol.StructSym
import ca.uwaterloo.flix.language.ast.{Symbol, TypedAst}

object StructCompleter {

  def getCompletions(ctx: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root): Iterable[StructCompletion] = {
    val structsInModule = getStructSymsInModule(ctx)
    getStructCompletions(structsInModule, ctx)
  }

  private def getStructSymsInModule(ctx: CompletionContext)(implicit root: TypedAst.Root): List[Symbol.StructSym] = {
    val modFragment = ModuleSymFragment.parseModuleSym(ctx.word)

    modFragment match {
      case ModuleSymFragment.Complete(modSym) => root.modules.getOrElse(modSym, Nil).collect {
        case sym: StructSym => sym
      }
      case ModuleSymFragment.Partial(modSym, suffix) => root.modules.getOrElse(modSym, Nil).collect {
        case sym: StructSym if matches(sym, suffix) => sym
      }
    }
  }

  /**
    * Returns `true` if the given struct `sym` matches the given `suffix`.
    *
    * (Color, "Col") => true
    * (Color, "Li")  => false
    */
  private def matches(sym: StructSym, suffix: String): Boolean = {
    sym.name.startsWith(suffix)
  }

  private def getStructCompletions(structs: List[Symbol.StructSym], ctx: CompletionContext)(implicit root: TypedAst.Root): Iterable[StructCompletion] = {
    structs.map(sym => getStructCompletion(root.structs(sym), ctx))
  }

  private def getStructCompletion(decl: TypedAst.Struct, ctx: CompletionContext): StructCompletion = {
    val sym = decl.sym
    val name = decl.sym.name
    val internalPriority = getInternalPriority(decl.loc, decl.sym.namespace)(ctx)
    Completion.StructCompletion(sym, formatTParams(decl.tparams), priorityBoostForTypes(internalPriority(name))(ctx),
      TextEdit(ctx.range, s"${sym.toString}${formatTParamsSnippet(decl.tparams)}"), Some(decl.doc.text))
  }

}
