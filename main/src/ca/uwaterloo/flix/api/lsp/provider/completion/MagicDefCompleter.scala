/*
 * Copyright 2025 Magnus Madsen
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

import ca.uwaterloo.flix.api.lsp.Range
import ca.uwaterloo.flix.language.ast.shared.AnchorPosition
import ca.uwaterloo.flix.language.ast.{Name, SourceLocation, Type, TypeConstructor, TypedAst}

object MagicDefCompleter {

  def getCompletions(ident: Name.Ident, tpe: Type, range: Range, loc: SourceLocation, root: TypedAst.Root): Iterable[Completion] = {
    val prefix = ident.name

    tpe.typeConstructor match {
      case Some(TypeConstructor.Enum(sym, _)) =>
        val candidates = root.defs.values.collect {
          case defn if sym.namespace ::: sym.name :: Nil == defn.sym.namespace => defn
        }.filter {
          case defn => CompletionUtils.isAvailable(defn.spec) //&& CompletionUtils.matchesName(decl.sym, qn, qualified = false)
        }.filter {
          case defn => defn.sym.text.startsWith(prefix)
        }


        val result = candidates.map {
          case defn =>
            val label = loc.text.getOrElse("") + "." + defn.sym.text
            Completion.MagicDefCompletion(label, defn, range, Priority.Highest(0), AnchorPosition(0, 0, 0), qualified = false, inScope = false, ExprContext.Unknown)
        }
        result.take(5)
      case _ =>
        Nil
    }
  }

}

