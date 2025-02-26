/*
 * Copyright 2025 Chenhao Gao
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

import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.StructCompletion
import ca.uwaterloo.flix.language.ast.NamedAst.Declaration.Struct
import ca.uwaterloo.flix.language.ast.{Name, TypedAst}
import ca.uwaterloo.flix.language.ast.shared.{AnchorPosition, LocalScope, Resolution}
import ca.uwaterloo.flix.language.errors.ResolutionError

object StructCompleter {

  /**
    * Returns a List of Completion for structs.
    * Whether the returned completions are qualified is based on whether the name in the error is qualified.
    * When providing completions for unqualified enums that is not in scope, we will also automatically use the enum.
    */
  def getCompletions(err: ResolutionError.UndefinedType)(implicit root: TypedAst.Root): Iterable[Completion] = {
    getCompletions(err.qn.loc.source.name, err.ap, err.env, err.qn)
  }

  private def getCompletions(uri: String, ap: AnchorPosition, env: LocalScope, qn: Name.QName)(implicit root: TypedAst.Root): Iterable[Completion] = {
    if (qn.namespace.nonEmpty)
      root.structs.values.collect{
        case struct if CompletionUtils.isAvailable(struct) && CompletionUtils.matchesName(struct.sym, qn, qualified = true) =>
          StructCompletion(struct, ap, qualified = true, inScope = true)
      }
    else
      root.structs.values.collect({
        case struct if CompletionUtils.isAvailable(struct) && CompletionUtils.matchesName(struct.sym, qn, qualified = false) =>
          StructCompletion(struct, ap, qualified = false, inScope = inScope(struct, env))
      })
  }

  /**
   * Checks if the definition is in scope.
   * If we can find the definition in the scope or the definition is in the root namespace, it is in scope.
   */
  private def inScope(struct: TypedAst.Struct, scope: LocalScope): Boolean = {
    val thisName = struct.sym.toString
    val isResolved = scope.m.values.exists(_.exists {
      case Resolution.Declaration(Struct(_, _, _, thatName, _, _, _, _)) => thisName == thatName.toString
      case _ => false
    })
    val isRoot = struct.sym.namespace.isEmpty
    isRoot || isResolved
  }
}
