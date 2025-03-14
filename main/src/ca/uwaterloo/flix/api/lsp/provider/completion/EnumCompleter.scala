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

import ca.uwaterloo.flix.api.lsp.Range
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.EnumCompletion
import ca.uwaterloo.flix.language.ast.NamedAst.Declaration.Enum
import ca.uwaterloo.flix.language.ast.shared.{AnchorPosition, LocalScope, Resolution}
import ca.uwaterloo.flix.language.ast.{Name, TypedAst}

object EnumCompleter {
  /**
    * Returns a List of Completion for enums.
    * Whether the returned completions are qualified is based on whether the name in the error is qualified.
    * When providing completions for unqualified enums that is not in scope, we will also automatically use the enum.
    */
  def getCompletions(qn: Name.QName, range: Range, ap: AnchorPosition, env: LocalScope,  withTypeParameters: Boolean)(implicit root: TypedAst.Root): Iterable[Completion] = {
    if (qn.namespace.nonEmpty)
      root.enums.values.collect{
        case enum if CompletionUtils.isAvailable(enum) && CompletionUtils.matchesName(enum.sym, qn, qualified = true) =>
          EnumCompletion(enum, range, ap, qualified = true, inScope = true, withTypeParameters = withTypeParameters)
      }
    else
      root.enums.values.collect({
        case enum if CompletionUtils.isAvailable(enum) && CompletionUtils.matchesName(enum.sym, qn, qualified = false) =>
          EnumCompletion(enum, range, ap, qualified = false, inScope = inScope(enum, env), withTypeParameters = withTypeParameters)
      })
  }

  /**
   * Checks if the definition is in scope.
   * If we can find the definition in the scope or the definition is in the root namespace, it is in scope.
   */
  private def inScope(decl: TypedAst.Enum, scope: LocalScope): Boolean = {
    val thisName = decl.sym.toString
    val isResolved = scope.m.values.exists(_.exists {
      case Resolution.Declaration(Enum(_, _, _, thatName, _, _, _, _)) => thisName == thatName.toString
      case _ => false
    })
    val isRoot = decl.sym.namespace.isEmpty
    isRoot || isResolved
  }
}
