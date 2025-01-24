/*
 * Copyright 2023 Lukas Rønn
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

import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.EnumTagCompletion
import ca.uwaterloo.flix.language.ast.NamedAst.Declaration.Case
import ca.uwaterloo.flix.language.ast.shared.{AnchorPosition, LocalScope, Resolution}
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.errors.ResolutionError

object EnumTagCompleter {
  /**
    * Returns a List of Completion for Tag for UndefinedName.
    */
  def getCompletions(err: ResolutionError.UndefinedName, namespace: List[String], ident: String)(implicit root: TypedAst.Root): Iterable[Completion] = {
    getCompletions(err.loc.source.name, err.ap, err.env, namespace, ident)
  }

  /**
    * Returns a List of Completion for Tag for UndefinedTag.
    */
  def getCompletions(err: ResolutionError.UndefinedTag, namespace: List[String], ident: String)(implicit root: TypedAst.Root): Iterable[Completion] = {
    getCompletions(err.loc.source.name, err.ap, err.env, namespace, ident)
  }

  private def getCompletions(uri: String, ap: AnchorPosition, env: LocalScope, namespace: List[String], ident: String)(implicit root: TypedAst.Root): Iterable[Completion] = {
    if (namespace.nonEmpty)
      root.enums.values.flatMap(_.cases.values).collect{
        case tag if matchesTag(tag, namespace, ident, uri, qualified = true) =>
          EnumTagCompletion(tag, ap, qualified = true, inScope = true)
      }
    else
      root.enums.values.flatMap(_.cases.values).collect{
        case tag if matchesTag(tag, namespace, ident, uri, qualified = false) =>
          EnumTagCompletion(tag, ap, qualified = false, inScope = inScope(tag, env))
      }
  }

  private def inScope(tag: TypedAst.Case, scope: LocalScope): Boolean = {
    val thisName = tag.sym.toString
    val isResolved = scope.m.values.exists(_.exists {
      case Resolution.Declaration(Case(thatName, _, _)) => thisName == thatName.toString
      case _ => false
    })
    val isRoot = tag.sym.namespace.isEmpty
    isRoot || isResolved
  }

  /**
    * Returns `true` if the given signature `sig` should be included in the suggestions.
    */
  private def matchesTag(tag: TypedAst.Case, namespace: List[String], ident: String, uri: String, qualified: Boolean): Boolean =
    if (qualified)
      CompletionUtils.matchesQualifiedName(tag.sym.namespace, tag.sym.name, namespace, ident)
    else
      CompletionUtils.fuzzyMatch(ident, tag.sym.name)
}
