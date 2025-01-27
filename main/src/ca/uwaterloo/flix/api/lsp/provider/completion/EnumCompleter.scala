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

import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.EnumCompletion
import ca.uwaterloo.flix.api.lsp.provider.completion.CompletionUtils.{fuzzyMatch, matchesQualifiedName}
import ca.uwaterloo.flix.language.ast.NamedAst.Declaration.Enum
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.ast.shared.{AnchorPosition, LocalScope, Resolution}
import ca.uwaterloo.flix.language.errors.ResolutionError

object EnumCompleter {
  /**
    * Returns a List of Completion for enums.
    * Whether the returned completions are qualified is based on whether the name in the error is qualified.
    * When providing completions for unqualified enums that is not in scope, we will also automatically use the enum.
    */
  def getCompletions(err: ResolutionError.UndefinedName, namespace: List[String], ident: String)(implicit root: TypedAst.Root): Iterable[Completion] = {
    getCompletions(err.qn.loc.source.name, err.ap, err.env, namespace, ident)
  }

  def getCompletions(err: ResolutionError.UndefinedTag, namespace: List[String], ident: String)(implicit root: TypedAst.Root): Iterable[Completion] = {
    getCompletions(err.qn.loc.source.name, err.ap, err.env, namespace, ident)
  }

  def getCompletions(err: ResolutionError.UndefinedType, namespace: List[String], ident: String)(implicit root: TypedAst.Root): Iterable[Completion] = {
    getCompletions(err.qn.loc.source.name, err.ap, err.env, namespace, ident)
  }

  private def getCompletions(uri: String, ap: AnchorPosition, env: LocalScope, namespace: List[String], ident: String)(implicit root: TypedAst.Root): Iterable[Completion] = {
    if (namespace.nonEmpty)
      root.enums.values.collect{
        case enum if matchesEnum(enum, namespace, ident, uri, qualified = true) =>
          EnumCompletion(enum, ap, qualified = true, inScope = true)
      }
    else
      root.enums.values.collect({
        case enum if matchesEnum(enum, namespace, ident, uri, qualified = false) =>
          EnumCompletion(enum, ap, qualified = false, inScope = inScope(enum, env))
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

  /**
   * Checks if the definition matches the QName.
   * Names should match and the definition should be available.
   */
  private def matchesEnum(enm: TypedAst.Enum, namespace: List[String], ident: String, uri: String, qualified: Boolean): Boolean = {
    val isPublic = enm.mod.isPublic && !enm.ann.isInternal
    val isInFile = enm.sym.loc.source.name == uri
    val isMatch = if (qualified)
      matchesQualifiedName(enm.sym.namespace, enm.sym.name, namespace, ident)
    else
      fuzzyMatch(ident, enm.sym.name)
    isMatch && (isPublic || isInFile)
  }
}
