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
import ca.uwaterloo.flix.language.ast.NamedAst.Declaration.{Case, Enum}
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
        fullyQualifiedCompletion(uri, ap, namespace, ident) ++ partiallyQualifiedCompletions(uri, ap, env, namespace, ident)
    else
      root.enums.values.flatMap(enm =>
        enm.cases.values.collect{
          case tag if matchesTag(enm, tag, namespace, ident, uri, qualified = false) =>
            EnumTagCompletion(tag, Nil, ap, qualified = false, inScope = inScope(tag, env))
        }
      )
  }

  private def fullyQualifiedCompletion(uri: String, ap: AnchorPosition, namespace: List[String], ident: String)(implicit root: TypedAst.Root): Iterable[Completion] = {
    root.enums.values.flatMap(enm =>
      enm.cases.values.collect{
        case tag if matchesTag(enm, tag, namespace, ident, uri, qualified = true) =>
          EnumTagCompletion(tag, Nil,  ap, qualified = true, inScope = true)
      }
    )
  }

  /**
    * Returns a List of Completion for Tag for partially qualified names.
    *
    * Example:
    *   - If `Foo.Bar.Color.Red` is fully qualified, then `Color.Red` is partially qualified
    *
    * We need to first find the fully qualified namespace by looking up the local environment, then use it to provide completions.
    */
  private def partiallyQualifiedCompletions(uri: String, ap: AnchorPosition, env: LocalScope, namespace: List[String], ident: String)(implicit root: TypedAst.Root): Iterable[Completion] = {
    env.m.getOrElse(namespace.mkString("."), Nil).collect {
      case Resolution.Declaration(Enum(_, _, _, enumSym, _, _, _, _)) =>
          root.enums.get(enumSym).map(enm =>
            enm.cases.values.collect {
              case tag if matchesTag(enm, tag, namespace, ident, uri, qualified = false) =>
                EnumTagCompletion(tag, namespace, ap, qualified = true, inScope = true)
            }
          ).getOrElse(Nil)
    }.flatten
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
    *
    * For visibility, we just need to check the parent enum.
    */
  private def matchesTag(enm: TypedAst.Enum, tag: TypedAst.Case, namespace: List[String], ident: String, uri: String, qualified: Boolean): Boolean = {
    val isPublic = enm.mod.isPublic && !enm.ann.isInternal
    val isInFile = enm.loc.source.name == uri
    val isMatch = if (qualified)
      CompletionUtils.matchesQualifiedName(tag.sym.namespace, tag.sym.name, namespace, ident)
    else
      CompletionUtils.fuzzyMatch(ident, tag.sym.name)
    isMatch && (isPublic || isInFile)
  }
}
