/*
 * Copyright 2022 Paul Butcher, Lukas Rønn
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

import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.SigCompletion
import ca.uwaterloo.flix.language.ast.NamedAst.Declaration.Sig
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.ast.shared.{AnchorPosition, LocalScope, Resolution}
import ca.uwaterloo.flix.language.errors.ResolutionError

object SignatureCompleter {
  /**
    * Returns a List of Completion for Sig for UndefinedName.
    */
  def getCompletions(err: ResolutionError.UndefinedName, namespace: List[String], ident: String)(implicit root: TypedAst.Root): Iterable[Completion] = {
    getCompletions(err.loc.source.name, err.ap, err.env, namespace, ident)
  }

  private def getCompletions(uri: String, ap: AnchorPosition, env: LocalScope, namespace: List[String], ident: String)(implicit root: TypedAst.Root): Iterable[Completion] = {
    if (namespace.nonEmpty)
      root.sigs.values.collect{
        case sig if matchesSig(sig, namespace, ident, uri, qualified = true) =>
          SigCompletion(sig, ap, qualified = true, inScope = true)
      }
    else
      root.sigs.values.collect{
        case sig if matchesSig(sig, namespace, ident, uri, qualified = false) =>
          SigCompletion(sig, ap, qualified = false, inScope = inScope(sig, env))
      }
  }

  private def inScope(sig: TypedAst.Sig, scope: LocalScope): Boolean = {
    val thisName = sig.sym.toString
    val isResolved = scope.m.values.exists(_.exists {
      case Resolution.Declaration(Sig(thatName, _, _, _)) => thisName == thatName.toString
      case _ => false
    })
    val isRoot = sig.sym.namespace.isEmpty
    isRoot || isResolved
  }

  /**
    * Returns `true` if the given trait signature `sig` should be included in the suggestions.
    */
  private def matchesSig(sig: TypedAst.Sig, namespace: List[String], ident: String, uri: String, qualified: Boolean): Boolean = {
    val isPublic = sig.spec.mod.isPublic && !sig.spec.ann.isInternal
    val isInFile = sig.sym.loc.source.name == uri
    val isMatch = if (qualified)
      CompletionUtils.matchesQualifiedName(sig.sym.namespace, sig.sym.name, namespace, ident)
    else
      CompletionUtils.fuzzyMatch(ident, sig.sym.name)
    isMatch && (isPublic || isInFile)
  }
}
