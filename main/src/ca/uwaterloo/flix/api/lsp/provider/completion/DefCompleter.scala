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

import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.DefCompletion
import ca.uwaterloo.flix.api.lsp.provider.completion.CompletionUtils.fuzzyMatch
import ca.uwaterloo.flix.language.ast.NamedAst.Declaration.Def
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.ast.shared.{LocalScope, Resolution}
import ca.uwaterloo.flix.language.errors.ResolutionError

object DefCompleter {
  /**
    * Returns a List of Completion for definitions.
    * Whether the returned completions are qualified is based on whether the UndefinaedName is qualified.
    * When providing completions for unqualified defs that is not in scope, we will also automatically use the def.
    */
  def getCompletions(err: ResolutionError.UndefinedName, namespace: List[String], ident: String)(implicit root: TypedAst.Root): Iterable[Completion] ={
    if (namespace.nonEmpty)
      root.defs.values.collect{
        case decl if matchesDef(decl, namespace, ident, err.loc.source.name, qualified = true) =>
          DefCompletion(decl, err.ap, qualified = true, inScope = true)
      }
    else
      root.defs.values.collect{
        case decl if matchesDef(decl, namespace, ident, err.loc.source.name, qualified = false) =>
          DefCompletion(decl, err.ap, qualified = false, inScope = inScope(decl, err.env))
      }
  }

  /**
    * Checks if the definition is in scope.
    * If we can find the definition in the scope or the definition is in the root namespace, it is in scope.
    */
  private def inScope(decl: TypedAst.Def, scope: LocalScope): Boolean = {
    val thisName = decl.sym.toString
    val isResolved = scope.m.values.exists(_.exists {
      case Resolution.Declaration(Def(thatName, _, _, _)) => thisName == thatName.toString
      case _ => false
    })
    val isRoot = decl.sym.namespace.isEmpty
    isRoot || isResolved
  }

  /**
    * Checks if the definition matches the QName.
    * Names should match and the definition should be available.
    */
  private def matchesDef(decl: TypedAst.Def, namespace: List[String], ident: String, uri: String, qualified: Boolean): Boolean = {
    val isPublic = decl.spec.mod.isPublic && !decl.spec.ann.isInternal
    val isInFile = decl.sym.loc.source.name == uri
    val isMatch = if (qualified)
      fuzzyMatch(namespace.mkString(".") + "." + ident, decl.sym.toString)
    else
      fuzzyMatch(ident, decl.sym.name)
    isMatch && (isPublic || isInFile)
  }
}
