/*
 * Copyright 2024 Chenhao Gao
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

import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.AutoUseDefCompletion
import ca.uwaterloo.flix.api.lsp.provider.completion.CompletionUtils.shouldComplete
import ca.uwaterloo.flix.language.ast.Name.QName
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.ast.NamedAst.Declaration.Def
import ca.uwaterloo.flix.language.ast.shared.{AnchorPosition, LocalScope, Resolution}
import ca.uwaterloo.flix.language.errors.ResolutionError

object AutoUseCompleter {

  /**
    * Returns a list of auto-use completions to complete the name and use the flix construct.
    *
    * Example:
    *  If we have an undefined name which is the prefix of an existing and unused flix function
    *
    *  {{{
    *    mod A{
    *      pub def bar(): Unit = ???
    *    }
    *    ...
    *    let s = ba // undefined name error
    *  }}}
    *
    *  We propose to complete the name to `bar` and use the function `A.bar`
    *
    *  {{{
    *    use A.bar;
    *    ...
    *    let s = bar
    *  }}}
    */
  def getCompletions(err: ResolutionError.UndefinedName)(implicit root: TypedAst.Root): Iterable[Completion] = {
    if (!shouldComplete(err.qn.ident.name)) return Nil
    defCompletions(err.qn, err.env, err.ap)
  }

  /**
    * Returns a List of Completion for defs.
    */
  private def defCompletions(qn: QName, env: LocalScope, ap: AnchorPosition)(implicit root: TypedAst.Root): Iterable[AutoUseDefCompletion] = {
    if (qn.namespace.idents.nonEmpty)
      return Nil
    root.defs.values
      .filter(decl => matchesDef(decl, qn.ident.name) && outOfScope(decl, env))
      .map(Completion.AutoUseDefCompletion(_,ap))
  }

  /**
    * Returns `true` if the given definition `decl` is not in the given `scope`.
    */
  private def outOfScope(decl: TypedAst.Def, scope: LocalScope): Boolean = {
    val thisName = decl.sym.toString
    scope.m.values.forall(_.exists {
      case Resolution.Declaration(Def(thatName, _, _, _)) =>
        thisName != thatName.toString
      case _ => true
    })
  }

  /**
    * Returns `true` if the given definition `decl` should be included in the suggestions.
    */
  private def matchesDef(decl: TypedAst.Def, word: String): Boolean = {
    def isInternal(decl: TypedAst.Def): Boolean = decl.spec.ann.isInternal

    val isPublic = decl.spec.mod.isPublic && !isInternal(decl)
    val isMatch = decl.sym.text.startsWith(word)

    isMatch && isPublic
  }

}
