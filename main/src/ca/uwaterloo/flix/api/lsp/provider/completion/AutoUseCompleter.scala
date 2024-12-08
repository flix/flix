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
import ca.uwaterloo.flix.api.lsp.provider.completion.CompletionUtils.{filterDefsByScope, fuzzyMatch, shouldComplete}
import ca.uwaterloo.flix.language.ast.NamedAst.Declaration.{Effect, Enum}
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.ast.shared.{AnchorPosition, LocalScope, Resolution}
import ca.uwaterloo.flix.language.errors.ResolutionError

object AutoUseCompleter {

  /**
    * Returns a list of auto-use completions to complete the name and use the flix construct.
    * The completions should fit in an expression context.
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
    if (err.qn.namespace.idents.nonEmpty) return Nil
    mkDefCompletions(err.qn.ident.name, err.env, err.ap)
  }

  /**
    * Returns a list of auto-use completions to complete the name and use the flix construct.
    * The completions should fit in a type context.
    */
  def getCompletions(err: ResolutionError.UndefinedType)(implicit root: TypedAst.Root): Iterable[Completion] = {
    if (!shouldComplete(err.qn.ident.name)) return Nil
    if (err.qn.namespace.idents.nonEmpty) return Nil
    mkEffCompletions(err.qn.ident.name, err.env, err.ap) ++ mkEnumCompletions(err.qn.ident.name, err.env, err.ap)
  }

  /**
    * Returns a list of completions for effects.
    */
  private def mkEffCompletions(word: String, env: LocalScope, ap: AnchorPosition)(implicit root: TypedAst.Root): Iterable[Completion] =
    root.effects.collect{
        case (sym, eff) if fuzzyMatch(word, sym.name) && checkEffScope(eff, env) => Completion.AutoUseCompletion(sym.name, sym.toString, eff.doc.text, ap)
    }

  /**
    * Checks if the effect is in the scope.
    */
  private def checkEffScope(eff: TypedAst.Effect, scope: LocalScope): Boolean = {
    val thisName = eff.sym.toString
    scope.m.values.forall(_.exists {
      case Resolution.Declaration(Effect(_, _, _, thatName, _, _)) => thisName != thatName.toString
      case _ => true
    })
  }

  /**
   * Returns a list of completions for enums.
   */
  private def mkEnumCompletions(word: String, env: LocalScope, ap: AnchorPosition)(implicit root: TypedAst.Root): Iterable[Completion] =
    root.enums.collect{
      case (sym, enum) if  fuzzyMatch(word, sym.name) && checkEnumScope(enum, env) => Completion.AutoUseCompletion(sym.name, sym.toString, enum.doc.text, ap)
    }

  /**
   * Checks if the enum is in the scope.
   */
  private def checkEnumScope(eff: TypedAst.Enum, scope: LocalScope): Boolean = {
    val thisName = eff.sym.toString
    scope.m.values.forall(_.exists {
      case Resolution.Declaration(Enum(_, _, _, thatName, _, _, _, _)) => thisName != thatName.toString
      case _ => true
    })
  }

  /**
    * Returns a List of Completion for defs.
    */
  private def mkDefCompletions(word: String, env: LocalScope, ap: AnchorPosition)(implicit root: TypedAst.Root): Iterable[AutoUseDefCompletion] = {
    filterDefsByScope(word, root, env, whetherInScope = false)
      .map(Completion.AutoUseDefCompletion(_, ap))
  }
}
