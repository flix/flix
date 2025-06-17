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

import ca.uwaterloo.flix.api.lsp.Range
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.ModuleCompletion
import ca.uwaterloo.flix.language.ast.NamedAst.Declaration.*
import ca.uwaterloo.flix.language.ast.shared.{AnchorPosition, LocalScope, Resolution}
import ca.uwaterloo.flix.language.ast.{Name, Symbol, TypedAst}

object ModuleCompleter {
  /**
    * Returns a List of Completion for modules.
    * Whether the returned completions are qualified is based on whether the name in the error is qualified.
    * When providing completions for unqualified enums that is not in scope, we will also automatically use the module.
    */
  def getCompletions(qn: Name.QName, range: Range, ap: AnchorPosition, scp: LocalScope)(implicit root: TypedAst.Root): Iterable[Completion] = {
    if (qn.namespace.nonEmpty)
      root.modules.keys.collect {
        case module if module.ns.nonEmpty && matchesModule(module, qn, qualified = true) =>
          ModuleCompletion(module, range, Priority.High(0), ap, qualified = true, inScope = true)
      }
    else
      root.modules.keys.collect({
        case module if module.ns.nonEmpty && matchesModule(module, qn, qualified = false) =>
          val s = inScope(module, scp)
          val priority = if (s) Priority.High(0) else Priority.Lower(0)
          ModuleCompletion(module, range, priority, ap, qualified = false, inScope = s)
      })
  }

  /**
    * Checks if the module is in scope.
    * If we can find the module in the scope or the module is in the root namespace, it is in scope.
    *
    * Note: module.ns is required to be non-empty.
    */
  private def inScope(module: Symbol.ModuleSym, scope: LocalScope): Boolean = {
    val thisName = module.toString
    val isResolved = scope.m.values.exists(_.exists {
      case Resolution.Declaration(Namespace(thatName, _, _, _)) => thatName.toString == thisName
      case Resolution.Declaration(Trait(_, _, _, thatName, _, _, _, _, _, _)) => thatName.toString == thisName
      case Resolution.Declaration(Enum(_, _, _, thatName, _, _, _, _)) => thatName.toString == thisName
      case Resolution.Declaration(Struct(_, _, _, thatName, _, _, _)) => thatName.toString == thisName
      case Resolution.Declaration(Effect(_, _, _, thatName, _, _)) => thatName.toString == thisName
      case _ => false
    })
    val isRoot = module.ns.length <= 1
    isRoot || isResolved
  }

  /**
    * Checks if the module name matches the QName.
    *
    * Note: module.ns is required to be non-empty.
    */
  private def matchesModule(module: Symbol.ModuleSym, qn: Name.QName, qualified: Boolean): Boolean = {
    if (qualified) {
      CompletionUtils.matchesQualifiedName(module.ns.dropRight(1), module.ns.last, qn)
    } else
      CompletionUtils.fuzzyMatch(qn.ident.name, module.ns.last)
  }

}
