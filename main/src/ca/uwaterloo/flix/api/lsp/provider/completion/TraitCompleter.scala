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
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.{InstanceCompletion, TraitCompletion}
import ca.uwaterloo.flix.language.ast.NamedAst.Declaration.Trait
import ca.uwaterloo.flix.language.ast.shared.{AnchorPosition, LocalScope, Resolution, TraitUsageKind}
import ca.uwaterloo.flix.language.ast.{Name, TypedAst}

object TraitCompleter {

  /**
    * List of derivable traits.
    */
  private val derivable_traits = List("Eq", "Order", "ToString", "Sendable", "Coerce")

  /**
    * Returns a List of Completion for traits.
    * Whether the returned completions are qualified is based on whether the name in the error is qualified.
    * When providing completions for unqualified enums that is not in scope, we will also automatically use the enum.
    */
  def getCompletions(qn: Name.QName, traitUsageKind: TraitUsageKind, range: Range, ap: AnchorPosition, scp: LocalScope)(implicit root: TypedAst.Root): Iterable[Completion] = {
    if (qn.namespace.nonEmpty)
      root.traits.values.flatMap {
        case trt if CompletionUtils.isAvailable(trt) && CompletionUtils.matchesName(trt.sym, qn, qualified = true) =>
          getTraitCompletions(trt, traitUsageKind, range, ap, qualified = true, inScope = true)
        case _ => Nil
      }
    else
      root.traits.values.flatMap({
        case trt if CompletionUtils.isAvailable(trt) && CompletionUtils.matchesName(trt.sym, qn, qualified = false) =>
          getTraitCompletions(trt, traitUsageKind, range, ap, qualified = false, inScope = inScope(trt, scp))
        case _ => Nil
      })
  }

  /**
    * Returns a list of completions for the given trait.
    * If the trait is derivable, we will only provide completions for derivation.
    */
  private def getTraitCompletions(trt: TypedAst.Trait, traitUsageKind: TraitUsageKind, range: Range, ap: AnchorPosition, qualified: Boolean, inScope: Boolean): List[Completion] = {
    traitUsageKind match {
      case TraitUsageKind.Expr =>
        TraitCompletion(trt, range, ap, qualified = qualified, inScope = inScope, withTypeParameter = false) :: Nil
      case TraitUsageKind.Constraint =>
        TraitCompletion(trt, range, ap, qualified = qualified, inScope = inScope, withTypeParameter = true) :: Nil
      case TraitUsageKind.Derivation if derivable_traits.contains(trt.sym.name) =>
        TraitCompletion(trt, range, ap, qualified = qualified, inScope = inScope, withTypeParameter = false) :: Nil
      case TraitUsageKind.Derivation =>
        Nil
      case TraitUsageKind.Implementation =>
       InstanceCompletion(trt, range, ap, qualified = qualified, inScope = inScope) :: Nil
    }
  }

  /**
    * Checks if the definition is in scope.
    * If we can find the definition in the scope or the definition is in the root namespace, it is in scope.
    */
  private def inScope(struct: TypedAst.Trait, scope: LocalScope): Boolean = {
    val thisName = struct.sym.toString
    val isResolved = scope.m.values.exists(_.exists {
      case Resolution.Declaration(Trait(_, _, _, thatName, _, _, _, _, _, _)) => thisName == thatName.toString
      case _ => false
    })
    val isRoot = struct.sym.namespace.isEmpty
    isRoot || isResolved
  }
}
