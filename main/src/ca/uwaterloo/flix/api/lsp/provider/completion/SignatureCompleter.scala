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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.lsp.Range
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.SigCompletion
import ca.uwaterloo.flix.language.ast.NamedAst.Declaration.{Namespace, Sig, Trait}
import ca.uwaterloo.flix.language.ast.shared.{AnchorPosition, LocalScope, Resolution}
import ca.uwaterloo.flix.language.ast.{Name, Symbol, TypedAst}

object SignatureCompleter {
  /**
    * Returns a List of Completion for Sig for UndefinedName.
    */
  def getCompletions(qn: Name.QName, range: Range, ap: AnchorPosition, scp: LocalScope)(implicit root: TypedAst.Root, flix: Flix): Iterable[Completion] = {
    if (qn.namespace.nonEmpty) {
      fullyQualifiedCompletion(qn, range, ap) ++ partiallyQualifiedCompletions(qn, range, ap, scp)
    } else
      root.traits.values.flatMap(trt =>
        trt.sigs.collect {
          case sig if CompletionUtils.isAvailable(trt) && CompletionUtils.matchesName(sig.sym, qn, qualified = false) =>
            SigCompletion(sig, "", range, ap, qualified = false, inScope = inScope(sig, scp))
        }
      )
  }

  /**
    * Returns a List of Completion for Sig for fully qualified names.
    *
    * We assume the user is trying to type a fully qualified name and will only match against fully qualified names.
    */
  private def fullyQualifiedCompletion(qn: Name.QName, range: Range, ap: AnchorPosition)(implicit root: TypedAst.Root): Iterable[Completion] = {
    root.traits.values.flatMap(trt =>
      trt.sigs.collect {
        case sig if CompletionUtils.isAvailable(trt) && CompletionUtils.matchesName(sig.sym, qn, qualified = true) =>
          SigCompletion(sig, "", range, ap, qualified = true, inScope = true)
      }
    )
  }

  /**
    * Returns a List of Completion for Sig for partially qualified names.
    *
    * Example:
    *   - If `Foo.Bar.Addable.add` is fully qualified, then `Addable.add` is partially qualified
    *
    * We assume the user is trying to type a partially qualified name and will only match against partially qualified names.
    */
  private def partiallyQualifiedCompletions(qn: Name.QName, range: Range, ap: AnchorPosition, scp: LocalScope)(implicit root: TypedAst.Root): Iterable[Completion] = {
    val fullyQualifiedNamespaceHead = scp.resolve(qn.namespace.idents.head.name) match {
      case Some(Resolution.Declaration(Trait(_, _, _, name, _, _, _, _, _, _))) => name.toString
      case Some(Resolution.Declaration(Namespace(name, _, _, _))) => name.toString
      case _ => return Nil
    }
    val namespaceTail = qn.namespace.idents.tail.map(_.name).mkString(".")
    val fullyQualifiedTrait = if (namespaceTail.isEmpty) fullyQualifiedNamespaceHead else s"$fullyQualifiedNamespaceHead.$namespaceTail"
    for {
      trt <- root.traits.get(Symbol.mkTraitSym(fullyQualifiedTrait)).toList
      sig <- trt.sigs
      if CompletionUtils.isAvailable(trt) && CompletionUtils.matchesName(sig.sym, qn, qualified = false)
    } yield SigCompletion(sig, qn.namespace.toString, range, ap, qualified = true, inScope = true)
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
}
