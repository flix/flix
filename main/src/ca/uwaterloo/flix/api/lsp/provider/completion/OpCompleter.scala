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

import ca.uwaterloo.flix.api.lsp.Range
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.OpCompletion
import ca.uwaterloo.flix.language.ast.NamedAst.Declaration.{Effect, Namespace, Op}
import ca.uwaterloo.flix.language.ast.shared.{AnchorPosition, LocalScope, Resolution}
import ca.uwaterloo.flix.language.ast.{Name, Symbol, TypedAst}

object OpCompleter {
  /**
    * Returns a List of Completion for Op for UndefinedType.
    */
  def getCompletionsInHandler(qn: Name.QName, range: Range, ap: AnchorPosition)(implicit root: TypedAst.Root): Iterable[OpCompletion] = {
    val effSym = Symbol.mkEffectSym(qn.namespace.toString)
    root.effects.get(effSym).toList.flatMap (eff =>
      eff.ops.collect {
        case op if CompletionUtils.isAvailable(eff) && CompletionUtils.matchesName(op.sym, qn, qualified = false) =>
          OpCompletion(op, "", range, ap, qualified = false, inScope = true, isHandler = true)
      }
    )
  }

  /**
    * Returns a List of Completion for Op for UndefinedName.
    */
  def getCompletions(qn: Name.QName, range: Range, ap: AnchorPosition, env: LocalScope)(implicit root: TypedAst.Root): Iterable[OpCompletion] = {
    // If isHandler is true, there is no need to qualify the op.
    if (qn.namespace.nonEmpty) {
      fullyQualifiedCompletion(qn, range, ap) ++ partiallyQualifiedCompletions(qn, range, ap ,env)
    } else {
      root.effects.values.flatMap(eff =>
        eff.ops.collect {
          case op if CompletionUtils.isAvailable(eff) && CompletionUtils.matchesName(op.sym, qn, qualified = false) =>
            OpCompletion(op, "", range, ap, qualified = false, inScope(op, env), isHandler = false)
        }
      )
    }
  }

  /**
    * Returns a List of Completion for Op for fully qualified names.
    *
    * We assume the user is trying to type a fully qualified name and will only match against fully qualified names.
    */
  private def fullyQualifiedCompletion(qn: Name.QName, range: Range, ap: AnchorPosition)(implicit root: TypedAst.Root): Iterable[OpCompletion] = {
    val effSym = Symbol.mkEffectSym(qn.namespace.toString)
    root.effects.get(effSym).toList.flatMap(eff =>
      eff.ops.collect {
        case op if CompletionUtils.isAvailable(eff) && CompletionUtils.matchesName(op.sym, qn, qualified = false) =>
          OpCompletion(op, "", range, ap, qualified = true, inScope = true, isHandler = false)
      }
    )
  }

  /**
    * Returns a List of Completion for Op for partially qualified names.
    *
    * Example:
    *   - If `Foo.Bar.Addable.add` is fully qualified, then `Addable.add` is partially qualified
    *
    * We assume the user is trying to type a partially qualified name and will only match against partially qualified names.
    */
  private def partiallyQualifiedCompletions(qn: Name.QName, range: Range, ap: AnchorPosition, env: LocalScope)(implicit root: TypedAst.Root): Iterable[OpCompletion] = {
    val fullyQualifiedNamespaceHead = env.resolve(qn.namespace.idents.head.name) match {
      case Some(Resolution.Declaration(Effect(_, _, _, name, _, _))) => name.toString
      case Some(Resolution.Declaration(Namespace(name, _, _, _))) => name.toString
      case _ => return Nil
    }
    val namespaceTail = qn.namespace.idents.tail.map(_.name).mkString(".")
    val fullyQualifiedEffect = if (namespaceTail.isEmpty) fullyQualifiedNamespaceHead else s"$fullyQualifiedNamespaceHead.$namespaceTail"
    for {
      eff <- root.effects.get(Symbol.mkEffectSym(fullyQualifiedEffect)).toList
      op <- eff.ops
      if CompletionUtils.isAvailable(eff) && CompletionUtils.matchesName(op.sym, qn, qualified = false)
    } yield OpCompletion(op, qn.namespace.toString, range, ap, qualified = true, inScope = true, isHandler = false)
  }

  private def inScope(op: TypedAst.Op, scope: LocalScope): Boolean = {
    val thisName = op.sym.toString
    val isResolved = scope.m.values.exists(_.exists {
      case Resolution.Declaration(Op(thatName, _, _)) => thisName == thatName.toString
      case _ => false
    })
    val isRoot = op.sym.namespace.isEmpty
    isRoot || isResolved
  }
}
