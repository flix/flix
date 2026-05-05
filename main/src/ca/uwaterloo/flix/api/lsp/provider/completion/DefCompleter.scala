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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.DefCompletion
import ca.uwaterloo.flix.api.lsp.{Position, Range}
import ca.uwaterloo.flix.language.ast.NamedAst.Declaration.{Def, Effect, Mod}
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.ast.shared.{AnchorPosition, LocalScope, Resolution}
import ca.uwaterloo.flix.language.ast.{Name, TypedAst}

object DefCompleter {
  /**
    * Returns a List of Completion for definitions.
    * Whether the returned completions are qualified is based on whether the UndefinedName is qualified.
    * When providing completions for unqualified defs that is not in scope, we will also automatically use the def.
    */
  def getCompletions(uri: String, pos: Position, qn: Name.QName, range: Range, ap: AnchorPosition, scp: LocalScope)(implicit root: Root, flix: Flix): Iterable[Completion] = {
    val ectx = ExprContext.getExprContext(uri, pos)

    if (qn.namespace.nonEmpty) {
      fullyQualifiedCompletion(qn, range, ap, ectx) ++ partiallyQualifiedCompletions(qn, range, ap, scp, ectx)
    } else {
      root.defs.values.collect {
        case decl if CompletionUtils.isAvailable(decl.spec) && CompletionUtils.matchesName(decl.sym, qn, qualified = false) =>
          val s = inScope(decl, scp)
          val priority = if (s) Priority.High(0) else Priority.Lower(0)
          DefCompletion(decl, "", range, priority, ap, qualified = false, inScope = s, ectx)
      }
    }
  }

  /**
    * Returns a List of Completion for Def for fully qualified names.
    */
  private def fullyQualifiedCompletion(qn: Name.QName, range: Range, ap: AnchorPosition, ectx: ExprContext)(implicit root: Root): Iterable[Completion] = {
    root.defs.values.collect {
      case decl if CompletionUtils.isAvailable(decl.spec) && CompletionUtils.matchesName(decl.sym, qn, qualified = true) =>
        DefCompletion(decl, "", range, Priority.High(0), ap, qualified = true, inScope = true, ectx)
    }
  }

  /**
    * Returns a List of Completion for Def for partially qualified names.
    *
    * Example:
    *   - If `Net.Http.get` is fully qualified, then `Http.get` is partially qualified
    */
  private def partiallyQualifiedCompletions(qn: Name.QName, range: Range, ap: AnchorPosition, scp: LocalScope, ectx: ExprContext)(implicit root: Root): Iterable[Completion] = {
    val fullyQualifiedNamespaceHead = scp.resolve(qn.namespace.idents.head.name) match {
      case Some(Resolution.Declaration(Mod(_, _, _, name, _, _, _, _))) => name.toString
      case Some(Resolution.Declaration(Effect(_, _, _, name, _, _, _))) => name.toString
      case _ => return Nil
    }
    val namespaceTail = qn.namespace.idents.tail.map(_.name).mkString(".")
    val fullyQualifiedModule = if (namespaceTail.isEmpty) fullyQualifiedNamespaceHead else s"$fullyQualifiedNamespaceHead.$namespaceTail"
    val moduleNamespace = fullyQualifiedModule.split('.').toList
    for {
      decl <- root.defs.values
      if CompletionUtils.isAvailable(decl.spec)
      if decl.sym.namespace == moduleNamespace
      if CompletionUtils.matchesName(decl.sym, qn, qualified = false)
    } yield DefCompletion(decl, qn.namespace.toString, range, Priority.High(0), ap, qualified = true, inScope = true, ectx)
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
}
