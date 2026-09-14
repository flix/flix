/*
 * Copyright 2022 Paul Butcher, Lukas Rønn
 * Copyright 2025 Chenhao Gao
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.api.lsp.provider.completion

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.OpCompletion
import ca.uwaterloo.flix.api.lsp.{Position, Range}
import ca.uwaterloo.flix.language.ast.NamedAst.Declaration.{Effect, Mod, Op}
import ca.uwaterloo.flix.language.ast.shared.{AnchorPosition, LocalScope, Resolution}
import ca.uwaterloo.flix.language.ast.{Name, Symbol, TypedAst}

object OpCompleter {
  /**
    * Returns a List of Completion for Op for UndefinedName.
    */
  def getCompletions(uri: String, pos: Position, qn: Name.QName, range: Range, ap: AnchorPosition, scp: LocalScope)(implicit root: TypedAst.Root, flix: Flix): Iterable[OpCompletion] = {
    val ectx = ExprContext.getExprContext(uri, pos)
    if (qn.namespace.nonEmpty) {
      fullyQualifiedCompletion(qn, range, ap, ectx) ++ partiallyQualifiedCompletions(qn, range, ap, scp, ectx)
    } else {
      root.effects.values.flatMap(eff =>
        eff.ops.collect {
          case op if CompletionUtils.isAvailable(eff) && CompletionUtils.matchesName(op.sym, qn, qualified = false) =>
            val s = inScope(op, scp)
            val priority = if (s) Priority.High(0) else Priority.Lower(0)
            OpCompletion(op, "", range, priority, ap, qualified = false, s, ectx)
        }
      )
    }
  }

  /**
    * Returns a List of Completion for Op for fully qualified names.
    *
    * We assume the user is trying to type a fully qualified name and will only match against fully qualified names.
    */
  private def fullyQualifiedCompletion(qn: Name.QName, range: Range, ap: AnchorPosition, ectx: ExprContext)(implicit root: TypedAst.Root): Iterable[OpCompletion] = {
    val effSym = Symbol.mkEffSym(qn.namespace.toString)
    root.effects.get(effSym).toList.flatMap(eff =>
      eff.ops.collect {
        case op if CompletionUtils.isAvailable(eff) && CompletionUtils.matchesName(op.sym, qn, qualified = false) =>
          OpCompletion(op, "", range, Priority.High(0), ap, qualified = true, inScope = true, ectx)
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
  private def partiallyQualifiedCompletions(qn: Name.QName, range: Range, ap: AnchorPosition, scp: LocalScope, ectx: ExprContext)(implicit root: TypedAst.Root): Iterable[OpCompletion] = {
    val fullyQualifiedNamespaceHead = scp.resolve(qn.namespace.idents.head.name) match {
      case Some(Resolution.Declaration(Effect(_, _, _, name, _, _, _))) => name.toString
      case Some(Resolution.Declaration(Mod(_, _, _, name, _, _, _, _))) => name.toString
      case _ => return Nil
    }
    val namespaceTail = qn.namespace.idents.tail.map(_.name).mkString(".")
    val fullyQualifiedEffect = if (namespaceTail.isEmpty) fullyQualifiedNamespaceHead else s"$fullyQualifiedNamespaceHead.$namespaceTail"
    for {
      eff <- root.effects.get(Symbol.mkEffSym(fullyQualifiedEffect)).toList
      op <- eff.ops
      if CompletionUtils.isAvailable(eff) && CompletionUtils.matchesName(op.sym, qn, qualified = false)
    } yield OpCompletion(op, qn.namespace.toString, range, Priority.High(0), ap, qualified = true, inScope = true, ectx)
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
