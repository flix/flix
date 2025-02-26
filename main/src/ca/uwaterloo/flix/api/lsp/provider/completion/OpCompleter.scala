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

import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.OpCompletion
import ca.uwaterloo.flix.language.ast.NamedAst.Declaration.Op
import ca.uwaterloo.flix.language.ast.{Name, TypedAst}
import ca.uwaterloo.flix.language.ast.shared.{AnchorPosition, LocalScope, Resolution}
import ca.uwaterloo.flix.language.errors.ResolutionError

object OpCompleter {
  /**
    * Returns a List of Completion for Op for UndefinedOp.
    */
  def getCompletions(err: ResolutionError.UndefinedOp)(implicit root: TypedAst.Root): Iterable[OpCompletion] = {
    val uri = err.loc.source.name
    root.effects.values.flatMap(eff =>
      eff.ops.collect {
        case op if CompletionUtils.isAvailable(eff) && CompletionUtils.matchesName(op.sym, err.qn, qualified = false) =>
          OpCompletion(op, err.ap, qualified = false, inScope = true, isHandler = true)
      }
    )
  }

  /**
    * Returns a List of Completion for Op for UndefinedName.
    */
  def getCompletions(err: ResolutionError.UndefinedName)(implicit root: TypedAst.Root): Iterable[OpCompletion] = {
    getCompletions(err.loc.source.name, err.ap, err.env, err.qn)
  }

  private def getCompletions(uri: String, ap: AnchorPosition, env: LocalScope, qn: Name.QName)(implicit root: TypedAst.Root): Iterable[OpCompletion] = {
    if (qn.namespace.nonEmpty) {
      root.effects.values.flatMap(eff =>
        eff.ops.collect {
          case op if CompletionUtils.isAvailable(eff) && CompletionUtils.matchesName(op.sym, qn, qualified = true) =>
            OpCompletion(op, ap, qualified = true, inScope = true, isHandler = false)
        }
      )
    } else {
      root.effects.values.flatMap(eff =>
        eff.ops.collect {
          case op if CompletionUtils.isAvailable(eff) && CompletionUtils.matchesName(op.sym, qn, qualified = false) =>
            OpCompletion(op, ap, qualified = false, inScope(op, env), isHandler = false)
        }
      )
    }
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
