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

import ca.uwaterloo.flix.api.lsp.acceptors.{AllAcceptor, FileAcceptor}
import ca.uwaterloo.flix.api.lsp.{Consumer, Visitor}
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.LabelCompletion
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.ast.{Name, SourceLocation, Type, TypeConstructor, TypedAst}

object LabelCompleter {

  /**
    * Returns a list of [[LabelCompletion]]s.
    */
  def getCompletions(context: CompletionContext)(implicit root: Root): Iterable[LabelCompletion] = {
    // Do not get label completions if we are importing or using.
    if (context.prefix.contains("import") || context.prefix.contains("use")) {
      return Nil
    }

    val regex = raw"(.*)#.*".r

    context.word match {
      case regex(prefix) if isFirstCharLowerCase(prefix) =>
        var labels: Set[Name.Label] = Set.empty
        object LabelConsumer extends Consumer {
          override def consumeExpr(exp: TypedAst.Expr): Unit = exp match {
            case TypedAst.Expr.RecordRestrict(label, _, _, _, _) => labels += label
            case TypedAst.Expr.RecordExtend(label, _, _, _, _, _) => labels += label
            case TypedAst.Expr.RecordSelect(_, label, _, _, _) => labels += label
            case _ => ()
          }

          override def consumeType(tpe: Type): Unit = tpe match {
            case Type.Cst(TypeConstructor.RecordRowExtend(label), _) => labels += label
            case _ => ()
          }
        }

        Visitor.visitRoot(root, LabelConsumer, FileAcceptor(context.uri))

        labels.map(label => Completion.LabelCompletion(label, prefix))
      case _ => Nil
    }
  }

  /**
    * Returns true if the first char of the string is lowerCase, false otherwise.
    */
  private def isFirstCharLowerCase(str: String): Boolean = str.headOption.exists(_.isLower)
}
