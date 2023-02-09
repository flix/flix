/*
 * Copyright 2023 Magnus Madsen
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

import ca.uwaterloo.flix.api.lsp.provider.CompletionProvider.Priority
import ca.uwaterloo.flix.api.lsp.{CompletionItem, CompletionItemKind, TextEdit}

/**
  * A common super-type for auto-completions.
  */
sealed trait Completion {
  /**
    * Returns a LSP completion item for `this`.
    */
  def toCompletionItem: CompletionItem = this match {
    case Completion.KeywordCompletion(name, context) =>
      CompletionItem(label = name, sortText = Priority.normal(name), textEdit = TextEdit(context.range, s"$name "), kind = CompletionItemKind.Keyword)
    case Completion.FieldCompletion(name, context) =>
      CompletionItem(label = name, sortText = Priority.high(name), textEdit = TextEdit(context.range, s"$name "), kind = CompletionItemKind.Field)
    case Completion.PredicateCompletion(name, priority, context) =>
      CompletionItem(label = name, sortText = priority, textEdit = TextEdit(context.range, name), kind = CompletionItemKind.Variable) // Variable?
    case Completion.EnumTypeCompletion(context) => ??? // TODO
  }
}

object Completion {

  /**
    * Represents a keyword completion.
    *
    * @param name the name of the keyword.
    */
  case class KeywordCompletion(name: String, context: CompletionContext) extends Completion

  /**
    * Represents a field completion.
    *
    * @param name the name of the field.
    */
  case class FieldCompletion(name: String, context: CompletionContext) extends Completion


  /**
    * Represents a predicate completion.
    *
    * @param name the name of the predicate
    * @param priority the priority of the predicate
    */
  case class PredicateCompletion(name: String, priority: String, context: CompletionContext) extends Completion

  // TODO:
  case class EnumTypeCompletion(/* stuff goes here, */ context: CompletionContext) extends Completion

}
