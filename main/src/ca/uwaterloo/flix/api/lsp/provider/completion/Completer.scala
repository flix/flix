/*
 * Copyright 2023 Lukas Rønn
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

import ca.uwaterloo.flix.api.lsp.{CompletionItem, Index}
import ca.uwaterloo.flix.language.ast.TypedAst

/**
  * A common super-type for completers.
  */
sealed trait Completer {
  /**
    * Returns a List of LSP completion items for `this`.
    */
  def getCompletions(implicit context: CompletionContext, index: Index, root: TypedAst.Root): Iterable[CompletionItem] = this match {
    case Completer.KeywordCompleter() =>
      KeywordCompletionProvider.getKeywords() map (word => word.toCompletionItem)
    case Completer.FieldCompleter() =>
      FieldCompletionProvider.getFields() map (field => field.toCompletionItem)
    case Completer.PredicateCompleter() =>
      PredicateCompletionProvider.getPredicates() map (predicate => predicate.toCompletionItem)
    case Completer.TypeCompleter() =>
      TypeCompletionProvider.getTypes() map (typ => typ.toCompletionItem)
  }
}

object Completer {

  /**
    * Represents a keyword completer.
    */
  case class KeywordCompleter() extends Completer

  /**
    * Represents a field completer.
    */
  case class FieldCompleter() extends Completer

  /**
    * Represents a predicate completer.
    */
  case class PredicateCompleter() extends Completer

  /**
    * Represents a type completer (enums, aliases, and built-in types).
    */
  case class TypeCompleter() extends Completer
}
