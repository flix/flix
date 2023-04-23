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
import ca.uwaterloo.flix.api.lsp.Index
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.KeywordCompletion
import ca.uwaterloo.flix.language.ast.TypedAst

object KeywordCompleter extends Completer {

  /**
    * Returns a List of Completion for keywords.
    */
  override def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root, delta: DeltaContext): Iterable[KeywordCompletion] =
  // NB: Please keep the list alphabetically sorted.
    List(
      "@Deprecated",
      "@Parallel",
      "@ParallelWhenPure",
      "@Lazy",
      "@LazyWhenPure",
      "@Test",
      "and",
      "as",
      "case",
      "class",
      "def",
      "deref",
      "discard",
      "do",
      "eff",
      "else",
      "enum",
      "false",
      "fix",
      "for",
      "forA",
      "forM",
      "forall",
      "force",
      "foreach",
      "from",
      "get",
      "if",
      "inject",
      "import",
      "instance",
      "into",
      "lat",
      "law",
      "lazy",
      "let",
      "match",
      "mod",
      "new",
      "not",
      "null",
      "opaque",
      "or",
      "override",
      "par",
      "pub",
      "query",
      "Record",
      "ref",
      "region",
      "rel",
      "Schema",
      "sealed",
      "select",
      "set",
      "solve",
      "spawn",
      "true",
      "try",
      "type",
      "typematch",
      "use",
      "where",
      "with",
      "without",
      "yield"
    ) map (name => Completion.KeywordCompletion(name))
}
