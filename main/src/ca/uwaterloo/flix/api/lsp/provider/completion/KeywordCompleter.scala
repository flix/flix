/*
 * Copyright 2024 Alexander Dybdahl Troelsen
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
import ca.uwaterloo.flix.language.ast.TypedAst

/**
  * Completions for keywords
  */
object KeywordCompleter {
  /**
    * Miscellaneous keywords.
    */
  def getOtherKeywords(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root): Iterable[Completion] =
    List(
      Completion.KeywordCompletion("with", Priority.highest("with")),
      Completion.KeywordCompletion("law", Priority.higher("law")),
      Completion.KeywordCompletion("@Test", Priority.high("@Test")),
      Completion.KeywordCompletion("where", Priority.low("where")),
      Completion.KeywordCompletion("fix", Priority.low("fix")),
      Completion.KeywordCompletion("@Deprecated", Priority.lowest("@Deprecated")),
      Completion.KeywordCompletion("@Parallel", Priority.lowest("@Parallel")),
      Completion.KeywordCompletion("@ParallelWhenPure", Priority.lowest("@ParallelWhenPure")),
      Completion.KeywordCompletion("@Lazy", Priority.lowest("@Lazy")),
      Completion.KeywordCompletion("@LazyWhenPure", Priority.lowest("@LazyWhenPure")),
      Completion.KeywordCompletion("Record", Priority.lowest("Record")),
      Completion.KeywordCompletion("redef", Priority.lowest("redef")),
      Completion.KeywordCompletion("Schema", Priority.lowest("Schema")),
    )

  /**
    * Trait declaration keywords. These are keywords that occur within a trait declaration.
    */
  def getTraitKeywords(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root): Iterable[Completion] =
    List(
      Completion.KeywordCompletion("def", Priority.lower("def")),
      Completion.KeywordCompletion("pub", Priority.lower("pub")),
    )


  /**
    * Declaration keywords. These are keywords that denote a declaration.
    */
  def getDeclKeywords(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root): Iterable[Completion] =
    List(
      Completion.KeywordCompletion("def"      , Priority.highest("def")),
      Completion.KeywordCompletion("pub"      , Priority.higher("pub")),
      Completion.KeywordCompletion("enum"     , Priority.high("enum")),
      Completion.KeywordCompletion("type"     , Priority.high("type")),
      Completion.KeywordCompletion("instance" , Priority.high("instance")),
      Completion.KeywordCompletion("mod"      , Priority.low("mod")),
      Completion.KeywordCompletion("eff"      , Priority.lower("eff")),
      Completion.KeywordCompletion("struct"   , Priority.lower("struct")),
      Completion.KeywordCompletion("sealed"   , Priority.lowest("sealed")),
      Completion.KeywordCompletion("trait"    , Priority.lowest("trait")),
      Completion.KeywordCompletion("import"   , Priority.lowest("import")),
    )

  /**
    * Enum keywords. These are keywords that can appear within the declaration of an enum.
    */
  def getEnumKeywords(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root): Iterable[Completion] =
    List(
      Completion.KeywordCompletion("case", Priority.low("case"))
    )

  /**
    * Expression keywords. These are keywords that can appear within expressions (fx within the body of a function).
    */
  def getExprKeywords(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root): Iterable[Completion] =
    List(
      "and",
      "as",
      "def",
      "discard",
      "do",
      "else",
      "false",
      "forA",
      "forM",
      "force",
      "foreach",
      "from",
      "if",
      "inject",
      "into",
      "lazy",
      "let",
      "match",
      "new",
      "not",
      "or",
      "par",
      "query",
      "region",
      "select",
      "solve",
      "spawn",
      "struct",
      "true",
      "try",
      "typematch",
      "unsafe",
      "use",
      "without",
      "yield"
    ) map (name => Completion.KeywordCompletion(name, Priority.lower(name)))
}
