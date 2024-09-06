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
    * Declaration keywords. These are keywords that denote a declaration.
    */
  def getDeclKeywords: Iterable[Completion] =
    List(
      Completion.KeywordCompletion("def"      , Priority.Highest),
      Completion.KeywordCompletion("pub"      , Priority.Higher),
      Completion.KeywordCompletion("enum"     , Priority.High),
      Completion.KeywordCompletion("type"     , Priority.High),
      Completion.KeywordCompletion("instance" , Priority.High),
      Completion.KeywordCompletion("mod"      , Priority.Low),
      Completion.KeywordCompletion("eff"      , Priority.Lower),
      Completion.KeywordCompletion("struct"   , Priority.Lower),
      Completion.KeywordCompletion("sealed"   , Priority.Lowest),
      Completion.KeywordCompletion("trait"    , Priority.Lowest),
      Completion.KeywordCompletion("import"   , Priority.Lowest),
    )

  /**
    * Enum keywords. These are keywords that can appear within the declaration of an enum.
    */
  def getEnumKeywords: Iterable[Completion] =
    List(
      Completion.KeywordCompletion("case", Priority.Low)
    )

  /**
    * Expression keywords. These are keywords that can appear within expressions (fx within the body of a function).
    */
  def getExprKeywords: Iterable[Completion] =
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
    ) map (name => Completion.KeywordCompletion(name, Priority.Lower))
           
  /**
    * Miscellaneous keywords.
    */
  def getOtherKeywords: Iterable[Completion] =
    List(
      Completion.KeywordCompletion("with"             , Priority.Highest),
      Completion.KeywordCompletion("law"              , Priority.Higher),
      Completion.KeywordCompletion("@Test"            , Priority.High),
      Completion.KeywordCompletion("where"            , Priority.Low),
      Completion.KeywordCompletion("fix"              , Priority.Low),
      Completion.KeywordCompletion("@Deprecated"      , Priority.Lowest),
      Completion.KeywordCompletion("@Parallel"        , Priority.Lowest),
      Completion.KeywordCompletion("@ParallelWhenPure", Priority.Lowest),
      Completion.KeywordCompletion("@Lazy"            , Priority.Lowest),
      Completion.KeywordCompletion("@LazyWhenPure"    , Priority.Lowest),
      Completion.KeywordCompletion("redef"            , Priority.Lowest),
    )

  /**
    * Trait declaration keywords. These are keywords that occur within a trait declaration.
    */
  def getTraitKeywords: Iterable[Completion] =
    List(
      Completion.KeywordCompletion("def", Priority.Lower),
      Completion.KeywordCompletion("pub", Priority.Lower),
    )
}
