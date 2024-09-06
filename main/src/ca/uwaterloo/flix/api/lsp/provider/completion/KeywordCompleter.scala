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
      // D
      Completion.KeywordCompletion("def"      , Priority.Default),
      // E
      Completion.KeywordCompletion("eff"      , Priority.Low),
      Completion.KeywordCompletion("enum"     , Priority.High),
      // I
      Completion.KeywordCompletion("import"   , Priority.Low),
      Completion.KeywordCompletion("instance" , Priority.High),
      // M
      Completion.KeywordCompletion("mod"      , Priority.Default),
      // P
      Completion.KeywordCompletion("pub"      , Priority.Default),
      // S
      Completion.KeywordCompletion("sealed"   , Priority.Low),
      Completion.KeywordCompletion("struct"   , Priority.High),
      // T
      Completion.KeywordCompletion("trait"    , Priority.Low),
      Completion.KeywordCompletion("type"     , Priority.High),
    )

  /**
    * Enum keywords. These are keywords that can appear within the declaration of an enum.
    */
  def getEnumKeywords: Iterable[Completion] =
    List(
      Completion.KeywordCompletion("case", Priority.Default)
    )

  /**
    * Expression keywords. These are keywords that can appear within expressions (fx within the body of a function).
    */
  def getExprKeywords: Iterable[Completion] =
    List(
      // A
      Completion.KeywordCompletion("and"      , Priority.High),
      Completion.KeywordCompletion("as"       , Priority.Low),
      // D
      Completion.KeywordCompletion("def"      , Priority.Higher),
      Completion.KeywordCompletion("discard"  , Priority.Low),
      Completion.KeywordCompletion("do"       , Priority.High),
      // E
      Completion.KeywordCompletion("else"     , Priority.Default),
      // F
      Completion.KeywordCompletion("false"    , Priority.Higher),
      Completion.KeywordCompletion("forA"     , Priority.Lowest),
      Completion.KeywordCompletion("forM"     , Priority.Low),
      Completion.KeywordCompletion("force"    , Priority.High),
      Completion.KeywordCompletion("foreach"  , Priority.Lower),
      Completion.KeywordCompletion("from"     , Priority.Highest),
      // I
      Completion.KeywordCompletion("if"       , Priority.Higher),
      Completion.KeywordCompletion("inject"   , Priority.Low),
      Completion.KeywordCompletion("into"     , Priority.High),
      // L
      Completion.KeywordCompletion("lazy"     , Priority.Low),
      Completion.KeywordCompletion("let"      , Priority.High),
      // M
      Completion.KeywordCompletion("match"    , Priority.Default),
      // N
      Completion.KeywordCompletion("new"      , Priority.Low),
      Completion.KeywordCompletion("not"      , Priority.High),
      // O
      Completion.KeywordCompletion("or"       , Priority.Default),
      // P
      Completion.KeywordCompletion("par"      , Priority.Default),
      // Q
      Completion.KeywordCompletion("query"    , Priority.Default),
      // R
      Completion.KeywordCompletion("region"   , Priority.Default),
      // S
      Completion.KeywordCompletion("select"   , Priority.Higher),
      Completion.KeywordCompletion("solve"    , Priority.High),
      Completion.KeywordCompletion("spawn"    , Priority.Low),
      Completion.KeywordCompletion("struct"   , Priority.Lower),
      // T
      Completion.KeywordCompletion("true"     , Priority.Higher),
      Completion.KeywordCompletion("try"      , Priority.High),
      Completion.KeywordCompletion("typematch", Priority.Low),
      // U    
      // experitments on occurances in stdlib shows *exact* same amount of
      // occurances of these two. However, I'm fairly confident that
      // `use` would occur much more often in most programs.
      Completion.KeywordCompletion("unsafe"   , Priority.Low),
      Completion.KeywordCompletion("use"      , Priority.High),
      // W
      Completion.KeywordCompletion("without"  , Priority.Default),
      // Y
      Completion.KeywordCompletion("yield"    , Priority.Default)
    )
           
  /**
    * Miscellaneous keywords.
    */
  def getOtherKeywords: Iterable[Completion] =
    List(
      // @
      Completion.KeywordCompletion("@Test"            , Priority.Highest),
      Completion.KeywordCompletion("@ParallelWhenPure", Priority.Low),
      Completion.KeywordCompletion("@Parallel"        , Priority.Higher),
      Completion.KeywordCompletion("@LazyWhenPure"    , Priority.Lower),
      Completion.KeywordCompletion("@Lazy"            , Priority.High),
      Completion.KeywordCompletion("@Deprecated"      , Priority.Lowest),
      // S
      Completion.KeywordCompletion("Schema"           , Priority.Default),
      // F
      Completion.KeywordCompletion("fix"              , Priority.Default),
      // L
      Completion.KeywordCompletion("law"              , Priority.Default),
      // R
      Completion.KeywordCompletion("redef"            , Priority.High),
      Completion.KeywordCompletion("Record"           , Priority.Low),
      // W
      Completion.KeywordCompletion("with"             , Priority.High),
      Completion.KeywordCompletion("where"            , Priority.Low),
    )

  /**
    * Trait declaration keywords. These are keywords that occur within a trait declaration.
    */
  def getTraitKeywords: Iterable[Completion] =
    List(
      Completion.KeywordCompletion("def", Priority.Default),
      Completion.KeywordCompletion("pub", Priority.Default),
    )
}
