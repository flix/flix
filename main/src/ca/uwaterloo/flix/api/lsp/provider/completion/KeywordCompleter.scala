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
      Completion.KeywordCompletion("import"   , Priority.High),
      Completion.KeywordCompletion("inline"   , Priority.Low),
      Completion.KeywordCompletion("instance" , Priority.Higher),
      // M
      Completion.KeywordCompletion("mod"      , Priority.Default),
      // P
      Completion.KeywordCompletion("pub"      , Priority.Default),
      // R
      Completion.KeywordCompletion("restrictable", Priority.Default),
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
      Completion.CollectionKeywordCompletion("Array", Priority.Low),
      Completion.KeywordCompletion("as"       , Priority.Low),
      // C
      "catch",
      "checked_cast",
      "checked_ecast",
      "choose",
      "choose*",
      // D
      "debug",
      "debug!",
      "debug!!",
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
      "instanceof",
      Completion.KeywordCompletion("into"     , Priority.High),
      // J
      "java_get_field",
      "java_set_field",
      "java_new",
      // L
      Completion.KeywordCompletion("lazy"     , Priority.Low),
      Completion.KeywordCompletion("let"      , Priority.High),
      Completion.CollectionKeywordCompletion("List", Priority.Low),
      // M
      Completion.CollectionKeywordCompletion("Map", Priority.Low),
      "masked_cast",
      Completion.KeywordCompletion("match"    , Priority.Default),
      // N
      Completion.KeywordCompletion("new"      , Priority.Low),
      Completion.KeywordCompletion("not"      , Priority.High),
      "null",
      // O
      "open_variant",
      "open_variant_as",
      Completion.KeywordCompletion("or"       , Priority.Default),
      // P
      Completion.KeywordCompletion("par"      , Priority.Default),
      "project",
      // Q
      Completion.KeywordCompletion("query"    , Priority.Default),
      // R
      Completion.KeywordCompletion("region"   , Priority.Default),
      // S
      Completion.KeywordCompletion("select"   , Priority.Higher),
      Completion.CollectionKeywordCompletion("Set", Priority.Low),
      Completion.KeywordCompletion("solve"    , Priority.High),
      Completion.KeywordCompletion("spawn"    , Priority.Low),
      "static",
      "Static",
      // T
      "throw",
      Completion.KeywordCompletion("true"     , Priority.Higher),
      Completion.KeywordCompletion("try"      , Priority.High),
      Completion.KeywordCompletion("typematch", Priority.Low),
      // U    
      // experiments on occurrences in stdlib shows the *exact* same amount of
      // occurrences of these two. However, I'm fairly confident that
      // `use` would occur much more often in most programs.
      "unchecked_cast",
      Completion.KeywordCompletion("unsafe"   , Priority.Low),
      Completion.KeywordCompletion("use"      , Priority.High),
      // V
      Completion.CollectionKeywordCompletion("Vector", Priority.Low),
      // W
      Completion.KeywordCompletion("without"  , Priority.Default),
      // Y
      Completion.KeywordCompletion("yield"    , Priority.Default)
    )
           
  /**
    * Instance declaration keywords.
    */
  def getInstanceKeywords: Iterable[Completion] =
    List(
      Completion.KeywordCompletion("override", Priority.Low),
      Completion.KeywordCompletion("pub", Priority.Low),
      Completion.KeywordCompletion("def", Priority.Low)
    )      

  /**
    * Miscellaneous keywords.
    */
  def getOtherKeywords: Iterable[Completion] =
    List(
      // @
      Completion.KeywordCompletion("@Deprecated"      , Priority.Lowest),
      Completion.KeywordCompletion("@Lazy"            , Priority.High),
      Completion.KeywordCompletion("@LazyWhenPure"    , Priority.Lower),
      Completion.KeywordCompletion("@Parallel"        , Priority.Higher),
      Completion.KeywordCompletion("@ParallelWhenPure", Priority.Low),
      Completion.KeywordCompletion("@Test"            , Priority.Highest),
      // F
      Completion.KeywordCompletion("fix"              , Priority.Default),
      // R
      Completion.KeywordCompletion("redef"            , Priority.High),
      // W
      Completion.KeywordCompletion("where"            , Priority.Low),
      Completion.KeywordCompletion("with"             , Priority.High),
    )

  def getStructKeywords: Iterable[Completion] =
    List(
      Completion.KeywordCompletion("mut", Priority.Low)
    )

  /**
    * Trait declaration keywords. These are keywords that occur within a trait declaration.
    */
  def getTraitKeywords: Iterable[Completion] =
    List(
      Completion.KeywordCompletion("def", Priority.Default),
      Completion.KeywordCompletion("pub", Priority.Default),
    )

  /**
    * Type declaration keywords. These are the keywords that can occur
    * within a type declaration.
    * @return iterable with type declaration keyword completions.
    */
  def getTypeKeywords: Iterable[Completion] =
    List(
      Completion.KeywordCompletion("alias", Priority.Low)
    )
}
