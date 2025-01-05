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
package ca.uwaterloo.flix.api.lsp.provider.completion.syntactic

import ca.uwaterloo.flix.api.lsp.provider.completion.{Completion, Priority}

/**
  * Completions for keywords.
  */
object KeywordCompleter {

  /**
    * Constraint keywords. These are keywords that can occur in datalog constraints.
    */
  def getConstraintKeywords: List[Completion] =
    List(
      Completion.KeywordCompletion("fix", Priority.Default),
      Completion.KeywordCompletion("if" , Priority.Default),
      Completion.KeywordCompletion("not", Priority.Default),
    )

  /**
    * Module keywords. These are keywords that can occur in a module.
    */
  def getModKeywords: List[Completion] =
    List(
      // D
      Completion.KeywordCompletion("@Deprecated"      , Priority.Low),
      Completion.KeywordCompletion("def"              , Priority.High),
      // E
      Completion.KeywordCompletion("eff"              , Priority.Low),
      Completion.KeywordCompletion("enum"             , Priority.High),
      // I
      Completion.KeywordCompletion("import"           , Priority.Low),
      Completion.KeywordCompletion("instance"         , Priority.High),
      // L
      Completion.KeywordCompletion("@Lazy"            , Priority.High),
      Completion.KeywordCompletion("@LazyWhenPure"    , Priority.Low),
      // M
      Completion.KeywordCompletion("mod"              , Priority.Default),
      // P
      Completion.KeywordCompletion("@Parallel"        , Priority.Low),
      Completion.KeywordCompletion("@ParallelWhenPure", Priority.Lower),
      Completion.KeywordCompletion("pub"              , Priority.High),
      // S
      Completion.KeywordCompletion("sealed"           , Priority.Low),
      Completion.KeywordCompletion("struct"           , Priority.High),
      // T
      Completion.KeywordCompletion("@Test"            , Priority.Low),
      Completion.KeywordCompletion("trait"            , Priority.High),
      Completion.KeywordCompletion("type"             , Priority.Higher),
      // U
      Completion.KeywordCompletion("use"              , Priority.Default),
      // W
      Completion.KeywordCompletion("with"             , Priority.Default),
    )

  /**
    * Enum keywords. These are keywords that can appear within the declaration of an enum.
    */
  def getEnumKeywords: List[Completion] =
    List(
      Completion.KeywordCompletion("case", Priority.Default)
    )

  /**
    * Expression keywords. These are keywords that can appear within expressions (fx within the body of a function).
    */
  def getExprKeywords: List[Completion] =
    List(
      // A
      Completion.KeywordCompletion("and"         , Priority.Default),
      // C
      Completion.KeywordCompletion("catch"       , Priority.Default),
      // D
      Completion.KeywordCompletion("def"         , Priority.Higher),
      Completion.KeywordCompletion("discard"     , Priority.Low),
      Completion.KeywordCompletion("do"          , Priority.High),
      // E
      Completion.KeywordCompletion("else"        , Priority.Default),
      // F
      Completion.KeywordLiteralCompletion("false", Priority.Higher),
      Completion.KeywordCompletion("forA"        , Priority.Lowest),
      Completion.KeywordCompletion("forM"        , Priority.Low),
      Completion.KeywordCompletion("force"       , Priority.High),
      Completion.KeywordCompletion("foreach"     , Priority.Lower),
      Completion.KeywordCompletion("from"        , Priority.Highest),
      // I
      Completion.KeywordCompletion("if"          , Priority.Higher),
      Completion.KeywordCompletion("inject"      , Priority.Low),
      Completion.KeywordCompletion("instanceof"  , Priority.Lowest),
      Completion.KeywordCompletion("into"        , Priority.High),
      // L
      Completion.KeywordCompletion("lazy"        , Priority.Low),
      Completion.KeywordCompletion("let"         , Priority.High),
      // M
      Completion.KeywordCompletion("match"       , Priority.Default),
      // N
      Completion.KeywordCompletion("new"         , Priority.Low),
      Completion.KeywordCompletion("not"         , Priority.High),
      Completion.KeywordLiteralCompletion("null" , Priority.Lower),
      // O
      Completion.KeywordCompletion("or"          , Priority.Default),
      // P
      Completion.KeywordCompletion("par"         , Priority.Low),
      Completion.KeywordCompletion("project"     , Priority.High),
      // Q
      Completion.KeywordCompletion("query"       , Priority.Default),
      // R
      Completion.KeywordCompletion("region"      , Priority.Default),
      // S
      Completion.KeywordCompletion("select"      , Priority.Higher),
      Completion.KeywordCompletion("solve"       , Priority.High),
      Completion.KeywordCompletion("spawn"       , Priority.Low),
      // T
      Completion.KeywordCompletion("throw"       , Priority.Lowest),
      Completion.KeywordLiteralCompletion("true" , Priority.Higher),
      Completion.KeywordCompletion("try"         , Priority.High),
      Completion.KeywordCompletion("typematch"   , Priority.Low),
      // U
      Completion.KeywordCompletion("unsafe"      , Priority.Low),
      Completion.KeywordCompletion("use"         , Priority.High),
      // W
      Completion.KeywordCompletion("with"        , Priority.Default),
      Completion.KeywordCompletion("without"     , Priority.Default),
      // Y
      Completion.KeywordCompletion("yield"       , Priority.Default)
    )

  /**
    * Instance declaration keywords.
    */
  def getInstanceKeywords: List[Completion] =
    List(
      Completion.KeywordCompletion("def"  , Priority.Default),
      Completion.KeywordCompletion("pub"  , Priority.Default),
      Completion.KeywordCompletion("redef", Priority.Default),
    )

  /**
    * Struct declaration keywords. These are keywords that occur within a struct declaration.
    */
  def getStructKeywords: List[Completion] =
    List(
      Completion.KeywordCompletion("mut", Priority.Default)
    )

  /**
    * Trait declaration keywords. These are keywords that occur within a trait declaration.
    */
  def getTraitKeywords: List[Completion] =
    List(
      Completion.KeywordCompletion("def", Priority.Default),
      Completion.KeywordCompletion("pub", Priority.Default),
    )

  /**
    * Type declaration keywords. These are the keywords that can occur
    * within a type declaration.
    */
  def getTypeKeywords: List[Completion] =
    List(
      Completion.KeywordCompletion("alias", Priority.Default)
    )

}
