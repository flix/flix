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

/**
  * Completions for keywords
  */
object KeywordCompleter {

  /**
    * Constraint keywords. These are keywords that can occur in datalog constraints.
    */
  def getConstraintKeywords: Iterable[Completion] =
    List(
      Completion.KeywordCompletion("fix", Priority.Default),
      Completion.KeywordCompletion("if" , Priority.Default),
      Completion.KeywordCompletion("not", Priority.Default),
    )

  /**
    * Module keywords. These are keywords that can occur in a module.
    */
  def getModKeywords: Iterable[Completion] =
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
      // W
      Completion.KeywordCompletion("with"             , Priority.Default),
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
      Completion.KeywordCompletion("and"         , Priority.Default),
      // C
      Completion.KeywordCompletion("catch"       , Priority.Default),
      // D
      Completion.KeywordCompletion("def"         , Priority.High),
      Completion.KeywordCompletion("discard"     , Priority.Lower),
      Completion.KeywordCompletion("do"          , Priority.Low),
      // E
      Completion.KeywordCompletion("else"        , Priority.Default),
      // F
      Completion.KeywordLiteralCompletion("false", Priority.High),
      Completion.KeywordCompletion("forA"        , Priority.Lowest),
      Completion.KeywordCompletion("forM"        , Priority.Lower),
      Completion.KeywordCompletion("force"       , Priority.Low),
      Completion.KeywordCompletion("foreach"     , Priority.Low),
      Completion.KeywordCompletion("from"        , Priority.High),
      // I
      Completion.KeywordCompletion("if"          , Priority.High),
      Completion.KeywordCompletion("inject"      , Priority.Lower),
      Completion.KeywordCompletion("instanceof"  , Priority.Lowest),
      Completion.KeywordCompletion("into"        , Priority.Low),
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
      Completion.KeywordCompletion("par"         , Priority.Lower),
      Completion.KeywordCompletion("project"     , Priority.Low),
      // Q
      Completion.KeywordCompletion("query"       , Priority.Default),
      // R
      Completion.KeywordCompletion("region"      , Priority.Default),
      // S
      Completion.KeywordCompletion("select"      , Priority.High),
      Completion.KeywordCompletion("solve"       , Priority.Low),
      Completion.KeywordCompletion("spawn"       , Priority.Lower),
      // T
      Completion.KeywordCompletion("throw"       , Priority.Lowest),
      Completion.KeywordLiteralCompletion("true" , Priority.High),
      Completion.KeywordCompletion("try"         , Priority.Low),
      Completion.KeywordCompletion("typematch"   , Priority.Lower),
      // U
      Completion.KeywordCompletion("unsafe"      , Priority.Low),
      Completion.KeywordCompletion("use"         , Priority.High),
      // W
      Completion.KeywordCompletion("without"     , Priority.Default),
      // Y
      Completion.KeywordCompletion("yield"       , Priority.Default)
    )

  /**
    * Instance declaration keywords.
    */
  def getInstanceKeywords: Iterable[Completion] =
    List(
      Completion.KeywordCompletion("def"  , Priority.Default),
      Completion.KeywordCompletion("pub"  , Priority.Default),
      Completion.KeywordCompletion("redef", Priority.Default),
    )

  /**
    * Struct declaration keywords. These are keywords that occur within a struct declaration.
    */
  def getStructKeywords: Iterable[Completion] =
    List(
      Completion.KeywordCompletion("mut", Priority.Default)
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
    */
  def getTypeKeywords: Iterable[Completion] =
    List(
      Completion.KeywordCompletion("alias", Priority.Default)
    )
}
