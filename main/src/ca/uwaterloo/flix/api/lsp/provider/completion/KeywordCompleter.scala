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

import ca.uwaterloo.flix.api.lsp.Range

/**
  * Completions for keywords.
  */
object KeywordCompleter {

  /**
    * Constraint keywords. These are keywords that can occur in datalog constraints.
    */
  def getConstraintKeywords(range: Range): List[Completion] =
    List(
      Completion.KeywordCompletion("fix", range, Priority.Default),
      Completion.KeywordCompletion("if" , range, Priority.Default),
      Completion.KeywordCompletion("not", range, Priority.Default),
    )

  /**
    * Module keywords. These are keywords that can occur in a module.
    */
  def getModKeywords(range: Range): List[Completion] =
    List(
      // D
      Completion.KeywordCompletion("@Deprecated"      , range, Priority.Low),
      Completion.KeywordCompletion("def"              , range, Priority.High),
      // E
      Completion.KeywordCompletion("eff"              , range, Priority.Low),
      Completion.KeywordCompletion("enum"             , range, Priority.High),
      // I
      Completion.KeywordCompletion("import"           , range, Priority.Low),
      Completion.KeywordCompletion("instance"         , range, Priority.High),
      // L
      Completion.KeywordCompletion("@Lazy"            , range, Priority.High),
      Completion.KeywordCompletion("@LazyWhenPure"    , range, Priority.Low),
      // M
      Completion.KeywordCompletion("mod"              , range, Priority.Default),
      // P
      Completion.KeywordCompletion("@Parallel"        , range, Priority.Low),
      Completion.KeywordCompletion("@ParallelWhenPure", range, Priority.Lower),
      Completion.KeywordCompletion("pub"              , range, Priority.High),
      // S
      Completion.KeywordCompletion("sealed"           , range, Priority.Low),
      Completion.KeywordCompletion("struct"           , range, Priority.High),
      // T
      Completion.KeywordCompletion("@Test"            , range, Priority.Low),
      Completion.KeywordCompletion("trait"            , range, Priority.High),
      Completion.KeywordCompletion("type"             , range, Priority.Higher),
      // U
      Completion.KeywordCompletion("use"              , range, Priority.Default),
      // W
      Completion.KeywordCompletion("with"             , range, Priority.Default),
    )

  /**
    * Enum keywords. These are keywords that can appear within the declaration of an enum.
    */
  def getEnumKeywords(range: Range): List[Completion] =
    List(
      Completion.KeywordCompletion("case", range, Priority.Default)
    )

  /**
    * Effect keywords. These are keywords that can appear within the declaration of an effect.
    */
  def getEffectKeywords(range: Range): List[Completion] =
    List(
      Completion.KeywordCompletion("def", range, Priority.Default)
    )

  /**
    * Expression keywords. These are keywords that can appear within expressions (fx within the body of a function).
    */
  def getExprKeywords(range: Range): List[Completion] =
    List(
      // A
      Completion.KeywordCompletion("and"         , range, Priority.Default),
      // C
      Completion.KeywordCompletion("catch"       , range, Priority.Default),
      // D
      Completion.KeywordCompletion("def"         , range, Priority.Higher),
      Completion.KeywordCompletion("discard"     , range, Priority.Low),
      Completion.KeywordCompletion("do"          , range, Priority.High),
      // E
      Completion.KeywordCompletion("else"        , range, Priority.Default),
      // F
      Completion.KeywordLiteralCompletion("false", range, Priority.Higher),
      Completion.KeywordCompletion("forA"        , range, Priority.Lowest),
      Completion.KeywordCompletion("forM"        , range, Priority.Low),
      Completion.KeywordCompletion("force"       , range, Priority.High),
      Completion.KeywordCompletion("foreach"     , range, Priority.Lower),
      Completion.KeywordCompletion("from"        , range, Priority.Highest),
      // H
      Completion.KeywordCompletion("handler"     , range, Priority.Default),
      // I
      Completion.KeywordCompletion("if"          , range, Priority.Higher),
      Completion.KeywordCompletion("inject"      , range, Priority.Low),
      Completion.KeywordCompletion("instanceof"  , range, Priority.Lowest),
      Completion.KeywordCompletion("into"        , range, Priority.High),
      // L
      Completion.KeywordCompletion("lazy"        , range, Priority.Low),
      Completion.KeywordCompletion("let"         , range, Priority.High),
      // M
      Completion.KeywordCompletion("match"       , range, Priority.Default),
      // N
      Completion.KeywordCompletion("new"         , range, Priority.Low),
      Completion.KeywordCompletion("not"         , range, Priority.High),
      Completion.KeywordLiteralCompletion("null" , range, Priority.Lower),
      // O
      Completion.KeywordCompletion("or"          , range, Priority.Default),
      // P
      Completion.KeywordCompletion("par"         , range, Priority.Low),
      Completion.KeywordCompletion("project"     , range, Priority.High),
      // Q
      Completion.KeywordCompletion("query"       , range, Priority.Default),
      // R
      Completion.KeywordCompletion("region"      , range, Priority.Default),
      // S
      Completion.KeywordCompletion("select"      , range, Priority.Higher),
      Completion.KeywordCompletion("solve"       , range, Priority.High),
      Completion.KeywordCompletion("spawn"       , range, Priority.Low),
      // T
      Completion.KeywordCompletion("throw"       , range, Priority.Lowest),
      Completion.KeywordLiteralCompletion("true" , range, Priority.Higher),
      Completion.KeywordCompletion("try"         , range, Priority.High),
      Completion.KeywordCompletion("typematch"   , range, Priority.Low),
      // U
      Completion.KeywordCompletion("unsafe"      , range, Priority.Low),
      Completion.KeywordCompletion("use"         , range, Priority.High),
      // W
      Completion.KeywordCompletion("with"        , range, Priority.Default),
      Completion.KeywordCompletion("without"     , range, Priority.Default),
      // Y
      Completion.KeywordCompletion("yield"       , range, Priority.Default)
    )

  /**
    * Instance declaration keywords.
    */
  def getInstanceKeywords(range: Range): List[Completion] =
    List(
      Completion.KeywordCompletion("def"  , range, Priority.Default),
      Completion.KeywordCompletion("pub"  , range, Priority.Default),
      Completion.KeywordCompletion("redef", range, Priority.Default),
    )

  /**
    * Struct declaration keywords. These are keywords that occur within a struct declaration.
    */
  def getStructKeywords(range: Range): List[Completion] =
    List(
      Completion.KeywordCompletion("mut", range, Priority.Default)
    )

  /**
    * Trait declaration keywords. These are keywords that occur within a trait declaration.
    */
  def getTraitKeywords(range: Range): List[Completion] =
    List(
      Completion.KeywordCompletion("def", range, Priority.Default),
      Completion.KeywordCompletion("pub", range, Priority.Default),
    )

  /**
    * Type declaration keywords. These are the keywords that can occur
    * within a type declaration.
    */
  def getTypeKeywords(range: Range): List[Completion] =
    List(
      Completion.KeywordCompletion("alias", range, Priority.Default)
    )

}
