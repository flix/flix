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
import ca.uwaterloo.flix.language.ast.Name

/**
  * Completer for keywords.
  */
object KeywordCompleter {

  /**
    * Returns keywords that may occur within Datalog constraints.
    */
  def getConstraintKeywords(range: Range): List[Completion] =
    List(
      Completion.KeywordCompletion("fix", range, Priority.Medium),
      Completion.KeywordCompletion("if" , range, Priority.Medium),
      Completion.KeywordCompletion("not", range, Priority.Medium),
    )

  /**
    * Returns keywords that may occur inside modules.
    */
  def getModKeywords(range: Range): List[Completion] =
    List(
      // D
      Completion.KeywordCompletion("def"              , range, Priority.Medium),
      // E
      Completion.KeywordCompletion("eff"              , range, Priority.Medium),
      Completion.KeywordCompletion("enum"             , range, Priority.MediumHigh),
      // I
      Completion.KeywordCompletion("import"           , range, Priority.Medium),
      Completion.KeywordCompletion("instance"         , range, Priority.MediumHigh),
      // M
      Completion.KeywordCompletion("mod"              , range, Priority.Medium),
      // P
      Completion.KeywordCompletion("pub"              , range, Priority.Medium),
      // S
      Completion.KeywordCompletion("sealed"           , range, Priority.Medium),
      Completion.KeywordCompletion("struct"           , range, Priority.MediumHigh),
      // T
      Completion.KeywordCompletion("trait"            , range, Priority.MediumHigh),
      Completion.KeywordCompletion("type"             , range, Priority.Medium),
      // U
      Completion.KeywordCompletion("use"              , range, Priority.Medium),
      // W
      Completion.KeywordCompletion("with"             , range, Priority.Medium),
    )

  /**
    * Returns keywords that may occur inside enum declarations.
    */
  def getEnumKeywords(range: Range): List[Completion] =
    List(
      Completion.KeywordCompletion("case", range, Priority.Medium)
    )

  /**
    * Returns keywords that may occur inside effect declarations.
    */
  def getEffectKeywords(range: Range): List[Completion] =
    List(
      Completion.KeywordCompletion("def", range, Priority.Medium)
    )

  /**
    * Returns keywords that may occur inside expressions.
    *
    * Returns only those keywords that are a prefix of the given `qname` (if present).
    */
  def getExprKeywords(qname: Option[Name.QName], range: Range): List[Completion] =
    List(
      // A
      Completion.KeywordCompletion("and"         , range, Priority.Medium),
      // C
      Completion.KeywordCompletion("catch"       , range, Priority.Medium),
      // D
      Completion.KeywordCompletion("def"         , range, Priority.MediumHigh),
      Completion.KeywordCompletion("discard"     , range, Priority.Medium),
      // E
      Completion.KeywordCompletion("else"        , range, Priority.Medium),
      // F
      Completion.KeywordCompletion("false"       , range, Priority.MediumHigh, withSpace = false),
      Completion.KeywordCompletion("forA"        , range, Priority.MediumLow),
      Completion.KeywordCompletion("forM"        , range, Priority.MediumLow),
      Completion.KeywordCompletion("force"       , range, Priority.MediumLow),
      Completion.KeywordCompletion("foreach"     , range, Priority.Medium),
      Completion.KeywordCompletion("from"        , range, Priority.Medium),
      // H
      Completion.KeywordCompletion("handler"     , range, Priority.Medium),
      // I
      Completion.KeywordCompletion("if"          , range, Priority.Medium),
      Completion.KeywordCompletion("inject"      , range, Priority.MediumLow),
      Completion.KeywordCompletion("instanceof"  , range, Priority.MediumLow),
      Completion.KeywordCompletion("into"        , range, Priority.Medium),
      // L
      Completion.KeywordCompletion("lazy"        , range, Priority.Medium),
      Completion.KeywordCompletion("let"         , range, Priority.MediumHigh),
      // M
      Completion.KeywordCompletion("match"       , range, Priority.Medium),
      // N
      Completion.KeywordCompletion("new"         , range, Priority.Medium),
      Completion.KeywordCompletion("not"         , range, Priority.MediumHigh),
      Completion.KeywordCompletion("null"        , range, Priority.Medium, withSpace = false),
      // O
      Completion.KeywordCompletion("or"          , range, Priority.Medium),
      // P
      Completion.KeywordCompletion("par"         , range, Priority.Medium),
      Completion.KeywordCompletion("project"     , range, Priority.MediumLow),
      // Q
      Completion.KeywordCompletion("query"       , range, Priority.Medium),
      // R
      Completion.KeywordCompletion("region"      , range, Priority.Medium),
      Completion.KeywordCompletion("run"         , range, Priority.Medium),
      // S
      Completion.KeywordCompletion("select"      , range, Priority.Medium),
      Completion.KeywordCompletion("solve"       , range, Priority.Medium),
      Completion.KeywordCompletion("spawn"       , range, Priority.Medium),
      // T
      Completion.KeywordCompletion("throw"       , range, Priority.MediumLow),
      Completion.KeywordCompletion("true"        , range, Priority.MediumHigh, withSpace = false),
      Completion.KeywordCompletion("try"         , range, Priority.MediumLow),
      Completion.KeywordCompletion("typematch"   , range, Priority.Medium),
      // U
      Completion.KeywordCompletion("unsafe"      , range, Priority.MediumLow),
      Completion.KeywordCompletion("use"         , range, Priority.Medium),
      // W
      Completion.KeywordCompletion("with"        , range, Priority.Medium),
      Completion.KeywordCompletion("without"     , range, Priority.MediumLow),
      // Y
      Completion.KeywordCompletion("yield"       , range, Priority.Medium)
    ).filter {
      case c => qname match {
        case None => true
        case Some(qn) => qn.isUnqualified && c.name.startsWith(qn.ident.name)
      }
    }.map {
      case c: Completion.KeywordCompletion =>
        // We want keyword completions to be lower than other completions,
        // hence we downgrade the relative priorities three times.
        val p = c.priority.downgrade.downgrade.downgrade
        c.copy(priority = p)
    }

  /**
    * Returns keywords that may occur inside instance declarations.
    */
  def getInstanceKeywords(range: Range): List[Completion] =
    List(
      Completion.KeywordCompletion("def"  , range, Priority.Medium),
      Completion.KeywordCompletion("pub"  , range, Priority.Medium),
      Completion.KeywordCompletion("redef", range, Priority.Medium),
    )

  /**
    * Returns keywords that may occur inside struct declarations.
    */
  def getStructKeywords(range: Range): List[Completion] =
    List(
      Completion.KeywordCompletion("mut", range, Priority.Medium)
    )

  /**
    * Returns keywords that may occur inside trait declarations.
    */
  def getTraitKeywords(range: Range): List[Completion] =
    List(
      Completion.KeywordCompletion("def", range, Priority.Medium),
      Completion.KeywordCompletion("pub", range, Priority.Medium),
    )

  /**
    * Returns keywords that may occur inside type declarations.
    */
  def getTypeKeywords(range: Range): List[Completion] =
    List(
      Completion.KeywordCompletion("alias", range, Priority.Medium)
    )

}
