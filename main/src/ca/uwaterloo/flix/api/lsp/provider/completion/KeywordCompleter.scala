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
      Completion.KeywordCompletion("fix", range, Priority.Medium(0)),
      Completion.KeywordCompletion("if" , range, Priority.Medium(0)),
      Completion.KeywordCompletion("not", range, Priority.Medium(0)),
    )

  /**
    * Returns keywords that may occur inside modules.
    */
  def getModKeywords(range: Range): List[Completion] =
    List(
      // D
      Completion.KeywordCompletion("def"              , range, Priority.Medium(0)),
      // E
      Completion.KeywordCompletion("eff"              , range, Priority.Medium(0)),
      Completion.KeywordCompletion("enum"             , range, Priority.Medium(-1)),
      // I
      Completion.KeywordCompletion("import"           , range, Priority.Medium(0)),
      Completion.KeywordCompletion("instance"         , range, Priority.Medium(-1)),
      // M
      Completion.KeywordCompletion("mod"              , range, Priority.Medium(0)),
      // P
      Completion.KeywordCompletion("pub"              , range, Priority.Medium(0)),
      // S
      Completion.KeywordCompletion("sealed"           , range, Priority.Medium(0)),
      Completion.KeywordCompletion("struct"           , range, Priority.Medium(-1)),
      // T
      Completion.KeywordCompletion("trait"            , range, Priority.Medium(-1)),
      Completion.KeywordCompletion("type"             , range, Priority.Medium(0)),
      // U
      Completion.KeywordCompletion("use"              , range, Priority.Medium(0)),
      // W
      Completion.KeywordCompletion("with"             , range, Priority.Medium(0)),
    )

  /**
    * Returns keywords that may occur inside enum declarations.
    */
  def getEnumKeywords(range: Range): List[Completion] =
    List(
      Completion.KeywordCompletion("case", range, Priority.Medium(0))
    )

  /**
    * Returns keywords that may occur inside effect declarations.
    */
  def getEffectKeywords(range: Range): List[Completion] =
    List(
      Completion.KeywordCompletion("def", range, Priority.Medium(0))
    )

  /**
    * Returns keywords that may occur inside expressions.
    *
    * Returns only those keywords that are a prefix of the given `qname` (if present).
    */
  def getExprKeywords(qname: Option[Name.QName], range: Range): List[Completion] =
    List(
      // A
      Completion.KeywordCompletion("and"         , range, Priority.Medium(0)),
      // C
      Completion.KeywordCompletion("catch"       , range, Priority.Medium(0)),
      // D
      Completion.KeywordCompletion("def"         , range, Priority.Medium(-1)),
      Completion.KeywordCompletion("discard"     , range, Priority.Medium(0)),
      // E
      Completion.KeywordCompletion("else"        , range, Priority.Medium(0)),
      // F
      Completion.KeywordCompletion("false"       , range, Priority.Medium(-1), withSpace = false),
      Completion.KeywordCompletion("forA"        , range, Priority.Medium(1)),
      Completion.KeywordCompletion("forM"        , range, Priority.Medium(1)),
      Completion.KeywordCompletion("force"       , range, Priority.Medium(1)),
      Completion.KeywordCompletion("foreach"     , range, Priority.Medium(0)),
      Completion.KeywordCompletion("from"        , range, Priority.Medium(0)),
      // H
      Completion.KeywordCompletion("handler"     , range, Priority.Medium(0)),
      // I
      Completion.KeywordCompletion("if"          , range, Priority.Medium(0)),
      Completion.KeywordCompletion("inject"      , range, Priority.Medium(1)),
      Completion.KeywordCompletion("instanceof"  , range, Priority.Medium(1)),
      Completion.KeywordCompletion("into"        , range, Priority.Medium(0)),
      // L
      Completion.KeywordCompletion("lazy"        , range, Priority.Medium(0)),
      Completion.KeywordCompletion("let"         , range, Priority.Medium(-1)),
      // M
      Completion.KeywordCompletion("match"       , range, Priority.Medium(0)),
      // N
      Completion.KeywordCompletion("new"         , range, Priority.Medium(0)),
      Completion.KeywordCompletion("not"         , range, Priority.Medium(-1)),
      Completion.KeywordCompletion("null"        , range, Priority.Medium(0), withSpace = false),
      // O
      Completion.KeywordCompletion("or"          , range, Priority.Medium(0)),
      // P
      Completion.KeywordCompletion("par"         , range, Priority.Medium(0)),
      Completion.KeywordCompletion("project"     , range, Priority.Medium(1)),
      // Q
      Completion.KeywordCompletion("query"       , range, Priority.Medium(0)),
      // R
      Completion.KeywordCompletion("region"      , range, Priority.Medium(0)),
      Completion.KeywordCompletion("run"         , range, Priority.Medium(0)),
      // S
      Completion.KeywordCompletion("select"      , range, Priority.Medium(0)),
      Completion.KeywordCompletion("solve"       , range, Priority.Medium(0)),
      Completion.KeywordCompletion("spawn"       , range, Priority.Medium(0)),
      // T
      Completion.KeywordCompletion("throw"       , range, Priority.Medium(1)),
      Completion.KeywordCompletion("true"        , range, Priority.Medium(-1), withSpace = false),
      Completion.KeywordCompletion("try"         , range, Priority.Medium(1)),
      Completion.KeywordCompletion("typematch"   , range, Priority.Medium(0)),
      // U
      Completion.KeywordCompletion("unsafe"      , range, Priority.Medium(1)),
      Completion.KeywordCompletion("use"         , range, Priority.Medium(0)),
      // W
      Completion.KeywordCompletion("with"        , range, Priority.Medium(0)),
      Completion.KeywordCompletion("without"     , range, Priority.Medium(1)),
      // Y
      Completion.KeywordCompletion("yield"       , range, Priority.Medium(0))
    ).filter {
      case c => qname match {
        case None => true
        case Some(qn) => qn.isUnqualified && c.name.startsWith(qn.ident.name)
      }
    }

  /**
    * Returns keywords that may occur inside instance declarations.
    */
  def getInstanceKeywords(range: Range): List[Completion] =
    List(
      Completion.KeywordCompletion("def"  , range, Priority.Medium(0)),
      Completion.KeywordCompletion("pub"  , range, Priority.Medium(0)),
      Completion.KeywordCompletion("redef", range, Priority.Medium(0)),
    )

  /**
    * Returns keywords that may occur inside struct declarations.
    */
  def getStructKeywords(range: Range): List[Completion] =
    List(
      Completion.KeywordCompletion("mut", range, Priority.Medium(0))
    )

  /**
    * Returns keywords that may occur inside trait declarations.
    */
  def getTraitKeywords(range: Range): List[Completion] =
    List(
      Completion.KeywordCompletion("def", range, Priority.Medium(0)),
      Completion.KeywordCompletion("pub", range, Priority.Medium(0)),
    )

  /**
    * Returns keywords that may occur inside type declarations.
    */
  def getTypeKeywords(range: Range): List[Completion] =
    List(
      Completion.KeywordCompletion("alias", range, Priority.Medium(0))
    )

}
