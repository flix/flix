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
      Completion.KeywordCompletion("fix", range, Priority.Lowest(0)),
      Completion.KeywordCompletion("if" , range, Priority.Lowest(0)),
      Completion.KeywordCompletion("not", range, Priority.Lowest(0)),
    )

  /**
    * Returns keywords that may occur inside modules.
    */
  def getModKeywords(range: Range): List[Completion] =
    List(
      // D
      Completion.KeywordCompletion("def"              , range, Priority.Lowest(0)),
      // E
      Completion.KeywordCompletion("eff"              , range, Priority.Lowest(0)),
      Completion.KeywordCompletion("enum"             , range, Priority.Lowest(-1)),
      // I
      Completion.KeywordCompletion("import"           , range, Priority.Lowest(0)),
      Completion.KeywordCompletion("instance"         , range, Priority.Lowest(-1)),
      // M
      Completion.KeywordCompletion("mod"              , range, Priority.Lowest(0)),
      // P
      Completion.KeywordCompletion("pub"              , range, Priority.Lowest(0)),
      // S
      Completion.KeywordCompletion("sealed"           , range, Priority.Lowest(0)),
      Completion.KeywordCompletion("struct"           , range, Priority.Lowest(-1)),
      // T
      Completion.KeywordCompletion("trait"            , range, Priority.Lowest(-1)),
      Completion.KeywordCompletion("type"             , range, Priority.Lowest(0)),
      // U
      Completion.KeywordCompletion("use"              , range, Priority.Lowest(0)),
      // W
      Completion.KeywordCompletion("with"             , range, Priority.Lowest(0)),
    )

  /**
    * Returns keywords that may occur inside enum declarations.
    */
  def getEnumKeywords(range: Range): List[Completion] =
    List(
      Completion.KeywordCompletion("case", range, Priority.Lowest(0))
    )

  /**
    * Returns keywords that may occur inside effect declarations.
    */
  def getEffectKeywords(range: Range): List[Completion] =
    List(
      Completion.KeywordCompletion("def", range, Priority.Lowest(0))
    )

  /**
    * Returns keywords that may occur inside expressions.
    *
    * Returns only those keywords that are a prefix of the given `qname` (if present).
    */
  def getExprKeywords(qname: Option[Name.QName], range: Range): List[Completion] =
    List(
      // A
      Completion.KeywordCompletion("and"         , range, Priority.Lowest(0)),
      // C
      Completion.KeywordCompletion("catch"       , range, Priority.Lowest(0)),
      // D
      Completion.KeywordCompletion("def"         , range, Priority.Lowest(-1)),
      Completion.KeywordCompletion("discard"     , range, Priority.Lowest(0)),
      // E
      Completion.KeywordCompletion("else"        , range, Priority.Lowest(0)),
      Completion.KeywordCompletion("ematch"      , range, Priority.Lowest(0)),
      // F
      Completion.KeywordCompletion("false"       , range, Priority.Lowest(-1), withSpace = false),
      Completion.KeywordCompletion("forA"        , range, Priority.Lowest(1)),
      Completion.KeywordCompletion("forM"        , range, Priority.Lowest(1)),
      Completion.KeywordCompletion("force"       , range, Priority.Lowest(1)),
      Completion.KeywordCompletion("foreach"     , range, Priority.Lowest(0)),
      Completion.KeywordCompletion("from"        , range, Priority.Lowest(0)),
      // H
      Completion.KeywordCompletion("handler"     , range, Priority.Lowest(0)),
      // I
      Completion.KeywordCompletion("if"          , range, Priority.Lowest(0)),
      Completion.KeywordCompletion("inject"      , range, Priority.Lowest(1)),
      Completion.KeywordCompletion("instanceof"  , range, Priority.Lowest(1)),
      Completion.KeywordCompletion("into"        , range, Priority.Lowest(0)),
      // L
      Completion.KeywordCompletion("lazy"        , range, Priority.Lowest(0)),
      Completion.KeywordCompletion("let"         , range, Priority.Lowest(-1)),
      // M
      Completion.KeywordCompletion("match"       , range, Priority.Lowest(0)),
      // N
      Completion.KeywordCompletion("new"         , range, Priority.Lowest(0)),
      Completion.KeywordCompletion("not"         , range, Priority.Lowest(-1)),
      Completion.KeywordCompletion("null"        , range, Priority.Lowest(0), withSpace = false),
      // O
      Completion.KeywordCompletion("or"          , range, Priority.Lowest(0)),
      // P
      Completion.KeywordCompletion("par"         , range, Priority.Lowest(0)),
      Completion.KeywordCompletion("pquery"      , range, Priority.Lowest(0)),
      Completion.KeywordCompletion("project"     , range, Priority.Lowest(1)),
      // Q
      Completion.KeywordCompletion("query"       , range, Priority.Lowest(0)),
      // R
      Completion.KeywordCompletion("region"      , range, Priority.Lowest(0)),
      Completion.KeywordCompletion("run"         , range, Priority.Lowest(0)),
      // S
      Completion.KeywordCompletion("select"      , range, Priority.Lowest(0)),
      Completion.KeywordCompletion("solve"       , range, Priority.Lowest(0)),
      Completion.KeywordCompletion("spawn"       , range, Priority.Lowest(0)),
      // T
      Completion.KeywordCompletion("throw"       , range, Priority.Lowest(1)),
      Completion.KeywordCompletion("true"        , range, Priority.Lowest(-1), withSpace = false),
      Completion.KeywordCompletion("try"         , range, Priority.Lowest(1)),
      // U
      Completion.KeywordCompletion("unsafe"      , range, Priority.Lowest(1)),
      Completion.KeywordCompletion("use"         , range, Priority.Lowest(0)),
      // W
      Completion.KeywordCompletion("with"        , range, Priority.Lowest(0)),

      // Y
      Completion.KeywordCompletion("yield"       , range, Priority.Lowest(0))
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
      Completion.KeywordCompletion("def"  , range, Priority.Lowest(0)),
      Completion.KeywordCompletion("pub"  , range, Priority.Lowest(0)),
      Completion.KeywordCompletion("redef", range, Priority.Lowest(0)),
    )

  /**
    * Returns keywords that may occur inside struct declarations.
    */
  def getStructKeywords(range: Range): List[Completion] =
    List(
      Completion.KeywordCompletion("mut", range, Priority.Lowest(0))
    )

  /**
    * Returns keywords that may occur inside trait declarations.
    */
  def getTraitKeywords(range: Range): List[Completion] =
    List(
      Completion.KeywordCompletion("def", range, Priority.Lowest(0)),
      Completion.KeywordCompletion("pub", range, Priority.Lowest(0)),
    )

  /**
    * Returns keywords that may occur inside type declarations.
    */
  def getTypeKeywords(range: Range): List[Completion] =
    List(
      Completion.KeywordCompletion("alias", range, Priority.Lowest(0))
    )

}
