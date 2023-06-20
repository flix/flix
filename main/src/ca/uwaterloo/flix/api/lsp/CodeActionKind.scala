/*
 * Copyright 2023 Holger Dal Mogensen
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
package ca.uwaterloo.flix.api.lsp

import ca.uwaterloo.flix.util.Result
import ca.uwaterloo.flix.util.Result.{Ok, Err}
import org.json4s.{JString, JValue}

sealed trait CodeActionKind {
  def toJSON: JValue = this match {
    case CodeActionKind.Empty => JString("")
    case CodeActionKind.QuickFix => JString("quickfix")
    case CodeActionKind.Refactor => JString("refactor")
    case CodeActionKind.RefactorExtract => JString("refactor.extract")
    case CodeActionKind.RefactorInline => JString("refactor.inline")
    case CodeActionKind.RefactorRewrite => JString("refactor.rewrite")
    case CodeActionKind.Source => JString("source")
    case CodeActionKind.SourceOrganizeImports => JString("source.organizeImports")
    case CodeActionKind.SourceFixAll => JString("source.fixAll")
  }
}

object CodeActionKind {
  def parse(json: JValue): Result[CodeActionKind, String] = json match {
    case JString("") => Ok(CodeActionKind.Empty)
    case JString("quickfix") => Ok(CodeActionKind.QuickFix)
    case JString("refactor") => Ok(CodeActionKind.Refactor)
    case JString("refactor.extract") => Ok(CodeActionKind.RefactorExtract)
    case JString("refactor.inline") => Ok(CodeActionKind.RefactorInline)
    case JString("refactor.rewrite") => Ok(CodeActionKind.RefactorRewrite)
    case JString("source") => Ok(CodeActionKind.Source)
    case JString("source.organizeImports") => Ok(CodeActionKind.SourceOrganizeImports)
    case JString("source.fixAll") => Ok(CodeActionKind.SourceFixAll)
    case v => Err(s"Code action kind not recognized as a valid string: '$v'.")
  }

  case object Empty extends CodeActionKind

  case object QuickFix extends CodeActionKind

  case object Refactor extends CodeActionKind

  case object RefactorExtract extends CodeActionKind

  case object RefactorInline extends CodeActionKind

  case object RefactorRewrite extends CodeActionKind

  case object Source extends CodeActionKind

  case object SourceOrganizeImports extends CodeActionKind

  case object SourceFixAll extends CodeActionKind
}
