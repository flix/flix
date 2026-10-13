/*
 * Copyright 2023 Holger Dal Mogensen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.api.lsp

import ca.uwaterloo.flix.util.Result
import ca.uwaterloo.flix.util.Result.{Ok, Err}
import org.eclipse.lsp4j
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

  def toLsp4j: String = this match {
    case CodeActionKind.Empty => lsp4j.CodeActionKind.Empty
    case CodeActionKind.QuickFix => lsp4j.CodeActionKind.QuickFix
    case CodeActionKind.Refactor => lsp4j.CodeActionKind.Refactor
    case CodeActionKind.RefactorExtract => lsp4j.CodeActionKind.RefactorExtract
    case CodeActionKind.RefactorInline => lsp4j.CodeActionKind.RefactorInline
    case CodeActionKind.RefactorRewrite => lsp4j.CodeActionKind.RefactorRewrite
    case CodeActionKind.Source => lsp4j.CodeActionKind.Source
    case CodeActionKind.SourceOrganizeImports => lsp4j.CodeActionKind.SourceOrganizeImports
    case CodeActionKind.SourceFixAll => lsp4j.CodeActionKind.SourceFixAll
  }
}

object CodeActionKind {
  def parse(json: JValue): Result[CodeActionKind, String] = json match {
    case JString(s) => s match {
      case "" => Ok(Empty)
      case "quickfix" => Ok(QuickFix)
      case "refactor" => Ok(Refactor)
      case "refactor.extract" => Ok(RefactorExtract)
      case "refactor.inline" => Ok(RefactorInline)
      case "refactor.rewrite" => Ok(RefactorRewrite)
      case "source" => Ok(Source)
      case "source.organizeImports" => Ok(SourceOrganizeImports)
      case "source.fixAll" => Ok(SourceFixAll)
      case v => Err(s"Unexpected code action kind string value: '$v'.")
    }
    case v => Err(s"Unexpected non-string code action kind: '$v'.")
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
