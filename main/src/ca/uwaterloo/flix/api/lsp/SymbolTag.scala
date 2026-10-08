/*
 * Copyright 2021 Nicola Dardanis
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.api.lsp

import org.eclipse.lsp4j
import org.json4s.JsonAST.{JInt, JValue}

/**
  * Represents a `SymbolTag` in LSP.
  */
trait SymbolTag {
  def toJSON: JValue = this match {
    case SymbolTag.Deprecated => JInt(1)
  }

  def toLsp4j: lsp4j.SymbolTag = this match {
    case SymbolTag.Deprecated => lsp4j.SymbolTag.Deprecated
  }
}

object SymbolTag {
  case object Deprecated extends SymbolTag
}
