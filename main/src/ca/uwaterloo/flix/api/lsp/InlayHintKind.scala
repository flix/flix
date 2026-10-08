/*
 * Copyright 2022 Nicola Dardanis
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.api.lsp

import org.eclipse.lsp4j
import org.json4s.{JInt, JValue}

/**
  * Represents an `InlayHintKind` in LSP.
  */
sealed trait InlayHintKind {
  def toJSON: JValue = this match {
    case InlayHintKind.Type => JInt(1)
    case InlayHintKind.Parameter => JInt(2)
  }

  def toLsp4j: lsp4j.InlayHintKind = this match {
    case InlayHintKind.Type => lsp4j.InlayHintKind.Type
    case InlayHintKind.Parameter => lsp4j.InlayHintKind.Parameter
  }
}

object InlayHintKind {
  /**
    * An inlay hint that for a type annotation.
    */
  case object Type extends InlayHintKind
  /**
    * An inlay hint that is for a parameter.
    */
  case object Parameter extends InlayHintKind
}
