/*
 * Copyright 2020 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.api.lsp

import ca.uwaterloo.flix.language.ast.SourceLocation

import org.eclipse.lsp4j
import org.json4s.JsonDSL.*
import org.json4s.*

/**
  * Companion object of [[Location]].
  */
object Location {
  def from(loc: SourceLocation): Location = Location(loc.source.name, Range.from(loc))
}

/**
  * Represents a `Location` in LSP.
  */
case class Location(uri: String, range: Range) {
  def toJSON: JValue = ("uri" -> uri) ~ ("range" -> range.toJSON)
  def toLsp4j: lsp4j.Location = new lsp4j.Location(uri, range.toLsp4j)
}
