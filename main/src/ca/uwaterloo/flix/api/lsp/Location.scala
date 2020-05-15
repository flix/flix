package ca.uwaterloo.flix.api.lsp

import ca.uwaterloo.flix.language.ast.SourceLocation
import org.json4s.JsonAST.{JField, JObject, JString}

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
  def toJSON: JObject = {
    JObject(
      JField("uri", JString(uri)),
      JField("range", range.toJSON),
    )
  }
}
