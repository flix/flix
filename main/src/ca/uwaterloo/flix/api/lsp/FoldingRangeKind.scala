package ca.uwaterloo.flix.api.lsp

import org.json4s.JsonDSL._
import org.json4s._

class FoldingRangeKind {
  def toJSON: JObject =
    ("startLine" -> 123) // TODO
}
