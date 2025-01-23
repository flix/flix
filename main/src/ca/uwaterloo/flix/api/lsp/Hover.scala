package ca.uwaterloo.flix.api.lsp

import org.eclipse.lsp4j
import org.json4s.JObject
import org.json4s.JsonDSL.*

object Hover {
  def from(contents: MarkupContent, range: Range): Hover = Hover(contents, range)
}

case class Hover(contents: MarkupContent, range: Range) {
  def toJSON: JObject = {
    val result = ("contents" -> contents.toJSON) ~ ("range" -> range.toJSON)
    ("status" -> ResponseStatus.Success) ~ ("result" -> result)
  }

  def toLsp4j: lsp4j.Hover = {
    val hover = new lsp4j.Hover()
    hover.setContents(contents.toLsp4j)
    hover.setRange(range.toLsp4j)
    hover
  }
}
