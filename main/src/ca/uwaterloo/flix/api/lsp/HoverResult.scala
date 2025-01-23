package ca.uwaterloo.flix.api.lsp

import org.eclipse.lsp4j
import org.json4s.JObject
import org.json4s.JsonDSL.*

sealed trait HoverResult{
  def toJSON: JObject
  def toLsp4j: lsp4j.Hover
}

case class Hover(contents: MarkupContent, range: Range) extends HoverResult {
  override def toJSON: JObject = {
    val result = ("contents" -> contents.toJSON) ~ ("range" -> range.toJSON)
    ("status" -> ResponseStatus.Success) ~ ("result" -> result)
  }

  override def toLsp4j: lsp4j.Hover = {
    val hover = new lsp4j.Hover()
    hover.setContents(contents.toLsp4j)
    hover.setRange(range.toLsp4j)
    hover
  }
}

case class HoverNotFound(uri: String, pos: Position) extends HoverResult {
  override def toJSON: JObject = ("status" -> ResponseStatus.InvalidRequest) ~ ("message" -> s"Nothing found in '$uri' at '$pos'.")

  override def toLsp4j: lsp4j.Hover = {
    val hover = new lsp4j.Hover()
    hover.setContents(new lsp4j.MarkupContent("plaintext", s"Nothing found in '$uri' at '$pos'."))
    hover
  }
}
