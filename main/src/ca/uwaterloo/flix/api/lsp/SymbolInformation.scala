package ca.uwaterloo.flix.api.lsp

import org.json4s.JsonDSL._
import org.json4s._

/**
 * Companion object of [[SymbolInformation]].
 */
object SymbolInformation {

}

/**
 * Represents a `SymbolInformation` in LSP.
 */
case class SymbolInformation(name: String, kind: SymbolKind, location: Location, containerName: String) {
  def toJSON: JValue =
    ("name" -> name) ~
      ("kind" -> kind.toInt) ~
      ("location" -> location.toJSON) ~
      ("containerName" -> containerName)
}