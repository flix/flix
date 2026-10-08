/*
 * Copyright 2021 Nicola Dardanis
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.api.lsp

import org.eclipse.lsp4j
import org.json4s.JsonDSL.*
import org.json4s.*

import scala.jdk.CollectionConverters.SeqHasAsJava

/**
  * Represents a `DocumentSymbol` in LSP.
  *
  * @param name           The name of this symbol. Will be displayed in the user interface. Should be non-empty.
  * @param detail         More detail for this symbol, e.g the signature of a function.
  * @param kind           The kind of this symbol.
  * @param range          The range enclosing this symbol not including leading/trailing whitespace
  *                       but everything else like comments. This information is typically used to
  *                       determine if the clients cursor is inside the symbol to reveal in the
  *                       symbol in the UI.
  * @param selectionRange The range that should be selected and revealed when this symbol is being
  *                       picked, e.g. the name of a function. Must be contained by the `range`.
  * @param tags           Tags for this symbol.
  * @param children       Children of this symbol, e.g. properties of a class.
  */
case class DocumentSymbol(name: String,
                          detail: Option[String],
                          kind: SymbolKind,
                          range: Range,
                          selectionRange: Range,
                          tags: List[SymbolTag] = List(),
                          children: List[DocumentSymbol]) {
  def toJSON: JValue =
    ("name" -> name) ~
      ("detail" -> detail) ~
      ("kind" -> JInt(kind.toInt)) ~
      ("range" -> range.toJSON) ~
      ("selectionRange" -> selectionRange.toJSON) ~
      ("tags" -> tags.map(_.toJSON)) ~
      ("children" -> children.map(_.toJSON))

  def toLsp4j: lsp4j.DocumentSymbol = {
    val ds = new lsp4j.DocumentSymbol()
    ds.setName(name)
    ds.setDetail(detail.getOrElse(""))
    ds.setKind(kind.toLsp4j)
    ds.setRange(range.toLsp4j)
    ds.setSelectionRange(selectionRange.toLsp4j)
    ds.setTags(tags.map(_.toLsp4j).asJava)
    ds.setChildren(children.map(_.toLsp4j).asJava)
    ds
  }
}

