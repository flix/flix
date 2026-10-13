/*
 * Copyright 2021 Magnus Madsen
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
  * Represents a `CompletionList` in LSP.
  *
  * @param isIncomplete This list is not complete. Further typing should result in recomputing this list.
  *                     Recomputed lists have all their items replaced (not appended) in the incomplete completion sessions.
  * @param items        The completion items.
  */
case class CompletionList(isIncomplete: Boolean, items: Iterable[CompletionItem]) {
  def toJSON: JValue = ("isIncomplete" -> isIncomplete) ~ ("items" -> items.map(_.toJSON))

  def toLsp4j: lsp4j.CompletionList = {
    val cl = new lsp4j.CompletionList()
    cl.setIsIncomplete(isIncomplete)
    cl.setItems(items.map(_.toLsp4j).toList.asJava)
    cl
  }
}
