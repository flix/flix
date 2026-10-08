/*
 * Copyright 2020 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.api.lsp

import org.eclipse.lsp4j
import org.json4s.JsonDSL.*
import org.json4s.*

import scala.jdk.CollectionConverters.*

/**
  * Represents a `WorkspaceEdit` in LSP.
  *
  * @param changes A map from URIs to lists of changes.
  */
case class WorkspaceEdit(changes: Map[String, List[TextEdit]]) {
  def toJSON: JValue = ("changes" -> mapValues(changes)(xs => xs.map(_.toJSON)))

  private def mapValues[K, V1, V2](m: Map[K, V1])(f: V1 => V2): Map[K, V2] = m map {
    case (k, v) => k -> f(v)
  }

  def toLsp4j: lsp4j.WorkspaceEdit = {
    val w = new lsp4j.WorkspaceEdit()
    for ((uri, edits) <- changes) {
      val textEdits = edits.map(_.toLsp4j).asJava
      w.getChanges.put(uri, textEdits)
    }
    w
  }
}
