/*
 * Copyright 2020 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.fmt

import ca.uwaterloo.flix.language.ast.shared.Doc

object FormatDoc {

  /**
    * Returns a markdown string for the given documentation `doc`.
    */
  def asMarkDown(doc: Doc): String = doc.lines.mkString("\n")

}
