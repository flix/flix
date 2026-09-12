/*
 * Copyright 2022 Jonathan Lindegaard Starup
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.api.lsp.provider

import ca.uwaterloo.flix.api.{CompilerConstants, Flix}

import java.nio.file.Path

object ShowAstProvider {

  /**
    * Returns a Path
    */
  def showAst()(implicit flix: Flix): Path = {
    val oldOpts = flix.options
    flix.setOptions(oldOpts.copy(xprintphases = true))
    flix.compile()
    flix.setOptions(oldOpts)
    CompilerConstants.AstDirectory
  }
}
