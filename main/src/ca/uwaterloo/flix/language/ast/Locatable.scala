/*
 * Copyright 2021 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.language.ast.shared.Source

trait Locatable extends Sourceable {

  /**
    * Returns the source location of `this`.
    */
  def loc: SourceLocation

  /**
    * Returns the source of `this`.
    */
  override def src: Source = loc.source

}
