/*
 * Copyright 2021 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.language.ast.shared.Source

trait Sourceable {

  /**
    * Returns the source of `this`.
    */
  def src: Source

}
