/*
 * Copyright 2016 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.language

import java.util.concurrent.atomic.AtomicInteger

final class GenSym {

  /**
    * An internal counter.
    */
  private val counter: AtomicInteger = new AtomicInteger(0)

  /**
    * Returns a freshly generated unique id.
    */
  def freshId(): Int = {
    counter.getAndIncrement()
  }

}
