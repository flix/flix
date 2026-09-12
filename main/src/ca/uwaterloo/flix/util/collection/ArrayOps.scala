/*
 * Copyright 2024 Matthew Lutze
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.util.collection

/**
  * Operations on arrays.
  */
object ArrayOps {

  /**
    * Returns the value at the given index in the array, if it is in-bounds.
    */
  def getOption[A](arr: Array[A], i: Int): Option[A] = {
    if (i >= 0 && i < arr.length) {
      Some(arr(i))
    } else {
      None
    }
  }
}
