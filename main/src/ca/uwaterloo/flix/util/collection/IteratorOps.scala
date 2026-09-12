/*
 * Copyright 2024 Matthew Lutze
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.util.collection

object IteratorOps {

  /**
    * Concatenates the given iterators.
    */
  def all[A](iters: IterableOnce[A]*): Iterator[A] = {
    iters.foldLeft(Iterator.empty[A])(_ ++ _)
  }

}
