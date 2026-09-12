/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.util.collection

/**
  * Operations on options.
  */
object OptionOps {

  /**
    * Applies `f` to the value of `opt`, returning `opt` itself if `f`
    * returns a reference-equal (`eq`) value.
    *
    * Callers can detect "nothing changed" with a single reference equality check.
    */
  def mapWithReuse[T <: AnyRef](opt: Option[T])(f: T => T): Option[T] = opt match {
    case Some(x) =>
      val y = f(x)
      if (y eq x) opt else Some(y)
    case None => opt
  }
}
