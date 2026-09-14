/*
 * Copyright 2024 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.util.tc

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.dbg.AstPrinter

/** Trait for values logged to disk. */
trait Debug[-A] {
  def hasAst: Boolean = true

  /** Appends the phase name to the phases file and emits the debug information. */
  def output(name: String, a: A)(implicit flix: Flix): Unit = {
    AstPrinter.appendPhaseToDisk(name, hasAst)
    emit(name, a)
  }

  /** Emit the debug information of `a` to disk. */
  protected def emit(name: String, a: A)(implicit flix: Flix): Unit
}

object Debug {

  /**
    * A [[Debug]] instance for a pair `(A, B)` (typically an AST root and its errors)
    * that emits only the first component.
    */
  implicit def debugPair[A, B](implicit d: Debug[A]): Debug[(A, B)] = new Debug[(A, B)] {
    override def hasAst: Boolean = d.hasAst

    override def output(name: String, p: (A, B))(implicit flix: Flix): Unit = d.output(name, p._1)

    override protected def emit(name: String, p: (A, B))(implicit flix: Flix): Unit = ()
  }

  /**
    * A [[Debug]] instance for an [[Option]] that emits the value if present and nothing otherwise.
    */
  implicit def debugOption[T](implicit d: Debug[T]): Debug[Option[T]] = new Debug[Option[T]] {
    override def hasAst: Boolean = d.hasAst

    override def output(name: String, o: Option[T])(implicit flix: Flix): Unit =
      o.foreach(x => d.output(name, x))

    override protected def emit(name: String, o: Option[T])(implicit flix: Flix): Unit = ()
  }

}
