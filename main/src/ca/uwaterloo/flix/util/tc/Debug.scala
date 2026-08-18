/*
 * Copyright 2024 Magnus Madsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
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
