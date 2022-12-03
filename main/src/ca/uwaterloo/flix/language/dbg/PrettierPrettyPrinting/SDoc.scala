/*
 * Copyright 2022 Jonathan Lindegaard Starup
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

package ca.uwaterloo.flix.language.dbg.PrettierPrettyPrinting

import scala.annotation.tailrec
import scala.collection.mutable

trait SDoc

object SDoc {

  case object Nil extends SDoc

  class Text(val s: String, dd: => SDoc) extends SDoc {
    lazy val d: SDoc = dd
  }

  object Text {
    def apply(s: String, d: SDoc): Text = new Text(s, d)
    def unapply(t: Text): Option[(String, SDoc)] = Some(t.s, t.d)
  }

  class Line(val i: Int, dd: => SDoc) extends SDoc {
    lazy val d: SDoc = dd
  }

  object Line {
    def apply(s: Int, d: SDoc): Line = new Line(s, d)
    def unapply(l: Line): Option[(Int, SDoc)] = Some(l.i, l.d)
  }

  def layout(d: SDoc): String = {
    val sb = new mutable.StringBuilder()
    @tailrec
    def aux(d: SDoc): Unit = d match {
      case Nil => ()
      case Text(s, x) => sb.append(s); aux(x)
      case Line(i, x) => sb.append("\n"); sb.append(" " * i); aux(x)
    }
    aux(d)
    sb.toString
  }

  def better(w: Int, k: Int, x: => SDoc, y: => SDoc): SDoc =
    if (fits(w - k, x)) x else y

  @tailrec
  private def fits(w: Int, d: SDoc): Boolean = d match {
    case x if w < 0 => false
    case Nil => true
    case Text(s, x) => fits(w - s.length, x)
    case Line(i, d) => true
  }

}
