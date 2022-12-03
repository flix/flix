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

package ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.grouped

import scala.annotation.tailrec
import scala.collection.mutable

trait Doc

object Doc {

  implicit class DocOps(d: Doc) {
    def <>(d: Doc): Doc = Doc.<>(this.d, d)

    def <+>(d: Doc): Doc = Doc.<+>(this.d, d)
  }

  case object Nix extends Doc

  class Text(val s: String, dd: => Doc) extends Doc {
    lazy val d: Doc = dd
  }

  object Text {
    def apply(s: String, d: => Doc): Text = new Text(s, d)
    def unapply(t: Text): Option[(String, Doc)] = Some(t.s, t.d)
  }

  class Line(val i: Int, dd: => Doc) extends Doc {
    lazy val d: Doc = dd
  }

  object Line {
    def apply(i: Int, d: => Doc): Line = new Line(i, d)
    def unapply(l: Line): Option[(Int, Doc)] = Some(l.i, l.d)
  }

  class Union(dd1: => Doc, dd2: => Doc) extends Doc {
    lazy val d1: Doc = dd1
    lazy val d2: Doc = dd2
  }

  object Union {
    def apply(d1: => Doc, d2: => Doc): Union = new Union(d1, d2)
    def unapply(u: Union): Option[(Doc, Doc)] = Some(u.d1, u.d2)
  }

  def nil: Doc = Nix

  def text(s: String): Doc = Text(s, Nix)

  def line: Doc = Line(0, Nix)

  def <>(d1: Doc, d2: Doc): Doc = (d1, d2) match {
    case (Text(s, x), y) => Text(s, x <> y)
    case (Line(i, x), y) => Line(i, x <> y)
    case (Nix, y) => y
    case (Union(x, y), z) => Union(x <> z, y <> z)
  }

  def nest(indent: Int, d: Doc): Doc = (indent, d) match {
    case (i, Text(s, x)) => Text(s, nest(i, x))
    case (i, Line(j, x)) => Line(i + j, nest(i, x))
    case (i, Nix) => Nix
    case (k, Union(x, y)) => Union(nest(k, x), nest(k, y))
  }

  private def layout(d: Doc): String = {
    val sb = new mutable.StringBuilder()
    @tailrec
    def aux(d: Doc): Unit = d match {
      case Doc.Text(s, x) => sb.append(s); aux(x)
      case Doc.Line(i, x) => sb.append("\n"); sb.append(" " * i); aux(x)
      case Doc.Nix => ()
      case Union(_, _) => throw new AssertionError("fits was called on union")
    }
    aux(d)
    sb.toString
  }

  def group(d: Doc, to: String = " "): Doc = d match {
    case Nix => Nix
    case Line(i, x) => Union(Text(to, flatten(x, to)), Line(i, x))
    case Text(s, x) => Text(s, group(x, to))
    case Union(x, y) => Union(group(x, to), y)
  }

  private def flatten(d: Doc, to: String = " "): Doc = d match {
    case Nix => Nix
    case Line(i, x) => Text(to, flatten(x, to))
    case Text(s, x) => Text(s, flatten(x, to))
    case Union(x, y) => flatten(x, to)
  }

  /**
    *
    * @param w available width
    * @param k chars already placed
    */
  private def best(w: Int, k: Int, d: Doc): Doc = d match {
    case Nix => Nix
    case Line(i, x) => Line(i, best(w, i, x))
    case Text(s, x) => Text(s, best(w, k + s.length, x))
    case Union(x, y) => better(w, k, best(w, k, x), best(w, k, y))
  }

  private def better(w: Int, k: Int, x: => Doc, y: => Doc): Doc =
    if (fits(w-k, x)) x else y

  @tailrec
  private def fits(diff: Int, d: Doc): Boolean = (diff, d) match {
    case (w, x) if w < 0 => false
    case (w, Nix) => true
    case (w, Text(s, x)) => fits(w - s.length, x)
    case (w, Line(i, d)) => true
    case (_, Union(_, _)) => throw new AssertionError("fits was called on union")
  }

  def pretty(w: Int, d: Doc): String = layout (best(w, 0, d))


  // aux
  def <+>(d1: Doc, d2: Doc): Doc = d1 <> text(" ") <> d2

  def parens(d: Doc): Doc = text("(") <> d <> text(")")

  def indent(d: Doc)(implicit indent: Int): Doc = nest(indent, d)
}

