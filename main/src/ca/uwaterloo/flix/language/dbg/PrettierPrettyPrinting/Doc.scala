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

import scala.collection.immutable

trait Doc

object Doc {

  implicit class DocOps(d: Doc) {
    def <>(d: Doc): Doc = Doc.<>(this.d, d)

    def <+>(d: Doc): Doc = Doc.<+>(this.d, d)

    def <\>(d: Doc): Doc = Doc.<\>(this.d, d)

    def <+\>(d: Doc): Doc = Doc.<+\>(this.d, d)

    def :<>(d: Doc): Doc = Doc.:<>(this.d, d)

    def :<|>(d: Doc): Doc = Doc.:<|>(this.d, d)
  }

  case object Nil extends Doc

  class :<>(dd1: => Doc, dd2: => Doc) extends Doc {
    lazy val d1: Doc = dd1
    lazy val d2: Doc = dd2
  }

  object :<> {
    def apply(d1: => Doc, d2: => Doc): :<> = new :<>(d1, d2)

    def unapply(c: :<>): Option[(Doc, Doc)] = Some(c.d1, c.d2)
  }

  class Nest(val i: Int, dd: => Doc) extends Doc {
    lazy val d: Doc = dd
  }

  object Nest {
    def apply(i: Int, d: => Doc): Nest = new Nest(i, d)

    def unapply(t: Nest): Option[(Int, Doc)] = Some(t.i, t.d)
  }

  case class Text(s: String) extends Doc

  case object Line extends Doc

  class :<|>(dd1: => Doc, dd2: => Doc) extends Doc {
    lazy val d1: Doc = dd1
    lazy val d2: Doc = dd2
  }

  object :<|> {
    def apply(d1: => Doc, d2: => Doc): :<|> = new :<|>(d1, d2)

    def unapply(u: :<|>): Option[(Doc, Doc)] = Some(u.d1, u.d2)
  }


  def nil: Doc = Nil

  def <>(d1: Doc, d2: Doc): Doc = d1 :<> d2

  def nest(x: Doc)(implicit indent: Int): Doc = Nest(indent, x)

  def text(s: String): Doc = Text(s)

  def line: Doc = Line

  def group(x: Doc, to: String = " "): Doc = flatten(x :<|> x, to)

  private def flatten(d: Doc, to: String = " "): Doc = d match {
    case Nil => Nil
    case x :<> y => flatten(x, to) :<> flatten(y, to)
    case Nest(i, x) => Nest(i, flatten(x, to))
    case Text(s) => Text(s)
    case Line => Text(to)
    case x :<|> y => flatten(x, to)
  }

  /**
    *
    * @param w available width
    * @param k chars already placed
    */
  private def best(w: Int, k: Int, x: Doc): SDoc = be(w, k, List((0, x)))

  private def be(w: Int, k: Int, x: List[(Int, Doc)]): SDoc = x match {
    case immutable.Nil => SDoc.Nil
    case (i, Nil) :: z => be(w, k, z)
    case (i, x :<> y) :: z => be(w, k, (i, x) :: (i, y) :: z)
    case (i, Nest(j, x)) :: z => be(w, k, (i + j, x) :: z)
    case (i, Text(s)) :: z => SDoc.Text(s, be(w, k + s.length, z))
    case (i, Line) :: z => SDoc.Line(i, be(w, i, z))
    case (i, x :<|> y) :: z =>
      SDoc.better(w, k, be(w, k, (i, x) :: z), be(w, k, (i, y) :: z))
  }

  def pretty(w: Int, x: Doc): String = SDoc.layout(best(w, 0, x))


  // aux
  def <+>(d1: Doc, d2: Doc): Doc = d1 <> text(" ") <> d2

  def <\>(d1: Doc, d2: Doc): Doc = d1 <> line <> d2

  def fold(f: (Doc, Doc) => Doc, d: List[Doc]): Doc = d match {
    case immutable.Nil => Nil
    case x :: immutable.Nil => x
    case x :: xs => f(x, fold(f, xs))
  }

  def spread(d: List[Doc]): Doc = fold(_ <+> _, d)

  def stack(d: List[Doc]): Doc = fold(_ <\> _, d)

  def bracket(l: String, x: Doc, r: String)(implicit indent: Int): Doc = group(
    text(l) <> nest(line <> x) <\> text(r)
  )

  def <+\>(x: Doc, y: Doc): Doc = x <> (text(" ") :<|> line) <> y

  def fillWords(s: String): Doc = fold(_ <+\> _, s.split(" ").map(text).toList)

  def fill(d: List[Doc]): Doc = d match {
    case immutable.Nil => nil
    case x :: immutable.Nil => x
    case x :: y :: zs =>
      (flatten(x) <+> fill(flatten(y) :: zs)) :<|> (x <\> fill(y :: zs))
  }

  def parens(d: Doc)(implicit indent: Int): Doc = bracket("(", d, ")")

}

