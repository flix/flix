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

package ca.uwaterloo.flix.language.dbg.PrettierPrettyPrinting.Simple

import scala.annotation.tailrec
import scala.collection.mutable

sealed trait Doc

object Doc {

  implicit class DocOps(s: Doc) {
    def <>(d: Doc): Doc = Doc.<>(this.s, d)

    def <+>(d: Doc): Doc = Doc.<+>(this.s, d)
  }

  case object Nix extends Doc

  case class Text(s: String, d: Doc) extends Doc

  case class Line(i: Int, d: Doc) extends Doc

  def <>(d1: Doc, y: Doc): Doc = d1 match {
    case Doc.Nix => y
    case Doc.Text(s, x) => Doc.Text(s, x <> y)
    case Doc.Line(i, x) => Doc.Line(i, x <> y)
  }

  def text(s: String): Doc = Doc.Text(s, Doc.Nix)

  def nil: Doc = Doc.Nix

  def line: Doc = Doc.Line(0, Doc.Nix)

  def nest(i: Int, d: Doc): Doc = d match {
    case Doc.Nix => d
    case Doc.Text(s, x) => Doc.Text(s, nest(i, x))
    case Doc.Line(j, x) => Doc.Line(i + j, nest(i, x))
  }

  def layout(d: Doc): String = {
    val sb = new mutable.StringBuilder()
    @tailrec
    def aux(d: Doc): Unit = d match {
      case Doc.Text(s, x) => sb.append(s); aux(x)
      case Doc.Line(i, x) => sb.append("\n"); sb.append(" " * i); aux(x)
      case Doc.Nix => ()
    }
    aux(d)
    sb.toString
  }

  // aux
  def <+>(d1: Doc, d2: Doc): Doc = d1 <> text(" ") <> d2

  def parens(d: Doc): Doc = text("(") <> d <> text(")")

  def indent(d: Doc)(implicit indent: Int): Doc = nest(indent, d)
}

