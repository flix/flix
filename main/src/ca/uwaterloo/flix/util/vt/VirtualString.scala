/*
 *  Copyright 2017 Magnus Madsen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package ca.uwaterloo.flix.util.vt

import ca.uwaterloo.flix.language.ast.SourceLocation

import scala.language.implicitConversions

sealed trait VirtualString {

  /**
    * Formats `this` text according to the given terminal context.
    */
  def fmt(implicit ctx: TerminalContext): String = this match {
    case VirtualString.Text(s) => s
    case VirtualString.Black(s) => ctx.emitBlack(s.fmt)
    case VirtualString.Blue(s) => ctx.emitBlue(s.fmt)
    case VirtualString.Cyan(s) => ctx.emitCyan(s.fmt)
    case VirtualString.Green(s) => ctx.emitGreen(s.fmt)
    case VirtualString.Magenta(s) => ctx.emitMagenta(s.fmt)
    case VirtualString.Red(s) => ctx.emitRed(s.fmt)
    case VirtualString.Yellow(s) => ctx.emitYellow(s.fmt)
    case VirtualString.White(s) => ctx.emitWhite(s.fmt)
    case VirtualString.Bold(s) => ctx.emitBold(s.fmt)
    case VirtualString.Underline(s) => ctx.emitUnderline(s.fmt)
    case VirtualString.Line(l, r) => ctx.emitBlue(s"-- $l -------------------------------------------------------------- $r\n")
    case VirtualString.Quote(t) => "'" + t.fmt + "'"
  }

}

object VirtualString {

  // TODO: Remove
  implicit def string2rich(s: String): VirtualString = VirtualString.Text(s)

  case object NewLine extends VirtualString

  case class Line(left: String, right: String) extends VirtualString

  case class Code(loc: SourceLocation, text: String) extends VirtualString

  // TODO: Remove
  case class Text(t: String) extends VirtualString

  case class Black(t: VirtualString) extends VirtualString

  case class Blue(t: VirtualString) extends VirtualString

  case class Cyan(t: VirtualString) extends VirtualString

  case class Green(t: VirtualString) extends VirtualString

  case class Magenta(t: VirtualString) extends VirtualString

  case class Red(t: VirtualString) extends VirtualString

  case class Yellow(t: VirtualString) extends VirtualString

  case class White(t: VirtualString) extends VirtualString

  case class Bold(t: VirtualString) extends VirtualString

  case class Underline(t: VirtualString) extends VirtualString

  // TODO: Remove
  case class Quote(t: VirtualString) extends VirtualString

}