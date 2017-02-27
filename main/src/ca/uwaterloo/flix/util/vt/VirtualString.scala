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
    case VirtualString.NewLine => "\n"
    case VirtualString.Text(s) => s
    case VirtualString.Black(s) => ctx.emitBlack(s)
    case VirtualString.Blue(s) => ctx.emitBlue(s)
    case VirtualString.Cyan(s) => ctx.emitCyan(s)
    case VirtualString.Green(s) => ctx.emitGreen(s)
    case VirtualString.Magenta(s) => ctx.emitMagenta(s)
    case VirtualString.Red(s) => ctx.emitRed(s)
    case VirtualString.Yellow(s) => ctx.emitYellow(s)
    case VirtualString.White(s) => ctx.emitWhite(s)
    case VirtualString.Bold(s) => ctx.emitBold(s)
    case VirtualString.Underline(s) => ctx.emitUnderline(s)
    case VirtualString.Line(l, r) => ctx.emitBlue(s"-- $l -------------------------------------------------- $r\n")
    case _ => ""
  }

}

object VirtualString {

  // TODO: Remove
  implicit def string2rich(s: String): VirtualString = VirtualString.Text(s)

  case object NewLine extends VirtualString

  case object Indent extends VirtualString

  case object Dedent extends VirtualString

  case class Line(left: String, right: String) extends VirtualString

  case class Code(loc: SourceLocation, text: String) extends VirtualString

  case class RichCode(loc: SourceLocation, text: VirtualString*) extends VirtualString

  case class Text(s: String) extends VirtualString

  case class Black(s: String) extends VirtualString

  case class Blue(s: String) extends VirtualString

  case class Cyan(s: String) extends VirtualString

  case class Green(s: String) extends VirtualString

  case class Magenta(s: String) extends VirtualString

  case class Red(s: String) extends VirtualString

  case class Yellow(s: String) extends VirtualString

  case class White(s: String) extends VirtualString

  case class Bold(s: String) extends VirtualString

  case class Underline(s: String) extends VirtualString

}