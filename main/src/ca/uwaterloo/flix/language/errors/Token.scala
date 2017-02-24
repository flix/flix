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

package ca.uwaterloo.flix.language.errors

sealed trait Token {
  /**
    * Formats `this` text according to the given color context.
    */
  def fmt(implicit ctx: ColorContext): String = this match {
    case Token.Txt(s) => s
    case Token.Black(s) => ctx.fmtBlack(s.fmt)
    case Token.Blue(s) => ctx.fmtBlue(s.fmt)
    case Token.Cyan(s) => ctx.fmtCyan(s.fmt)
    case Token.Green(s) => ctx.fmtGreen(s.fmt)
    case Token.Magenta(s) => ctx.fmtMagenta(s.fmt)
    case Token.Red(s) => ctx.fmtRed(s.fmt)
    case Token.Yellow(s) => ctx.fmtYellow(s.fmt)
    case Token.White(s) => ctx.fmtWhite(s.fmt)
    case Token.Bold(s) => ctx.fmtBold(s.fmt)
    case Token.Underline(s) => ctx.fmtUnderline(s.fmt)
    case Token.Quote(t) => "'" + t.fmt + "'"
  }
}

object Token {

  implicit def string2token(s: String): Token = Token.Txt(s)

  case class Txt(t: String) extends Token

  case class Black(t: Token) extends Token

  case class Blue(t: Token) extends Token

  case class Cyan(t: Token) extends Token

  case class Green(t: Token) extends Token

  case class Magenta(t: Token) extends Token

  case class Red(t: Token) extends Token

  case class Yellow(t: Token) extends Token

  case class White(t: Token) extends Token

  case class Bold(t: Token) extends Token

  case class Underline(t: Token) extends Token

  case class Quote(t: Token) extends Token

}