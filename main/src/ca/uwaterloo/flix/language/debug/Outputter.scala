/*
 * Copyright 2017 Magnus Madsen
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

package ca.uwaterloo.flix.language.debug

import ca.uwaterloo.flix.util.Highlight._

class Outputter {
  val sb = new StringBuilder()
  var i = 0

  def indent(): Outputter = {
    i = i + 1
    this
  }

  def dedent(): Outputter = {
    i = i - 1
    this
  }

  def text(s: Any): Outputter = {
    sb.append(s.toString)
    this
  }

  def bold(s: AnyRef): Outputter = {
    sb.append(Bold(s.toString).toString)
    this
  }

  def underline(s: AnyRef): Outputter = {
    sb.append(Underline(s.toString).toString)
    this
  }

  def blue(s: AnyRef): Outputter = {
    sb.append(Blue(s.toString).toString)
    this
  }

  def cyan(s: AnyRef): Outputter = {
    sb.append(Cyan(s.toString).toString)
    this
  }

  def magenta(s: AnyRef): Outputter = {
    sb.append(Magenta(s.toString).toString)
    this
  }

  def red(s: AnyRef): Outputter = {
    sb.append(Red(s.toString).toString)
    this
  }

  def newline(): Outputter = {
    sb.append("\n")
    for (i <- 0 until i) {
      sb.append("  ")
    }
    this
  }

  override def toString: String = sb.toString()
}