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

sealed trait VirtualString

object VirtualString {

  case object NewLine extends VirtualString

  case object Indent extends VirtualString

  case object Dedent extends VirtualString

  case class line(left: String, right: String) extends VirtualString

  case class code(loc: SourceLocation, text: String) extends VirtualString

  case class text(s: String) extends VirtualString

  case class black(s: String) extends VirtualString

  case class blue(s: String) extends VirtualString

  case class cyan(s: String) extends VirtualString

  case class green(s: String) extends VirtualString

  case class magenta(s: String) extends VirtualString

  case class red(s: String) extends VirtualString

  case class yellow(s: String) extends VirtualString

  case class white(s: String) extends VirtualString

  case class bold(s: String) extends VirtualString

  case class underline(s: String) extends VirtualString

}