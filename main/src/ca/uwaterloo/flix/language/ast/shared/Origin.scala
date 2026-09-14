/*
 * Copyright 2026 Magnus Madsen
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
package ca.uwaterloo.flix.language.ast.shared

/**
  * Where a source came from.
  *
  * The origin is the only thing to consult when a phase or a tool needs to know whether a location
  * belongs to the code the user wrote, to the library bundled with the compiler, or to a package
  * the program depends on.
  */
sealed trait Origin {

  /**
    * Returns `true` if the source was supplied by the user: a file on the command line, a file in
    * a project, a fragment typed into the REPL, or a buffer open in an editor.
    */
  def isUser: Boolean = this match {
    case Origin.User => true
    case Origin.Library => false
    case Origin.Package => false
    case Origin.Unknown => false
  }

}

object Origin {

  /**
    * A source supplied by the user.
    */
  case object User extends Origin

  /**
    * A source of the library bundled with the compiler, whether core or standard.
    */
  case object Library extends Origin

  /**
    * A source unpacked from a Flix package the program depends on.
    */
  case object Package extends Origin

  /**
    * A synthetic source with no origin. Used only by [[Source.Unknown]].
    */
  case object Unknown extends Origin

}
