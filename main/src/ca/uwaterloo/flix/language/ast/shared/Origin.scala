/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
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
    case Origin.Package(_) => false
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
    *
    * @param id the package, e.g. `github:flix/museum-clerk`.
    */
  case class Package(id: PackageId) extends Origin

  /**
    * A synthetic source with no origin. Used only by [[Source.Unknown]].
    */
  case object Unknown extends Origin

}
