/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast.shared

/**
  * A host that Flix packages are published to.
  */
sealed trait Repository

object Repository {

  /** Returns `s` as a repository, if it names one Flix can download a package from. */
  def mkRepository(s: String): Option[Repository] = s match {
    case "github" => Some(Repository.GitHub)
    case _ => None
  }

  /** A GitHub repository. */
  case object GitHub extends Repository {
    override def toString: String = "github"
  }

}
