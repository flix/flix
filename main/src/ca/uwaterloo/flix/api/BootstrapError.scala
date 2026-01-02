/*
 * Copyright 2023 Anna Blume Jakobsen
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
package ca.uwaterloo.flix.api

import ca.uwaterloo.flix.language.ast.{Scheme, SourceLocation}
import ca.uwaterloo.flix.tools.pkg
import ca.uwaterloo.flix.tools.pkg.{ManifestError, PackageError}
import ca.uwaterloo.flix.util.Formatter

sealed trait BootstrapError {
  /**
    * Returns a human-readable and formatted string representation of this error.
    */
  def message(f: Formatter): String
}

object BootstrapError {
  case class ManifestParseError(e: ManifestError) extends BootstrapError {
    override def message(f: Formatter): String = e.message(f)
  }

  case class FlixPackageError(e: PackageError) extends BootstrapError {
    override def message(f: Formatter): String = e.message(f)
  }

  case class MavenPackageError(e: PackageError) extends BootstrapError {
    override def message(f: Formatter): String = e.message(f)
  }

  case class JarPackageError(e: PackageError) extends BootstrapError {
    override def message(f: Formatter): String = e.message(f)
  }

  case class ReleaseError(e: pkg.ReleaseError) extends BootstrapError {
    override def message(f: Formatter): String = e.message(f)
  }

  case class FileError(e: String) extends BootstrapError {
    override def message(f: Formatter): String = e
  }

  case class GeneralError(e: List[String]) extends BootstrapError {
    override def message(f: Formatter): String = e.mkString(System.lineSeparator())
  }

  case class EffectUpgradeError(e: List[(String, Scheme, List[SourceLocation])]) extends BootstrapError {
    override def message(f: Formatter): String = {
      s"""@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         |@  WARNING! YOU MAY BE SUBJECT TO A SUPPLY CHAIN ATTACK!  @
         |@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         |            ~~ Effect signatures have changed! ~~
         |
         |The following potentially harmful changes were detected:
         |$effectSets
         |
         |The functions are used in these places:
         |$uses
         |""".stripMargin
    }

    /**
      * Returns a formatted string containing each new symbol and what new effects it has.
      *
      * E.g.,if `f` has effect set `A, B, C` then the string is formatted as
      *
      * `"  + 'f' now uses *{ A, B, C }*"`
      */
    private def effectSets: String = {
      e.map {
        case (sym, upgrade, _) =>
          val effs = upgrade.base.effects.mkString("*{ ", ", ", " }*")
          s"  + '$sym' now uses $effs"
      }.mkString(System.lineSeparator())
    }

    private def uses: String = {
      e.map {
        case (sym, _, uses) =>
          val formattedSym = s"  '$sym':"
          val formattedUses = uses.map(loc => s"    $loc").mkString(System.lineSeparator())
          s""""$formattedSym
             |$formattedUses""".stripMargin
      }.mkString(System.lineSeparator())
    }
  }
}
