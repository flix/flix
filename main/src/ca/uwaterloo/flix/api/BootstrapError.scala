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

import ca.uwaterloo.flix.api.effectlock.EffectLockError
import ca.uwaterloo.flix.language.ast.shared.PackageId
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.fmt.{FormatOptions, FormatSignature}
import ca.uwaterloo.flix.tools.pkg
import ca.uwaterloo.flix.tools.pkg.{LockError, ManifestError, PackageError, SemVer}
import ca.uwaterloo.flix.util.Formatter

import java.nio.file.Path

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

  case class LockParseError(e: LockError) extends BootstrapError {
    override def message(f: Formatter): String = e.message(f)
  }

  /**
    * An error raised to indicate that the project at `path` requires a newer version of Flix
    * than the one currently running.
    *
    * @param path     the path of the `flix.toml` file.
    * @param required the Flix version required by the project.
    * @param current  the Flix version currently running.
    */
  case class FlixVersionTooOld(path: Path, required: SemVer, current: SemVer) extends BootstrapError {
    override def message(f: Formatter): String =
      s"""The project requires Flix version ${f.bold(required.toString)}, but the current version is ${f.red(current.toString)}.
         |Please upgrade to Flix ${f.bold(required.toString)} or newer.
         |The project file was found at ${f.cyan(path.toString)}.
         |""".stripMargin
  }

  /**
    * An error raised to indicate that `p` is not a Flix project: there is no manifest to add
    * dependencies to, and no build output to clean.
    */
  case class NoProject(p: Path) extends BootstrapError {
    override def message(f: Formatter): String =
      s"""No ${f.cyan(p.toString)} found.
         |Run ${f.bold("flix init")} to create a project.
         |""".stripMargin
  }

  /**
    * An error raised to indicate that `spec` does not name a package.
    */
  case class IllegalPackageSpec(spec: String) extends BootstrapError {
    override def message(f: Formatter): String =
      s"""${f.red(spec)} does not name a package.
         |A package is written as ${f.bold("<owner>/<name>")}, optionally followed by ${f.bold("@<version>")}.
         |For example: ${f.cyan("flix/museum-clerk")} or ${f.cyan("flix/museum-clerk@1.1.0")}.
         |""".stripMargin
  }

  /**
    * An error raised to indicate that `id` is already a dependency of the project.
    */
  case class DependencyAlreadyDeclared(id: PackageId, version: SemVer) extends BootstrapError {
    override def message(f: Formatter): String =
      s"""${f.red(id.toString)} is already a dependency of this project, at version ${f.bold(version.toString)}.
         |Use ${f.bold("flix upgrade")} to declare it at another version.
         |""".stripMargin
  }

  /**
    * An error raised to indicate that `spec` carries a version where none is asked for.
    */
  case class UnexpectedVersion(spec: String) extends BootstrapError {
    override def message(f: Formatter): String =
      s"""${f.red(spec)} names a version, but a package is declared at one version, so there is none to choose.
         |Write the package on its own, as ${f.cyan("flix/museum-clerk")}.
         |""".stripMargin
  }

  /**
    * An error raised to indicate that `id` is not a dependency of the project.
    */
  case class DependencyNotDeclared(id: PackageId) extends BootstrapError {
    override def message(f: Formatter): String =
      s"""${f.red(id.toString)} is not a dependency of this project.
         |A package that is reached through another dependency is declared by that dependency, and not by this project.
         |""".stripMargin
  }

  /**
    * An error raised to indicate that `id` is not installed, and so cannot be locked.
    */
  case class PackageNotInstalled(id: PackageId) extends BootstrapError {
    override def message(f: Formatter): String =
      s"""${f.red(id.toString)} is not installed.
         |Only a package the project has installed can be locked or checked. Run the command on its own to cover every installed package.
         |""".stripMargin
  }

  /**
    * An error raised to indicate that `id` has no release to install.
    */
  case class NoReleases(id: PackageId) extends BootstrapError {
    override def message(f: Formatter): String =
      s"""${f.red(id.toString)} has no releases.
         |""".stripMargin
  }

  /**
    * An error raised to indicate that no mount could be chosen for `id` without asking, and that
    * there was no one to ask.
    */
  case class NoMount(id: PackageId) extends BootstrapError {
    override def message(f: Formatter): String =
      s"""Unable to choose a mount for ${f.red(id.toString)}.
         |Run ${f.bold("flix install")} without ${f.bold("--yes")} to choose one, or add the dependency to ${f.cyan(Bootstrap.FLIX_TOML)} by hand.
         |""".stripMargin
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

  case class GeneralError(e: String) extends BootstrapError {
    override def message(f: Formatter): String = e
  }

  /**
    * An error raised when a package no longer has the signatures it was locked at.
    *
    * @param e the package, the symbol, and the spec the symbol is declared with now.
    */
  case class SignaturesChangedError(e: List[(PackageId, String, TypedAst.Spec)]) extends BootstrapError {
    override def message(f: Formatter): String = {
      s"""@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         |@  WARNING! YOU MAY BE SUBJECT TO A SUPPLY CHAIN ATTACK!  @
         |@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         |            ~~ Signatures have changed! ~~
         |
         |These declarations are not the ones that were locked. A dependency may now
         |be able to do things it could not do when you locked it.
         |
         |${fmtChanges(f)}
         |
         |Do not build or run this project until you know why. Find out who published
         |the version you are building, and what changed in it.
         |
         |Record the new signatures with ${f.bold("flix eff-lock")} only once you are certain the
         |change is one the author made, and one you want.
         |""".stripMargin
    }

    /**
      * Returns a formatted string containing each package, each of its symbols that no longer has
      * the signature it was locked at, and the signature that symbol has now.
      *
      * The symbol is named under the package it belongs to, so its name is written without the
      * package, which the line above it already gives. The declaration goes on a line of its own:
      * a polymorphic declaration with constraints is long, and putting it after the name of the
      * symbol leaves it nowhere to fit.
      *
      * The declaration is written as it would be written in a source file, rather than as the
      * type scheme it gives rise to, since that is the form the reader is being asked to look at
      * the source and compare against.
      *
      * E.g., if `Clerk.work` of `github:flix/museum-clerk` is now `Unit -> Unit \ IO` then the
      * string is formatted as
      *
      * {{{
      * "  github:flix/museum-clerk:
      *     Clerk.work
      *       def work(): Unit \ IO
      * "
      * }}}
      */
    private def fmtChanges(f: Formatter): String = e.groupBy {
      case (id, _, _) => id
    }.toList.sortBy {
      case (id, _) => id
    }.map {
      case (id, changes) =>
        val formattedPkg = s"  ${f.bold(id.toString)}:"
        val formattedChanges = changes.sortBy { case (_, sym, _) => sym }.map {
          case (_, sym, spec) =>
            val name = sym.stripPrefix(s"$id.")
            val declaration = FormatSignature.formatSpecWithOptions(shortNameOf(name), spec, FormatOptions(FormatOptions.VarName.NameBased))
            s"    ${f.bold(name)}${System.lineSeparator()}      ${f.red(declaration)}"
        }.mkString(System.lineSeparator())
        s"$formattedPkg${System.lineSeparator()}$formattedChanges"
    }.mkString(System.lineSeparator())

    /**
      * Returns the name of `sym` without the modules it is declared in, which is how a
      * declaration names itself.
      */
    private def shortNameOf(sym: String): String = sym.substring(sym.lastIndexOf('.') + 1)
  }

  /**
    * An error raised when the `effects.lock` file cannot be read.
    *
    * @param e what is wrong with the file.
    */
  case class EffectLockParseError(e: EffectLockError) extends BootstrapError {
    override def message(f: Formatter): String = e.message(f)
  }
}
