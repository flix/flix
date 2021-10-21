/*
 * Copyright 2016 Magnus Madsen
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

package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.{Name, SourceLocation}
import ca.uwaterloo.flix.util.vt.VirtualString._

/**
 * A common super-type for naming errors.
 */
sealed trait NameError extends CompilationMessage {
  val kind = "Name Error"
}

object NameError {

  /**
   * An error raised to indicate that the given `name` is ambiguous.
   *
   * @param name the ambiguous name.
   * @param loc  the location of the ambiguous name.
   * @param loc1 the location of the var.
   * @param loc2 the location of the use.
   */
  case class AmbiguousVarOrUse(name: String, loc: SourceLocation, loc1: SourceLocation, loc2: SourceLocation) extends NameError {
    def summary: String = s"Ambiguous name. The name may refer to both a variable and a use."

    def message: String = {
      s"""${line(kind, source.format)}
         |>> Ambiguous name '${red(name)}'. The name may refer to both a variable and a use.
         |
         |${code(loc, "ambiguous name.")}
         |
         |The relevant declarations are:
         |
         |${code(loc1, "the 'var' was declared here.")}
         |
         |${code(loc2, "the 'use' was declared here.")}
         |""".stripMargin
    }
  }

  /**
   * An error raised to indicate that the given def `name` is defined multiple times.
   *
   * @param name the name.
   * @param loc1 the location of the first definition.
   * @param loc2 the location of the second definition.
   */
  case class DuplicateDefOrSig(name: String, loc1: SourceLocation, loc2: SourceLocation) extends NameError {
    def summary: String = s"Duplicate definition."

    def message: String = {
      s"""${line(kind, source.format)}
         |>> Duplicate definition '${red(name)}'.
         |
         |${code(loc1, "the first occurrence was here.")}
         |
         |${code(loc2, "the second occurrence was here.")}
         |""".stripMargin
    }

    def loc: SourceLocation = loc1

    override def explain: String = s"${underline("Tip:")} Remove or rename one of the occurrences."
  }

  /**
   * An error raised to indicate that the given def or sig `name` is used twice.
   *
   * @param name the clashing name.
   * @param loc1 the location of the first use.
   * @param loc2 the location of the second use.
   */
  case class DuplicateUseDefOrSig(name: String, loc1: SourceLocation, loc2: SourceLocation) extends NameError {
    def summary: String = s"Duplicate use."

    def message: String = {
      s"""${line(kind, source.format)}
         |>> Duplicate use of '${red(name)}'.
         |
         |${code(loc1, "the first use was here.")}
         |
         |${code(loc2, "the second use was here.")}
         |""".stripMargin
    }

    def loc: SourceLocation = loc1
  }

  /**
   * An error raised to indicate that the given type or class `name` is used twice.
   *
   * @param name the clashing name.
   * @param loc1 the location of the first use.
   * @param loc2 the location of the second use.
   */
  case class DuplicateUseTypeOrClass(name: String, loc1: SourceLocation, loc2: SourceLocation) extends NameError {
    def summary: String = s"Duplicate use."

    def message: String = {
      s"""${line(kind, source.format)}
         |>> Duplicate use of the type or class '${red(name)}'.
         |
         |${code(loc1, "the first use was here.")}
         |
         |${code(loc2, "the second use was here.")}
         |""".stripMargin
    }

    def loc: SourceLocation = loc1

  }

  /**
   * An error raised to indicate that the given `tag` is used twice.
   *
   * @param name the clashing name.
   * @param loc1 the location of the first use.
   * @param loc2 the location of the second use.
   */
  case class DuplicateUseTag(name: String, loc1: SourceLocation, loc2: SourceLocation) extends NameError {
    def summary: String = s"Duplicate use."

    def message: String = {
      s"""${line(kind, source.format)}
         |>> Duplicate use of the tag '${red(name)}'.
         |
         |${code(loc1, "the first use was here.")}
         |
         |${code(loc2, "the second use was here.")}
         |""".stripMargin
    }

    def loc: SourceLocation = loc1

  }

  /**
   * An error raised to indicate that the given type alias or enum `name` is defined multiple times.
   *
   * @param name the name.
   * @param loc1 the location of the first definition.
   * @param loc2 the location of the second definition.
   */
  case class DuplicateTypeOrClass(name: String, loc1: SourceLocation, loc2: SourceLocation) extends NameError {
    def summary: String = s"Duplicate type or class declaration."

    def message: String = {
      s"""${line(kind, source.format)}
         |>> Duplicate type or class declaration '${red(name)}'.
         |
         |${code(loc1, "the first occurrence was here.")}
         |
         |${code(loc2, "the second occurrence was here.")}
         |""".stripMargin
    }

    def loc: SourceLocation = loc1

    override def explain: String = s"${underline("Tip:")} Remove or rename one of the occurrences."

  }

  /**
   * An error raised to indicate a suspicious type variable name.
   *
   * @param name the name of the type variable.
   * @param loc  the location of the suspicious type variable.
   */
  case class SuspiciousTypeVarName(name: String, loc: SourceLocation) extends NameError {
    def summary: String = s"Suspicious type variable. Did you mean: '${name.capitalize}'?"

    def message: String = {
      s"""${line(kind, source.format)}
         |>> Suspicious type variable '${red(name)}'. Did you mean: '${cyan(name.capitalize)}'?
         |
         |${code(loc, "Suspicious type variable.")}
         |""".stripMargin
    }

    override def explain: String = s"${underline("Tip:")} Type variables are always lowercase. Named types are uppercase."

  }

  /**
   * An error raised to indicate that the class name was not found.
   *
   * @param name the class name.
   * @param loc  the location of the class name.
   */
  case class UndefinedNativeClass(name: String, loc: SourceLocation) extends NameError {
    def summary: String = s"Undefined class."

    def message: String = {
      s"""${line(kind, source.format)}
         |>> Undefined class '${red(name)}'.
         |
         |${code(loc, "undefined class.")}
         |""".stripMargin
    }
  }

  /**
   * An error raised to indicate that the local variable was not found.
   *
   * @param name the name of the variable.
   * @param loc  the location of the undefined variable.
   */
  case class UndefinedVar(name: String, loc: SourceLocation) extends NameError {
    def summary: String = s"Undefined variable."

    def message: String = {
      s"""${line(kind, source.format)}
         |>> Undefined variable '${red(name)}'.
         |
         |${code(loc, "undefined variable.")}
         |""".stripMargin
    }
  }

  /**
   * An error raised to indicate that the type variable was not found.
   *
   * @param name the name of the type variable.
   * @param loc  the location of the undefined type variable.
   */
  case class UndefinedTypeVar(name: String, loc: SourceLocation) extends NameError {
    def summary: String = s"Undefined type variable."

    def message: String = {
      s"""${line(kind, source.format)}
         |>> Undefined type variable '${red(name)}'.
         |
         |${code(loc, "undefined type variable.")}
         |""".stripMargin


    }
  }

  /**
   * An error raised to indicate that a signature does not include the class's type parameter.
   *
   * @param name the name of the signature.
   * @param loc  the location where the error occurred.
   */
  case class IllegalSignature(name: Name.Ident, loc: SourceLocation) extends NameError {
    def summary: String = "Illegal signature."

    def message: String = {
      s"""${line(kind, source.format)}
         |>> Illegal signature '${red(name.name)}'.
         |
         |${code(loc, "Illegal signature.")}
         |""".stripMargin
    }

    override def explain: String = s"${underline("Tip:")} Change the signature to include the class type parameter, or remove the signature."

  }

}
