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
import ca.uwaterloo.flix.util.Formatter

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

    def message(formatter: Formatter): String = {
      s"""${formatter.line(kind, source.format)}
         |>> Ambiguous name '${formatter.red(name)}'. The name may refer to both a variable and a use.
         |
         |${formatter.code(loc, "ambiguous name.")}
         |
         |The relevant declarations are:
         |
         |${formatter.code(loc1, "the 'var' was declared here.")}
         |
         |${formatter.code(loc2, "the 'use' was declared here.")}
         |""".stripMargin
    }

    /**
      * Returns a formatted string with helpful suggestions.
      */
    override def explain(formatter: Formatter): Option[String] = None
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

    def message(formatter: Formatter): String = {
      s"""${formatter.line(kind, source.format)}
         |>> Duplicate definition '${formatter.red(name)}'.
         |
         |${formatter.code(loc1, "the first occurrence was here.")}
         |
         |${formatter.code(loc2, "the second occurrence was here.")}
         |""".stripMargin
    }

    def loc: SourceLocation = loc1

    override def explain(formatter: Formatter): Option[String] = Some(s"${formatter.underline("Tip:")} Remove or rename one of the occurrences.")
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

    def message(formatter: Formatter): String = {
      s"""${formatter.line(kind, source.format)}
         |>> Duplicate use of '${formatter.red(name)}'.
         |
         |${formatter.code(loc1, "the first use was here.")}
         |
         |${formatter.code(loc2, "the second use was here.")}
         |""".stripMargin
    }

    def loc: SourceLocation = loc1

    /**
      * Returns a formatted string with helpful suggestions.
      */
    override def explain(formatter: Formatter): Option[String] = None
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

    def message(formatter: Formatter): String = {
      s"""${formatter.line(kind, source.format)}
         |>> Duplicate use of the type or class '${formatter.red(name)}'.
         |
         |${formatter.code(loc1, "the first use was here.")}
         |
         |${formatter.code(loc2, "the second use was here.")}
         |""".stripMargin
    }

    def loc: SourceLocation = loc1

    /**
      * Returns a formatted string with helpful suggestions.
      */
    override def explain(formatter: Formatter): Option[String] = None
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

    def message(formatter: Formatter): String = {
      s"""${formatter.line(kind, source.format)}
         |>> Duplicate use of the tag '${formatter.red(name)}'.
         |
         |${formatter.code(loc1, "the first use was here.")}
         |
         |${formatter.code(loc2, "the second use was here.")}
         |""".stripMargin
    }

    def loc: SourceLocation = loc1

    /**
      * Returns a formatted string with helpful suggestions.
      */
    override def explain(formatter: Formatter): Option[String] = None
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

    def message(formatter: Formatter): String = {
      s"""${formatter.line(kind, source.format)}
         |>> Duplicate type or class declaration '${formatter.red(name)}'.
         |
         |${formatter.code(loc1, "the first occurrence was here.")}
         |
         |${formatter.code(loc2, "the second occurrence was here.")}
         |""".stripMargin
    }

    def loc: SourceLocation = loc1

    override def explain(formatter: Formatter): Option[String] = Some(s"${formatter.underline("Tip:")} Remove or rename one of the occurrences.")

  }

  /**
    * An error raised to indicate a suspicious type variable name.
    *
    * @param name the name of the type variable.
    * @param loc  the location of the suspicious type variable.
    */
  case class SuspiciousTypeVarName(name: String, loc: SourceLocation) extends NameError {
    def summary: String = s"Suspicious type variable. Did you mean: '${name.capitalize}'?"

    def message(formatter: Formatter): String = {
      s"""${formatter.line(kind, source.format)}
         |>> Suspicious type variable '${formatter.red(name)}'. Did you mean: '${formatter.cyan(name.capitalize)}'?
         |
         |${formatter.code(loc, "Suspicious type variable.")}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some(s"${formatter.underline("Tip:")} Type variables are always lowercase. Named types are uppercase.")

  }

  /**
    * An error raised to indicate that the class name was not found.
    *
    * @param name the class name.
    * @param loc  the location of the class name.
    */
  case class UndefinedNativeClass(name: String, loc: SourceLocation) extends NameError {
    def summary: String = s"Undefined class."

    def message(formatter: Formatter): String = {
      s"""${formatter.line(kind, source.format)}
         |>> Undefined class '${formatter.red(name)}'.
         |
         |${formatter.code(loc, "undefined class.")}
         |""".stripMargin
    }

    /**
      * Returns a formatted string with helpful suggestions.
      */
    override def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate that the local variable was not found.
    *
    * @param name the name of the variable.
    * @param loc  the location of the undefined variable.
    */
  case class UndefinedVar(name: String, loc: SourceLocation) extends NameError {
    def summary: String = s"Undefined variable."

    def message(formatter: Formatter): String = {
      s"""${formatter.line(kind, source.format)}
         |>> Undefined variable '${formatter.red(name)}'.
         |
         |${formatter.code(loc, "undefined variable.")}
         |""".stripMargin
    }

    /**
      * Returns a formatted string with helpful suggestions.
      */
    override def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate that the type variable was not found.
    *
    * @param name the name of the type variable.
    * @param loc  the location of the undefined type variable.
    */
  case class UndefinedTypeVar(name: String, loc: SourceLocation) extends NameError {
    def summary: String = s"Undefined type variable."

    def message(formatter: Formatter): String = {
      s"""${formatter.line(kind, source.format)}
         |>> Undefined type variable '${formatter.red(name)}'.
         |
         |${formatter.code(loc, "undefined type variable.")}
         |""".stripMargin


    }

    /**
      * Returns a formatted string with helpful suggestions.
      */
    override def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate that a signature does not include the class's type parameter.
    *
    * @param name the name of the signature.
    * @param loc  the location where the error occurred.
    */
  case class IllegalSignature(name: Name.Ident, loc: SourceLocation) extends NameError {
    def summary: String = "Illegal signature."

    def message(formatter: Formatter): String = {
      s"""${formatter.line(kind, source.format)}
         |>> Illegal signature '${formatter.red(name.name)}'.
         |
         |${formatter.code(loc, "Illegal signature.")}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some(s"${formatter.underline("Tip:")} Change the signature to include the class type parameter, or remove the signature.")

  }

}
