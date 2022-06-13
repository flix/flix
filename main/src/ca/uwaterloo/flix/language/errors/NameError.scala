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
    def summary: String = s"Ambiguous name: '$name'. The name may refer to both a variable and a use of a name."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Ambiguous name '${red(name)}'. The name may refer to both a variable and a use of a name.
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

    def explain(formatter: Formatter): Option[String] = Some({
      """Flix is not able to determine if the name refers to a local variable or to a
        |name that has been brought into scope with a use declaration.
        |""".stripMargin
    })
  }

  /**
    * An error raised to indicate that the given def `name` is defined multiple times.
    *
    * @param name the name.
    * @param loc1 the location of the first definition.
    * @param loc2 the location of the second definition.
    */
  case class DuplicateLowerName(name: String, loc1: SourceLocation, loc2: SourceLocation) extends NameError {
    def summary: String = s"Duplicate definition of '$name'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Duplicate definition of '${red(name)}'.
         |
         |${code(loc1, "the first definition was here.")}
         |
         |${code(loc2, "the second definition was here.")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      """Flix does not support function overloading, i.e. you cannot define two functions
        |with the same name, even if their formal parameters differ.
        |
        |If you want two functions to share the same name you have to either:
        |
        |    (a) put each function into its own namespace, or
        |    (b) introduce a type class and implement two instances.
        |""".stripMargin
    })

    def loc: SourceLocation = loc1
  }

  /**
    * An error raised to indicate that the given lowercase `name` is used twice.
    *
    * @param name the clashing name.
    * @param loc1 the location of the first use.
    * @param loc2 the location of the second use.
    */
  case class DuplicateUseLower(name: String, loc1: SourceLocation, loc2: SourceLocation) extends NameError {
    def summary: String = s"Duplicate use of '$name'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Duplicate use of '${red(name)}'.
         |
         |${code(loc1, "the first use was here.")}
         |
         |${code(loc2, "the second use was here.")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None

    def loc: SourceLocation = loc1
  }

  /**
    * An error raised to indicate that the given uppercase `name` is used twice.
    *
    * @param name the clashing name.
    * @param loc1 the location of the first use.
    * @param loc2 the location of the second use.
    */
  case class DuplicateUseUpper(name: String, loc1: SourceLocation, loc2: SourceLocation) extends NameError {
    def summary: String = s"Duplicate use of '$name'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Duplicate use of the type or class '${red(name)}'.
         |
         |${code(loc1, "the first use was here.")}
         |
         |${code(loc2, "the second use was here.")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None

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
    def summary: String = s"Duplicate use of '$name'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Duplicate use of the tag '${red(name)}'.
         |
         |${code(loc1, "the first use was here.")}
         |
         |${code(loc2, "the second use was here.")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None

    def loc: SourceLocation = loc1
  }

  /**
    * An error raised to indicate that the given uppercase `name` is defined multiple times.
    *
    * @param name the name.
    * @param loc1 the location of the first definition.
    * @param loc2 the location of the second definition.
    */
  case class DuplicateUpperName(name: String, loc1: SourceLocation, loc2: SourceLocation) extends NameError {
    def summary: String = s"Duplicate declaration '$name'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Duplicate declaration '${red(name)}'.
         |
         |${code(loc1, "the first occurrence was here.")}
         |
         |${code(loc2, "the second occurrence was here.")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None

    def loc: SourceLocation = loc1

  }

  /**
    * An error raised to indicate a suspicious type variable name.
    *
    * @param name the name of the type variable.
    * @param loc  the location of the suspicious type variable.
    */
  case class SuspiciousTypeVarName(name: String, loc: SourceLocation) extends NameError {
    def summary: String = s"Suspicious type variable '$name'. Did you mean: '${name.capitalize}'?"

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Suspicious type variable '${red(name)}'. Did you mean: '${cyan(name.capitalize)}'?
         |
         |${code(loc, "Suspicious type variable.")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      """Flix uses lowercase variables. The provided type variable looks suspiciously
        |like the name of a built-in type. Perhaps you meant to use the built-in type?
        |
        |For example, `Int32` is a built-in type whereas `int32` is a type variable.
        |""".stripMargin
    })

  }

  /**
    * An error raised to indicate that the class name was not found.
    *
    * @param name the class name.
    * @param loc  the location of the class name.
    */
  case class UndefinedNativeClass(name: String, loc: SourceLocation) extends NameError {
    def summary: String = s"Undefined Java class '$name'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Undefined Java class '${red(name)}'.
         |
         |${code(loc, "undefined Java class.")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      """Flix cannot find the Java class. You can check:
        |
        |    (a) if there is a simple typo.
        |    (b) that the relevant JARs are included.
        |    (c) that you are using the right Java version.
        |
        |Flix automatically includes JARs that are passed as arguments and JAR files
        |located in the `lib` directory.
        |""".stripMargin
    })
  }

  /**
    * An error raised to indicate that the local variable was not found.
    *
    * @param name the name of the variable.
    * @param loc  the location of the undefined variable.
    */
  case class UndefinedVar(name: String, loc: SourceLocation) extends NameError {
    def summary: String = s"Undefined variable '$name'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Undefined variable '${red(name)}'.
         |
         |${code(loc, "undefined variable.")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      "Flix cannot find the variable. Maybe there is a typo?"
    })
  }

  /**
    * An error raised to indicate that the type variable was not found.
    *
    * @param name the name of the type variable.
    * @param loc  the location of the undefined type variable.
    */
  case class UndefinedTypeVar(name: String, loc: SourceLocation) extends NameError {
    def summary: String = s"Undefined type variable '$name'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Undefined type variable '${red(name)}'.
         |
         |${code(loc, "undefined type variable.")}
         |""".stripMargin

    }

    def explain(formatter: Formatter): Option[String] = Some({
      "Flix cannot find the type variable. Maybe there is a typo?"
    })
  }

  /**
    * An error raised to indicate that a signature does not include the class's type parameter.
    *
    * @param name the name of the signature.
    * @param loc  the location where the error occurred.
    */
  case class IllegalSignature(name: Name.Ident, loc: SourceLocation) extends NameError {
    def summary: String = s"Unexpected signature which does not mention the type variable of the class."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Unexpected signature '${red(name.name)}' which does not mention the type variable of the class.
         |
         |${code(loc, "unexpected signature.")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      "Every signature in a type class must mention the type variable of the class."
    })

  }

}
