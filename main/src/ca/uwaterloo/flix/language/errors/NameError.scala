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

import ca.uwaterloo.flix.language.ast.{Name, SourceLocation, Symbol}
import ca.uwaterloo.flix.language.{CompilationMessage, CompilationMessageKind}
import ca.uwaterloo.flix.util.Formatter

import java.nio.file.Path

/**
  * A common super-type for naming errors.
  */
sealed trait NameError extends CompilationMessage {
  val kind: CompilationMessageKind = CompilationMessageKind.NameError
}

object NameError {

  /**
    * An error raised to indicate a deprecated feature.
    *
    * @param loc the location of the deprecated feature.
    */
  case class Deprecated(loc: SourceLocation) extends NameError {
    def code: ErrorCode = ErrorCode.E5281

    def summary: String = s"Deprecated feature."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Deprecated feature. Use --Xdeprecated to enable.
         |
         |${src(loc, "deprecated")}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate that the given `name` is defined multiple time.
    *
    * @param name the name.
    * @param loc1 the location of the first name.
    * @param loc2 the location of the second name.
    */
  case class DuplicateLowerName(name: String, loc1: SourceLocation, loc2: SourceLocation) extends NameError {
    def code: ErrorCode = ErrorCode.E5394

    def summary: String = s"Duplicate definition of '$name'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Duplicate definition of '${red(name)}'.
         |
         |${src(loc1, "the first definition was here.")}
         |
         |${src(loc2, "the second definition was here.")}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      """Flix does not support overloading. For example, you cannot define two
        |functions with the same name, even if their formal parameters differ.
        |
        |If you want two functions to share the same name you have to either:
        |
        |    (a) put each function into its own namespace, or
        |    (b) introduce a trait and implement two instances.
        |""".stripMargin
    })

    def loc: SourceLocation = loc1
  }

  /**
    * An error raised to indicate that the given `name` is defined multiple time.
    *
    * @param name the name.
    * @param loc1 the location of the first name.
    * @param loc2 the location of the second name.
    */
  case class DuplicateUpperName(name: String, loc1: SourceLocation, loc2: SourceLocation) extends NameError {
    def code: ErrorCode = ErrorCode.E5407

    def summary: String = s"Duplicate definition of '$name'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Duplicate definition of '${red(name)}'.
         |
         |${src(loc1, "the first definition was here.")}
         |
         |${src(loc2, "the second definition was here.")}
         |""".stripMargin
    }

    def loc: SourceLocation = loc1

  }

  /**
    * An error raised to indicate that the module `sym` is orphaned because the module `parentSym` does not exist.
    *
    * @param sym       the orphaned module symbol.
    * @param parentSym the missing parent module symbol.
    * @param loc       the location where the orphaned module is declared.
    */
  case class OrphanModule(sym: Symbol.ModuleSym, parentSym: Symbol.ModuleSym, loc: SourceLocation) extends NameError {
    def code: ErrorCode = ErrorCode.E5512

    def summary: String = s"Module '$sym' is orphaned. Missing declaration of parent: '$parentSym'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Module '${blue(sym.toString)}' is orphaned. Missing declaration of parent: '${red(parentSym.toString)}'.
         |
         |${src(loc, "orphaned module")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that the module `qname` is wrongly declared in the file specified by `path`.
    *
    * @param qname The name of the module.
    * @param path  The real or virtual path where the module is declared.
    * @param loc   The source location the qname.
    */
  case class IllegalModuleFile(qname: Name.QName, path: Path, loc: SourceLocation) extends NameError {
    def code: ErrorCode = ErrorCode.E5623

    def summary: String = s"Module '$qname' unexpectedly declared in file '$path'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Module '${blue(qname.toString)}' unexpectedly declared in '${red(path.toString)}'.
         |
         |${src(loc, "mismatched module name and path.")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that the given `name` is a reserved name
    *
    * @param name The reserved name with location
    */
  case class IllegalReservedName(name: Name.Ident) extends NameError {
    def code: ErrorCode = ErrorCode.E5736

    def summary: String = s"Redefinition of a reserved name: ${name.name}"

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Redefinition of a reserved name: ${name.name}
         |
         |${src(name.loc, "illegal name")}
         |""".stripMargin
    }

    def loc: SourceLocation = name.loc
  }

  /**
    * An error raised to indicate a suspicious type variable name.
    *
    * @param name the name of the type variable.
    * @param loc  the location of the suspicious type variable.
    */
  case class SuspiciousTypeVarName(name: String, loc: SourceLocation) extends NameError {
    def code: ErrorCode = ErrorCode.E5849

    def summary: String = s"Suspicious type variable '$name'. Did you mean: '${name.capitalize}'?"

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Suspicious type variable '${red(name)}'. Did you mean: '${cyan(name.capitalize)}'?
         |
         |${src(loc, "suspicious type variable.")}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      """Flix uses lowercase variable names.
        |
        |The type variable looks suspiciously like the name of a built-in type.
        |
        |Perhaps you meant to use the built-in type?
        |
        |For example, `Int32` is a built-in type whereas `int32` is a type variable.
        |""".stripMargin
    })

  }
}
