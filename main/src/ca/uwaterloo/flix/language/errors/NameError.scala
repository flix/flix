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

import ca.uwaterloo.flix.language.ast.{Name, SourceLocation, Symbol, TypedAst}
import ca.uwaterloo.flix.language.{CompilationMessage, CompilationMessageKind}
import ca.uwaterloo.flix.language.errors.Highlighter.highlight
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

    def summary: String = "Deprecated feature."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Deprecated feature.
         |
         |${highlight(loc, "deprecated feature", fmt)}
         |
         |${underline("Tip:")} Enable with the '${cyan("--Xdeprecated")}' compiler flag.
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that the given `name` is defined multiple times.
    *
    * @param name the name.
    * @param loc1 the location of the first definition.
    * @param loc2 the location of the second definition.
    */
  case class DuplicateLowerName(name: String, loc1: SourceLocation, loc2: SourceLocation) extends NameError {
    def code: ErrorCode = ErrorCode.E5394

    def summary: String = s"Duplicate definition: '$name'."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Duplicate definition: '${red(name)}'.
         |
         |${highlight(loc1, "first occurrence", fmt)}
         |
         |${highlight(loc2, "duplicate", fmt)}
         |
         |${underline("Explanation:")} Flix does not support overloading. You cannot define
         |two functions with the same name, even if their parameters differ.
         |
         |${underline("Possible fixes:")}
         |  - Put each definition into its own module.
         |  - Introduce a trait and implement two instances.
         |""".stripMargin
    }

    def loc: SourceLocation = loc1
  }

  /**
    * An error raised to indicate that the given `name` is defined multiple times.
    *
    * @param name the name.
    * @param loc1 the location of the first definition.
    * @param loc2 the location of the second definition.
    */
  case class DuplicateUpperName(name: String, loc1: SourceLocation, loc2: SourceLocation) extends NameError {
    def code: ErrorCode = ErrorCode.E5407

    def summary: String = s"Duplicate definition: '$name'."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Duplicate definition: '${red(name)}'.
         |
         |${highlight(loc1, "first occurrence", fmt)}
         |
         |${highlight(loc2, "duplicate", fmt)}
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

    def summary: String = s"Orphaned module: '$sym' (missing parent '$parentSym')."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Orphaned module: '${magenta(sym.toString)}' (missing parent '${red(parentSym.toString)}').
         |
         |${highlight(loc, "orphaned module", fmt)}
         |
         |${underline("Explanation:")} A module cannot be declared without its parent module.
         |Declare the parent module first. For example:
         |
         |  // File A.flix
         |  mod A { ... }
         |
         |  // File A/B.flix
         |  mod A.B { ... }  // OK: parent 'A' exists
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that the module `qname` is declared in an unexpected file.
    *
    * @param qname The name of the module.
    * @param path  The actual path where the module is declared.
    * @param loc   The source location of the module declaration.
    */
  case class IllegalModuleFile(qname: Name.QName, path: Path, loc: SourceLocation) extends NameError {
    def code: ErrorCode = ErrorCode.E5623

    def summary: String = s"Mismatched module and file: '$qname' in '$path'."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Mismatched module and file: '${magenta(qname.toString)}' in '${red(path.toString)}'.
         |
         |${highlight(loc, "unexpected location", fmt)}
         |
         |${underline("Explanation:")} A module must be declared in a file that matches its name.
         |For example:
         |
         |  // File A.flix
         |  mod A { ... }      // OK
         |
         |  // File A/B.flix
         |  mod A.B { ... }    // OK
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that the given `name` is a reserved name.
    *
    * @param name The reserved name with location.
    */
  case class IllegalReservedName(name: Name.Ident) extends NameError {
    def code: ErrorCode = ErrorCode.E5736

    def summary: String = s"Reserved name: '${name.name}' cannot be redefined."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Reserved name: '${red(name.name)}' cannot be redefined.
         |
         |${highlight(name.loc, "reserved identifier", fmt)}
         |
         |${underline("Explanation:")} Certain names are reserved for internal use and cannot
         |be used as identifiers. Choose a different name.
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

    def summary: String = s"Suspicious type variable: '$name'. Did you mean '${name.capitalize}'?"

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Suspicious type variable: '${red(name)}'. Did you mean '${cyan(name.capitalize)}'?
         |
         |${highlight(loc, "possible typo", fmt)}
         |
         |${underline("Explanation:")} Type variables in Flix are lowercase, but '${red(name)}'
         |looks like the built-in type '${cyan(name.capitalize)}'.
         |
         |For example, '${cyan("Int32")}' is a built-in type whereas '${red("int32")}' is a type variable.
         |""".stripMargin
    }
  }
}
