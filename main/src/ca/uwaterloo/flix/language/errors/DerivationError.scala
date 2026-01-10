/*
 * Copyright 2022 Paul Butcher
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

import ca.uwaterloo.flix.language.{CompilationMessage, CompilationMessageKind}
import ca.uwaterloo.flix.language.ast.*
import ca.uwaterloo.flix.util.Formatter

/**
  * A common super-type for derivation errors.
  */
sealed trait DerivationError extends CompilationMessage {
  val kind: CompilationMessageKind = CompilationMessageKind.DerivationError
}

object DerivationError {

  /**
    * An error raised to indicate an illegal derivation.
    *
    * @param sym       the trait symbol of the illegal derivation.
    * @param legalSyms the list of trait symbols of legal derivations.
    * @param loc       the location where the error occurred.
    */
  case class IllegalDerivation(sym: Symbol.TraitSym, legalSyms: List[Symbol.TraitSym], loc: SourceLocation) extends DerivationError {
    def code: ErrorCode = ErrorCode.E0147

    override def summary: String = s"Illegal derivation: ${sym.name}"

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Illegal derivation '${red(sym.name)}'.
         |
         |${src(loc, "Illegal derivation.")}
         |
         |${underline("Tip:")} Only the following traits may be derived: ${legalSyms.map(_.name).mkString(", ")}.
         |""".stripMargin
    }
  }

  /**
    * Illegal trait derivation for an empty enum.
    *
    * @param sym      the enum symbol.
    * @param traitSym the trait symbol of what is being derived.
    * @param loc      The source location where the error occurred.
    */
  case class IllegalDerivationForEmptyEnum(sym: Symbol.EnumSym, traitSym: Symbol.TraitSym, loc: SourceLocation) extends DerivationError {
    def code: ErrorCode = ErrorCode.E0283

    def summary: String = s"Cannot derive '${traitSym.name}' for the empty enum '${sym.name}'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Cannot derive '${magenta(traitSym.name)}' for the empty enum '${red(sym.name)}'.
         |
         |${src(loc, "illegal derivation")}
         |
         |Flix cannot derive any instances for an empty enumeration.
         |""".stripMargin
    }
  }

  /**
    * An error to indicate the derivation of Coerce for a non-singleton enum.
    *
    * @param sym the enum symbol
    * @param loc the source location where the error occurred.
    */
  case class IllegalNonSingletonCoerce(sym: Symbol.EnumSym, loc: SourceLocation) extends DerivationError {
    def code: ErrorCode = ErrorCode.E0519

    def summary: String = s"Cannot derive 'Coerce' for the non-singleton enum '${sym.name}'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Cannot derive '${magenta("Coerce")}' for the non-singleton enum '${red(sym.name)}'.
         |
         |${src(loc, "illegal derivation")}
         |
         |'Coerce' can only be derived for enums with exactly one case.
         |""".stripMargin
    }
  }
}
