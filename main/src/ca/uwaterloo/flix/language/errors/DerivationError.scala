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

import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.*
import ca.uwaterloo.flix.util.Formatter

/**
  * A common super-type for derivation errors.
  */
sealed trait DerivationError extends CompilationMessage {
  val kind: String = "Derivation Error"
}

object DerivationError {

  /**
    * An error raised to indicate an illegal derivation.
    *
    * @param sym       the class symbol of the illegal derivation.
    * @param legalSyms the list of class symbols of legal derivations.
    * @param loc       the location where the error occurred.
    */
  case class IllegalDerivation(sym: Symbol.TraitSym, legalSyms: List[Symbol.TraitSym], loc: SourceLocation) extends DerivationError {
    override def summary: String = s"Illegal derivation: ${sym.name}"

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Illegal derivation '${red(sym.name)}'.
         |
         |${code(loc, "Illegal derivation.")}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      import formatter.*
      s"${underline("Tip:")} Only the following classes may be derived: ${legalSyms.map(_.name).mkString(", ")}."
    })
  }

  /**
    * Illegal type class derivation for an empty enum.
    *
    * @param sym      the enum symbol.
    * @param classSym the class symbol of what is being derived.
    * @param loc      The source location where the error occurred.
    */
  case class IllegalDerivationForEmptyEnum(sym: Symbol.EnumSym, classSym: Symbol.TraitSym, loc: SourceLocation) extends DerivationError {
    def summary: String = s"Cannot derive '${classSym.name}' for the empty enum '${sym.name}'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Cannot derive '${magenta(classSym.name)}' for the empty enum '${red(sym.name)}'.
         |
         |${code(loc, "illegal derivation")}
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
    def summary: String = s"Cannot derive 'Coerce' for the non-singleton enum '${sym.name}'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Cannot derive '${magenta("Coerce")}' for the non-singleton enum '${red(sym.name)}'.
         |
         |${code(loc, "illegal derivation")}
         |
         |'Coerce' can only be derived for enums with exactly one case.
         |""".stripMargin
    }
  }
}
