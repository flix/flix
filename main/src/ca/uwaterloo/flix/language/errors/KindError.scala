/*
 * Copyright 2021 Matthew Lutze
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
import ca.uwaterloo.flix.language.ast.{Kind, SourceLocation, Symbol, TypedAst}
import ca.uwaterloo.flix.language.errors.Highlighter.highlight
import ca.uwaterloo.flix.language.fmt.FormatKind.formatKind
import ca.uwaterloo.flix.util.{Formatter, Grammar}

/**
  * A common super-type for kind errors.
  */
sealed trait KindError extends CompilationMessage {
  val kind: CompilationMessageKind = CompilationMessageKind.KindError
}

object KindError {

  /**
    * An error raised to indicate wrong number of type arguments for an enum.
    *
    * @param sym           the enum symbol.
    * @param expectedArity the expected number of type arguments.
    * @param actualArity   the actual number of type arguments.
    * @param loc           the location where the error occurred.
    */
  case class MismatchedArityOfEnum(sym: Symbol.EnumSym, expectedArity: Int, actualArity: Int, loc: SourceLocation) extends KindError {
    def code: ErrorCode = ErrorCode.E3414

    private val expected = Grammar.n_things(expectedArity, "type argument")
    private val actual = Grammar.n_things(actualArity, "type argument")
    private val wasOrWere = if (actualArity == 1) "was" else "were"

    def summary: String =
      s"Enum '${sym.name}' expects $expected but $actual $wasOrWere given."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Enum '${cyan(sym.name)}' expects $expected but $actual $wasOrWere given.
         |
         |${highlight(loc, "wrong number of type arguments", fmt)}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate wrong number of type arguments for a struct.
    *
    * @param sym           the struct symbol.
    * @param expectedArity the expected number of type arguments.
    * @param actualArity   the actual number of type arguments.
    * @param loc           the location where the error occurred.
    */
  case class MismatchedArityOfStruct(sym: Symbol.StructSym, expectedArity: Int, actualArity: Int, loc: SourceLocation) extends KindError {
    def code: ErrorCode = ErrorCode.E3421

    private val expected = Grammar.n_things(expectedArity, "type argument")
    private val actual = Grammar.n_things(actualArity, "type argument")
    private val wasOrWere = if (actualArity == 1) "was" else "were"

    def summary: String =
      s"Struct '${sym.name}' expects $expected but $actual $wasOrWere given."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Struct '${cyan(sym.name)}' expects $expected but $actual $wasOrWere given.
         |
         |${highlight(loc, "wrong number of type arguments", fmt)}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate two incompatible kinds.
    *
    * @param k1  the first kind.
    * @param k2  the second kind.
    * @param loc the location where the error occurred.
    */
  case class MismatchedKinds(k1: Kind, k2: Kind, loc: SourceLocation) extends KindError {
    def code: ErrorCode = ErrorCode.E3407

    override def summary: String = s"Mismatched kinds: '${formatKind(k1)}' and '${formatKind(k2)}'."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Mismatched kinds: '${red(formatKind(k1))}' and '${red(formatKind(k2))}'.
         |
         |${highlight(loc, "mismatched kind usage", fmt)}
         |
         |First kind:  ${cyan(formatKind(k1))}
         |Second kind: ${magenta(formatKind(k2))}
         |
         |${underline("Explanation:")} A type variable must have a consistent kind throughout
         |its scope. For example:
         |
         |  def f(x: a): Int32 \\ a = ???
         |
         |Here 'a' is used as both a type (x: a) and an effect (\\ a), which is impossible.
         |""".stripMargin
    }
  }

  /**
    * An error describing a kind that doesn't match the expected kind.
    *
    * @param expectedKind the expected kind.
    * @param actualKind   the actual kind.
    * @param loc          the location where the error occurred.
    */
  case class UnexpectedKind(expectedKind: Kind, actualKind: Kind, loc: SourceLocation) extends KindError {
    def code: ErrorCode = ErrorCode.E3512

    override def summary: String = s"Unexpected kind: expected '${formatKind(expectedKind)}', found '${formatKind(actualKind)}'."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Unexpected kind: expected '${cyan(formatKind(expectedKind))}', found '${red(formatKind(actualKind))}'.
         |
         |${highlight(loc, "has unexpected kind", fmt)}
         |
         |Expected: ${cyan(formatKind(expectedKind))}
         |Actual:   ${red(formatKind(actualKind))}
         |""".stripMargin
    }
  }

  /**
    * An error resulting from a type whose kind cannot be inferred.
    *
    * @param loc The location where the error occurred.
    */
  case class UninferrableKind(loc: SourceLocation) extends KindError {
    def code: ErrorCode = ErrorCode.E3623

    override def summary: String = "Uninferrable kind: cannot determine kind from context."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      s""">> Uninferrable kind: cannot determine kind from context.
         |
         |${highlight(loc, "uninferrable kind", fmt)}
         |
         |${underline("Explanation:")} The kind of this type cannot be determined from the
         |surrounding context. Add a kind annotation to resolve the ambiguity.
         |""".stripMargin
    }
  }
}
