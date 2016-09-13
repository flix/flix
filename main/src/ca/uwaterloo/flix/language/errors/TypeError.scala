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

import ca.uwaterloo.flix.language.{CompilationError, Compiler}
import ca.uwaterloo.flix.language.ast.{ResolvedAst, SourceLocation, Type}

/**
  * A common super-type for type errors.
  */
sealed trait TypeError extends CompilationError

object TypeError {

  implicit val consoleCtx = Compiler.ConsoleCtx

  /**
    * An error raised to indicate a type mismatch between an `expected` and an `actual` type.
    *
    * @param expected the expected type.
    * @param actual   the actual type.
    * @param loc      the source location.
    */
  case class ExpectedType(expected: Type, actual: Type, loc: SourceLocation) extends TypeError {
    val message =
      s"""${consoleCtx.blue(s"-- TYPE ERROR -------------------------------------------------- ${loc.source.format}")}
         |
         |${consoleCtx.red(s">> Expected type `$expected' but actual type is `$actual'.")}
         |
         |${loc.highlight}
         """.stripMargin
  }

  /**
    * An error raised to indicate that the two given types `tpe1` and `tpe2` were expected to be equal.
    *
    * @param tpe1 the first type.
    * @param tpe2 the second type.
    * @param loc1 the source location of the first type.
    * @param loc2 the source location of the second type.
    */
  case class ExpectedEqualTypes(tpe1: Type, tpe2: Type, loc1: SourceLocation, loc2: SourceLocation) extends TypeError {
    val message =
      s"""${consoleCtx.blue(s"-- TYPE ERROR -------------------------------------------------- ${loc1.source.format}")}
         |
         |${consoleCtx.red(s">> Expected equal types `$tpe1' and `$tpe2'.")}
         |
         |${loc1.highlight}
         |${loc2.highlight}
         """.stripMargin
  }

  /**
    * An error raised to indicate that the given type `tpe` was expected to be a function type.
    *
    * @param tpe the erroneous type.
    * @param loc the source location.
    */
  // TODO: Pretty print
  case class IllegalApply(tpe: Type, loc: SourceLocation) extends TypeError {
    val message = s"Type Error: The type `$tpe' is not a function type at ${loc.format}.\n"
  }

  /**
    * An error raised to indicate a type mismatch between a pattern `pat` and an expected type `tpe`.
    *
    * @param pat the pattern.
    * @param tpe the type.
    * @param loc the source location.
    */
  // TODO: Pretty print
  case class IllegalPattern(pat: ResolvedAst.Pattern, tpe: Type, loc: SourceLocation) extends TypeError {
    val message = s"Type Error: Pattern `$pat' does not match expected type `$tpe' at ${loc.format}.\n"
  }

  // TODO: Check arity of function calls, predicates, etc.

  /**
    * An error raised to indicate that a type has no associated lattice.
    *
    * @param tpe the type that has no lattice.
    * @param loc the source location.
    */
  case class NoSuchLattice(tpe: Type, loc: SourceLocation) extends TypeError {
    val message =
      s"""${consoleCtx.blue(s"-- TYPE ERROR -------------------------------------------------- ${loc.source.format}")}
         |
         |${consoleCtx.red(s">> No lattice declared for `$tpe'.")}
         |
         |${loc.highlight}
         |Tip: Associate a lattice with the type.
         """.stripMargin
  }

  // TODO
  case class MergeError() extends TypeError {
    val message = "MergeError" // TODO
  }

  // TODO
  case class UnificationError(tpe1: Type, tpe2: Type) extends TypeError {
    val message = "UnificationError" // TODO
  }

  // TODO
  case class OccursCheck() extends TypeError {
    val message = "OccursCheck" // TODO
  }

  // TODO
  case class KindError() extends TypeError {
    val message = "KindError" // TODO
  }

}