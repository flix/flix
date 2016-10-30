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

package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.language.ast.ExecutableAst.Property
import ca.uwaterloo.flix.language.{CompilationError, Compiler}
import ca.uwaterloo.flix.util.Highlight._

/**
  * A common super-type for verification errors.
  */
sealed trait PropertyError extends CompilationError {
  val kind = "Verifier Error"
}

object PropertyError {

  /**
    * Returns a property error for the given property `prop` under the given environment `env`.
    */
  def mk(prop: Property, env: Map[String, String]): PropertyError = prop.law match {
    case Law.Associativity => AssociativityError(env, prop.loc)
    case Law.Commutativity => CommutativityError(env, prop.loc)
    case Law.Reflexivity => ReflexivityError(env, prop.loc)
    case Law.AntiSymmetry => AntiSymmetryError(env, prop.loc)
    case Law.Transitivity => TransitivityError(env, prop.loc)
    case Law.LeastElement => LeastElementError(prop.loc)
    case Law.UpperBound => UpperBoundError(env, prop.loc)
    case Law.LeastUpperBound => LeastUpperBoundError(env, prop.loc)
    case Law.GreatestElement => GreatestElementError(prop.loc)
    case Law.LowerBound => LowerBoundError(env, prop.loc)
    case Law.GreatestLowerBound => GreatestLowerBoundError(env, prop.loc)
    case Law.Strict => StrictError(prop.loc)
    case Law.Monotone => MonotoneError(env, prop.loc)
    case Law.HeightNonNegative => HeightNonNegativeError(env, prop.loc)
    case Law.HeightStrictlyDecreasing => HeightStrictlyDecreasingError(env, prop.loc)
  }

  /**
    * An error raised to indicate that a function is not associative.
    */
  case class AssociativityError(m: Map[String, String], loc: SourceLocation) extends PropertyError {
    val source = loc.source
    val message =
      hl"""|>> The function is not associative.
           |
           |Counter-example: ${Red(m.mkString(", "))}
           |
           |${Code(loc, "is not associative.")}
        """.stripMargin
  }

  /**
    * An error raised to indicate that a function is not commutative.
    */
  case class CommutativityError(m: Map[String, String], loc: SourceLocation) extends PropertyError {
    val source = loc.source
    val message =
      hl"""|>> The function is not commutative.
           |
           |Counter-example: ${Red(m.mkString(", "))}
           |
           |${Code(loc, "is not commutative.")}
        """.stripMargin
  }

  /**
    * An error raised to indicate that a partial order is not reflexive.
    */
  case class ReflexivityError(m: Map[String, String], loc: SourceLocation) extends PropertyError {
    val source = loc.source
    val message =
      hl"""|>> The partial order is not reflexive.
           |
           |Counter-example: ${Red(m.mkString(", "))}
           |
           |${Code(loc, "is not reflexive.")}
        """.stripMargin
  }

  /**
    * An error raised to indicate that a partial order is not anti-symmetric.
    */
  case class AntiSymmetryError(m: Map[String, String], loc: SourceLocation) extends PropertyError {
    val source = loc.source
    val message =
      hl"""|>> The partial order is not anti-symmetric.
           |
           |Counter-example: ${Red(m.mkString(", "))}
           |
           |${Code(loc, "is not anti-symmetric.")}
        """.stripMargin
  }

  /**
    * An error raised to indicate that a partial order is not transitive.
    */
  case class TransitivityError(m: Map[String, String], loc: SourceLocation) extends PropertyError {
    val source = loc.source
    val message =
      hl"""|>> The partial order is not transitive.
           |
           |Counter-example: ${Red(m.mkString(", "))}
           |
           |${Code(loc, "is not transitive.")}
        """.stripMargin
  }

  /**
    * An error raised to indicate that the least element is not smallest.
    */
  case class LeastElementError(loc: SourceLocation) extends PropertyError {
    val source = loc.source
    val message =
      hl"""|>> The least element is not the smallest element.
           |
           |${Code(loc, "is not the smallest element.")}
        """.stripMargin
  }

  /**
    * An error raised to indicate that the lub is not an upper bound.
    */
  case class UpperBoundError(m: Map[String, String], loc: SourceLocation) extends PropertyError {
    val source = loc.source
    val message =
      hl"""|>> The lub is not an upper bound.
           |
           |Counter-example: ${Red(m.mkString(", "))}
           |
           |${Code(loc, "is not an upper bound.")}
        """.stripMargin
  }

  /**
    * An error raised to indicate that the lub is not a least upper bound.
    */
  case class LeastUpperBoundError(m: Map[String, String], loc: SourceLocation) extends PropertyError {
    val source = loc.source
    val message =
      hl"""|>> The lub is not the least upper bound.
           |
           |Counter-example: ${Red(m.mkString(", "))}
           |
           |${Code(loc, "is not the least upper bound.")}
        """.stripMargin
  }

  /**
    * An error raised to indicate that the greatest element is not the largest.
    */
  case class GreatestElementError(loc: SourceLocation) extends PropertyError {
    val source = loc.source
    val message =
      hl"""|>> The greatest element is not the largest element.
           |
           |${Code(loc, "is not the largest element.")}
        """.stripMargin
  }

  /**
    * An error raised to indicate that the glb is not a lower bound.
    */
  case class LowerBoundError(m: Map[String, String], loc: SourceLocation) extends PropertyError {
    val source = loc.source
    val message =
      hl"""|>> The glb is not a lower bound.
           |
           |Counter-example: ${Red(m.mkString(", "))}
           |
           |${Code(loc, "is not a lower bound.")}
        """.stripMargin
  }

  /**
    * An error raised to indicate that the glb is not the greatest lower bound.
    */
  case class GreatestLowerBoundError(m: Map[String, String], loc: SourceLocation) extends PropertyError {
    val source = loc.source
    val message =
      hl"""|>> The glb is not the greatest lower bound.
           |
           |Counter-example: ${Red(m.mkString(", "))}
           |
           |${Code(loc, "is not the greatest lower bound.")}
        """.stripMargin
  }

  /**
    * An error raised to indicate that the function is not strict.
    */
  case class StrictError(loc: SourceLocation) extends PropertyError {
    val source = loc.source
    val message =
      hl"""|>> The function is not strict.
           |
           |${Code(loc, "is not strict.")}
        """.stripMargin
  }

  /**
    * An error raised to indicate that the function is not monotone.
    */
  case class MonotoneError(m: Map[String, String], loc: SourceLocation) extends PropertyError {
    val source = loc.source
    val message =
      hl"""|>> The function is not monotone.
           |
           |Counter-example: ${m.mkString(", ")}
           |
           |${Code(loc, "is not monotone.")}
        """.stripMargin
  }


  /**
    * An error raised to indicate that the height function may be negative.
    */
  case class HeightNonNegativeError(m: Map[String, String], loc: SourceLocation) extends PropertyError {
    val source = loc.source
    val message =
      hl"""|>> The height function is not non-negative.
           |
           |Counter-example: ${m.mkString(", ")}
           |
           |${Code(loc, "is not non-negative.")}
        """.stripMargin
  }

  /**
    * An error raised to indicate that the height function is not strictly decreasing.
    */
  case class HeightStrictlyDecreasingError(m: Map[String, String], loc: SourceLocation) extends PropertyError {
    val source = loc.source
    val message =
      hl"""|>> The height function is not strictly decreasing.
           |
           |Counter-example: ${m.mkString(", ")}
           |
           |${Code(loc, "is not strictly decreasing.")}
        """.stripMargin
  }

}