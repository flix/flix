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

package ca.uwaterloo.flix.runtime.quickchecker

import ca.uwaterloo.flix.language.ast.ExecutableAst.Expression.Var
import ca.uwaterloo.flix.language.ast.ExecutableAst.{Property, Root}
import ca.uwaterloo.flix.language.ast.{ExecutableAst, Type}
import ca.uwaterloo.flix.language.phase.Verifier.VerifierError
import ca.uwaterloo.flix.language.phase.{GenSym, Verifier}
import ca.uwaterloo.flix.runtime.verifier.SymVal.Tuple
import ca.uwaterloo.flix.runtime.verifier.{PropertyResult, SymVal, SymbolicEvaluator}
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{Options, Validation, Verbosity}

import scala.language.implicitConversions
import scala.util.Random

object QuickChecker {

  // TODO: Refactor shared components:
  // - VerifierError
  // - SymbolicEvaluator.


  /**
    * Attempts to quickcheck all properties in the given AST.
    */
  def quickCheck(root: ExecutableAst.Root, options: Options)(implicit genSym: GenSym): Validation[ExecutableAst.Root, VerifierError] = {
    /*
     * Check if the quickchecker is enabled. Otherwise return success immediately.
     */
    if (!options.quickchecker) {
      return root.toSuccess
    }

    /*
     * Verify each property.
     */
    val results = root.properties.map(p => quickCheckProperty(p, root))

    /*
     * Print verbose information (if enabled).
     */
    if (options.verbosity == Verbosity.Verbose) {
      // TODO
      //printVerbose(results)
    }

    /*
     * Returns the original AST root if all properties verified successfully.
     */
    if (isSuccess(results)) {
      root.toSuccess
    } else {
      val errors = results.collect {
        case PropertyResult.Failure(_, _, _, _, error) => error
      }
      val unknowns = results.collect {
        case PropertyResult.Unknown(_, _, _, _, error) => error
      }
      Validation.Failure((errors ++ unknowns).toVector)
    }
  }

  /**
    * Attempts to quickcheck the given `property`.
    */
  def quickCheckProperty(property: Property, root: Root)(implicit genSym: GenSym): PropertyResult = {
    val exp0 = property.exp

    val exp1 = Verifier.peelUniversallyQuantifiers(exp0)

    val quantifiers = Verifier.getUniversallyQuantifiedVariables(exp0)

    val env0 = genEnv(quantifiers)

    val ls = SymbolicEvaluator.eval(exp1, env0, root)

    ls.head match {
      case (Nil, SymVal.True) =>
        // Case 1: The symbolic evaluator proved the property.
        PropertyResult.Success(property, 0, 0, 0)
      case (Nil, SymVal.False) =>
        // Case 2: The symbolic evaluator disproved the property.
        val err = ??? // TODO
        PropertyResult.Failure(property, 0, 0, 0, err)
      case (_, v) => throw new IllegalStateException(s"The symbolic evaluator returned a non-boolean value: $v.")
    }
  }

  def genEnv(quantifiers: List[Var]): Map[String, SymVal] = {

    def visit(tpe: Type): List[SymVal] = tpe match {
      case _ => ???
    }

    ???
  }

  /////////////////////////////////////////////////////////////////////////////
  // Stream API                                                              //
  /////////////////////////////////////////////////////////////////////////////

  /**
    * A global random number generator.
    */
  // TODO: Move into each stream.
  val Random = new Random()


  /**
    * A common super-type for infinite generator streams.
    */
  trait Stream[A] {
    def head: A

    def tail: Stream[A]

    def #::(h: A): Stream[A] = Hd(h, this)
  }

  /**
    * A single stream element.
    */
  case class Hd[A](head: A, tail: Stream[A]) extends Stream[A]

  /////////////////////////////////////////////////////////////////////////////
  // Generators                                                              //
  /////////////////////////////////////////////////////////////////////////////

  /**
    * Generates a stream of boolean values.
    */
  class GenBool extends Stream[SymVal] {
    val stream = SymVal.True #:: SymVal.False #:: this

    def head: SymVal = stream.head

    def tail: Stream[SymVal] = stream.tail
  }

  /**
    * Generates a stream of tuple values.
    */
  class GenTuple[A](gs: List[Stream[A]]) extends Stream[SymVal.Tuple] {
    def head: Tuple = ???

    def tail: Stream[Tuple] = ???
  }

  /**
    * Generates a stream of Int32 values.
    */
  class GenInt32 extends Stream[Int] {
    val stream = 0 #:: 1 #:: -1 #:: 2 #:: -2 #:: 3 #:: -3 #:: GenRandomInt32

    def head: Int = stream.head

    def tail: Stream[Int] = stream.tail
  }

  /**
    * Generates Random Int32 values.
    */
  object GenRandomInt32 extends Stream[Int] {
    def head: Int = Random.nextInt()

    def tail: Stream[Int] = GenRandomInt32
  }

  /**
    * Generates Random Int64 values.
    */
  object GenRandomInt64 extends Stream[SymVal] {
    def head: SymVal = SymVal.Int64(Random.nextLong())

    def tail: Stream[SymVal] = GenRandomInt64
  }


  /**
    * Returns `true` if all the given property results `rs` are successful
    */
  // TODO: Share?
  private def isSuccess(rs: List[PropertyResult]): Boolean = rs.forall {
    case p: PropertyResult.Success => true
    case p: PropertyResult.Failure => false
    case p: PropertyResult.Unknown => false
  }
}
