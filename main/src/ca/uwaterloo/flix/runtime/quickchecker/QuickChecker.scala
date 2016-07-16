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
import ca.uwaterloo.flix.language.ast.Type
import ca.uwaterloo.flix.language.phase.Verifier.VerifierError
import ca.uwaterloo.flix.language.phase.{GenSym, Verifier}
import ca.uwaterloo.flix.runtime.verifier.SymVal.Unit
import ca.uwaterloo.flix.runtime.verifier.{PropertyResult, SymVal, SymbolicEvaluator}
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util._

import scala.language.implicitConversions
import scala.util.Random

object QuickChecker {

  // TODO: Refactor shared components:
  // - VerifierError
  // - SymbolicEvaluator.

  val Limit = 1000

  /**
    * Attempts to quickcheck all properties in the given AST.
    */
  def quickCheck(root: Root, options: Options)(implicit genSym: GenSym): Validation[Root, VerifierError] = {
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

    val envStream = genEnv(quantifiers)

    for (i <- 0 until Limit) {
      // val env = envStream.next()

      val ls = SymbolicEvaluator.eval(exp1, envStream, root)
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

    ???
  }

  def genEnv(quantifiers: List[Var]): Map[String, SymVal] = {

    val r: Random = ???

    def visit(tpe: Type): SymVal = tpe match {
      // TODO: Should use arb.
      case Type.Unit => ArbUnit.get.mk(r)
      case Type.Bool => ArbBool.get.mk(r)
      case Type.Char => ???
      case Type.Float32 => ???
      case Type.Float64 => ???
      case Type.Int8 => ArbInt8.get.mk(r)

      case Type.Enum(name, cases) => ???
      case _ => throw InternalRuntimeException(s"Unable to enumerate type `$tpe'.")
    }

    quantifiers.foldLeft(Map.empty[String, SymVal]) {
      case (macc, Var(ident, offset, tpe, loc)) => macc + (ident.name -> visit(tpe))
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Arbitrary and Generator                                                 //
  /////////////////////////////////////////////////////////////////////////////
  trait Arbitrary[A] {
    def get: Gen[A]
  }

  trait Gen[A] {
    def mk(r: Random): A
  }


  /////////////////////////////////////////////////////////////////////////////
  // Arbitrary                                                               //
  /////////////////////////////////////////////////////////////////////////////
  object ArbUnit extends Arbitrary[SymVal.Unit.type] {
    def get: Gen[SymVal.Unit.type] = GenUnit
  }

  object ArbBool extends Arbitrary[SymVal.Bool] {
    def get: Gen[SymVal.Bool] = GenBool
  }


  object ArbInt8 extends Arbitrary[SymVal.Int8] {
    def get: Gen[SymVal.Int8] = oneOf(Int8Cst(0), GenInt8)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Generators                                                              //
  /////////////////////////////////////////////////////////////////////////////

  /**
    * A trivial generator for the unit value.
    */
  object GenUnit extends Gen[SymVal.Unit.type] {
    def mk(r: Random): Unit.type = SymVal.Unit
  }

  /**
    * A generator for boolean values.
    */
  object GenBool extends Gen[SymVal.Bool] {
    def mk(r: Random): SymVal.Bool = if (r.nextBoolean()) SymVal.True else SymVal.False
  }

  /**
    * A generator for char values.
    */
  object GenChar extends Gen[SymVal.Char] {
    def mk(r: Random): SymVal.Char = ??? // TODO
  }

  /**
    * A generator for float32 values.
    */
  object GenFloat32 extends Gen[SymVal.Float32] {
    def mk(r: Random): SymVal.Float32 = ??? // TODO
  }

  /**
    * A generator for float64 values.
    */
  object GenFloat64 extends Gen[SymVal.Float64] {
    def mk(r: Random): SymVal.Float64 = ??? // TODO
  }

  /**
    * A generator for int8 values.
    */
  object GenInt8 extends Gen[SymVal.Int8] {
    def mk(r: Random): SymVal.Int8 = SymVal.Int8(???) // TODO
  }

  /**
    * A generator for the constant int8 value `c`.
    */
  case class Int8Cst(c: Byte) extends Gen[SymVal.Int8] {
    def mk(r: Random): SymVal.Int8 = SymVal.Int8(c)
  }

  /**
    * A generator for int16 values.
    */
  object GenInt16 extends Gen[SymVal.Int16] {
    def mk(r: Random): SymVal.Int16 = SymVal.Int16(???) // TODO
  }

  /**
    * A generator for int32 values.
    */
  object GenInt32 extends Gen[SymVal.Int32] {
    def mk(r: Random): SymVal.Int32 = SymVal.Int32(???) // TODO
  }

  /**
    * A generator for int64 values.
    */
  object GenInt64 extends Gen[SymVal.Int64] {
    def mk(r: Random): SymVal.Int64 = SymVal.Int64(???) // TODO
  }

  /**
    * A generator for bigint values.
    */
  object GenBigInt extends Gen[SymVal.BigInt] {
    def mk(r: Random): SymVal.BigInt = SymVal.BigInt(???) // TODO
  }

  /**
    * A generator for str values.
    */
  object GenStr extends Gen[SymVal.Str] {
    def mk(r: Random): SymVal.Str = ??? // TODO
  }

  private def oneOf[A](gs: Gen[A]*): Gen[A] = ???

  // %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
