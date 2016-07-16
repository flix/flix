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

import ca.uwaterloo.flix.language.Compiler
import ca.uwaterloo.flix.language.ast.ExecutableAst.Expression.Var
import ca.uwaterloo.flix.language.ast.ExecutableAst.{Property, Root}
import ca.uwaterloo.flix.language.ast.Type
import ca.uwaterloo.flix.language.phase.Verifier.VerifierError
import ca.uwaterloo.flix.language.phase.{GenSym, Verifier}
import ca.uwaterloo.flix.runtime.verifier.SymVal.{Char, Unit}
import ca.uwaterloo.flix.runtime.verifier.{SymVal, SymbolicEvaluator}
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util._

import scala.collection.mutable
import scala.language.implicitConversions
import scala.util.Random

import java.math.BigInteger

object QuickChecker {

  // TODO: Refactor shared components:
  // - VerifierError
  // - SymbolicEvaluator.

  val Limit = 1000


  sealed trait QCRunResult {
    def property: Property
  }

  object QCRunResult {

    case class Success(property: Property) extends QCRunResult

    case class Failure(property: Property, error: VerifierError) extends QCRunResult

    // TODO: add model

  }


  sealed trait QuickCheckResult {

    def isSuccess: Boolean = isInstanceOf[QuickCheckResult.Success]

    def isFailure: Boolean = !isSuccess

    def property: Property

    def elapsed: Long

  }

  object QuickCheckResult {

    case class Success(property: Property, runs: Int, elapsed: Long) extends QuickCheckResult

    case class Failure(property: Property, runs: Int, elapsed: Long, error: VerifierError) extends QuickCheckResult

  }


  /**
    * Attempts to quick check all properties in the given AST.
    */
  def quickCheck(root: Root, options: Options)(implicit genSym: GenSym): Validation[Root, VerifierError] = {
    /*
     * Check if the quick checker is enabled. Otherwise return success immediately.
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
      printVerbose(results)
    }

    /*
     * Returns the original AST root if all properties verified successfully.
     */
    if (results.forall(_.isSuccess)) {
      root.toSuccess
    } else {
      val errors = results.collect {
        case QuickCheckResult.Failure(_, _, _, error) => error
      }
      Validation.Failure(errors.toVector)
    }
  }

  /**
    * Attempts to quick check the given `property`.
    */
  private def quickCheckProperty(property: Property, root: Root)(implicit genSym: GenSym): QuickCheckResult = {

    val t = System.nanoTime()

    val exp0 = property.exp

    val exp1 = Verifier.peelUniversallyQuantifiers(exp0)

    val quantifiers = Verifier.getUniversallyQuantifiedVariables(exp0)

    val success = mutable.ListBuffer.empty[QCRunResult.Success]
    val failure = mutable.ListBuffer.empty[QCRunResult.Failure]

    for (i <- 0 until Limit) {
      val env = genEnv(quantifiers)

      val ls = SymbolicEvaluator.eval(exp1, env, root)

      ls.head match {
        case (Nil, SymVal.True) =>
          // Case 1: The symbolic evaluator proved the property.
          success += QCRunResult.Success(property)
        case (Nil, SymVal.False) =>
          // Case 2: The symbolic evaluator disproved the property.
          val error = Verifier.toVerifierError(property, Verifier.mkModel(env, None)) // TODO: Dont rely on Verifier.
          failure += QCRunResult.Failure(property, error)
        case (_, v) => throw new IllegalStateException(s"The symbolic evaluator returned a non-boolean value: $v.")
      }
    }

    val e = System.nanoTime()

    if (failure.nonEmpty) {
      QuickCheckResult.Failure(property, Limit, e, failure.head.error) // TODO: Limit
    } else {
      QuickCheckResult.Success(property, Limit, e) // TODO: Limit
    }
  }

  // TODO: DOC
  def genEnv(quantifiers: List[Var]): Map[String, SymVal] = {
    val r: Random = new Random()
    quantifiers.foldLeft(Map.empty[String, SymVal]) {
      case (macc, Var(ident, offset, tpe, loc)) => macc + (ident.name -> new ArbSymVal(tpe).gen.mk(r))
    }
  }

  /**
    * Prints verbose results.
    */
  private def printVerbose(results: List[QuickCheckResult]): Unit = {
    implicit val consoleCtx = Compiler.ConsoleCtx
    Console.println(consoleCtx.blue(s"-- QUICK CHECKER RESULTS ---------------------------------------------"))

    for ((source, properties) <- results.groupBy(_.property.loc.source)) {

      Console.println()
      Console.println(s"  -- Quick Check ${source.format} -- ")
      Console.println()

      for (result <- properties.sortBy(_.property.loc)) {
        result match {
          case QuickCheckResult.Success(property, runs, elapsed) =>
            Console.println("  " + consoleCtx.cyan("✓ ") + property.law + " (" + property.loc.format + ") (" + runs + " tests.)") // TODO: Elapsed toSeconds

          case QuickCheckResult.Failure(property, runs, elapsed, error) =>
            Console.println("  " + consoleCtx.red("✗ ") + property.law + " (" + property.loc.format + ") " + runs + " tests.") // TODO: Elapsed toSeconds
        }
      }

      val s = properties.count(_.isSuccess)
      val f = properties.count(_.isFailure)
      val t = properties.length
      val e = properties.map(_.elapsed).sum

      Console.println()
      Console.println(s"  Properties: $s / $t quick checked in ${TimeOps.toSeconds(e)} seconds. (success = $s; failure = $f).")
      Console.println()
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Arbitrary and Generator                                                 //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * An interface for types `A` equipped with a generator.
    */
  trait Arbitrary[A] {
    /**
      * Returns a generator for the type `A`.
      */
    def gen: Generator[A]
  }

  /**
    * An interface for types which can be randomly generated.
    */
  trait Generator[+A] {
    /**
      * Returns a random value of type `A`.
      */
    def mk(r: Random): A
  }

  /////////////////////////////////////////////////////////////////////////////
  // Arbitrary Values                                                        //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * An arbitrary for symbolic values based on the given type `tpe`.
    */
  class ArbSymVal(tpe: Type) extends Arbitrary[SymVal] {
    def gen: Generator[SymVal] = tpe match {
      case Type.Unit => ArbUnit.gen
      case Type.Bool => ArbBool.gen

      case Type.Int8 => ArbInt8.gen

      case Type.BigInt => GenBigInt

      case Type.Enum(name, cases) => oneOf(cases.values.map(t =>
        new Generator[SymVal] {
          def mk(r: Random): SymVal = SymVal.Tag(t.tag.name, new ArbSymVal(t.tpe).gen.mk(r))
        }
      ).toArray: _*)

      case Type.Var(_) => throw InternalCompilerException(s"Unable to generate values of type `$tpe'.")
    }
  }


  /////////////////////////////////////////////////////////////////////////////
  // Arbitrary Instances                                                     //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * An arbitrary for unit.
    */
  object ArbUnit extends Arbitrary[SymVal.Unit.type] {
    def gen: Generator[SymVal.Unit.type] = GenUnit
  }

  /**
    * An arbitrary for boolean values.
    */
  object ArbBool extends Arbitrary[SymVal.Bool] {
    def gen: Generator[SymVal.Bool] = GenBool
  }

  /**
    * An arbitrary for char values.
    */
  object ArbChar extends Arbitrary[SymVal.Char] {
    def gen: Generator[SymVal.Char] = GenChar
  }

  /**
    * An arbitrary for float32 values.
    */
  object ArbFloat32 extends Arbitrary[SymVal.Float32] {
    def gen: Generator[SymVal.Float32] = oneOf(
      CstFloat32(+0.0f),
      CstFloat32(-0.0f),
      CstFloat32(+1.0f),
      CstFloat32(-1.0f),
      CstFloat32(Float.MinValue),
      CstFloat32(Float.MaxValue),
      CstFloat32(Float.NegativeInfinity),
      CstFloat32(Float.PositiveInfinity),
      CstFloat32(Float.NaN),
      GenFloat32
    )
  }

  /**
    * An arbitrary for float64 values.
    */
  object ArbFloat64 extends Arbitrary[SymVal.Float64] {
    def gen: Generator[SymVal.Float64] = oneOf(
      CstFloat64(+0.0d),
      CstFloat64(-0.0d),
      CstFloat64(+1.0d),
      CstFloat64(-1.0d),
      CstFloat64(Double.MinValue),
      CstFloat64(Double.MaxValue),
      CstFloat64(Double.NegativeInfinity),
      CstFloat64(Double.PositiveInfinity),
      CstFloat64(Double.NaN),
      GenFloat64
    )
  }

  /**
    * An arbitrary for int8 values.
    */
  object ArbInt8 extends Arbitrary[SymVal.Int8] {
    def gen: Generator[SymVal.Int8] = oneOf(
      CstInt8(+0),
      CstInt8(-1),
      CstInt8(+1),
      CstInt8(Byte.MinValue),
      CstInt8(Byte.MaxValue),
      GenInt8
    )
  }

  /**
    * An arbitrary for int16 values.
    */
  object ArbInt16 extends Arbitrary[SymVal.Int16] {
    def gen: Generator[SymVal.Int16] = oneOf(
      CstInt16(+0),
      CstInt16(-1),
      CstInt16(+1),
      CstInt16(Short.MinValue),
      CstInt16(Short.MaxValue),
      GenInt16
    )
  }

  /**
    * An arbitrary for int32 values.
    */
  object ArbInt32 extends Arbitrary[SymVal.Int32] {
    def gen: Generator[SymVal.Int32] = oneOf(
      CstInt32(+0),
      CstInt32(-1),
      CstInt32(+1),
      CstInt32(Int.MinValue),
      CstInt32(Int.MaxValue),
      GenInt32
    )
  }

  /**
    * An arbitrary for int64 values.
    */
  object ArbInt64 extends Arbitrary[SymVal.Int64] {
    def gen: Generator[SymVal.Int64] = oneOf(
      CstInt64(+0),
      CstInt64(-1),
      CstInt64(+1),
      CstInt64(Long.MinValue),
      CstInt64(Long.MaxValue),
      GenInt64
    )
  }

  /**
    * An arbitrary for bigint values.
    */
  object ArbBigInt extends Arbitrary[SymVal.BigInt] {
    def gen: Generator[SymVal.BigInt] = oneOf(
      CstBigInt(new BigInteger("+0")),
      CstBigInt(new BigInteger("-1")),
      CstBigInt(new BigInteger("+1")),
      CstBigInt(new BigInteger(Long.MinValue.toString)),
      CstBigInt(new BigInteger(Long.MaxValue.toString)),
      CstBigInt(new BigInteger(Long.MaxValue.toString).add(new BigInteger(Long.MaxValue.toString))),
      GenBigInt
    )
  }

  /**
    * An arbitrary for str values.
    */
  object ArbStr extends Arbitrary[SymVal.Str] {
    def gen: Generator[SymVal.Str] = oneOf(
      CstStr(""),
      GenStr
    )
  }

  /////////////////////////////////////////////////////////////////////////////
  // Generator Instances                                                     //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * A trivial generator for the unit value.
    */
  object GenUnit extends Generator[SymVal.Unit.type] {
    def mk(r: Random): Unit.type = SymVal.Unit
  }

  /**
    * A generator for boolean values.
    */
  object GenBool extends Generator[SymVal.Bool] {
    def mk(r: Random): SymVal.Bool = if (r.nextBoolean()) SymVal.True else SymVal.False
  }

  /**
    * A generator for char values.
    */
  object GenChar extends Generator[SymVal.Char] {
    def mk(r: Random): SymVal.Char = SymVal.Char(r.nextPrintableChar())
  }

  /**
    * A generator for float32 values.
    */
  object GenFloat32 extends Generator[SymVal.Float32] {
    def mk(r: Random): SymVal.Float32 = SymVal.Float32(r.nextFloat())
  }

  /**
    * A generator for float64 values.
    */
  object GenFloat64 extends Generator[SymVal.Float64] {
    def mk(r: Random): SymVal.Float64 = SymVal.Float64(r.nextDouble())
  }

  /**
    * A generator for int8 values.
    */
  object GenInt8 extends Generator[SymVal.Int8] {
    def mk(r: Random): SymVal.Int8 = SymVal.Int8(r.nextInt().asInstanceOf[Byte])
  }

  /**
    * A generator for int16 values.
    */
  object GenInt16 extends Generator[SymVal.Int16] {
    def mk(r: Random): SymVal.Int16 = SymVal.Int16(r.nextInt().asInstanceOf[Short])
  }

  /**
    * A generator for int32 values.
    */
  object GenInt32 extends Generator[SymVal.Int32] {
    def mk(r: Random): SymVal.Int32 = SymVal.Int32(r.nextInt())
  }

  /**
    * A generator for int64 values.
    */
  object GenInt64 extends Generator[SymVal.Int64] {
    def mk(r: Random): SymVal.Int64 = SymVal.Int64(r.nextLong())
  }

  /**
    * A generator for bigint values.
    */
  object GenBigInt extends Generator[SymVal.BigInt] {
    def mk(r: Random): SymVal.BigInt = SymVal.BigInt(new BigInteger(128, r.self))
  }

  /**
    * A generator for str values.
    */
  object GenStr extends Generator[SymVal.Str] {
    def mk(r: Random): SymVal.Str = SymVal.Str(r.nextString(3))
  }

  /////////////////////////////////////////////////////////////////////////////
  // Constant Generator Instances                                            //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * A generator for the constant char value `c`.
    */
  case class CstChar(c: Char) extends Generator[SymVal.Char] {
    def mk(r: Random): SymVal.Char = SymVal.Char(c.asInstanceOf[Int])
  }

  /**
    * A generator for the constant float32 value `c`.
    */
  case class CstFloat32(c: Float) extends Generator[SymVal.Float32] {
    def mk(r: Random): SymVal.Float32 = SymVal.Float32(c)
  }

  /**
    * A generator for the constant float64 value `c`.
    */
  case class CstFloat64(c: Double) extends Generator[SymVal.Float64] {
    def mk(r: Random): SymVal.Float64 = SymVal.Float64(c)
  }

  /**
    * A generator for the constant int8 value `c`.
    */
  case class CstInt8(c: Byte) extends Generator[SymVal.Int8] {
    def mk(r: Random): SymVal.Int8 = SymVal.Int8(c)
  }

  /**
    * A generator for the constant int16 value `c`.
    */
  case class CstInt16(c: Short) extends Generator[SymVal.Int16] {
    def mk(r: Random): SymVal.Int16 = SymVal.Int16(c)
  }

  /**
    * A generator for the constant int32 value `c`.
    */
  case class CstInt32(c: Int) extends Generator[SymVal.Int32] {
    def mk(r: Random): SymVal.Int32 = SymVal.Int32(c)
  }

  /**
    * A generator for the constant int64 value `c`.
    */
  case class CstInt64(c: Long) extends Generator[SymVal.Int64] {
    def mk(r: Random): SymVal.Int64 = SymVal.Int64(c)
  }

  /**
    * A generator for the constant big int value `c`.
    */
  case class CstBigInt(c: BigInteger) extends Generator[SymVal.BigInt] {
    def mk(r: Random): SymVal.BigInt = SymVal.BigInt(c)
  }

  /**
    * A generator for the constant str value `c`.
    */
  case class CstStr(c: String) extends Generator[SymVal.Str] {
    def mk(r: Random): SymVal.Str = SymVal.Str(c)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Generator Combinators                                                   //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * A generator combinator that randomly selects one of the given generators `gs`.
    */
  private def oneOf[A](gs: Generator[A]*): Generator[A] = new Generator[A] {
    def mk(r: Random): A = gs(r.nextInt(gs.length)).mk(r)
  }

}
