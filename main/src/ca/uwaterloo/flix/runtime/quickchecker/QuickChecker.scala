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

import java.math.BigInteger

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.FinalAst.{Property, Root}
import ca.uwaterloo.flix.language.ast.{FinalAst, Symbol, MonoType}
import ca.uwaterloo.flix.language.errors.PropertyError
import ca.uwaterloo.flix.language.phase.Phase
import ca.uwaterloo.flix.runtime.evaluator.SymVal.{Char, Unit}
import ca.uwaterloo.flix.runtime.evaluator.{SymVal, SymbolicEvaluator}
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util._
import ca.uwaterloo.flix.util.vt.VirtualString.{Cyan, Dedent, Indent, Line, NewLine, Red}
import ca.uwaterloo.flix.util.vt.{TerminalContext, VirtualTerminal}

import scala.collection.mutable
import scala.language.implicitConversions
import scala.util.Random

object QuickChecker extends Phase[FinalAst.Root, FinalAst.Root] {

  /**
    * The result of a single symbolic execution.
    */
  sealed trait PathResult

  object PathResult {

    /**
      * The property was true in the single execution.
      */
    case object Success extends PathResult

    /**
      * The property was false in the single execution.
      */
    case class Failure(model: Map[Symbol.VarSym, String]) extends PathResult

  }

  /**
    * Represents the result of a single test execution.
    */
  sealed trait TestResult {
    /**
      * Returns the property associated with `this` property result.
      */
    def property: Property
  }

  object TestResult {

    /**
      * A successful test.
      */
    case class Success(property: Property) extends TestResult

    /**
      * A failed test.
      */
    case class Failure(property: Property, error: PropertyError) extends TestResult

  }

  /**
    * Represents the results of checking all the properties in the program.
    */
  case class PropertyResults(results: List[PropertyResult]) {
    /**
      * Prints verbose results.
      */
    def fmt: VirtualTerminal = {
      val vt = new VirtualTerminal
      for ((source, properties) <- results.groupBy(_.property.loc.source).toList.sortBy(_._1.format)) {
        vt << Line("Quick Checker", source.format)
        vt << Indent << NewLine

        for (result <- properties.sortBy(_.property.loc)) {
          val name = result.property.defn.toString
          val law = result.property.law.toString
          val loc = result.property.loc.format

          result match {
            case PropertyResult.Success(property, tests, elapsed) =>
              vt << Cyan("✓") << " " << name << " satisfies " << law << " (" << loc << ") (" << tests << " tests, " << TimeOps.toSeconds(elapsed) << " seconds.)" << NewLine
            case PropertyResult.Failure(property, success, failure, elapsed, error) =>
              vt << Red("✗") << " " << name << " satisfies " << law << " (" << loc << ") (" << success << " SUCCESS, " << failure << " FAILED, " << TimeOps.toSeconds(elapsed) << " seconds.)" << NewLine
          }
        }
        vt << NewLine

        val s = properties.count(_.isSuccess)
        val f = properties.count(_.isFailure)
        val t = properties.length
        val e = properties.map(_.elapsed).sum

        vt << s"Properties: $s / $t quick checked in ${TimeOps.toSeconds(e)} seconds. (success = $s; failure = $f)."
        vt << Dedent << NewLine
      }
      vt
    }

  }

  /**
    * Represents the result of multiple tests of a property.
    */
  sealed trait PropertyResult {

    /**
      * Returns `true` iff all tests of the property succeeded.
      */
    def isSuccess: Boolean = isInstanceOf[PropertyResult.Success]

    /**
      * Returns `true` iff some tests of the property failed.
      */
    def isFailure: Boolean = !isSuccess

    /**
      * Returns the property associated with `this` property result.
      */
    def property: Property

    /**
      * Returns the total time spent testing the property.
      */
    def elapsed: Long

  }

  object PropertyResult {

    /**
      * A property that passed the quick checker.
      */
    case class Success(property: Property, success: Int, elapsed: Long) extends PropertyResult

    /**
      * A property that failed the quick checker.
      */
    case class Failure(property: Property, success: Int, failure: Int, elapsed: Long, error: PropertyError) extends PropertyResult

  }

  /**
    * Attempts to quick check all properties in the given AST.
    */
  def run(root: FinalAst.Root)(implicit flix: Flix): Validation[FinalAst.Root, PropertyError] = {
    /*
     * Number of times to instantiate a quantified variable.
     */
    val Limit = 10

    /*
     * Check if the quick checker is enabled. Otherwise return success immediately.
     */
    if (!flix.options.quickchecker) {
      return root.toSuccess
    }

    /*
     * Initialize a shared random instance.
     */
    implicit val random: Random = new Random()

    /*
     * Quick check each property.
     */
    val results = root.properties.map(p => quickCheckProperty(p, Limit, root))

    /*
     * Print verbose information (if enabled).
     */
    if (flix.options.verbosity == Verbosity.Verbose) {
      Console.println(
        PropertyResults(results).fmt.fmt(TerminalContext.AnsiTerminal)
      )
    }

    /*
     * Returns the original AST root if all properties verified successfully.
     */
    if (results.forall(_.isSuccess)) {
      root.toSuccess
    } else {
      val errors = results.collect {
        case PropertyResult.Failure(_, _, _, _, error) => error
      }
      Validation.Failure(errors.to(LazyList))
    }
  }

  /**
    * Attempts to quick check the given `property`.
    */
  private def quickCheckProperty(p: Property, limit: Int, root: Root)(implicit flix: Flix, random: Random): PropertyResult = {
    /*
     * Start the clock.
     */
    val t = System.nanoTime()

    /*
     * Accumulate successes and failures.
     */
    val success = mutable.ListBuffer.empty[PathResult.Success.type]
    val failure = mutable.ListBuffer.empty[PathResult.Failure]

    /*
     * The initial empty environment.
     */
    val env0 = Map.empty: SymbolicEvaluator.Environment

    /*
     * Run the symbolic evaluator on the generated environment.
     */
    val results = try {
      SymbolicEvaluator.eval(p.exp, env0, Map.empty, enumerate(limit, root, flix, random), root) map {
        case (Nil, qua, SymVal.True) =>
          success += PathResult.Success
        case (Nil, qua, SymVal.False) =>
          failure += PathResult.Failure(SymVal.mkModel(qua, None))
        case (_, _, v) => throw new IllegalStateException(s"The symbolic evaluator returned a non-boolean value: $v.")
      }
    } catch {
      case ex: Exception => failure += PathResult.Failure(Map.empty) // TODO: Improve exception handling.
    }

    /*
     * Stop the clock.
     */
    val e = System.nanoTime() - t

    /*
     * Determine if the property failed any executions.
     */
    if (failure.isEmpty) {
      PropertyResult.Success(p, success.size, e)
    } else {
      PropertyResult.Failure(p, success.size, failure.size, e, PropertyError(p, failure.head.model))
    }
  }

  /**
    * Randomly generates a list of symbolic values for the given type.
    */
  private def enumerate(limit: Int, root: Root, flix: Flix, random: Random)(sym: Symbol.VarSym, tpe: MonoType): List[SymVal] = {
    val arb = new ArbSymVal(tpe, root).gen
    (1 to limit).toList.map(_ => arb.mk(random))
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
  class ArbSymVal(tpe: MonoType, root: Root) extends Arbitrary[SymVal] {
    def gen: Generator[SymVal] = {
      tpe match {
        case MonoType.Unit => ArbUnit.gen
        case MonoType.Bool => ArbBool.gen
        case MonoType.Char => ArbChar.gen
        case MonoType.Float32 => ArbFloat32.gen
        case MonoType.Float64 => ArbFloat64.gen
        case MonoType.Int8 => ArbInt8.gen
        case MonoType.Int16 => ArbInt16.gen
        case MonoType.Int32 => ArbInt32.gen
        case MonoType.Int64 => ArbInt64.gen
        case MonoType.BigInt => ArbBigInt.gen
        case MonoType.Str => ArbStr.gen

        case MonoType.Enum(sym, _) =>
          val decl = root.enums(sym)
          val elms = decl.cases.map {
            case (tag, caze) =>
              val innerMonoType = caze.tpeDeprecated // TODO: Assumes that the enum is non-polymorphic.
              new Generator[SymVal] {
                def mk(r: Random): SymVal = SymVal.Tag(tag.name, new ArbSymVal(innerMonoType, root).gen.mk(r))
              }
          }
          oneOf(elms.toIndexedSeq: _*)

        case MonoType.Tuple(elms) => (r: Random) => {
          val vals = elms.map(t => new ArbSymVal(t, root).gen.mk(r))
          SymVal.Tuple(vals)
        }

        case _ => throw InternalCompilerException(s"Unable to generate values of type `$tpe'.")
      }
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
  private def oneOf[A](gs: Generator[A]*): Generator[A] = (r: Random) => gs(r.nextInt(gs.length)).mk(r)

}
