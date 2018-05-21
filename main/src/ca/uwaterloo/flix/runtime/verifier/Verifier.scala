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

package ca.uwaterloo.flix.runtime.verifier

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.GenSym
import ca.uwaterloo.flix.language.ast.ExecutableAst.{Property, Root}
import ca.uwaterloo.flix.language.ast.{Symbol, _}
import ca.uwaterloo.flix.language.errors.PropertyError
import ca.uwaterloo.flix.language.phase.Phase
import ca.uwaterloo.flix.runtime.evaluator.{SmtExpr, SymVal, SymbolicEvaluator}
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util._
import ca.uwaterloo.flix.util.vt.VirtualString._
import ca.uwaterloo.flix.util.vt.{TerminalContext, VirtualTerminal}
import com.microsoft.z3._

import scala.collection.mutable.ListBuffer

object Verifier extends Phase[ExecutableAst.Root, ExecutableAst.Root] {

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

    /**
      * Unknown whether the property was true/false in the single execution.
      */
    case class Unknown(model: Map[Symbol.VarSym, String]) extends PathResult

  }

  /**
    * A type to hold the result of a property verification.
    */
  sealed trait PropertyResult {

    /**
      * Returns the property associated with `this` property result.
      */
    def property: ExecutableAst.Property

    /**
      * Returns the number of paths explored by symbolic execution for `this` property.
      */
    def paths: Int

    /**
      * Returns the number of SMT queries issued for `this` property.
      */
    def queries: Int

    /**
      * Returns the total time spent evaluating `this` property.
      */
    def elapsed: Long

  }

  object PropertyResult {

    /**
      * A property that was proven.
      */
    case class Success(property: ExecutableAst.Property, paths: Int, queries: Int, elapsed: Long) extends PropertyResult

    /**
      * A property that was disproved.
      */
    case class Failure(property: ExecutableAst.Property, paths: Int, queries: Int, elapsed: Long, error: PropertyError) extends PropertyResult

    /**
      * A property whose validity is unknown.
      */
    case class Unknown(property: ExecutableAst.Property, paths: Int, queries: Int, elapsed: Long, error: PropertyError) extends PropertyResult

  }

  /**
    * Attempts to verify all properties in the given AST.
    */
  def run(root: ExecutableAst.Root)(implicit flix: Flix): Validation[ExecutableAst.Root, PropertyError] = {
    implicit val _ = flix.genSym

    /*
     * Check if verification is enabled. Otherwise return success immediately.
     */
    if (!flix.options.verifier) {
      return root.toSuccess
    }

    /*
     * Verify each property.
     */
    val results = root.properties.map(p => verifyProperty(p, root))

    /*
     * Print verbose information (if enabled).
     */
    if (flix.options.verbosity == Verbosity.Verbose) {
      printVerbose(results)
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
      Validation.Failure((errors ++ unknowns).toStream)
    }
  }

  /**
    * Attempts the verify that the given property `p` is valid.
    */
  def verifyProperty(p: Property, root: ExecutableAst.Root)(implicit genSym: GenSym): PropertyResult = {
    // start the clock.
    val t = System.nanoTime()

    // the number of queries issued to the SMT solver.
    var queries = 0

    // the initial empty environment.
    val env0 = Map.empty: SymbolicEvaluator.Environment

    // perform symbolic execution to obtain all possible paths.
    val contexts = SymbolicEvaluator.eval(p.exp, env0, Map.empty, enumerate(root, genSym), root)

    // a boolean to indicate whether to continue checking path satisfiability.
    var continue: Boolean = true

    // retrieve an iterator over the contexts.
    val iterator = contexts.iterator

    // a buffer to hold the result of processing each path.
    val results = ListBuffer.empty[PathResult]

    // check path satisfiability for each path or until we find an error.
    while (iterator.hasNext && continue) {
      val result: PathResult = iterator.next() match {
        case (Nil, qua, SymVal.True) =>
          // Case 1: The symbolic evaluator proved the property.
          PathResult.Success
        case (Nil, qua, SymVal.False) =>
          // Case 2: The symbolic evaluator disproved the property.
          PathResult.Failure(SymVal.mkModel(qua, None))
        case (pc, qua, v) => v match {
          case SymVal.True =>
            // Case 3.1: The property holds under some path condition.
            // The property holds regardless of whether the path condition is satisfiable.
            PathResult.Success
          case SymVal.False =>
            // Case 3.2: The property *does not* hold under some path condition.
            // If the path condition is satisfiable then the property *does not* hold.
            queries += 1
            assertUnsatisfiable(p, and(pc), qua)
          case SymVal.AtomicVar(id, _) =>
            // Case 3.3: The property *does not* hold iff the atomic variable `id` is false and the path condition is satisfiable.
            queries += 1
            assertUnsatisfiable(p, SmtExpr.LogicalAnd(SmtExpr.Not(SmtExpr.Var(id, Type.Bool)), and(pc)), qua)
          case _ => throw InternalCompilerException(s"Unexpected value: '$v'.")
        }
      }

      if (result.isInstanceOf[PathResult.Failure]) {
        // bail out if we have found a single counter example.
        continue = false
      }

      results += result
    }

    // stop the clock.
    val e = System.nanoTime() - t

    /*
     * Post-processing.
     */
    val paths = results.length
    val failures = results collect {
      case r: PathResult.Failure => r
    }
    val unknowns = results collect {
      case r: PathResult.Unknown => r
    }

    // Determine if the property was proved or disproved.
    if (failures.isEmpty && unknowns.isEmpty) {
      PropertyResult.Success(p, paths, queries, e)
    } else if (failures.nonEmpty) {
      PropertyResult.Failure(p, paths, queries, e, PropertyError(p, failures.head.model))
    } else {
      PropertyResult.Unknown(p, paths, queries, e, PropertyError(p, unknowns.head.model))
    }

  }

  /**
    * Optionally returns a verifier error if the given path constraint `pc` is satisfiable.
    */
  private def assertUnsatisfiable(p: Property, expr: SmtExpr, qua: SymbolicEvaluator.Quantifiers): PathResult = {
    SmtSolver.withContext(ctx => {
      val query = visitBoolExpr(expr, ctx)
      SmtSolver.checkSat(query, ctx) match {
        case SmtResult.Unsatisfiable =>
          // Case 3.1: The formula is UNSAT, i.e. the property HOLDS.
          PathResult.Success
        case SmtResult.Satisfiable(model) =>
          // Case 3.2: The formula is SAT, i.e. a counter-example to the property exists.
          PathResult.Failure(SymVal.mkModel(qua, Some(model)))
        case SmtResult.Unknown =>
          // Case 3.3: It is unknown whether the formula has a model.
          // Soundness require us to assume that there is a model.
          PathResult.Unknown(SymVal.mkModel(qua, None))
      }
    })
  }

  /**
    * Translates the given path constraint `pc` into a single smt expression.
    */
  private def and(pc: List[SmtExpr]): SmtExpr = pc.reduceLeft[SmtExpr] {
    case (acc, e) => SmtExpr.LogicalAnd(acc, e)
  }

  /**
    * Translates the given SMT expression `exp0` into a Z3 arithmetic expression.
    */
  private def visitArithExpr(exp0: SmtExpr, ctx: Context): ArithExpr = exp0 match {
    case SmtExpr.BigInt(lit) => ctx.mkInt(lit.longValueExact())
    case SmtExpr.Var(sym, tpe) => ctx.mkIntConst(sym.toString)
    case SmtExpr.Plus(e1, e2) => ctx.mkAdd(visitArithExpr(e1, ctx), visitArithExpr(e2, ctx))
    case SmtExpr.Minus(e1, e2) => ctx.mkSub(visitArithExpr(e1, ctx), visitArithExpr(e2, ctx))
    case SmtExpr.Times(e1, e2) => ctx.mkMul(visitArithExpr(e1, ctx), visitArithExpr(e2, ctx))
    case SmtExpr.Divide(e1, e2) => ctx.mkDiv(visitArithExpr(e1, ctx), visitArithExpr(e2, ctx))
    case SmtExpr.Modulo(e1, e2) => ctx.mkRem(visitIntExpr(e1, ctx), visitIntExpr(e2, ctx))
    case SmtExpr.BitwiseNegate(e) => throw InternalCompilerException(s"BitwiseNegate not supported for BigInt.")
    case SmtExpr.BitwiseAnd(e1, e2) => throw InternalCompilerException(s"BitwiseAnd not supported for BigInt.")
    case SmtExpr.BitwiseOr(e1, e2) => throw InternalCompilerException(s"BitwiseOr not supported for BigInt.")
    case SmtExpr.BitwiseXor(e1, e2) => throw InternalCompilerException(s"BitwiseXor not supported for BigInt.")
    case SmtExpr.BitwiseLeftShift(e1, e2) => throw InternalCompilerException(s"BitwiseLeftShift not supported for BigInt.")
    case SmtExpr.BitwiseRightShift(e1, e2) => throw InternalCompilerException(s"BitwiseRightShift not supported for BigInt.")
    case SmtExpr.Exponentiate(e1, e2) => throw InternalCompilerException(s"Exponentiation is not supported.")
    case _ => throw InternalCompilerException(s"Unexpected SMT expression: '$exp0'.")
  }

  /**
    * Translates the given SMT expression `exp0` into a Z3 boolean expression.
    */
  private def visitBoolExpr(exp0: SmtExpr, ctx: Context): BoolExpr = exp0 match {
    case SmtExpr.Var(sym, tpe) => ctx.mkBoolConst(sym.toString)
    case SmtExpr.Not(e) => ctx.mkNot(visitBoolExpr(e, ctx))
    case SmtExpr.LogicalAnd(e1, e2) => ctx.mkAnd(visitBoolExpr(e1, ctx), visitBoolExpr(e2, ctx))
    case SmtExpr.LogicalOr(e1, e2) => ctx.mkOr(visitBoolExpr(e1, ctx), visitBoolExpr(e2, ctx))
    case SmtExpr.Implication(e1, e2) => ctx.mkImplies(visitBoolExpr(e1, ctx), visitBoolExpr(e2, ctx))
    case SmtExpr.Bicondition(e1, e2) => ctx.mkIff(visitBoolExpr(e1, ctx), visitBoolExpr(e2, ctx))
    case SmtExpr.Less(e1, e2) => e1.tpe match {
      case Type.Int8 | Type.Int16 | Type.Int32 | Type.Int64 => ctx.mkBVSLT(visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx))
      case Type.BigInt => ctx.mkLt(visitArithExpr(e1, ctx), visitArithExpr(e2, ctx))
      case t => throw InternalCompilerException(s"Unexpected type: '$t'.")
    }
    case SmtExpr.LessEqual(e1, e2) => e1.tpe match {
      case Type.Int8 | Type.Int16 | Type.Int32 | Type.Int64 => ctx.mkBVSLE(visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx))
      case Type.BigInt => ctx.mkLe(visitArithExpr(e1, ctx), visitArithExpr(e2, ctx))
      case t => throw InternalCompilerException(s"Unexpected type: '$t'.")
    }
    case SmtExpr.Greater(e1, e2) => e1.tpe match {
      case Type.Int8 | Type.Int16 | Type.Int32 | Type.Int64 => ctx.mkBVSGT(visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx))
      case Type.BigInt => ctx.mkGt(visitArithExpr(e1, ctx), visitArithExpr(e2, ctx))
      case t => throw InternalCompilerException(s"Unexpected type: '$t'.")
    }
    case SmtExpr.GreaterEqual(e1, e2) => e1.tpe match {
      case Type.Int8 | Type.Int16 | Type.Int32 | Type.Int64 => ctx.mkBVSGE(visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx))
      case Type.BigInt => ctx.mkGe(visitArithExpr(e1, ctx), visitArithExpr(e2, ctx))
      case t => throw InternalCompilerException(s"Unexpected type: '$t'.")
    }
    case SmtExpr.Equal(e1, e2) => e1.tpe match {
      case Type.Bool => ctx.mkIff(visitBoolExpr(e1, ctx), visitBoolExpr(e2, ctx))
      case Type.Int8 | Type.Int16 | Type.Int32 | Type.Int64 => ctx.mkEq(visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx))
      case Type.BigInt => ctx.mkEq(visitArithExpr(e1, ctx), visitArithExpr(e2, ctx))
      case t => throw InternalCompilerException(s"Unexpected type: '$t'.")
    }
    case SmtExpr.NotEqual(e1, e2) => e1.tpe match {
      case Type.Bool => ctx.mkXor(visitBoolExpr(e1, ctx), visitBoolExpr(e2, ctx))
      case Type.Int8 | Type.Int16 | Type.Int32 | Type.Int64 => ctx.mkNot(ctx.mkEq(visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx)))
      case Type.BigInt => ctx.mkNot(ctx.mkEq(visitArithExpr(e1, ctx), visitArithExpr(e2, ctx)))
      case t => throw InternalCompilerException(s"Unexpected type: '$t'.")
    }
    case _ => throw InternalCompilerException(s"Unexpected SMT expression: '$exp0'.")
  }

  /**
    * Translates the given SMT expression `exp0` into a Z3 bit vector expression.
    */
  private def visitBitVecExpr(exp0: SmtExpr, ctx: Context): BitVecExpr = exp0 match {
    case SmtExpr.Int8(i) => ctx.mkBV(i, 8)
    case SmtExpr.Int16(i) => ctx.mkBV(i, 16)
    case SmtExpr.Int32(i) => ctx.mkBV(i, 32)
    case SmtExpr.Int64(i) => ctx.mkBV(i, 64)
    case SmtExpr.Var(sym, tpe) => tpe match {
      case Type.Int8 => ctx.mkBVConst(sym.toString, 8)
      case Type.Int16 => ctx.mkBVConst(sym.toString, 16)
      case Type.Int32 => ctx.mkBVConst(sym.toString, 32)
      case Type.Int64 => ctx.mkBVConst(sym.toString, 64)
      case _ => throw InternalCompilerException(s"Unexpected non-int type: '$tpe'.")
    }
    case SmtExpr.Plus(e1, e2) => ctx.mkBVAdd(visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx))
    case SmtExpr.Minus(e1, e2) => ctx.mkBVSub(visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx))
    case SmtExpr.Times(e1, e2) => ctx.mkBVMul(visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx))
    case SmtExpr.Divide(e1, e2) => ctx.mkBVSDiv(visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx))
    case SmtExpr.Modulo(e1, e2) => ctx.mkBVSMod(visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx))
    case SmtExpr.BitwiseNegate(e) => ctx.mkBVNeg(visitBitVecExpr(e, ctx))
    case SmtExpr.BitwiseAnd(e1, e2) => ctx.mkBVAND(visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx))
    case SmtExpr.BitwiseOr(e1, e2) => ctx.mkBVOR(visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx))
    case SmtExpr.BitwiseXor(e1, e2) => ctx.mkBVXOR(visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx))
    case SmtExpr.BitwiseLeftShift(e1, e2) => ctx.mkBVSHL(visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx))
    case SmtExpr.BitwiseRightShift(e1, e2) => ctx.mkBVLSHR(visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx))
    case SmtExpr.Exponentiate(e1, e2) => throw InternalCompilerException(s"Exponentiation is not supported.")
    case _ => throw InternalCompilerException(s"Unexpected SMT expression: '$exp0'.")
  }

  /**
    * Translates the given SMT expression `exp0` into a Z3 integer expression.
    */
  private def visitIntExpr(exp0: SmtExpr, ctx: Context): IntExpr = exp0 match {
    case SmtExpr.BigInt(i) => ctx.mkInt(i.longValueExact())
    case SmtExpr.Var(sym, tpe) => tpe match {
      case Type.BigInt => ctx.mkIntConst(sym.toString)
      case _ => throw InternalCompilerException(s"Unexpected non-int type: '$tpe'.")
    }
    case _ => throw InternalCompilerException(s"Unexpected SMT expression: '$exp0'.")
  }

  /**
    * Returns `true` if all the given property results `rs` are successful
    */
  private def isSuccess(rs: List[PropertyResult]): Boolean = rs.forall {
    case p: PropertyResult.Success => true
    case p: PropertyResult.Failure => false
    case p: PropertyResult.Unknown => false
  }

  /**
    * Returns the number of successes of the given property results `rs`.
    */
  private def numberOfSuccesses(rs: List[PropertyResult]): Int = rs.count {
    case p: PropertyResult.Success => true
    case p: PropertyResult.Failure => false
    case p: PropertyResult.Unknown => false
  }

  /**
    * Returns the number of failures of the given property results `rs`.
    */
  private def numberOfFailures(rs: List[PropertyResult]): Int = rs.count {
    case p: PropertyResult.Success => false
    case p: PropertyResult.Failure => true
    case p: PropertyResult.Unknown => false
  }

  /**
    * Returns the number of unknowns of the given property results `rs`.
    */
  private def numberOfUnknowns(rs: List[PropertyResult]): Int = rs.count {
    case p: PropertyResult.Success => false
    case p: PropertyResult.Failure => false
    case p: PropertyResult.Unknown => true
  }

  /**
    * Returns the total number of paths of the given property results `rs`.
    */
  private def totalPaths(rs: List[PropertyResult]): Int = rs.foldLeft(0) {
    case (acc, res) => acc + res.paths
  }

  /**
    * Returns the total number of queries of the given property results `rs`.
    **/
  private def totalQueries(rs: List[PropertyResult]): Int = rs.foldLeft(0) {
    case (acc, res) => acc + res.queries
  }

  /**
    * Returns the total elapsed time of the property results `rs`.
    */
  private def totalElapsed(rs: List[PropertyResult]): Long = rs.foldLeft(0L) {
    case (acc, res) => acc + res.elapsed
  }

  /**
    * Prints verbose results.
    */
  private def printVerbose(results: List[PropertyResult]): Unit = {
    val vt = new VirtualTerminal()

    for ((source, properties) <- results.groupBy(_.property.loc.source).toList.sortBy(_._1.format)) {
      vt << Line("Verifier", source.format)
      vt << Indent << NewLine
      for (result <- properties.sortBy(_.property.defn.loc)) {
        val name = result.property.defn.toString
        val law = result.property.law.toString
        val loc = result.property.loc.format

        result match {
          case PropertyResult.Success(property, paths, queries, elapsed) =>
            vt << Cyan("✓") << " " << name << " satisfies " << law << " (" << loc << ") (" << paths << " paths, " << queries << " queries, " << TimeOps.toSeconds(elapsed) << " seconds.)" << NewLine
          case PropertyResult.Failure(property, paths, queries, elapsed, _) =>
            vt << Red("✗") << " " << name << " satisfies " << law << " (" << loc << ") (" << paths << " paths, " << queries << " queries, " << TimeOps.toSeconds(elapsed) << " seconds.)" << NewLine
          case PropertyResult.Unknown(property, paths, queries, elapsed, _) =>
            vt << Red("?") << " " << name << " satisfies " << law << " (" << loc << ") (" << paths << " paths, " << queries << " queries, " << TimeOps.toSeconds(elapsed) << " seconds.)" << NewLine
        }
      }
      vt << NewLine

      val s = numberOfSuccesses(properties)
      val f = numberOfFailures(properties)
      val u = numberOfUnknowns(properties)
      val t = properties.length

      val mt = TimeOps.toSeconds(avgl(properties.map(_.elapsed)))
      val mp = avg(properties.map(_.paths))
      val mq = avg(properties.map(_.queries))

      vt << s"Properties: $s / $t proven in ${TimeOps.toSeconds(totalElapsed(properties))} seconds. (success = $s; failure = $f; unknown = $u)." << NewLine
      vt << s"Paths: ${totalPaths(properties)}. Queries: ${totalQueries(properties)} (avg time = $mt sec; avg paths = $mp; avg queries = $mq)." << NewLine
      vt << Dedent << NewLine

    }

    println(vt.fmt(TerminalContext.AnsiTerminal))
  }

  /**
    * Returns the median of the given list `xs`.
    */
  private def avg(xs: List[Int]): Int =
    if (xs.isEmpty) 0 else xs.sum / xs.length

  /**
    * Returns the median of the given list `xs`.
    */
  private def avgl(xs: List[Long]): Long =
    if (xs.isEmpty) 0 else xs.sum / xs.length

  /**
    * Enumerates all possible symbolic values of the given type.
    */
  private def enumerate(root: Root, genSym: GenSym)(sym: Symbol.VarSym, tpe: Type): List[SymVal] = {
    implicit val _ = genSym

    /*
     * Local visitor. Enumerates the symbolic values of a type.
     */
    def visit(tpe: Type): List[SymVal] = {
      val base = tpe.typeConstructor
      val args = tpe.typeArguments

      base match {
        case Type.Unit => List(SymVal.Unit)
        case Type.Bool => List(SymVal.True, SymVal.False)
        case Type.Char => List(SymVal.AtomicVar(Symbol.freshVarSym(sym), Type.Char))
        case Type.Float32 => List(SymVal.AtomicVar(Symbol.freshVarSym(sym), Type.Float32))
        case Type.Float64 => List(SymVal.AtomicVar(Symbol.freshVarSym(sym), Type.Float64))
        case Type.Int8 => List(SymVal.AtomicVar(Symbol.freshVarSym(sym), Type.Int8))
        case Type.Int16 => List(SymVal.AtomicVar(Symbol.freshVarSym(sym), Type.Int16))
        case Type.Int32 => List(SymVal.AtomicVar(Symbol.freshVarSym(sym), Type.Int32))
        case Type.Int64 => List(SymVal.AtomicVar(Symbol.freshVarSym(sym), Type.Int64))
        case Type.BigInt => List(SymVal.AtomicVar(Symbol.freshVarSym(sym), Type.BigInt))
        case Type.Str => List(SymVal.AtomicVar(Symbol.freshVarSym(sym), Type.Str))

        case Type.Enum(enumSym, _) =>
          val decl = root.enums(enumSym)
          decl.cases.flatMap {
            // TODO: Assumes non-polymorphic type.
            case (tag, caze) => visit(caze.tpe) map {
              case e => SymVal.Tag(tag, e)
            }
          }.toList

        case Type.Tuple(l) =>
          def visitn(xs: List[Type]): List[List[SymVal]] = xs match {
            case Nil => List(Nil)
            case t :: ts => visitn(ts) flatMap {
              case ls => visit(t) map {
                case l => l :: ls
              }
            }
          }

          visitn(args).map(es => SymVal.Tuple(es))

        case _ => throw InternalCompilerException(s"Unexpected type: '$tpe'.")
      }
    }

    visit(tpe)
  }

}
