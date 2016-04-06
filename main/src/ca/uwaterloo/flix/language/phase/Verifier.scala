package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language._
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.ast.ExecutableAst.Expression
import ca.uwaterloo.flix.language.ast.ExecutableAst.Expression._
import ca.uwaterloo.flix.runtime.verifier._
import ca.uwaterloo.flix.util._
import ca.uwaterloo.flix.util.Validation._
import com.microsoft.z3.{BitVecNum, Expr, _}

object Verifier {

  /**
    * A common super-type for verification errors.
    */
  sealed trait VerifierError extends CompilationError

  object VerifierError {

    implicit val consoleCtx = Compiler.ConsoleCtx

    /**
      * An error raised to indicate that a function is not associative.
      */
    case class AssociativityError(m: Map[String, String], loc: SourceLocation) extends VerifierError {
      val message =
        s"""${consoleCtx.blue(s"-- VERIFIER ERROR -------------------------------------------------- ${loc.source.format}")}
           |
           |${consoleCtx.red(s">> The function is not associative.")}
           |
           |Counter-example: ${m.mkString(", ")}
           |
           |The function was defined here:
           |${loc.underline}
           """.stripMargin
    }

    /**
      * An error raised to indicate that a function is not commutative.
      */
    case class CommutativityError(m: Map[String, String], loc: SourceLocation) extends VerifierError {
      val message =
        s"""${consoleCtx.blue(s"-- VERIFIER ERROR -------------------------------------------------- ${loc.source.format}")}
           |
           |${consoleCtx.red(s">> The function is not commutative.")}
           |
           |Counter-example: ${m.mkString(", ")}
           |
           |The function was defined here:
           |${loc.underline}
           """.stripMargin
    }

    /**
      * An error raised to indicate that a partial order is not reflexive.
      */
    case class ReflexivityError(m: Map[String, String], loc: SourceLocation) extends VerifierError {
      val message =
        s"""${consoleCtx.blue(s"-- VERIFIER ERROR -------------------------------------------------- ${loc.source.format}")}
           |
           |${consoleCtx.red(s">> The partial order is not reflexive.")}
           |
           |Counter-example: ${m.mkString(", ")}
           |
           |The partial order was defined here:
           |${loc.underline}
           """.stripMargin
    }

    /**
      * An error raised to indicate that a partial order is not anti-symmetric.
      */
    case class AntiSymmetryError(m: Map[String, String], loc: SourceLocation) extends VerifierError {
      val message =
        s"""${consoleCtx.blue(s"-- VERIFIER ERROR -------------------------------------------------- ${loc.source.format}")}
           |
           |${consoleCtx.red(s">> The partial order is not anti-symmetric.")}
           |
           |Counter-example: ${m.mkString(", ")}
           |
           |The partial order was defined here:
           |${loc.underline}
           """.stripMargin
    }

    /**
      * An error raised to indicate that a partial order is not transitive.
      */
    case class TransitivityError(m: Map[String, String], loc: SourceLocation) extends VerifierError {
      val message =
        s"""${consoleCtx.blue(s"-- VERIFIER ERROR -------------------------------------------------- ${loc.source.format}")}
           |
           |${consoleCtx.red(s">> The partial order is not transitive.")}
           |
           |Counter-example: ${m.mkString(", ")}
           |
           |The partial order was defined here:
           |${loc.underline}
           """.stripMargin
    }

    /**
      * An error raised to indicate that the least element is not smallest.
      */
    case class LeastElementError(loc: SourceLocation) extends VerifierError {
      val message =
        s"""${consoleCtx.blue(s"-- VERIFIER ERROR -------------------------------------------------- ${loc.source.format}")}
           |
           |${consoleCtx.red(s">> The least element is not the smallest.")}
           |
           |The partial order was defined here:
           |${loc.underline}
           """.stripMargin
    }

    /**
      * An error raised to indicate that the lub is not an upper bound.
      */
    case class UpperBoundError(m: Map[String, String], loc: SourceLocation) extends VerifierError {
      val message =
        s"""${consoleCtx.blue(s"-- VERIFIER ERROR -------------------------------------------------- ${loc.source.format}")}
           |
           |${consoleCtx.red(s">> The lub is not an upper bound.")}
           |
           |Counter-example: ${m.mkString(", ")}
           |
           |The lub was defined here:
           |${loc.underline}
           """.stripMargin
    }

    /**
      * An error raised to indicate that the lub is not a least upper bound.
      */
    case class LeastUpperBoundError(m: Map[String, String], loc: SourceLocation) extends VerifierError {
      val message =
        s"""${consoleCtx.blue(s"-- VERIFIER ERROR -------------------------------------------------- ${loc.source.format}")}
           |
           |${consoleCtx.red(s">> The lub is not a least upper bound.")}
           |
           |Counter-example: ${m.mkString(", ")}
           |
           |The lub was defined here:
           |${loc.underline}
           """.stripMargin
    }

    /**
      * An error raised to indicate that the greatest element is not the largest.
      */
    case class GreatestElementError(loc: SourceLocation) extends VerifierError {
      val message =
        s"""${consoleCtx.blue(s"-- VERIFIER ERROR -------------------------------------------------- ${loc.source.format}")}
           |
           |${consoleCtx.red(s">> The greatest element is not the largest.")}
           |
           |The partial order was defined here:
           |${loc.underline}
           """.stripMargin
    }

    /**
      * An error raised to indicate that the glb is not a lower bound.
      */
    case class LowerBoundError(m: Map[String, String], loc: SourceLocation) extends VerifierError {
      val message =
        s"""${consoleCtx.blue(s"-- VERIFIER ERROR -------------------------------------------------- ${loc.source.format}")}
           |
           |${consoleCtx.red(s">> The glb is not a lower bound.")}
           |
           |Counter-example: ${m.mkString(", ")}
           |
           |The glb was defined here:
           |${loc.underline}
           """.stripMargin
    }

    /**
      * An error raised to indicate that the glb is not the greatest lower bound.
      */
    case class GreatestLowerBoundError(m: Map[String, String], loc: SourceLocation) extends VerifierError {
      val message =
        s"""${consoleCtx.blue(s"-- VERIFIER ERROR -------------------------------------------------- ${loc.source.format}")}
           |
           |${consoleCtx.red(s">> The glb is not a greatest lower bound.")}
           |
           |Counter-example: ${m.mkString(", ")}
           |
           |The glb was defined here:
           |${loc.underline}
           """.stripMargin
    }

    /**
      * An error raised to indicate that the function is not strict.
      */
    case class StrictError(loc: SourceLocation) extends VerifierError {
      val message =
        s"""${consoleCtx.blue(s"-- VERIFIER ERROR -------------------------------------------------- ${loc.source.format}")}
           |
           |${consoleCtx.red(s">> The function is not strict.")}
           |
           |The function was defined here:
           |${loc.underline}
           """.stripMargin
    }

    /**
      * An error raised to indicate that the function is not monotone.
      */
    case class MonotoneError(m: Map[String, String], loc: SourceLocation) extends VerifierError {
      val message =
        s"""${consoleCtx.blue(s"-- VERIFIER ERROR -------------------------------------------------- ${loc.source.format}")}
           |
           |${consoleCtx.red(s">> The function is not monotone.")}
           |
           |Counter-example: ${m.mkString(", ")}
           |
           |The function was defined here:
           |${loc.underline}
           """.stripMargin
    }


    /**
      * An error raised to indicate that the height function may be negative.
      */
    case class HeightNonNegativeError(m: Map[String, String], loc: SourceLocation) extends VerifierError {
      val message =
        s"""${consoleCtx.blue(s"-- VERIFIER ERROR -------------------------------------------------- ${loc.source.format}")}
           |
           |${consoleCtx.red(s">> The height function is not non-negative.")}
           |
           |Counter-example: ${m.mkString(", ")}
           |
           |The height function was defined here:
           |${loc.underline}
           """.stripMargin
    }

    /**
      * An error raised to indicate that the height function is not strictly decreasing.
      */
    case class HeightStrictlyDecreasingError(m: Map[String, String], loc: SourceLocation) extends VerifierError {
      val message =
        s"""${consoleCtx.blue(s"-- VERIFIER ERROR -------------------------------------------------- ${loc.source.format}")}
           |
           |${consoleCtx.red(s">> The height function is not strictly decreasing.")}
           |
           |Counter-example: ${m.mkString(", ")}
           |
           |The height function was defined here:
           |${loc.underline}
           """.stripMargin
    }

  }

  /**
    * Attempts to verify all properties in the given AST.
    */
  def verify(root: ExecutableAst.Root, options: Options)(implicit genSym: GenSym): Validation[ExecutableAst.Root, VerifierError] = {
    /*
     * Check if verification is enabled. Otherwise return success immediately.
     */
    if (options.verify != Verify.Enabled) {
      return root.toSuccess
    }

    /*
     * Verify each property.
     */
    val results = root.properties.map(p => verifyProperty(p, root))

    /*
     * Print verbose information (if enabled).
     */
    if (options.verbosity == Verbosity.Verbose) {
      printVerbose(results)
    }

    /*
     * Returns the original AST root if all properties verified successfully.
     */
    if (isSuccess(results)) {
      root.toSuccess
    } else {
      ???
    }
  }

  /**
    * Attempts to verify the given `property`.
    *
    * Returns `None` if the property is satisfied.
    * Otherwise returns `Some` containing the verification error.
    */
  def verifyProperty(property: ExecutableAst.Property, root: ExecutableAst.Root)(implicit genSym: GenSym): PropertyResult = {
    val t = System.nanoTime()

    // the base expression
    val exp0 = property.exp

    // a sequence of environments under which the base expression must hold.
    val envs = enumerate(getVars(exp0))

    // the number of paths.
    var paths = 0

    // the number of issued SMT queries.
    var queries = 0

    // attempt to verify that the property holds under each environment.
    val violations = envs flatMap {
      case env0 =>

        //  TODO: Replace this by a different enumeration.
        def toSymVal(exp0: Expression): SymVal = exp0 match {
          case Expression.Unit => SymVal.Unit
          case Expression.Var(ident, _, _, _) => SymVal.AtomicVar(ident)
          case Expression.Tag(enum, tag, exp, tpe, loc) =>
            SymVal.Tag(tag.name, toSymVal(exp))
          case _ => ???
        }
        val initEnv = env0.foldLeft(Map.empty[String, SymVal]) {
          case (macc, (name, exp)) => macc + (name -> toSymVal(exp))
        }

        paths += 1

        SymbolicEvaluator.eval(peelQuantifiers(exp0), initEnv, root) flatMap {
          case (Nil, SymVal.True) =>
            // Case 1: The symbolic evaluator proved the property.
            Nil
          case (Nil, SymVal.False) =>
            // Case 2: The symbolic evaluator disproved the property.
            val env1 = env0.foldLeft(Map.empty[String, String]) {
              case (macc, (k, e)) => macc + (k -> e.toString)
            }
            List(toVerifierError(property, env1))
          case (pc, v) => v match {
            case SymVal.True =>
              // Case 3.1: The property holds under some path condition.
              // The property holds regardless of whether the path condition is satisfiable.
              Nil
            case SymVal.False =>
              // Case 3.2: The property *does not* hold under some path condition.
              // If the path condition is satisfiable then the property *does not* hold.
              queries += 1
              SmtSolver.mkContext(ctx => {
                val q = visitPathConstraint(pc, ctx)
                SmtSolver.checkSat(q, ctx) match {
                  case SmtResult.Unsatisfiable =>
                    // Case 3.1: The formula is UNSAT, i.e. the property HOLDS.
                    Nil
                  case SmtResult.Satisfiable(model) =>
                    // Case 3.2: The formula is SAT, i.e. a counter-example to the property exists.
                    List(toVerifierError(property, mkModel(env0, model))) // TODO MAp
                  case SmtResult.Unknown =>
                    // Case 3.3: It is unknown whether the formula has a model.
                    ???
                }
              })
          }
        }

    }

    val e = System.nanoTime() - t

    if (violations.isEmpty) {
      PropertyResult.Success(property, paths, queries, e)
    } else {
      PropertyResult.Failure(property, paths, queries, e, violations.head)
    }

  }

  def getVars(exp0: Expression): List[Var] = exp0 match {
    case Expression.Universal(params, _, _) => params.map {
      case Ast.FormalParam(ident, tpe) => Var(ident, -1, tpe, SourceLocation.Unknown)
    }
    case _ => Nil
  }

  def peelQuantifiers(exp0: Expression): Expression = exp0 match {
    case Expression.Existential(params, exp, loc) => peelQuantifiers(exp)
    case Expression.Universal(params, exp, loc) => peelQuantifiers(exp)
    case _ => exp0
  }

  /**
    * Enumerates all possible environments of the given universally quantified variables.
    */
  // TODO: replace string by name?
  // TODO: Cleanup
  // TODO: Return SymVal.
  def enumerate(q: List[Var])(implicit genSym: GenSym): List[Map[String, Expression]] = {
    // Unqualified formula. Used the empty environment.
    if (q.isEmpty)
      return List(Map.empty)

    def visit(tpe: Type): List[Expression] = tpe match {
      case Type.Unit => List(Expression.Unit)
      case Type.Bool => List(Expression.True, Expression.False)
      case Type.Int32 => List(Expression.Var(genSym.fresh2(), -1, Type.Int32, SourceLocation.Unknown))
      case Type.Tuple(elms) => ???
      case t@Type.Enum(name, cases) =>
        val enum = cases.head._2.enum
        val r = cases flatMap {
          case (tagName, tagType) =>
            val tag = Name.Ident(SourcePosition.Unknown, tagName, SourcePosition.Unknown)
            visit(tagType.tpe) map {
              case e => Expression.Tag(enum, tag, e, t, SourceLocation.Unknown)
            }
        }
        r.toList
      case _ => throw new UnsupportedOperationException("Not Yet Implemented. Sorry.")
    }

    def expand(rs: List[(String, List[Expression])]): List[Map[String, Expression]] = rs match {
      case Nil => List(Map.empty)
      case (quantifier, expressions) :: xs => expressions flatMap {
        case expression => expand(xs) map {
          case m => m + (quantifier -> expression)
        }
      }
    }

    val result = q map {
      case quantifier => quantifier.ident.name -> visit(quantifier.tpe)
    }
    expand(result)
  }

  private def mkModel(env: Map[String, Expression], model: Model): Map[String, String] = {
    val m = model2env(model)
    def visit(e0: Expression): String = e0 match {
      case Expression.Var(id, _, _, _) => m.get(id.name) match {
        case None => "Not found (?)" // TODO
        case Some(v) => v
      }
      case Expression.Unit => "#U"
      case Expression.Tag(_, tag, e, _, _) => tag + "(" + visit(e) + ")"
    }

    env.foldLeft(Map.empty[String, String]) {
      case (macc, (k, v)) => macc + (k -> visit(v))
    }
  }

  private def model2env(model: Model): Map[String, String] = {
    def visit(exp: Expr): String = exp match {
      case e: BoolExpr => if (e.isTrue) "true" else "false"
      case e: BitVecNum => e.getLong.toString
      case _ => throw InternalCompilerException(s"Unexpected Z3 expression: $exp.")
    }

    model.getConstDecls.foldLeft(Map.empty[String, String]) {
      case (macc, decl) => macc + (decl.getName.toString -> visit(model.getConstInterp(decl)))
    }
  }

  /**
    * Returns a verifier error for the given property `prop` under the given environment `env`.
    */
  private def toVerifierError(prop: ExecutableAst.Property, env: Map[String, String]): VerifierError = prop.law match {
    case Law.Associativity => VerifierError.AssociativityError(env, prop.loc)
    case Law.Commutativity => VerifierError.CommutativityError(env, prop.loc)
    case Law.Reflexivity => VerifierError.ReflexivityError(env, prop.loc)
    case Law.AntiSymmetry => VerifierError.AntiSymmetryError(env, prop.loc)
    case Law.Transitivity => VerifierError.TransitivityError(env, prop.loc)
    case Law.LeastElement => VerifierError.LeastElementError(prop.loc)
    case Law.UpperBound => VerifierError.UpperBoundError(env, prop.loc)
    case Law.LeastUpperBound => VerifierError.LeastUpperBoundError(env, prop.loc)
    case Law.GreatestElement => VerifierError.GreatestElementError(prop.loc)
    case Law.LowerBound => VerifierError.LowerBoundError(env, prop.loc)
    case Law.GreatestLowerBound => VerifierError.GreatestLowerBoundError(env, prop.loc)
    case Law.Strict => VerifierError.StrictError(prop.loc)
    case Law.Monotone => VerifierError.MonotoneError(env, prop.loc)
    case Law.HeightNonNegative => VerifierError.HeightNonNegativeError(env, prop.loc)
    case Law.HeightStrictlyDecreasing => VerifierError.HeightStrictlyDecreasingError(env, prop.loc)
  }

  /**
    * Translates the given path constraint `pc` into a boolean Z3 expression.
    */
  private def visitPathConstraint(pc: List[SmtExpr], ctx: Context): BoolExpr = pc.foldLeft(ctx.mkBool(true)) {
    case (f, e) => ctx.mkAnd(f, visitBoolExpr(e, ctx))
  }

  /**
    * Translates the given SMT expression `exp0` into a Z3 boolean expression.
    */
  private def visitBoolExpr(exp0: SmtExpr, ctx: Context): BoolExpr = exp0 match {
    case SmtExpr.Var(id, tpe) => ctx.mkBoolConst(id.name)
    case SmtExpr.Not(e) => ctx.mkNot(visitBoolExpr(e, ctx))
    case SmtExpr.LogicalAnd(e1, e2) => ctx.mkAnd(visitBoolExpr(e1, ctx), visitBoolExpr(e2, ctx))
    case SmtExpr.LogicalOr(e1, e2) => ctx.mkOr(visitBoolExpr(e1, ctx), visitBoolExpr(e2, ctx))
    case SmtExpr.Implication(e1, e2) => ctx.mkImplies(visitBoolExpr(e1, ctx), visitBoolExpr(e2, ctx))
    case SmtExpr.Bicondition(e1, e2) => ctx.mkIff(visitBoolExpr(e1, ctx), visitBoolExpr(e2, ctx))
    case SmtExpr.Less(e1, e2) => ctx.mkBVSLT(visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx))
    case SmtExpr.LessEqual(e1, e2) => ctx.mkBVSLE(visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx))
    case SmtExpr.Greater(e1, e2) => ctx.mkBVSGT(visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx))
    case SmtExpr.GreaterEqual(e1, e2) => ctx.mkBVSGE(visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx))
    case SmtExpr.Equal(e1, e2) => e1.tpe match {
      case Type.Bool => ctx.mkIff(visitBoolExpr(e1, ctx), visitBoolExpr(e2, ctx))
      case _ => ctx.mkEq(visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx))
    }
    case SmtExpr.NotEqual(e1, e2) => e1.tpe match {
      case Type.Bool => ctx.mkXor(visitBoolExpr(e1, ctx), visitBoolExpr(e2, ctx))
      case _ => ctx.mkNot(ctx.mkEq(visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx)))
    }
  }

  /**
    * Translates the given SMT expression `exp0` into a Z3 bit vector expression.
    */
  private def visitBitVecExpr(exp0: SmtExpr, ctx: Context): BitVecExpr = exp0 match {
    case SmtExpr.Int8(i) => ctx.mkBV(i, 8)
    case SmtExpr.Int16(i) => ctx.mkBV(i, 16)
    case SmtExpr.Int32(i) => ctx.mkBV(i, 32)
    case SmtExpr.Int64(i) => ctx.mkBV(i, 64)
    case SmtExpr.Var(id, tpe) => tpe match {
      case Type.Int8 => ctx.mkBVConst(id.name, 8)
      case Type.Int16 => ctx.mkBVConst(id.name, 16)
      case Type.Int32 => ctx.mkBVConst(id.name, 32)
      case Type.Int64 => ctx.mkBVConst(id.name, 64)
      case _ => throw InternalCompilerException(s"Unexpected non-int type: '$tpe'.")
    }
    case SmtExpr.Plus(e1, e2) => ctx.mkBVAdd(visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx))
    case SmtExpr.Minus(e1, e2) => ctx.mkBVSub(visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx))
    case SmtExpr.Times(e1, e2) => ctx.mkBVMul(visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx))
    case SmtExpr.Divide(e1, e2) => ctx.mkBVSDiv(visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx))
    case SmtExpr.Modulo(e1, e2) => ctx.mkBVSMod(visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx))
    case SmtExpr.Exponentiate(e1, e2) => ??? // TODO
    case SmtExpr.BitwiseNegate(e) => ctx.mkBVNeg(visitBitVecExpr(e, ctx))
    case SmtExpr.BitwiseAnd(e1, e2) => ctx.mkBVAND(visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx))
    case SmtExpr.BitwiseOr(e1, e2) => ctx.mkBVOR(visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx))
    case SmtExpr.BitwiseXor(e1, e2) => ctx.mkBVXOR(visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx))
    case SmtExpr.BitwiseLeftShift(e1, e2) => ctx.mkBVSHL(visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx))
    case SmtExpr.BitwiseRightShift(e1, e2) => ctx.mkBVLSHR(visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx))
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
  private def numberOfSuccess(rs: List[PropertyResult]): Int = rs.count {
    case p: PropertyResult.Success => true
    case p: PropertyResult.Failure => false
    case p: PropertyResult.Unknown => false
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
  def printVerbose(results: List[PropertyResult]): Unit = {
    implicit val consoleCtx = Compiler.ConsoleCtx
    Console.println(consoleCtx.blue(s"-- VERIFIER RESULTS --------------------------------------------------"))

    for (result <- results) {
      result match {
        case PropertyResult.Success(property, paths, queries, elapsed) =>
          Console.println(consoleCtx.cyan("✓ ") + property.law + " (" + property.loc.format + ")" + " (" + queries + " queries)")

        case PropertyResult.Failure(property, paths, queries, elapsed, error) =>
          Console.println(consoleCtx.red("✗ ") + property.law + " (" + property.loc.format + ")" + " (" + queries + " queries)")

        case PropertyResult.Failure(property, paths, queries, elapsed, error) =>
          Console.println(consoleCtx.red("? ") + property.law + " (" + property.loc.format + ")" + " (" + queries + " queries)")
      }
    }
    val timeInMiliseconds = f"${totalElapsed(results).toDouble / 1000000000.0}%3.1f"
    Console.println()
    Console.println(s"Result: ${numberOfSuccess(results)} / ${results.length} properties proven in $timeInMiliseconds second. (${totalPaths(results)} paths, ${totalQueries(results)} queries) ")
    Console.println()
  }

}
