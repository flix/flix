package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language._
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.ast.ExecutableAst.Expression
import ca.uwaterloo.flix.language.ast.ExecutableAst.Expression._
import ca.uwaterloo.flix.runtime.SymbolicEvaluator
import ca.uwaterloo.flix.util.InternalCompilerException
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
    case class AssociativityError(loc: SourceLocation) extends VerifierError {
      val message =
        s"""${consoleCtx.blue(s"-- VERIFIER ERROR -------------------------------------------------- ${loc.source.format}")}
           |
           |${consoleCtx.red(s">> The function is not associative.")}
           |
           |Counter-example: ???
           |
           |The partial order was defined here:
           |${loc.underline}
           """.stripMargin
    }

    /**
      * An error raised to indicate that a function is not commutative.
      */
    case class CommutativityError(loc: SourceLocation) extends VerifierError {
      val message =
        s"""${consoleCtx.blue(s"-- VERIFIER ERROR -------------------------------------------------- ${loc.source.format}")}
           |
           |${consoleCtx.red(s">> The function is not commutative.")}
           |
           |Counter-example: ???
           |
           |The partial order was defined here:
           |${loc.underline}
           """.stripMargin
    }

    /**
      * An error raised to indicate that a partial order is not reflexive.
      */
    case class ReflexivityError(loc: SourceLocation) extends VerifierError {
      val message =
        s"""${consoleCtx.blue(s"-- VERIFIER ERROR -------------------------------------------------- ${loc.source.format}")}
           |
           |${consoleCtx.red(s">> The partial order is not reflexive.")}
           |
           |Counter-example: ???
           |
           |The partial order was defined here:
           |${loc.underline}
           """.stripMargin
    }

    /**
      * An error raised to indicate that a partial order is not anti-symmetric.
      */
    case class AntiSymmetryError(loc: SourceLocation) extends VerifierError {
      val message =
        s"""${consoleCtx.blue(s"-- VERIFIER ERROR -------------------------------------------------- ${loc.source.format}")}
           |
           |${consoleCtx.red(s">> The partial order is not anti-symmetric.")}
           |
           |Counter-example: ???
           |
           |The partial order was defined here:
           |${loc.underline}
           """.stripMargin
    }

    /**
      * An error raised to indicate that a partial order is not transitive.
      */
    case class TransitivityError(loc: SourceLocation) extends VerifierError {
      val message =
        s"""${consoleCtx.blue(s"-- VERIFIER ERROR -------------------------------------------------- ${loc.source.format}")}
           |
           |${consoleCtx.red(s">> The partial order is not transitive.")}
           |
           |Counter-example: ???
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
           |Counter-example: ???
           |
           |The partial order was defined here:
           |${loc.underline}
           """.stripMargin
    }

    /**
      * An error raised to indicate that the lub is not an upper bound.
      */
    case class UpperBoundError(loc: SourceLocation) extends VerifierError {
      val message =
        s"""${consoleCtx.blue(s"-- VERIFIER ERROR -------------------------------------------------- ${loc.source.format}")}
           |
           |${consoleCtx.red(s">> The lub is not an upper bound.")}
           |
           |Counter-example: ???
           |
           |The lub was defined here:
           |${loc.underline}
           """.stripMargin
    }

    /**
      * An error raised to indicate that the lub is not a least upper bound.
      */
    case class LeastUpperBoundError(loc: SourceLocation) extends VerifierError {
      val message =
        s"""${consoleCtx.blue(s"-- VERIFIER ERROR -------------------------------------------------- ${loc.source.format}")}
           |
           |${consoleCtx.red(s">> The lub is not a least upper bound.")}
           |
           |Counter-example: ????
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
           |Counter-example: ???
           |
           |The partial order was defined here:
           |${loc.underline}
           """.stripMargin
    }

    /**
      * An error raised to indicate that the glb is not a lower bound.
      */
    case class LowerBoundError(loc: SourceLocation) extends VerifierError {
      val message =
        s"""${consoleCtx.blue(s"-- VERIFIER ERROR -------------------------------------------------- ${loc.source.format}")}
           |
           |${consoleCtx.red(s">> The glb is not a lower bound.")}
           |
           |Counter-example: ???
           |
           |The glb was defined here:
           |${loc.underline}
           """.stripMargin
    }

    /**
      * An error raised to indicate that the glb is not the greatest lower bound.
      */
    case class GreatestLowerBoundError(loc: SourceLocation) extends VerifierError {
      val message =
        s"""${consoleCtx.blue(s"-- VERIFIER ERROR -------------------------------------------------- ${loc.source.format}")}
           |
           |${consoleCtx.red(s">> The glb is not a greatest lower bound.")}
           |
           |Counter-example: ???
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
    case class MonotoneError(loc: SourceLocation) extends VerifierError {
      val message =
        s"""${consoleCtx.blue(s"-- VERIFIER ERROR -------------------------------------------------- ${loc.source.format}")}
           |
           |${consoleCtx.red(s">> The function is not monotone.")}
           |
           |The function was defined here:
           |${loc.underline}
           """.stripMargin
    }


    /**
      * An error raised to indicate that the height function may be negative.
      */
    case class HeightNonNegativeError(loc: SourceLocation) extends VerifierError {
      val message =
        s"""${consoleCtx.blue(s"-- VERIFIER ERROR -------------------------------------------------- ${loc.source.format}")}
           |
           |${consoleCtx.red(s">> The height function is not non-negative.")}
           |
           |Counter-example: ???
           |
           |The height function was defined here:
           |${loc.underline}
           """.stripMargin
    }

    /**
      * An error raised to indicate that the height function is not strictly decreasing.
      */
    case class HeightStrictlyDecreasingError(loc: SourceLocation) extends VerifierError {
      val message =
        s"""${consoleCtx.blue(s"-- VERIFIER ERROR -------------------------------------------------- ${loc.source.format}")}
           |
           |${consoleCtx.red(s">> The height function is not strictly decreasing.")}
           |
           |Counter-example: ???
           |
           |The height function was defined here:
           |${loc.underline}
           """.stripMargin
    }

  }

  /**
    * Attempts to verify all properties in the given AST.
    */
  def verify(root: ExecutableAst.Root)(implicit genSym: GenSym): List[VerifierError] = {
    root.properties flatMap (p => checkProperty(p, root))
  }

  /**
    * Attempts to verify the given `property`.
    *
    * Returns `None` if the property is satisfied.
    * Otherwise returns `Some` containing the verification error.
    */
  def checkProperty(property: ExecutableAst.Property, root: ExecutableAst.Root)(implicit genSym: GenSym): Option[VerifierError] = {
    // the base expression
    val exp0 = property.exp

    // a sequence of environments under which the base expression must hold.
    val envs = enumerate(getVars(exp0))

    // the number of issued SMT queries.
    var smt = 0

    // TODO: Count failures etc.

    // attempt to verify that the property holds under each environment.
    val violations = envs flatMap {
      case env0 =>
        SymbolicEvaluator.eval(peelQuantifiers(exp0), env0, root) match {
          case SymbolicEvaluator.SymVal.True => Nil
          case SymbolicEvaluator.SymVal.False =>
            List(fail(property, env0))
          case v => throw InternalCompilerException(s"Unexpected SymVal: $v.")
        }

      //        PartialEvaluator.eval(exp0, env0, root) match {
      //        case Expression.True =>
      //          // Case 1: The partial evaluator proved the property.
      //          Nil
      //        case Expression.False =>
      //          // Case 2: The partial evaluator disproved the property.
      //          List(property.fail(env0))
      //        case _: Expression.MatchError | _: Expression.SwitchError | _: Expression.UserError =>
      //          // Case 3: The partial evaluator failed with a user error.
      //          List(property.fail(env0))
      //        case residual =>
      //          // Case 4: The partial evaluator reduced the expression, but it is still residual.
      //          // Must translate the expression into an SMT formula and attempt to prove it.
      //          //println("Residual Expression: " + residual)
      //          smt = smt + 1
      //          mkContext(ctx => {
      //            // Check if the negation of the expression has a model.
      //            // If so, the property does not hold.
      //            val q = ctx.mkNot(visitBoolExpr(residual, ctx))
      //            checkSat(q, ctx) match {
      //              case Result.Unsatisfiable =>
      //                // Case 3.1: The formula is UNSAT, i.e. the property HOLDS.
      //                Nil
      //              case Result.Satisfiable(model) =>
      //                // Case 3.2: The formula is SAT, i.e. a counter-example to the property exists.
      //                List(property.fail(model2env(model)))
      //              case Result.Unknown =>
      //                // Case 3.3: It is unknown whether the formula has a model.
      //                List(property.fail(Map.empty))
      //            }
      //          })
      //      }

    }

    implicit val consoleCtx = Compiler.ConsoleCtx

    if (violations.isEmpty)
      Console.println(consoleCtx.cyan("✓ ") + property.law + " (" + smt + " SMT queries)")
    else
      Console.println(consoleCtx.red("✗ ") + property.law + " (" + smt + " SMT queries)")

    violations.headOption
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

  // TODO: Fix signature of map
  def fail(p: ExecutableAst.Property, m: Map[String, ExecutableAst.Expression]): VerifierError = p.law match {
    case Law.Associativity => VerifierError.AssociativityError(p.exp.loc)
    case Law.Commutativity => VerifierError.CommutativityError(p.exp.loc)
    case Law.Reflexivity => VerifierError.ReflexivityError(p.exp.loc)
    case Law.AntiSymmetry => VerifierError.AntiSymmetryError(p.exp.loc)
    case Law.Transitivity => VerifierError.TransitivityError(p.exp.loc)
    case Law.LeastElement => VerifierError.LeastElementError(p.exp.loc)
    case Law.UpperBound => VerifierError.UpperBoundError(p.exp.loc)
    case Law.LeastUpperBound => VerifierError.LeastUpperBoundError(p.exp.loc)
    case Law.GreatestElement => VerifierError.GreatestElementError(p.exp.loc)
    case Law.LowerBound => VerifierError.LowerBoundError(p.exp.loc)
    case Law.GreatestLowerBound => VerifierError.GreatestLowerBoundError(p.exp.loc)
    case Law.Strict => VerifierError.StrictError(p.exp.loc)
    case Law.HeightNonNegative => VerifierError.HeightNonNegativeError(p.exp.loc)
    case Law.HeightStrictlyDecreasing => VerifierError.HeightStrictlyDecreasingError(p.exp.loc)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Translation to Z3 Formulae                                              //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * Microsoft Z3 Solver API:
    *
    * http://z3prover.github.io/api/html/classcom_1_1microsoft_1_1z3_1_1_context.html
    */

  /**
    * Translates the given expression `e0` into a Z3 boolean expression.
    *
    * Assumes that all lambdas, calls and let bindings have been removed.
    * (In addition to all tags, tuples, sets, maps, etc.)
    */
  def visitBoolExpr(e0: Expression, ctx: Context): BoolExpr = e0 match {
    case True => ctx.mkBool(true)
    case False => ctx.mkBool(false)
    case Var(ident, offset, tpe, loc) => ctx.mkBoolConst(ident.name)
    case Unary(op, e1, tpe, loc) => op match {
      case UnaryOperator.LogicalNot => ctx.mkNot(visitBoolExpr(e1, ctx))
      case _ => throw InternalCompilerException(s"Illegal unary operator: $op.")
    }
    case Binary(op, e1, e2, tpe, loc) => op match {
      case BinaryOperator.Less => ctx.mkBVSLT(visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx))
      case BinaryOperator.LessEqual => ctx.mkBVSLE(visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx))
      case BinaryOperator.Greater => ctx.mkBVSGT(visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx))
      case BinaryOperator.GreaterEqual => ctx.mkBVSGE(visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx))
      case BinaryOperator.Equal | BinaryOperator.NotEqual =>
        val (f1, f2) = (e1.tpe, e2.tpe) match {
          case (Type.Bool, Type.Bool) => (visitBoolExpr(e1, ctx), visitBoolExpr(e2, ctx))
          case (Type.Int32, Type.Int32) => (visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx))
          case _ => throw InternalCompilerException(s"Illegal type: ${(e1.tpe, e2.tpe)}.")
        }
        if (op == BinaryOperator.Equal)
          ctx.mkEq(f1, f2)
        else
          ctx.mkNot(ctx.mkEq(f1, f2))
      case BinaryOperator.LogicalAnd => ctx.mkAnd(visitBoolExpr(e1, ctx), visitBoolExpr(e2, ctx))
      case BinaryOperator.LogicalOr => ctx.mkOr(visitBoolExpr(e1, ctx), visitBoolExpr(e2, ctx))
      case _ => throw InternalCompilerException(s"Illegal binary operator: $op.")
    }
    case IfThenElse(e1, e2, e3, tpe, loc) =>
      val f1 = visitBoolExpr(e1, ctx)
      val f2 = visitBoolExpr(e2, ctx)
      val f3 = visitBoolExpr(e3, ctx)
      ctx.mkOr(
        ctx.mkAnd(f1, f2),
        ctx.mkAnd(ctx.mkNot(f1), f3)
      )
    case _ => throw InternalCompilerException(s"Unexpected expression: $e0.")
  }

  /**
    * Translates the given expression `e` into a Z3 bit vector expression.
    *
    * Assumes that all lambdas, calls and let bindings have been removed.
    * (In addition to all tags, tuples, sets, maps, etc.)
    */
  def visitBitVecExpr(e0: Expression, ctx: Context): BitVecExpr = e0 match {
    case Var(ident, offset, tpe, loc) => ctx.mkBVConst(ident.name, 32)
    case Int32(i) => ctx.mkBV(i, 32)
    case Unary(op, e1, tpe, loc) => op match {
      case UnaryOperator.BitwiseNegate => ctx.mkBVNot(visitBitVecExpr(e1, ctx))
      case _ => throw InternalCompilerException(s"Illegal unary operator: $op.")
    }
    case Binary(op, e1, e2, tpe, loc) => op match {
      case BinaryOperator.Plus => ctx.mkBVAdd(visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx))
      case BinaryOperator.Minus => ctx.mkBVSub(visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx))
      case BinaryOperator.Times => ctx.mkBVMul(visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx))
      case BinaryOperator.Divide => ctx.mkBVSDiv(visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx))
      case BinaryOperator.Modulo => ctx.mkBVSMod(visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx))
      case BinaryOperator.BitwiseAnd => ctx.mkBVAND(visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx))
      case BinaryOperator.BitwiseOr => ctx.mkBVOR(visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx))
      case BinaryOperator.BitwiseXor => ctx.mkBVXOR(visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx))
      case BinaryOperator.BitwiseLeftShift => ctx.mkBVSHL(visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx))
      case BinaryOperator.BitwiseRightShift => ctx.mkBVLSHR(visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx))
      case _ => throw InternalCompilerException(s"Illegal binary operator: $op.")
    }
    case _ => throw InternalCompilerException(s"Unexpected expression: $e0.")
  }

  /////////////////////////////////////////////////////////////////////////////
  // Interface to Z3                                                         //
  /////////////////////////////////////////////////////////////////////////////
  def mkContext[A](f: Context => A): A = {
    // check that the path property is set.
    val prop = System.getProperty("java.library.path")
    if (prop == null) {
      Console.println(errorMessage)
      Console.println()
      Console.println("> java.library.path not set.")
      System.exit(1)
    }

    // attempt to load the native library.
    try {
      System.loadLibrary("libz3")
    } catch {
      case e: UnsatisfiedLinkError =>
        Console.println(errorMessage)
        Console.println()
        Console.println("> Unable to load the library. Stack Trace reproduced below: ")
        e.printStackTrace()
        System.exit(1)
    }

    val ctx = new Context()
    val r = f(ctx)
    ctx.dispose()
    r
  }

  /**
    * Returns an error message explaining how to configure Microsoft Z3.
    */
  private def errorMessage: String =
    """###############################################################################
      |###                                                                         ###
      |### You are running Flix with verification enabled (--verify).              ###
      |### Flix uses the Microsoft Z3 SMT solver to verify correctness.            ###
      |### For this to work, you must have the correct Z3 libraries installed.     ###
      |###                                                                         ###
      |### On Windows:                                                             ###
      |###   1. Unpack the z3 bundle.                                              ###
      |###   2. Ensure that java.library.path points to that path, i.e. run        ###
      |###      java -Djava.library.path=... -jar flix.jar                         ###
      |###   3. Ensure that you have the                                           ###
      |###      'Microsoft Visual Studio Redistributable 2012 Package' installed.  ###
      |###                                                                         ###
      |### NB: You must have the 64 bit version of Java, Z3 and the VS package.    ###
      |###                                                                         ###
      |###############################################################################
    """.stripMargin

  /**
    * Checks the satisfiability of the given boolean formula `f`.
    */
  def checkSat(f: BoolExpr, ctx: Context): Result = {
    val solver = ctx.mkSolver()
    solver.add(f)
    solver.check() match {
      case Status.SATISFIABLE => Result.Satisfiable(solver.getModel)
      case Status.UNSATISFIABLE => Result.Unsatisfiable
      case Status.UNKNOWN => Result.Unknown
    }
  }

  /**
    * A common super-type that represents the result of an SMT query.
    */
  sealed trait Result

  object Result {

    /**
      * The SMT query is satisfiable, i.e. it has at least one model.
      *
      * @param model a model that satisfies the SMT query.
      */
    case class Satisfiable(model: Model) extends Result

    /**
      * The SMT query is unsatisfiable, i.e. it has no model.
      */
    case object Unsatisfiable extends Result

    /**
      * The SMT query may or may not be satisfiable, i.e. it is unknown if there is a model.
      */
    case object Unknown extends Result

  }

  /**
    * Returns a Z3 model as a map from string variables to expressions.
    */
  def model2env(model: Model): Map[String, Expression] = {
    def visit(exp: Expr): Expression = exp match {
      case e: BoolExpr => if (e.isTrue) True else False
      case e: BitVecNum => Int32(e.getLong.toInt) // TODO: Size
      case _ => throw InternalCompilerException(s"Unexpected Z3 expression: $exp.")
    }

    model.getConstDecls.foldLeft(Map.empty[String, Expression]) {
      case (macc, decl) => macc + (decl.getName.toString -> visit(model.getConstInterp(decl)))
    }
  }

}
