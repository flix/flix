package ca.uwaterloo.flix.language.phase

import java.nio.file.{Files, Paths}

import ca.uwaterloo.flix.Flix.FlixError
import ca.uwaterloo.flix.language.Compiler
import ca.uwaterloo.flix.language.Compiler.InternalCompilerError
import ca.uwaterloo.flix.language.ast.Ast.Annotation
import ca.uwaterloo.flix.language.ast.SimplifiedAst.Definition._
import ca.uwaterloo.flix.language.ast.SimplifiedAst.Expression
import ca.uwaterloo.flix.language.ast.SimplifiedAst.Expression._
import ca.uwaterloo.flix.language.ast.Type
import ca.uwaterloo.flix.language.ast.{SimplifiedAst, _}
import ca.uwaterloo.flix.runtime.{PartialEvaluator, Value}
import com.microsoft.z3._

object Verifier {

  sealed trait Property

  object Property {

    /**
      * Associativity.
      */
    case class Associativity(op: SimplifiedAst.Expression.Lambda) extends Property {
      // val (f, x, y, z) = (op.lam, 'x.ofType(op.tpe), 'y.ofType(op.tpe), 'z.ofType(op.tpe))

      // val property = ∀(x, y, z)(f(f(x, y), z) ≡ f(x, f(y, z)))
    }

    /**
      * Commutativity.
      */
    case class Commutativity(op: SimplifiedAst.Expression.Lambda) extends Property {
      //  val (f, x, y) = (op.lam, 'x.ofType(op.tpe), 'y.ofType(op.tpe))

      // val property = ∀(x, y)(f(x, y) ≡ f(y, x))
    }


    //
    //  /**
    //   * Monotone: ?x1, x2. x1 ? x2 ? f(x1) ? f(x2).
    //   */
    //  def monotone1(f: Term.Abs, leq: Term.Abs): Term.Abs =
    //    Term.Abs('x1, leq.typ, Term.Abs('x2, leq.typ,
    //      leq.call('x1, 'x2) ==> leq.call(f.call('x1), f.call('x2))))
    //
    //  /**
    //   * Monotone: ?x1, x2, y1, y2. x1 ? x2 ? y1 ? y2 ? f(x1, y1) ? f(x2, y2).
    //   */
    //  def monotone2(f: Term.Abs, leq: Term.Abs): Term.Abs =
    //    Term.Abs('x1, leq.typ, Term.Abs('x2, leq.typ, Term.Abs('y1, leq.typ, Term.Abs('y2, leq.typ,
    //      (leq.call('x1, 'x2) && leq.call('y1, 'y2)) ==> leq.call(f.call('x1, 'y1), f.call('x2, 'y2))))))

    // TODO: Strictness.
    // TODO: Monotonicty

    /**
      * Properties of Partial Orders.
      */
    object PartialOrder {

      /**
        * Reflexivity.
        */
      case class Reflexivity(lattice: Lattice) extends Property {
        val ops = latticeOps(lattice)

        import ops._

        val property = {
          val x = mkVar("x")

          ∀(x)(⊑(x, x))
        }
      }

      /**
        * Anti-symmetry.
        */
      case class AntiSymmetry(lattice: Lattice) extends Property {
        val ops = latticeOps(lattice)

        import ops._

        val property = {
          val (x, y) = (mkVar("x"), mkVar("y"))

          ∀(x, y)(→(∧(⊑(x, y), ⊑(y, x)), ≡(x, y)))
        }
      }

      /**
        * Transitivity.
        */
      case class Transitivity(lattice: Lattice) extends Property {
        //  val (x, y, z) = ('x.ofType(lattice.tpe), 'y.ofType(lattice.tpe), 'z.ofType(lattice.tpe))

        //   val property = ∀(x, y, z)(((x ⊑ y) ∧ (y ⊑ z)) → (x ⊑ z))
      }

      /**
        * Ascending Chain Condition
        */
      //  /**
      //   * Non-Negative: ?x. f(x) > 0.
      //   */
      //  def nonNegative(h: Term.Abs): Term.Abs =
      //    Term.Abs('x, h.typ,
      //      Term.BinaryOp(BinaryOperator.Greater, h.call('x), Term.Int(0)))
      //
      //  /**
      //   * Stricly-Decreasing: ?x, y. x ? y ? x != y ? f(x) > f(y).
      //   */
      //  def strictlyDecreasing(h: Term.Abs, leq: Term.Abs): Term.Abs =
      //    Term.Abs('x, h.typ, Term.Abs('y, h.typ,
      //      (leq.call('x, 'y) && (Term.Var('x) !== Term.Var('y))) ==>
      //        Term.BinaryOp(BinaryOperator.Greater, h.call('x), h.call('y))))


    }

    /**
      * Properties of Join Semi Lattices.
      */
    object JoinSemiLattice {

      /**
        * The bottom element must be the least element.
        */
      case class LeastElement(lattice: Lattice) extends Property {
        val property = {
          //val (⊥, ⊑, ⊔, ⊓) = latticeOps(lattice)
          //  val x = 'x.ofType(lattice.tpe)

          //  ∀(x)(⊑(⊥, x))
        }
      }

      /**
        * The lub must be an upper bound.
        */
      case class UpperBound(lattice: Lattice) extends Property {
        //  val (x, y) = ('x.ofType(lattice.tpe), 'y.ofType(lattice.tpe))

        // val property = ∀(x, y)((x ⊑ (x ⊔ y)) ∧ (y ⊑ (x ⊔ y)))
      }


      //  /**
      //   * Least Upper Bound: ?x, y, z. x ? z ? y ? z ? x ? y ? z.
      //   */


    }

  }

  /**
    * A common super-type for verification errors.
    */
  sealed trait VerifierError extends FlixError

  object VerifierError {

    implicit val consoleCtx = Compiler.ConsoleCtx

    /**
      * An error raised to indicate that a partial order is not reflexive.
      *
      * @param lat  the lattice defining the partial order.
      * @param prop the violated property.
      * @param elm  the element that violates the property.
      * @param loc  the location of the definition of the partial order.
      */
    // TODO: has to take an optional model.
    case class ReflexivityError(lat: Lattice, prop: Property, elm: Value, loc: SourceLocation) extends VerifierError {
      val format =
        s"""${consoleCtx.blue(s"-- VERIFIER ERROR -------------------------------------------------- ${loc.source.format}")}
           |
           |${consoleCtx.red(s">> The partial order is not reflexive.")}
           |
           |The partial order was defined here:
           |${loc.underline}
           """.stripMargin
    }

    /**
      * An error raised to indicate that a partial order is not anti-symmetric.
      *
      * @param lat  the lattice defining the partial order.
      * @param prop the violated property.
      * @param elm1 the first element that violates the property.
      * @param elm2 the second element that violates the property.
      * @param loc  the location of the definition of the partial order.
      */
    case class AntiSymmetryError(lat: Lattice, prop: Property, elm1: Value, elm2: Value, loc: SourceLocation) extends VerifierError {
      val format = s"AntiSymmetry violated for $lat."
    }


  }

  // TODO: def check.
  def checkAll(root: SimplifiedAst.Root): List[VerifierError] = {

    val properties = collectProperties(root)

    properties map {
      case property => checkProperty(property)
    }
  }

  def checkProperty(property: Property): VerifierError = {

    // property.exp

    PartialEvaluator.eval(???, Map.empty, identity) match {
      case Expression.True => // success!
      case Expression.False => // failure!
      case residual =>
      // Case 3: Indeterminate. Must extract SMT verification condition.

    }

    ???
  }

  /**
    * Returns all the verification conditions required to ensure the safety of the given AST `root`.
    */
  def collectProperties(root: SimplifiedAst.Root): List[Property] = {

    val partialOrderProperties = lattices(root) flatMap {
      case l => List(
        Property.PartialOrder.Reflexivity(l),
        Property.PartialOrder.AntiSymmetry(l),
        Property.PartialOrder.Transitivity(l)
      )
    }

    val latticeProperties = lattices(root) flatMap {
      case l => List(
        Property.JoinSemiLattice.LeastElement(l)
      )
    }

    val functionProperties = lambdas(root) flatMap {
      case f if f.annotations.isUnchecked => Nil
      case f => f.annotations.annotations.collect {
        case Annotation.Associative(loc) => Property.Associativity(f)
        case Annotation.Commutative(loc) => Property.Commutativity(f)
        case Annotation.Strict(loc) => ???
      }
    }

    val properties = partialOrderProperties ++ latticeProperties ++ functionProperties
    properties.foreach(p => Console.println(p.toString))
    properties
  }


  /////////////////////////////////////////////////////////////////////////////
  // Property DSL                                                            //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * Returns an expression universally quantified by the given variables.
    *
    * We represent such an expression by sequence of lambda functions.
    */
  // TODO: Alternatively introduce a special constraint construct?
  // Probably need this to report errors.
  def ∀(x: Expression.Var*)(f: Expression): Expression.Lambda = ???

  /**
    * Returns the logical negation of the expression `e`.
    */
  def ¬(e: Expression): Expression =
    Unary(UnaryOperator.Not, e, Type.Bool, SourceLocation.Unknown)

  /**
    * Returns the logical conjunction of the two expressions `e1` and `e2`.
    */
  def ∧(e1: Expression, e2: Expression): Expression =
    Binary(BinaryOperator.And, e1, e2, Type.Bool, SourceLocation.Unknown)

  /**
    * Returns the logical disjunction of the two expressions `e1` and `e2`.
    */
  def ∨(e1: Expression, e2: Expression): Expression =
    Binary(BinaryOperator.Or, e1, e2, Type.Bool, SourceLocation.Unknown)

  /**
    * Returns the logical implication of the two expressions `e1` and `e2`.
    */
  def →(e1: Expression, e2: Expression): Expression =
    ∨(¬(e1), e2)

  /**
    * Returns the logical bi-implication of the two expressions `e1` and `e2`.
    */
  def ↔(e1: Expression, e2: Expression): Expression =
    ∧(→(e1, e2), →(e2, e1))

  /**
    * Returns an equality test of the two expressions `e1` and `e2`.
    */
  def ≡(e1: Expression, e2: Expression): Expression =
    Binary(BinaryOperator.Equal, e1, e2, Type.Bool, SourceLocation.Unknown)

  /**
    * Returns an object with convenience operations on a lattice.
    */
  def latticeOps(l: Lattice): LatticeOps = new LatticeOps(l)

  class LatticeOps(lattice: Lattice) {
    /**
      * Returns a variable expression of the given name `s`.
      */
    def mkVar(s: String): Expression.Var = {
      // TODO: Variable numbering
      Var(Name.Ident(SourcePosition.Unknown, s, SourcePosition.Unknown), -1, lattice.tpe, SourceLocation.Unknown)
    }

    /**
      * Returns the bottom element.
      */
    def ⊥(): Expression = lattice.bot

    /**
      * Returns the `true` if `e1` is less than or equal to `e2` according to the partial order.
      */
    // TODO: Function needs to be a name, not an arbitrary expression
    def ⊑(e1: Expression, e2: Expression): Expression =
      Apply(???, List(e1, e2), e1.tpe, SourceLocation.Unknown)

    /**
      * Returns the least upper bound of the two expressions `e1` and `e2`.
      */
    // TODO: Function needs to be a name, not an arbitrary expression
    def ⊔(e1: Expression, e2: Expression): Expression =
      Apply(???, List(e1, e2), e1.tpe, SourceLocation.Unknown)

    /**
      * Returns the greatest lower bound of the two expressions `e1` and `e2`.
      */
    // TODO: Function needs to be a name, not an arbitrary expression
    def ⊓(e1: Expression, e2: Expression): Expression =
      Apply(???, List(e1, e2), e1.tpe, SourceLocation.Unknown)

    /**
      * Returns the widening of the two expressions `e1` and `e2`.
      */
    def ▽(e1: Expression, e2: Expression): Expression =
      throw new UnsupportedOperationException("Not Yet Implemented. Sorry.")

    /**
      * Returns the narrowing of the two expressions `e1` and `e2`.
      */
    def △(e1: Expression, e2: Expression): Expression =
      throw new UnsupportedOperationException("Not Yet Implemented. Sorry.")
  }

  /**
    * Returns all lattice definitions in the given AST `root`.
    */
  def lattices(root: SimplifiedAst.Root): List[Lattice] =
    root.lattices.values.toList

  /**
    * Returns all lambdas in the program.
    */
  // TODO: Should also find inner lambdas.
  def lambdas(root: SimplifiedAst.Root): List[Expression.Lambda] =
    root.constants.values.map(_.exp.asInstanceOf[Expression.Lambda]).toList // TODO: Avoid cast?

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
    case Unary(op, exp, tpe, loc) => op match {
      case UnaryOperator.Not => ctx.mkNot(visitBoolExpr(e0, ctx))
      case _ => throw new InternalCompilerError(s"Illegal unary operator: $op.")
    }
    case Binary(op, e1, e2, tpe, loc) => op match {
      case BinaryOperator.Less => ctx.mkLt(visitArithExpr(e1, ctx), visitArithExpr(e2, ctx))
      case BinaryOperator.LessEqual => ctx.mkLe(visitArithExpr(e1, ctx), visitArithExpr(e2, ctx))
      case BinaryOperator.Greater => ctx.mkGt(visitArithExpr(e1, ctx), visitArithExpr(e2, ctx))
      case BinaryOperator.GreaterEqual => ctx.mkGe(visitArithExpr(e1, ctx), visitArithExpr(e2, ctx))
      case BinaryOperator.Equal | BinaryOperator.NotEqual =>
        val (f1, f2) = tpe match {
          case Type.Bool => (visitBoolExpr(e1, ctx), visitBoolExpr(e2, ctx))
          case Type.Int => (visitArithExpr(e1, ctx), visitArithExpr(e2, ctx))
          case _ => throw new InternalCompilerError(s"Illegal type: $tpe.")
        }
        if (op == BinaryOperator.Equal)
          ctx.mkEq(f1, f2)
        else
          ctx.mkNot(ctx.mkEq(f1, f2))
      case BinaryOperator.And => ctx.mkAnd(visitBoolExpr(e1, ctx), visitBoolExpr(e2, ctx))
      case BinaryOperator.Or => ctx.mkOr(visitBoolExpr(e1, ctx), visitBoolExpr(e2, ctx))
      case _ => throw new InternalCompilerError(s"Illegal binary operator: $op.")
    }
    case IfThenElse(e1, e2, e3, tpe, loc) =>
      val f1 = visitBoolExpr(e1, ctx)
      val f2 = visitBoolExpr(e2, ctx)
      val f3 = visitBoolExpr(e3, ctx)
      ctx.mkOr(
        ctx.mkAnd(f1, f2),
        ctx.mkAnd(ctx.mkNot(f1), f3)
      )
    case _ => throw new InternalCompilerError(s"Illegal boolean expression of type: ${e0.tpe}.")
  }

  /**
    * Translates the given expression `e` into a Z3 bit vector expression.
    *
    * Assumes that all lambdas, calls and let bindings have been removed.
    * (In addition to all tags, tuples, sets, maps, etc.)
    */
  // TODO: Who can call this?
  def visitBitVecExpr(e0: Expression, ctx: Context): BitVecExpr = e0 match {
    case Int(i) => ctx.mkBV(i, 32)
    case Unary(op, e1, tpe, loc) => op match {
      case UnaryOperator.BitwiseNegate => ctx.mkBVNot(visitBitVecExpr(e1, ctx))
      case _ => throw new InternalCompilerError(s"Illegal unary operator: $op.")
    }
    case Binary(op, e1, e2, tpe, loc) => op match {
      case BinaryOperator.BitwiseAnd => ctx.mkBVAND(visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx))
      case BinaryOperator.BitwiseOr => ctx.mkBVOR(visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx))
    }
  }

  // TODO: Need int Expr?

  /**
    * Translates the given expression `e` into a Z3 arithmetic expression.
    *
    * Assumes that all lambdas, calls and let bindings have been removed.
    * (In addition to all tags, tuples, sets, maps, etc.)
    */
  def visitArithExpr(e0: Expression, ctx: Context): ArithExpr = e0 match {
    case Int(i) => ctx.mkInt(i)
    case Var(name, offset, tpe, loc) => ctx.mkIntConst(name.name)
    case Unary(op, e1, tpe, loc) => op match {
      case UnaryOperator.Plus => visitArithExpr(e1, ctx)
      case UnaryOperator.Minus => ctx.mkSub(ctx.mkInt(0), visitArithExpr(e1, ctx))
      case UnaryOperator.BitwiseNegate => throw new InternalCompilerError(s"Not yet implemented. Sorry.")
      case _ => throw new InternalCompilerError(s"Illegal unary operator: $op.")
    }
    case Binary(op, e1, e2, tpe, loc) => op match {
      case BinaryOperator.Plus => ctx.mkAdd(visitArithExpr(e1, ctx), visitArithExpr(e2, ctx))
      case BinaryOperator.Minus => ctx.mkSub(visitArithExpr(e1, ctx), visitArithExpr(e2, ctx))
      case BinaryOperator.Times => ctx.mkMul(visitArithExpr(e1, ctx), visitArithExpr(e2, ctx))
      case BinaryOperator.Divide => ctx.mkDiv(visitArithExpr(e1, ctx), visitArithExpr(e2, ctx))
      case BinaryOperator.Modulo => throw new UnsupportedOperationException("Not Yet Implemented. Sorry.") // TODO: Need to split IntExp into ArithExp
      case BinaryOperator.BitwiseAnd => throw new UnsupportedOperationException("Not Yet Implemented. Sorry.")
      case BinaryOperator.BitwiseOr => throw new UnsupportedOperationException("Not Yet Implemented. Sorry.")
      case BinaryOperator.BitwiseXor => throw new UnsupportedOperationException("Not Yet Implemented. Sorry.")
      case BinaryOperator.BitwiseLeftShift => throw new UnsupportedOperationException("Not Yet Implemented. Sorry.")
      case BinaryOperator.BitwiseRightShift => throw new UnsupportedOperationException("Not Yet Implemented. Sorry.")
      case _ => throw new InternalCompilerError(s"Illegal binary operator: $op.")
    }

    case IfThenElse(e1, e2, e3, tpe, loc) => ???

    case _ => throw new InternalCompilerError(s"Expected int expression but got: ${e0.tpe}")
  }


  /////////////////////////////////////////////////////////////////////////////
  // Interface to Z3                                                         //
  /////////////////////////////////////////////////////////////////////////////
  def withContext[A](f: Context => A): A = {
    // check that the path property is set.
    val prop = System.getProperty("java.library.path")
    if (prop == null) {
      Console.println(errorMessage)
      Console.println()
      Console.println("> java.library.path not set.")
      System.exit(1)
    }

    // check that the path exists.
    val path = Paths.get(prop)// TODO: the path is actually a sequence of separeted paths...
    if (!Files.isDirectory(path) || !Files.isReadable(path)) {
      Console.println(errorMessage)
      Console.println()
      Console.println("> java.library.path is not a readable directory.")
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
  def check(f: BoolExpr, ctx: Context): Result = {
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

  // TODO: Might also be worth it to emit the constraints?


  // TODO: remove
  def main(args: Array[String]): Unit = {
    withContext(ctx => {

      val x = ctx.mkIntConst("x")
      val y = ctx.mkIntConst("y")

      val one = ctx.mkInt(1)
      val two = ctx.mkInt(1)

      val yPlusOne = ctx.mkAdd(y, one)

      val c1 = ctx.mkLt(x, yPlusOne)
      val c2 = ctx.mkGe(x, two)

      val q = ctx.mkAnd(c1, c2)

      Console.println("model for: x < y + 1")
      val m = check(q, ctx)
      println(m)
    })
  }

  // TODO: Can we use uinterpreted functions for anything?

}
