package ca.uwaterloo.flix.language.backend.phase

import ca.uwaterloo.flix.Flix.FlixError
import ca.uwaterloo.flix.language.Compiler
import ca.uwaterloo.flix.language.ast.Ast.Annotation
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.ast.TypedAst.Type
import ca.uwaterloo.flix.language.backend.ir.SimplifiedAst
import ca.uwaterloo.flix.language.backend.ir.SimplifiedAst.Expression
import ca.uwaterloo.flix.language.backend.ir.SimplifiedAst.Expression._
import ca.uwaterloo.flix.language.backend.ir.SimplifiedAst.Definition._
import ca.uwaterloo.flix.runtime.{PartialEvaluator, Value}

import java.nio.file.{Files, Paths}

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
      * @param lat the lattice defining the partial order.
      * @param prop the violated property.
      * @param elm the element that violates the property.
      * @param loc the location of the definition of the partial order.
      */
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
      * @param lat the lattice defining the partial order.
      * @param prop the violated property.
      * @param elm1 the first element that violates the property.
      * @param elm2 the second element that violates the property.
      * @param loc the location of the definition of the partial order.
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
      Var(Name.Ident(SourcePosition.Unknown, s, SourcePosition.Unknown), lattice.tpe, SourceLocation.Unknown)
    }

    /**
      * Returns the bottom element.
      */
    def ⊥(): Expression = lattice.bot

    /**
      * Returns the `true` if `e1` is less than or equal to `e2` according to the partial order.
      */
    def ⊑(e1: Expression, e2: Expression): Expression =
      Apply(lattice.lub, List(e1, e2), e1.tpe, SourceLocation.Unknown)

    /**
      * Returns the least upper bound of the two expressions `e1` and `e2`.
      */
    def ⊔(e1: Expression, e2: Expression): Expression =
      Apply(lattice.lub, List(e1, e2), e1.tpe, SourceLocation.Unknown)

    /**
      * Returns the greatest lower bound of the two expressions `e1` and `e2`.
      */
    def ⊓(e1: Expression, e2: Expression): Expression =
      Apply(lattice.glb, List(e1, e2), e1.tpe, SourceLocation.Unknown)

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
  // Microsoft Z3 Interface                                                  //
  /////////////////////////////////////////////////////////////////////////////

  /**
    * Translates the given expression `e` to a Z3 expression.
    */
  def translate(e: Expression, ctx: Context): Expr = e match {
    case True => ctx.mkBool(true)
    case False => ctx.mkBool(false)

    //case Unary()

  }

  // TODO: Might also be worth it to emit the constraints?
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
    val path = Paths.get(prop)
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

  // TODO: remove
  def main(args: Array[String]): Unit = {
    val ctx = withContext(ctx => {

      val x = ctx.mkIntConst("x")
      val y = ctx.mkIntConst("y")

      val one = ctx.mkInt(1)
      val two = ctx.mkInt(1)

      val yPlusOne = ctx.mkAdd(y, one)

      val c1 = ctx.mkLt(x, yPlusOne)
      val c2 = ctx.mkGe(x, two)

      val q = ctx.mkAnd(c1, c2)

      Console.println("model for: x < y + 1")
      val m = check(ctx, q, Status.SATISFIABLE)
      println(m)


    })

  }

  // TODO: SAT (Model), UNSAT, UNKNOWN

  def check(ctx: Context, f: BoolExpr, sat: Status): Model = {
    val s = ctx.mkSolver()
    s.add(f)
    if (s.check() != sat)
      throw null
    if (sat == Status.SATISFIABLE) {
      s.getModel
    } else
      ???
  }

  //      Model check(Context ctx, BoolExpr f, Status sat) throws TestFailedException
  //      {
  //        Solver s = ctx.mkSolver();
  //        s.add(f);
  //        if (s.check() != sat)
  //          throw new TestFailedException();
  //        if (sat == Status.SATISFIABLE)
  //          return s.getModel();
  //        else
  //          return null;
  //      }

  sealed trait Result

  object Result {

    case object Unknown extends Result

  }

}
