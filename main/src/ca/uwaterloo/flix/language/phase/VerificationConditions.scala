package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.{CompilationError, Compiler}
import ca.uwaterloo.flix.language.ast.Ast.Annotation
import ca.uwaterloo.flix.language.ast.SimplifiedAst.Definition._
import ca.uwaterloo.flix.language.ast.SimplifiedAst.Expression
import ca.uwaterloo.flix.language.ast.SimplifiedAst.Expression._
import ca.uwaterloo.flix.language.ast.{SimplifiedAst, Type, _}
import ca.uwaterloo.flix.language.phase.VerificationConditions.VerifierError._
import ca.uwaterloo.flix.runtime.{PartialEvaluator, SymbolicEvaluator}
import ca.uwaterloo.flix.util.InternalCompilerException
import com.microsoft.z3._

/**
  * This class is responsible for generating the necessary verification conditions.
  *
  * In the future it will be replaced by writing the necessary laws directly in Flix.
  */
object VerificationConditions {

  /**
    * A common super-type for properties of partial orders, lattices and functions.
    */
  sealed trait Property {
    /**
      * The formula corresponding to the property. May or may not be a theorem.
      */
    val formula: Formula

    def name: String

    def fail(env0: Map[String, Expression]): VerifierError
  }

  object Property {

    /**
      * Associativity.
      */
    case class Associativity(f: SimplifiedAst.Definition.Constant) extends Property {
      val formula = {
        val tpe = f.formals.head.tpe
        val (x, y, z) = (mkVar2("x", tpe), mkVar2("y", tpe), mkVar2("z", tpe))

        ∀(x, y, z)(≡(f(x, f(y, z)), f(f(x, y), z)))
      }

      val name = "Associativity(" + f.loc.format + ")"

      def fail(env0: Map[String, Expression]): VerifierError = {
        val x = env0.get("x")
        val y = env0.get("y")
        val z = env0.get("z")
        AssociativityError(x, y, z, f.loc)
      }
    }

    /**
      * Commutativity.
      */
    case class Commutativity(f: SimplifiedAst.Definition.Constant) extends Property {
      val formula = {
        val tpe = f.formals.head.tpe
        val (x, y) = (mkVar2("x", tpe), mkVar2("y", tpe))

        ∀(x, y)(≡(f(x, y), f(y, x)))
      }

      val name = "Commutativity(" + f.loc.format + ")"

      def fail(env0: Map[String, Expression]): VerifierError = {
        val x = env0.get("x")
        val y = env0.get("y")
        CommutativityError(x, y, f.loc)
      }
    }

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

        val formula = {
          val x = mkVar("x")

          ∀(x)(⊑(x, x))
        }

        val name = "Reflexivity(" + lattice.tpe + ")"

        override def fail(env0: Map[String, Expression]): VerifierError = {
          val x = env0.get("x")
          ReflexivityError(x, lattice.leq.loc)
        }

      }

      /**
        * Anti-symmetry.
        */
      case class AntiSymmetry(lattice: Lattice) extends Property {
        val ops = latticeOps(lattice)

        import ops._

        val formula = {
          val (x, y) = (mkVar("x"), mkVar("y"))

          ∀(x, y)(→(∧(⊑(x, y), ⊑(y, x)), ≡(x, y)))
        }

        val name = "AntiSymmetry(" + lattice.tpe + ")"

        def fail(env0: Map[String, Expression]): VerifierError = {
          val x = env0.get("x")
          val y = env0.get("y")
          AntiSymmetryError(x, y, lattice.leq.loc)
        }
      }

      /**
        * Transitivity.
        */
      case class Transitivity(lattice: Lattice) extends Property {
        val ops = latticeOps(lattice)

        import ops._

        val formula = {
          val (x, y, z) = (mkVar("x"), mkVar("y"), mkVar("z"))

          ∀(x, y, z)(→(∧(⊑(x, y), ⊑(y, z)), ⊑(x, z)))
        }

        val name = "Transitivity(" + lattice.tpe + ")"

        def fail(env0: Map[String, Expression]): VerifierError = {
          val x = env0.get("x")
          val y = env0.get("y")
          val z = env0.get("z")
          TransitivityError(x, y, z, lattice.leq.loc)
        }
      }

    }

    /**
      * Properties of Join Semi Lattices.
      */
    object JoinSemiLattice {

      /**
        * The bottom element must be the least element.
        */
      case class LeastElement(lattice: Lattice) extends Property {
        val ops = latticeOps(lattice)

        import ops._

        val formula = {
          val x = mkVar("x")

          ∀(x)(⊑(⊥(), x))
        }

        val name = "LeastElement(" + lattice.tpe + ")"

        def fail(env0: Map[String, Expression]): VerifierError = {
          val x = env0.get("x")
          LeastElementError(x, lattice.leq.loc)
        }
      }

      /**
        * The lub must be an upper bound.
        */
      case class UpperBound(lattice: Lattice) extends Property {

        val ops = latticeOps(lattice)

        import ops._

        val formula = {
          val (x, y) = (mkVar("x"), mkVar("y"))

          ∀(x, y)(∧(⊑(x, ⊔(x, y)), ⊑(y, ⊔(x, y))))
        }

        val name = "UpperBound(" + lattice.tpe + ")"

        def fail(env0: Map[String, Expression]): VerifierError = {
          val x = env0.get("x")
          val y = env0.get("y")
          UpperBoundError(x, y, lattice.lub.loc)
        }
      }

      /**
        * The lub must be the least upper bound.
        */
      case class LeastUpperBound(lattice: Lattice) extends Property {

        val ops = latticeOps(lattice)

        import ops._

        val formula = {
          val (x, y, z) = (mkVar("x"), mkVar("y"), mkVar("z"))

          ∀(x, y, z)(→(∧(⊑(x, z), ⊑(y, z)), ⊑(⊔(x, y), z)))
        }

        val name = "LeastUpperBound(" + lattice.tpe + ")"

        def fail(env0: Map[String, Expression]): VerifierError = {
          val x = env0.get("x")
          val y = env0.get("y")
          val z = env0.get("z")
          LeastUpperBoundError(x, y, z, lattice.lub.loc)
        }
      }

    }

    /**
      * Properties of Meet Semi Lattices.
      */
    object MeetSemiLattice {

      /**
        * The top element must be the greatest element.
        */
      case class GreatestElement(lattice: Lattice) extends Property {
        val ops = latticeOps(lattice)

        import ops._

        val formula = {
          val x = mkVar("x")

          ∀(x)(⊑(x, ⊤()))
        }

        val name = "GreatestElement(" + lattice.tpe + ")"

        def fail(env0: Map[String, Expression]): VerifierError = {
          val x = env0.get("x")
          GreatestElementError(x, lattice.leq.loc)
        }
      }

      /**
        * The glb must be a lower bound.
        */
      case class LowerBound(lattice: Lattice) extends Property {

        val ops = latticeOps(lattice)

        import ops._

        val formula = {
          val (x, y) = (mkVar("x"), mkVar("y"))

          ∀(x, y)(∧(⊑(⊓(x, y), x), ⊑(⊓(x, y), y)))
        }

        val name = "LowerBound(" + lattice.tpe + ")"

        def fail(env0: Map[String, Expression]): VerifierError = {
          val x = env0.get("x")
          val y = env0.get("y")
          LowerBoundError(x, y, lattice.glb.loc)
        }
      }

      /**
        * The glb must be the greatest lower bound.
        */
      case class GreatestLowerBound(lattice: Lattice) extends Property {

        val ops = latticeOps(lattice)

        import ops._

        val formula = {
          val (x, y, z) = (mkVar("x"), mkVar("y"), mkVar("z"))

          ∀(x, y, z)(→(∧(⊑(z, x), ⊑(z, y)), ⊑(z, ⊓(x, y))))
        }

        val name = "GreatestLowerBound(" + lattice.tpe + ")"

        def fail(env0: Map[String, Expression]): VerifierError = {
          val x = env0.get("x")
          val y = env0.get("y")
          val z = env0.get("z")
          GreatestLowerBoundError(x, y, z, lattice.glb.loc)
        }
      }

    }

    /**
      * The function `f` must be strict in all its arguments.
      */
    case class Strict1(f: SimplifiedAst.Definition.Constant, root: SimplifiedAst.Root) extends Property {
      val formula = {
        val (tpe :: Nil) = f.tpe.asInstanceOf[Type.Lambda].args
        val retTpe = f.tpe.asInstanceOf[Type.Lambda].retTpe
        val argLat = root.lattices(tpe)
        val retLat = root.lattices(retTpe)
        ∀()(≡(f(argLat.bot), retLat.bot))
      }

      val name = "Strict1(" + f.loc.format + ")"

      def fail(env0: Map[String, Expression]): VerifierError = {
        StrictError(f.loc)
      }
    }

    /**
      * The function `f` must be strict in all its arguments.
      */
    case class Strict2(f: SimplifiedAst.Definition.Constant, root: SimplifiedAst.Root) extends Property {
      val formula = {
        val (tpe1 :: tpe2 :: Nil) = f.tpe.asInstanceOf[Type.Lambda].args
        val retTpe = f.tpe.asInstanceOf[Type.Lambda].retTpe
        val (arg1Lat, arg2Lat) = (root.lattices(tpe1), root.lattices(tpe2))
        val retLat = root.lattices(retTpe)

        val (x, y) = (mkVar2("x", tpe1), mkVar2("y", tpe2))

        // TODO: This doesnt seem right. It would seem that bottom is when both args are bottom?
        ∀(x, y)(∧(≡(f(arg1Lat.bot, y), retLat.bot), ≡(f(x, arg2Lat.bot), retLat.bot)))

      }

      val name = "Strict2(" + f.loc.format + ")"

      def fail(env0: Map[String, Expression]): VerifierError = {
        StrictError(f.loc)
      }
    }

    /**
      * The function `f` must be monotone in all its arguments.
      */
    case class Monotone1(f: SimplifiedAst.Definition.Constant, root: SimplifiedAst.Root) extends Property {
      val formula = {
        val lat1 = latticeOps(root.lattices(f.formals.head.tpe))
        val lat2 = latticeOps(root.lattices(f.tpe.asInstanceOf[Type.Lambda].retTpe))

        val (x, y) = (lat1.mkVar("x"), lat1.mkVar("y"))

        ∀(x, y)(→(lat1.⊑(x, y), lat2.⊑(f(x), f(y))))
      }

      val name = "Monotone1(" + f.loc.format + ")"

      def fail(env0: Map[String, Expression]): VerifierError = {
        MonotoneError(f.loc)
      }
    }

    /**
      * The function `f` must be monotone in all its arguments.
      */
    case class Monotone2(f: SimplifiedAst.Definition.Constant, root: SimplifiedAst.Root) extends Property {
      val formula = {
        val (tpe1 :: tpe2 :: Nil) = f.formals.map(_.tpe)
        val (lat1, lat2) = (latticeOps(root.lattices(tpe1)), latticeOps(root.lattices(tpe2)))
        val lat3 = latticeOps(root.lattices(f.tpe.asInstanceOf[Type.Lambda].retTpe))

        val (x1, y1, x2, y2) = (lat1.mkVar("x1"), lat1.mkVar("y1"), lat2.mkVar("x2"), lat2.mkVar("y2"))

        ∀(x1, y1, x2, y2)(→(∧(lat1.⊑(x1, x2), lat2.⊑(y1, y2)), lat3.⊑(f(x1, y1), f(x2, y2))))
      }

      val name = "Monotone2(" + f.loc.format + ")"

      def fail(env0: Map[String, Expression]): VerifierError = {
        MonotoneError(f.loc)
      }
    }

    object AscendingChainCondition {

      /**
        * Height function must be non-negative.
        */
      case class HeightNonNegative(lattice: Lattice) extends Property {
        val ops = latticeOps(lattice)

        import ops._

        val formula = {
          val x = mkVar("x")

          ∀(x)(Expression.Binary(BinaryOperator.GreaterEqual, ???, Expression.Int32(0), Type.Bool, SourceLocation.Unknown))
        }

        val name = "HeightNonNegative(" + lattice.tpe + ")"

        def fail(env0: Map[String, Expression]): VerifierError = {
          val x = env0.get("x")
          HeightNonNegativeError(x, ???)
        }
      }

      /**
        * Height function must be decreasing.
        */
      case class HeightStrictlyDecreasing(lattice: Lattice) extends Property {
        val ops = latticeOps(lattice)

        import ops._

        val formula = {
          val (x, y) = (mkVar("x"), mkVar("y"))

          ∀(x, y)(
            →(
              ∧(⊑(x, y), ¬(≡(x, y))),
              Expression.Binary(BinaryOperator.Greater, ???, ???, Type.Bool, SourceLocation.Unknown)
            ))
        }

        val name = "HeightStrictlyDecreasing(" + lattice.tpe + ")"

        def fail(env0: Map[String, Expression]): VerifierError = {
          val x = env0.get("x")
          val y = env0.get("y")
          HeightStrictlyDecreasingError(x, y, ???)
        }
      }

    }

  }

  /**
    * A formula is a universally quantified expression.
    *
    * @param q the universally quantified variables.
    * @param e the expression
    */
  case class Formula(q: List[Expression.Var], e: Expression)


  /**
    * A common super-type for verification errors.
    */
  sealed trait VerifierError extends CompilationError

  object VerifierError {

    implicit val consoleCtx = Compiler.ConsoleCtx

    /**
      * An error raised to indicate that a function is not associative.
      */
    case class AssociativityError(x: Option[Expression], y: Option[Expression], z: Option[Expression], loc: SourceLocation) extends VerifierError {
      val message =
        s"""${consoleCtx.blue(s"-- VERIFIER ERROR -------------------------------------------------- ${loc.source.format}")}
           |
           |${consoleCtx.red(s">> The function is not associative.")}
           |
           |Counter-example: ($x, $y, $z)
           |
           |The partial order was defined here:
           |${loc.underline}
           """.stripMargin
    }

    /**
      * An error raised to indicate that a function is not commutative.
      */
    case class CommutativityError(x: Option[Expression], y: Option[Expression], loc: SourceLocation) extends VerifierError {
      val message =
        s"""${consoleCtx.blue(s"-- VERIFIER ERROR -------------------------------------------------- ${loc.source.format}")}
           |
           |${consoleCtx.red(s">> The function is not commutative.")}
           |
           |Counter-example: ($x, $y)
           |
           |The partial order was defined here:
           |${loc.underline}
           """.stripMargin
    }

    /**
      * An error raised to indicate that a partial order is not reflexive.
      */
    case class ReflexivityError(x: Option[Expression], loc: SourceLocation) extends VerifierError {
      val message =
        s"""${consoleCtx.blue(s"-- VERIFIER ERROR -------------------------------------------------- ${loc.source.format}")}
           |
           |${consoleCtx.red(s">> The partial order is not reflexive.")}
           |
           |Counter-example: ($x)
           |
           |The partial order was defined here:
           |${loc.underline}
           """.stripMargin
    }

    /**
      * An error raised to indicate that a partial order is not anti-symmetric.
      */
    case class AntiSymmetryError(x: Option[Expression], y: Option[Expression], loc: SourceLocation) extends VerifierError {
      val message =
        s"""${consoleCtx.blue(s"-- VERIFIER ERROR -------------------------------------------------- ${loc.source.format}")}
           |
           |${consoleCtx.red(s">> The partial order is not anti-symmetric.")}
           |
           |Counter-example: ($x, $y)
           |
           |The partial order was defined here:
           |${loc.underline}
           """.stripMargin
    }

    /**
      * An error raised to indicate that a partial order is not transitive.
      */
    case class TransitivityError(x: Option[Expression], y: Option[Expression], z: Option[Expression], loc: SourceLocation) extends VerifierError {
      val message =
        s"""${consoleCtx.blue(s"-- VERIFIER ERROR -------------------------------------------------- ${loc.source.format}")}
           |
           |${consoleCtx.red(s">> The partial order is not transitive.")}
           |
           |Counter-example: ($x, $y, $z)
           |
           |The partial order was defined here:
           |${loc.underline}
           """.stripMargin
    }

    /**
      * An error raised to indicate that the least element is not smallest.
      */
    case class LeastElementError(x: Option[Expression], loc: SourceLocation) extends VerifierError {
      val message =
        s"""${consoleCtx.blue(s"-- VERIFIER ERROR -------------------------------------------------- ${loc.source.format}")}
           |
           |${consoleCtx.red(s">> The least element is not the smallest.")}
           |
           |Counter-example: ($x)
           |
           |The partial order was defined here:
           |${loc.underline}
           """.stripMargin
    }

    /**
      * An error raised to indicate that the lub is not an upper bound.
      */
    case class UpperBoundError(x: Option[Expression], y: Option[Expression], loc: SourceLocation) extends VerifierError {
      val message =
        s"""${consoleCtx.blue(s"-- VERIFIER ERROR -------------------------------------------------- ${loc.source.format}")}
           |
           |${consoleCtx.red(s">> The lub is not an upper bound.")}
           |
           |Counter-example: ($x, $y)
           |
           |The lub was defined here:
           |${loc.underline}
           """.stripMargin
    }

    /**
      * An error raised to indicate that the lub is not a least upper bound.
      */
    case class LeastUpperBoundError(x: Option[Expression], y: Option[Expression], z: Option[Expression], loc: SourceLocation) extends VerifierError {
      val message =
        s"""${consoleCtx.blue(s"-- VERIFIER ERROR -------------------------------------------------- ${loc.source.format}")}
           |
           |${consoleCtx.red(s">> The lub is not a least upper bound.")}
           |
           |Counter-example: ($x, $y, $z)
           |
           |The lub was defined here:
           |${loc.underline}
           """.stripMargin
    }

    /**
      * An error raised to indicate that the greatest element is not the largest.
      */
    case class GreatestElementError(x: Option[Expression], loc: SourceLocation) extends VerifierError {
      val message =
        s"""${consoleCtx.blue(s"-- VERIFIER ERROR -------------------------------------------------- ${loc.source.format}")}
           |
           |${consoleCtx.red(s">> The greatest element is not the largest.")}
           |
           |Counter-example: ($x)
           |
           |The partial order was defined here:
           |${loc.underline}
           """.stripMargin
    }

    /**
      * An error raised to indicate that the glb is not a lower bound.
      */
    case class LowerBoundError(x: Option[Expression], y: Option[Expression], loc: SourceLocation) extends VerifierError {
      val message =
        s"""${consoleCtx.blue(s"-- VERIFIER ERROR -------------------------------------------------- ${loc.source.format}")}
           |
           |${consoleCtx.red(s">> The glb is not a lower bound.")}
           |
           |Counter-example: ($x, $y)
           |
           |The glb was defined here:
           |${loc.underline}
           """.stripMargin
    }

    /**
      * An error raised to indicate that the glb is not the greatest lower bound.
      */
    case class GreatestLowerBoundError(x: Option[Expression], y: Option[Expression], z: Option[Expression], loc: SourceLocation) extends VerifierError {
      val message =
        s"""${consoleCtx.blue(s"-- VERIFIER ERROR -------------------------------------------------- ${loc.source.format}")}
           |
           |${consoleCtx.red(s">> The glb is not a greatest lower bound.")}
           |
           |Counter-example: ($x, $y, $z)
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
    case class HeightNonNegativeError(x: Option[Expression], loc: SourceLocation) extends VerifierError {
      val message =
        s"""${consoleCtx.blue(s"-- VERIFIER ERROR -------------------------------------------------- ${loc.source.format}")}
           |
           |${consoleCtx.red(s">> The height function is not non-negative.")}
           |
           |Counter-example: ($x)
           |
           |The height function was defined here:
           |${loc.underline}
           """.stripMargin
    }

    /**
      * An error raised to indicate that the height function is not strictly decreasing.
      */
    case class HeightStrictlyDecreasingError(x: Option[Expression], y: Option[Expression], loc: SourceLocation) extends VerifierError {
      val message =
        s"""${consoleCtx.blue(s"-- VERIFIER ERROR -------------------------------------------------- ${loc.source.format}")}
           |
           |${consoleCtx.red(s">> The height function is not strictly decreasing.")}
           |
           |Counter-example: ($x)
           |
           |The height function was defined here:
           |${loc.underline}
           """.stripMargin
    }

  }



  /**
    * Attempts to verify the given `property`.
    *
    * Returns `None` if the property is satisfied.
    * Otherwise returns `Some` containing the verification error.
    */
  def checkProperty(property: Property, root: SimplifiedAst.Root)(implicit genSym: GenSym): Option[VerifierError] = {
    // the base expression
    val exp0 = property.formula.e

    // a sequence of environments under which the base expression must hold.
    val envs = enumerate(property.formula.q)

    // the number of issued SMT queries.
    var smt = 0

    // attempt to verify that the property holds under each environment.
    val violations = envs flatMap {
      case env0 =>
        // TODO: need to make properties part of the AST and lift them appropiately.
        val root2 = LambdaLift.lift(root)
        val exp = ClosureConv.convert(exp0) // TODO: Really need to make properties a part of the AST.

        SymbolicEvaluator.eval(exp, env0, root2) match {
          case SymbolicEvaluator.SymVal.True => Nil
          case SymbolicEvaluator.SymVal.False =>
            val err = property.fail(env0)
            println(err.message)
            List(err)
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
      Console.println(consoleCtx.cyan("✓ ") + property.name + " (" + smt + " SMT queries)")
    else
      Console.println(consoleCtx.red("✗ ") + property.name + " (" + smt + " SMT queries)")

    violations.headOption
  }

  /**
    * Attempts to verify all properties of every function in the given AST `root`.
    *
    * Returns a list of errors. If the list is empty, all properties were successfully verified.
    */
  def checkAll(root: SimplifiedAst.Root)(implicit genSym: GenSym): List[VerifierError] = {
    // find all properties to verify.
    val properties = collectProperties(root)

    // attempt to verify each property.
    properties flatMap (p => checkProperty(p, root))
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

  /**
    * Returns all the verification conditions required to ensure the safety of the given AST `root`.
    */
  def collectProperties(root: SimplifiedAst.Root): List[Property] = {
    val lattices = root.lattices.values.toList

    // Collect partial order properties.
    val partialOrderProperties = lattices flatMap {
      case l => List(
        Property.PartialOrder.Reflexivity(l),
        Property.PartialOrder.AntiSymmetry(l),
        Property.PartialOrder.Transitivity(l)
      )
    }

    // Collect lattice properties.
    val latticeProperties = lattices flatMap {
      case l => List(
        Property.JoinSemiLattice.LeastElement(l),
        Property.JoinSemiLattice.UpperBound(l),
        Property.JoinSemiLattice.LeastUpperBound(l),
        Property.MeetSemiLattice.GreatestElement(l),
        Property.MeetSemiLattice.LowerBound(l),
        Property.MeetSemiLattice.GreatestLowerBound(l)
        //        Property.AscendingChainCondition.HeightNonNegative(l), // TODO
        //        Property.AscendingChainCondition.HeightStrictlyDecreasing(l) // TODO
      )
    }

    // Collect function properties.
    val functionProperties = root.constants.values flatMap {
      case f if f.ann.isUnchecked => Nil
      case f => f.ann.annotations.flatMap {
        // TODO: Broken until we can make sure that properties are part of the ast and lifted appropiately.
        // case Annotation.Associative(loc) => Some(Property.Associativity(f))
        // case Annotation.Commutative(loc) => Some(Property.Commutativity(f))
        //        case Annotation.Strict(loc) => f.formals match {
        //          case Nil => None // A constant function is always strict.
        //          case a :: Nil => Some(Property.Strict1(f, root))
        //          case a1 :: a2 :: Nil => Some(Property.Strict2(f, root))
        //          case _ => throw new UnsupportedOperationException("Not Yet Implemented. Sorry.")
        //        }
        //        case Annotation.Monotone(loc) => f.formals match {
        //          case Nil => None // A constant function is always monotone.
        //          case a :: Nil => Some(Property.Monotone1(f, root))
        //          case a1 :: a2 :: Nil => Some(Property.Monotone2(f, root))
        //          case _ => throw new UnsupportedOperationException("Not Yet Implemented. Sorry.")
        //        }
        case _ => Nil
      }
    }

    // return all the collected properties.
    partialOrderProperties ++ latticeProperties ++ functionProperties
  }


  /////////////////////////////////////////////////////////////////////////////
  // Property DSL                                                            //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * Returns a variable expression of the given name `s`.
    */
  def mkVar2(s: String, tpe: Type): Expression.Var = {
    Var(Name.Ident(SourcePosition.Unknown, s, SourcePosition.Unknown), -1, tpe, SourceLocation.Unknown)
  }

  /**
    * Returns an expression universally quantified by the given variables.
    */
  def ∀(q: Expression.Var*)(e: Expression): Formula =
    Formula(q.toList, e)

  /**
    * Returns the logical negation of the expression `e`.
    */
  def ¬(e: Expression): Expression =
    Unary(UnaryOperator.LogicalNot, e, Type.Bool, SourceLocation.Unknown)

  /**
    * Returns the logical conjunction of the two expressions `e1` and `e2`.
    */
  def ∧(e1: Expression, e2: Expression): Expression =
    Binary(BinaryOperator.LogicalAnd, e1, e2, Type.Bool, SourceLocation.Unknown)

  /**
    * Returns the logical disjunction of the two expressions `e1` and `e2`.
    */
  def ∨(e1: Expression, e2: Expression): Expression =
    Binary(BinaryOperator.LogicalOr, e1, e2, Type.Bool, SourceLocation.Unknown)

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

  implicit class RichLambda(val f: SimplifiedAst.Definition.Constant) {
    def apply(e1: Expression): Expression = {
      val t = f.tpe.asInstanceOf[Type.Lambda]
      val l = Expression.Lambda(f.formals, f.exp, t, f.loc)
      Expression.Apply(l, List(e1), t.retTpe, SourceLocation.Unknown)
    }

    def apply(e1: Expression, e2: Expression): Expression = {
      val t = f.tpe.asInstanceOf[Type.Lambda]
      val l = Expression.Lambda(f.formals, f.exp, t, f.loc)
      Expression.Apply(l, List(e1, e2), t.retTpe, SourceLocation.Unknown)
    }
  }

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
      * Returns the top element.
      */
    def ⊤(): Expression = lattice.top

    /**
      * Returns the `true` if `e1` is less than or equal to `e2` according to the partial order.
      */
    def ⊑(e1: Expression, e2: Expression): Expression =
      Apply(lattice.leq, List(e1, e2), e1.tpe, SourceLocation.Unknown)

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
