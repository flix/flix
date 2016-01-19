package ca.uwaterloo.flix.language.phase

import java.nio.file.{Files, Paths}

import ca.uwaterloo.flix.Flix.FlixError
import ca.uwaterloo.flix.language.Compiler
import ca.uwaterloo.flix.language.Compiler.InternalCompilerError
import ca.uwaterloo.flix.language.ast.Ast.Annotation
import ca.uwaterloo.flix.language.ast.SimplifiedAst.Definition._
import ca.uwaterloo.flix.language.ast.SimplifiedAst.Expression
import ca.uwaterloo.flix.language.ast.SimplifiedAst.Expression._
import ca.uwaterloo.flix.language.ast.{SimplifiedAst, Type, _}
import ca.uwaterloo.flix.language.phase.Verifier.VerifierError._
import ca.uwaterloo.flix.runtime.PartialEvaluator
import com.microsoft.z3._

object Verifier {

  /**
    * A common super-type for properties of partial orders, lattices and functions.
    */
  sealed trait Property {
    /**
      * The formula corresponding to the property. May or may not be a theorem.
      */
    val formula: Formula


    def fail(env0: Map[String, Expression]): VerifierError
  }

  object Property {

    /**
      * Associativity.
      */
    case class Associativity(f: SimplifiedAst.Expression.Lambda) extends Property {
      val formula = {
        val tpe = f.args.head.tpe
        val (x, y, z) = (mkVar2("x", tpe), mkVar2("y", tpe), mkVar2("z", tpe))

        ∀(x, y, z)(≡(f(x, f(y, z)), f(f(x, y), z)))
      }

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
    case class Commutativity(f: SimplifiedAst.Expression.Lambda) extends Property {
      val formula = {
        val tpe = f.args.head.tpe
        val (x, y) = (mkVar2("x", tpe), mkVar2("y", tpe))

        ∀(x, y)(≡(f(x, y), f(y, x)))
      }

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
    case class Strict1(f: Expression.Lambda, root: SimplifiedAst.Root) extends Property {
      val formula = {
        val (tpe :: Nil) = f.tpe.args
        val retTpe = f.tpe.retTpe
        val argLat = root.lattices(tpe)
        val retLat = root.lattices(retTpe)
        ∀()(≡(f(argLat.bot), retLat.bot))
      }

      def fail(env0: Map[String, Expression]): VerifierError = {
        StrictError(f.loc)
      }
    }

    /**
      * The function `f` must be strict in all its arguments.
      */
    case class Strict2(f: Expression.Lambda, root: SimplifiedAst.Root) extends Property {
      val formula = {
        val (tpe1 :: tpe2 :: Nil) = f.tpe.args
        val retTpe = f.tpe.retTpe
        val (arg1Lat, arg2Lat) = (root.lattices(tpe1), root.lattices(tpe2))
        val retLat = root.lattices(retTpe)

        val (x, y) = (mkVar2("x", tpe1), mkVar2("y", tpe2))

        ∀(x, y)(∧(≡(f(arg1Lat.bot, y), retLat.bot), ≡(f(x, arg2Lat.bot), retLat.bot)))

      }

      def fail(env0: Map[String, Expression]): VerifierError = {
        StrictError(f.loc)
      }
    }

    /**
      * The function `f` must be monotone in all its arguments.
      */
    case class Monotone1(f: Expression.Lambda, root: SimplifiedAst.Root) extends Property {
      val formula = {
        val lat1 = latticeOps(root.lattices(f.args.head.tpe))
        val lat2 = latticeOps(root.lattices(f.tpe.retTpe))

        val (x, y) = (lat1.mkVar("x"), lat1.mkVar("y"))

        ∀(x, y)(→(lat1.⊑(x, y), lat2.⊑(f(x), f(y))))
      }

      def fail(env0: Map[String, Expression]): VerifierError = {
        MonotoneError(f.loc)
      }
    }

    /**
      * The function `f` must be monotone in all its arguments.
      */
    case class Monotone2(f: Expression.Lambda, root: SimplifiedAst.Root) extends Property {
      val formula = {
        val (tpe1 :: tpe2 :: Nil) = f.args.map(_.tpe)
        val (lat1, lat2) = (latticeOps(root.lattices(tpe1)), latticeOps(root.lattices(tpe2)))
        val lat3 = latticeOps(root.lattices(f.tpe.retTpe))

        val (x1, y1, x2, y2) = (lat1.mkVar("x1"), lat1.mkVar("y1"), lat2.mkVar("x2"), lat2.mkVar("y2"))

        ∀(x1, y1, x2, y2)(→(∧(lat1.⊑(x1, x2), lat2.⊑(y1, y2)), lat3.⊑(f(x1, y1), f(x2, y2))))
      }

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

          ∀(x)(Expression.Binary(BinaryOperator.GreaterEqual, lattice.acc(x), Expression.Int(0), Type.Bool, SourceLocation.Unknown))
        }

        def fail(env0: Map[String, Expression]): VerifierError = {
          val x = env0.get("x")
          HeightNonNegativeError(x, lattice.acc.loc)
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
              Expression.Binary(BinaryOperator.Greater, lattice.acc(x), lattice.acc(y), Type.Bool, SourceLocation.Unknown)
            ))
        }

        def fail(env0: Map[String, Expression]): VerifierError = {
          val x = env0.get("x")
          val y = env0.get("y")
          HeightStrictlyDecreasingError(x, y, lattice.acc.loc)
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
  sealed trait VerifierError extends FlixError

  object VerifierError {

    implicit val consoleCtx = Compiler.ConsoleCtx

    /**
      * An error raised to indicate that a function is not associative.
      */
    case class AssociativityError(x: Option[Expression], y: Option[Expression], z: Option[Expression], loc: SourceLocation) extends VerifierError {
      val format =
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
      val format =
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
      val format =
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
      val format =
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
      val format =
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
      val format =
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
      val format =
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
      val format =
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
      val format =
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
      val format =
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
      val format =
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
      val format =
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
      val format =
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
      val format =
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
      val format =
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
    * Attempts to verify all properties of every function in the given AST `root`.
    *
    * Returns a list of errors. If the list is empty, all properties were successfully verified.
    */
  def checkAll(root: SimplifiedAst.Root): List[VerifierError] = {
    // find all properties to verify.
    val properties = collectProperties(root)

    // attempt to verify each property.
    properties flatMap checkProperty
  }

  /**
    * Attempts to verify the given `property`.
    *
    * Returns `None` if the property is satisfied.
    * Otherwise returns `Some` containing the verification error.
    */
  def checkProperty(property: Property): Option[VerifierError] = {
    // the base expression
    val exp0 = property.formula.e

    // a sequence of environments under which the base expression must hold.
    val envs = enumerate(property.formula.q)

    // attempt to verify that the property holds under each environment.
    val violations = envs flatMap {
      case env0 => PartialEvaluator.eval(exp0, env0, identity) match {
        case Expression.True =>
          // Case 1: The partial evaluator proved the property.
          Nil
        case Expression.False =>
          // Case 2: The partial evaluator disproved the property.
          List(property.fail(env0))
        case residual =>
          // Case 3: The partial evaluator reduced the expression, but it is still residual.
          // Must translate the expression into an SMT formula and attempt to prove it.

          mkContext(ctx => {
            // Check if the negation of the expression has a model.
            // If so, the property does not hold.
            val q = ctx.mkNot(visitBoolExpr(residual, ctx))
            checkSat(q, ctx) match {
              case Result.Unsatisfiable =>
                // Case 3.1: The formula is UNSAT, i.e. the property HOLDS.
                Nil
              case Result.Satisfiable(model) =>
                // Case 3.2: The formula is SAT, i.e. a counter-example to the property exists.
                List(property.fail(model2env(model)))
              case Result.Unknown =>
                // Case 3.3: It is unknown whether the formula has a model.
                List(property.fail(Map.empty))
            }
          })
      }
    }

    violations.headOption
  }

  /**
    * Enumerates all possible environments of the given universally quantified variables.
    */
  def enumerate(q: List[Var]): List[Map[String, Expression]] = {
    def visit(tpe: Type): List[Expression] = tpe match {
      case Type.Unit => List(Expression.Unit)
      case Type.Bool => List(Expression.True, Expression.False)
      case Type.Int => List(Expression.Var(???, Type.Int, SourceLocation.Unknown)) // TODO: Need genSym
      case Type.Tuple(elms) => ???
      case Type.Enum(cases) => ???
      case _ => throw new UnsupportedOperationException("Not Yet Implemented. Sorry.")
    }

    ???
  }

  /**
    * Returns all the verification conditions required to ensure the safety of the given AST `root`.
    */
  def collectProperties(root: SimplifiedAst.Root): List[Property] = {
    // Collect partial order properties.
    val partialOrderProperties = lattices(root) flatMap {
      case l => List(
        Property.PartialOrder.Reflexivity(l),
        Property.PartialOrder.AntiSymmetry(l),
        Property.PartialOrder.Transitivity(l)
      )
    }

    // Collect lattice properties.
    val latticeProperties = lattices(root) flatMap {
      case l => List(
        Property.JoinSemiLattice.LeastElement(l),
        Property.JoinSemiLattice.UpperBound(l),
        Property.JoinSemiLattice.LeastUpperBound(l),
        Property.MeetSemiLattice.GreatestElement(l),
        Property.MeetSemiLattice.LowerBound(l),
        Property.MeetSemiLattice.GreatestLowerBound(l),
        Property.AscendingChainCondition.HeightNonNegative(l),
        Property.AscendingChainCondition.HeightStrictlyDecreasing(l)
      )
    }

    // Collect function properties.
    val functionProperties = lambdas(root) flatMap {
      case f if f.annotations.isUnchecked => Nil
      case f => f.annotations.annotations.flatMap {
        case Annotation.Associative(loc) => Some(Property.Associativity(f))
        case Annotation.Commutative(loc) => Some(Property.Commutativity(f))
        case Annotation.Strict(loc) => f.args match {
          case Nil => None // A constant function is always strict.
          case a :: Nil => Some(Property.Strict1(f, root))
          case a1 :: a2 :: Nil => Some(Property.Strict2(f, root))
          case _ => throw new UnsupportedOperationException("Not Yet Implemented. Sorry.")
        }
        case Annotation.Monotone(loc) => f.args match {
          case Nil => None // A constant function is always monotone.
          case a :: Nil => Some(Property.Monotone1(f, root))
          case a1 :: a2 :: Nil => Some(Property.Monotone2(f, root))
          case _ => throw new UnsupportedOperationException("Not Yet Implemented. Sorry.")
        }
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
    Var(Name.Ident(SourcePosition.Unknown, s, SourcePosition.Unknown), tpe, SourceLocation.Unknown)
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

  implicit class RichLambda(val f: Expression.Lambda) {
    def apply(e1: Expression): Expression =
      Expression.Apply(f, List(e1), f.tpe.retTpe, SourceLocation.Unknown)

    def apply(e1: Expression, e2: Expression): Expression =
      Expression.Apply(f, List(e1, e2), f.tpe.retTpe, SourceLocation.Unknown)
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
  // TODO: Avoid cast
  def lambdas(root: SimplifiedAst.Root): List[Expression.Lambda] =
    root.constants.values.map(_.exp.asInstanceOf[Expression.Lambda]).toList

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
  def visitBitVecExpr(e0: Expression, ctx: Context): BitVecExpr = e0 match {
    case Int(i) => ctx.mkBV(i, 32)
    case Unary(op, e1, tpe, loc) => op match {
      case UnaryOperator.BitwiseNegate => ctx.mkBVNot(visitBitVecExpr(e1, ctx))
      case _ => throw new InternalCompilerError(s"Illegal unary operator: $op.")
    }
    case Binary(op, e1, e2, tpe, loc) => op match {
      case BinaryOperator.BitwiseAnd => ctx.mkBVAND(visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx))
      case BinaryOperator.BitwiseOr => ctx.mkBVOR(visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx))
      case BinaryOperator.BitwiseXor => ctx.mkBVXOR(visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx))
      case BinaryOperator.BitwiseLeftShift => ctx.mkBVSHL(visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx))
      case BinaryOperator.BitwiseRightShift => ctx.mkBVLSHR(visitBitVecExpr(e1, ctx), visitBitVecExpr(e2, ctx))
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
      case BinaryOperator.Modulo => throw new UnsupportedOperationException("Not Yet Implemented. Sorry.")
      case BinaryOperator.BitwiseAnd => throw new UnsupportedOperationException("Not Yet Implemented. Sorry.")
      case BinaryOperator.BitwiseOr => throw new UnsupportedOperationException("Not Yet Implemented. Sorry.")
      case BinaryOperator.BitwiseXor => throw new UnsupportedOperationException("Not Yet Implemented. Sorry.")
      case BinaryOperator.BitwiseLeftShift => throw new UnsupportedOperationException("Not Yet Implemented. Sorry.")
      case BinaryOperator.BitwiseRightShift => throw new UnsupportedOperationException("Not Yet Implemented. Sorry.")
      case _ => throw new InternalCompilerError(s"Illegal binary operator: $op.")
    }
    case IfThenElse(e1, e2, e3, tpe, loc) =>
      val f1 = visitBoolExpr(e1, ctx)
      val f2 = visitArithExpr(e2, ctx)
      val f3 = visitArithExpr(e3, ctx)
      ??? // TODO
    case _ => throw new InternalCompilerError(s"Expected int expression but got: ${e0.tpe}")
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

  // TODO: Can we use un-interpreted functions for anything?

  def model2env(model: Model): Map[String, Expression] = ???

  // TODO: remove
  def main(args: Array[String]): Unit = {
    mkContext(ctx => {

      val x = ctx.mkIntConst("x")
      val y = ctx.mkIntConst("y")

      val one = ctx.mkInt(1)
      val two = ctx.mkInt(1)

      val yPlusOne = ctx.mkAdd(y, one)

      val c1 = ctx.mkLt(x, yPlusOne)
      val c2 = ctx.mkGe(x, two)

      val q = ctx.mkAnd(c1, c2)
      Console.println("model for: x < y + 1")
      val m = checkSat(q, ctx)
      println(m)
    })

    ////////////////////////////////
    ////////////////////////////////
    ////////////////////////////////
    ////////////////////////////////
    println()
    println()
    println()
    println()
    mkContext(ctx => {

      val x = ctx.mkIntConst("x")
      val q = ctx.mkLt(ctx.mkAdd(x, ctx.mkInt(1)), x)
      println(q)
      Console.println("model for: not x + 1 < x")
      val m = checkSat(ctx.mkNot(q), ctx)
      println(m)
    })
  }


}
