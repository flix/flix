package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.ast.Ast.Annotation
import ca.uwaterloo.flix.language.ast.Type
import ca.uwaterloo.flix.language.ast.TypedAst.Definition.BoundedLattice
import ca.uwaterloo.flix.language.ast.TypedAst.Expression
import ca.uwaterloo.flix.language.ast.TypedAst.Expression._
import ca.uwaterloo.flix.language.phase.Verifier.VerifierError
import ca.uwaterloo.flix.language.phase.Verifier.VerifierError._

/**
  * Generates verification conditions. In future these properties will be directly in the Flix source code.
  */
object PropertyGen {

  object Property {

    /**
      * Associativity.
      */
    case class Associativity(f: TypedAst.Definition.Constant) extends TypedAst.Property {
      val formula = {
        val tpe = f.formals.head.tpe
        val (x, y, z) = (mkVar2("x", tpe), mkVar2("y", tpe), mkVar2("z", tpe))

        ∀(x, y, z)(≡(f(x, f(y, z)), f(f(x, y), z)))
      }

      val name = "Associativity(" + f.loc.format + ")"

      def fail(env0: Map[String, String]): VerifierError = {
        val x = env0.get("x")
        val y = env0.get("y")
        val z = env0.get("z")
        AssociativityError(x, y, z, f.loc)
      }
    }

    /**
      * Commutativity.
      */
    case class Commutativity(f: TypedAst.Definition.Constant) extends TypedAst.Property {
      val formula = {
        val tpe = f.formals.head.tpe
        val (x, y) = (mkVar2("x", tpe), mkVar2("y", tpe))

        ∀(x, y)(≡(f(x, y), f(y, x)))
      }

      val name = "Commutativity(" + f.loc.format + ")"

      def fail(env0: Map[String, String]): VerifierError = {
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
      case class Reflexivity(lattice: BoundedLattice) extends TypedAst.Property {
        val ops = latticeOps(lattice)

        import ops._

        val formula = {
          val x = mkVar("x")

          ∀(x)(⊑(x, x))
        }

        val name = "Reflexivity(" + lattice.tpe + ")"

        override def fail(env0: Map[String, String]): VerifierError = {
          val x = env0.get("x")
          ReflexivityError(x, lattice.leq.loc)
        }

      }

      /**
        * Anti-symmetry.
        */
      case class AntiSymmetry(lattice: BoundedLattice) extends TypedAst.Property {
        val ops = latticeOps(lattice)

        import ops._

        val formula = {
          val (x, y) = (mkVar("x"), mkVar("y"))

          ∀(x, y)(→(∧(⊑(x, y), ⊑(y, x)), ≡(x, y)))
        }

        val name = "AntiSymmetry(" + lattice.tpe + ")"

        def fail(env0: Map[String, String]): VerifierError = {
          val x = env0.get("x")
          val y = env0.get("y")
          AntiSymmetryError(x, y, lattice.leq.loc)
        }
      }

      /**
        * Transitivity.
        */
      case class Transitivity(lattice: BoundedLattice) extends TypedAst.Property {
        val ops = latticeOps(lattice)

        import ops._

        val formula = {
          val (x, y, z) = (mkVar("x"), mkVar("y"), mkVar("z"))

          ∀(x, y, z)(→(∧(⊑(x, y), ⊑(y, z)), ⊑(x, z)))
        }

        val name = "Transitivity(" + lattice.tpe + ")"

        def fail(env0: Map[String, String]): VerifierError = {
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
      case class LeastElement(lattice: BoundedLattice) extends TypedAst.Property {
        val ops = latticeOps(lattice)

        import ops._

        val formula = {
          val x = mkVar("x")

          ∀(x)(⊑(⊥(), x))
        }

        val name = "LeastElement(" + lattice.tpe + ")"

        def fail(env0: Map[String, String]): VerifierError = {
          val x = env0.get("x")
          LeastElementError(x, lattice.leq.loc)
        }
      }

      /**
        * The lub must be an upper bound.
        */
      case class UpperBound(lattice: BoundedLattice) extends TypedAst.Property {

        val ops = latticeOps(lattice)

        import ops._

        val formula = {
          val (x, y) = (mkVar("x"), mkVar("y"))

          ∀(x, y)(∧(⊑(x, ⊔(x, y)), ⊑(y, ⊔(x, y))))
        }

        val name = "UpperBound(" + lattice.tpe + ")"

        def fail(env0: Map[String, String]): VerifierError = {
          val x = env0.get("x")
          val y = env0.get("y")
          UpperBoundError(x, y, lattice.lub.loc)
        }
      }

      /**
        * The lub must be the least upper bound.
        */
      case class LeastUpperBound(lattice: BoundedLattice) extends TypedAst.Property {

        val ops = latticeOps(lattice)

        import ops._

        val formula = {
          val (x, y, z) = (mkVar("x"), mkVar("y"), mkVar("z"))

          ∀(x, y, z)(→(∧(⊑(x, z), ⊑(y, z)), ⊑(⊔(x, y), z)))
        }

        val name = "LeastUpperBound(" + lattice.tpe + ")"

        def fail(env0: Map[String, String]): VerifierError = {
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
      case class GreatestElement(lattice: BoundedLattice) extends TypedAst.Property {
        val ops = latticeOps(lattice)

        import ops._

        val formula = {
          val x = mkVar("x")

          ∀(x)(⊑(x, ⊤()))
        }

        val name = "GreatestElement(" + lattice.tpe + ")"

        def fail(env0: Map[String, String]): VerifierError = {
          val x = env0.get("x")
          GreatestElementError(x, lattice.leq.loc)
        }
      }

      /**
        * The glb must be a lower bound.
        */
      case class LowerBound(lattice: BoundedLattice) extends TypedAst.Property {

        val ops = latticeOps(lattice)

        import ops._

        val formula = {
          val (x, y) = (mkVar("x"), mkVar("y"))

          ∀(x, y)(∧(⊑(⊓(x, y), x), ⊑(⊓(x, y), y)))
        }

        val name = "LowerBound(" + lattice.tpe + ")"

        def fail(env0: Map[String, String]): VerifierError = {
          val x = env0.get("x")
          val y = env0.get("y")
          LowerBoundError(x, y, lattice.glb.loc)
        }
      }

      /**
        * The glb must be the greatest lower bound.
        */
      case class GreatestLowerBound(lattice: BoundedLattice) extends TypedAst.Property {

        val ops = latticeOps(lattice)

        import ops._

        val formula = {
          val (x, y, z) = (mkVar("x"), mkVar("y"), mkVar("z"))

          ∀(x, y, z)(→(∧(⊑(z, x), ⊑(z, y)), ⊑(z, ⊓(x, y))))
        }

        val name = "GreatestLowerBound(" + lattice.tpe + ")"

        def fail(env0: Map[String, String]): VerifierError = {
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
    case class Strict1(f: TypedAst.Definition.Constant, root: TypedAst.Root) extends TypedAst.Property {
      val formula = {
        val (tpe :: Nil) = f.tpe.asInstanceOf[Type.Lambda].args
        val retTpe = f.tpe.asInstanceOf[Type.Lambda].retTpe
        val argLat = root.lattices(tpe)
        val retLat = root.lattices(retTpe)
        ∀()(≡(f(argLat.bot), retLat.bot))
      }

      val name = "Strict1(" + f.loc.format + ")"

      def fail(env0: Map[String, String]): VerifierError = {
        StrictError(f.loc)
      }
    }

    /**
      * The function `f` must be strict in all its arguments.
      */
    case class Strict2(f: TypedAst.Definition.Constant, root: TypedAst.Root) extends TypedAst.Property {
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

      def fail(env0: Map[String, String]): VerifierError = {
        StrictError(f.loc)
      }
    }

    /**
      * The function `f` must be monotone in all its arguments.
      */
    case class Monotone1(f: TypedAst.Definition.Constant, root: TypedAst.Root) extends TypedAst.Property {
      val formula = {
        val lat1 = latticeOps(root.lattices(f.formals.head.tpe))
        val lat2 = latticeOps(root.lattices(f.tpe.asInstanceOf[Type.Lambda].retTpe))

        val (x, y) = (lat1.mkVar("x"), lat1.mkVar("y"))

        ∀(x, y)(→(lat1.⊑(x, y), lat2.⊑(f(x), f(y))))
      }

      val name = "Monotone1(" + f.loc.format + ")"

      def fail(env0: Map[String, String]): VerifierError = {
        MonotoneError(f.loc)
      }
    }

    /**
      * The function `f` must be monotone in all its arguments.
      */
    case class Monotone2(f: TypedAst.Definition.Constant, root: TypedAst.Root) extends TypedAst.Property {
      val formula = {
        val (tpe1 :: tpe2 :: Nil) = f.formals.map(_.tpe)
        val (lat1, lat2) = (latticeOps(root.lattices(tpe1)), latticeOps(root.lattices(tpe2)))
        val lat3 = latticeOps(root.lattices(f.tpe.asInstanceOf[Type.Lambda].retTpe))

        val (x1, y1, x2, y2) = (lat1.mkVar("x1"), lat1.mkVar("y1"), lat2.mkVar("x2"), lat2.mkVar("y2"))

        ∀(x1, y1, x2, y2)(→(∧(lat1.⊑(x1, x2), lat2.⊑(y1, y2)), lat3.⊑(f(x1, y1), f(x2, y2))))
      }

      val name = "Monotone2(" + f.loc.format + ")"

      def fail(env0: Map[String, String]): VerifierError = {
        MonotoneError(f.loc)
      }
    }

    object AscendingChainCondition {

      /**
        * Height function must be non-negative.
        */
      case class HeightNonNegative(lattice: BoundedLattice) extends TypedAst.Property {
        val ops = latticeOps(lattice)

        import ops._

        val formula = {
          val x = mkVar("x")

          ∀(x)(Expression.Binary(BinaryOperator.GreaterEqual, ???, ???, Type.Bool, SourceLocation.Unknown))
        }

        val name = "HeightNonNegative(" + lattice.tpe + ")"

        def fail(env0: Map[String, String]): VerifierError = {
          val x = env0.get("x")
          HeightNonNegativeError(x, ???)
        }
      }

      /**
        * Height function must be decreasing.
        */
      case class HeightStrictlyDecreasing(lattice: BoundedLattice) extends TypedAst.Property {
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

        def fail(env0: Map[String, String]): VerifierError = {
          val x = env0.get("x")
          val y = env0.get("y")
          HeightStrictlyDecreasingError(x, y, ???)
        }
      }

    }

  }

  /**
    * Returns all the verification conditions required to ensure the safety of the given AST `root`.
    */
  def collectProperties(root: TypedAst.Root): TypedAst.Root = {
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
        //        TypedAst.Property.AscendingChainCondition.HeightNonNegative(l), // TODO
        //        TypedAst.Property.AscendingChainCondition.HeightStrictlyDecreasing(l) // TODO
      )
    }

    // Collect function properties.
    val functionProperties = root.constants.values flatMap {
      case f if f.ann.isUnchecked => Nil
      case f => f.ann.annotations.flatMap {
        case Annotation.Associative(loc) => Some(Property.Associativity(f))
        case Annotation.Commutative(loc) => Some(Property.Commutativity(f))
        case Annotation.Strict(loc) => f.formals match {
          case Nil => None // A constant function is always strict.
          case a :: Nil => Some(Property.Strict1(f, root))
          case a1 :: a2 :: Nil => Some(Property.Strict2(f, root))
          case _ => throw new UnsupportedOperationException("Not Yet Implemented. Sorry.")
        }
        case Annotation.Monotone(loc) => f.formals match {
          case Nil => None // A constant function is always monotone.
          case a :: Nil => Some(Property.Monotone1(f, root))
          case a1 :: a2 :: Nil => Some(Property.Monotone2(f, root))
          case _ => throw new UnsupportedOperationException("Not Yet Implemented. Sorry.")
        }
        case _ => Nil
      }
    }

    // return all the collected properties.
    val properties = partialOrderProperties ++ latticeProperties ++ functionProperties

    root.copy(properties = properties)
  }


  /////////////////////////////////////////////////////////////////////////////
  // TypedAst.Property DSL                                                   //
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
  def ∀(q: Expression.Var*)(e: Expression): TypedAst.Expression =
    ??? // TODO

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

  implicit class RichLambda(val f: TypedAst.Definition.Constant) {
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
  def latticeOps(l: BoundedLattice): LatticeOps = new LatticeOps(l)

  class LatticeOps(lattice: BoundedLattice) {
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

}
