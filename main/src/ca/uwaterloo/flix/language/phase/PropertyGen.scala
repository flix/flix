/*
 * Copyright 2015-2016 Magnus Madsen
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

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.ast.Ast.Annotation
import ca.uwaterloo.flix.language.ast.Type
import ca.uwaterloo.flix.language.ast.Type.Lambda
import ca.uwaterloo.flix.language.ast.TypedAst.Definition.BoundedLattice
import ca.uwaterloo.flix.language.ast.TypedAst.Expression
import ca.uwaterloo.flix.language.ast.TypedAst.Expression._
import ca.uwaterloo.flix.util.InternalCompilerException

/**
  * Generates verification conditions. In future these properties will be directly in the Flix source code.
  */
object PropertyGen {

  /**
    * Associativity.
    */
  def mkAssociativity(f: TypedAst.Definition.Constant): TypedAst.Property = {
    val exp = {
      val tpe = f.formals.head.tpe
      val (x, y, z) = (mkVar2("x", tpe), mkVar2("y", tpe), mkVar2("z", tpe))

      ∀(x, y, z)(≡(f(x, f(y, z)), f(f(x, y), z)))
    }

    TypedAst.Property(Law.Associativity, exp, f.loc)
  }

  /**
    * Commutativity.
    */
  def mkCommutativity(f: TypedAst.Definition.Constant): TypedAst.Property = {
    val exp = {
      val tpe = f.formals.head.tpe
      val (x, y) = (mkVar2("x", tpe), mkVar2("y", tpe))

      ∀(x, y)(≡(f(x, y), f(y, x)))
    }

    TypedAst.Property(Law.Commutativity, exp, f.loc)
  }

  /**
    * Properties of Partial Orders.
    */
  object PartialOrder {

    /**
      * Reflexivity.
      */
    def mkReflexivity(lattice: BoundedLattice): TypedAst.Property = {
      val ops = latticeOps(lattice)

      import ops._

      val exp = {
        val x = mkVar("x")

        ∀(x)(⊑(x, x))
      }

      TypedAst.Property(Law.Reflexivity, exp, lattice.loc)
    }

    /**
      * Anti-symmetry.
      */
    def mkAntiSymmetry(lattice: BoundedLattice): TypedAst.Property = {
      val ops = latticeOps(lattice)

      import ops._

      val exp = {
        val (x, y) = (mkVar("x"), mkVar("y"))

        ∀(x, y)(→(∧(⊑(x, y), ⊑(y, x)), ≡(x, y)))
      }

      TypedAst.Property(Law.AntiSymmetry, exp, lattice.loc)
    }

    /**
      * Transitivity.
      */
    def mkTransitivity(lattice: BoundedLattice): TypedAst.Property = {
      val ops = latticeOps(lattice)

      import ops._

      val exp = {
        val (x, y, z) = (mkVar("x"), mkVar("y"), mkVar("z"))

        ∀(x, y, z)(→(∧(⊑(x, y), ⊑(y, z)), ⊑(x, z)))
      }

      TypedAst.Property(Law.Transitivity, exp, lattice.loc)
    }

  }

  /**
    * Properties of Join Semi Lattices.
    */
  object JoinSemiLattice {

    /**
      * The bottom element must be the least element.
      */
    def mkLeastElement(lattice: BoundedLattice): TypedAst.Property = {
      val ops = latticeOps(lattice)

      import ops._

      val exp = {
        val x = mkVar("x")

        ∀(x)(⊑(⊥(), x))
      }

      TypedAst.Property(Law.LeastElement, exp, lattice.loc)
    }

    /**
      * The lub must be an upper bound.
      */
    def mkUpperBound(lattice: BoundedLattice): TypedAst.Property = {

      val ops = latticeOps(lattice)

      import ops._

      val exp = {
        val (x, y) = (mkVar("x"), mkVar("y"))

        ∀(x, y)(∧(⊑(x, ⊔(x, y)), ⊑(y, ⊔(x, y))))
      }

      TypedAst.Property(Law.UpperBound, exp, lattice.loc)
    }

    /**
      * The lub must be the least upper bound.
      */
    def mkLeastUpperBound(lattice: BoundedLattice): TypedAst.Property = {

      val ops = latticeOps(lattice)

      import ops._

      val exp = {
        val (x, y, z) = (mkVar("x"), mkVar("y"), mkVar("z"))

        ∀(x, y, z)(→(∧(⊑(x, z), ⊑(y, z)), ⊑(⊔(x, y), z)))
      }

      TypedAst.Property(Law.LeastUpperBound, exp, lattice.loc)
    }

  }

  /**
    * Properties of Meet Semi Lattices.
    */
  object MeetSemiLattice {

    /**
      * The top element must be the greatest element.
      */
    def mkGreatestElement(lattice: BoundedLattice): TypedAst.Property = {

      val ops = latticeOps(lattice)

      import ops._

      val exp = {
        val x = mkVar("x")

        ∀(x)(⊑(x, ⊤()))
      }

      TypedAst.Property(Law.GreatestElement, exp, lattice.loc)
    }

    /**
      * The glb must be a lower bound.
      */
    def mkLowerBound(lattice: BoundedLattice): TypedAst.Property = {

      val ops = latticeOps(lattice)

      import ops._

      val exp = {
        val (x, y) = (mkVar("x"), mkVar("y"))

        ∀(x, y)(∧(⊑(⊓(x, y), x), ⊑(⊓(x, y), y)))
      }

      TypedAst.Property(Law.LowerBound, exp, lattice.loc)
    }

    /**
      * The glb must be the greatest lower bound.
      */
    def mkGreatestLowerBound(lattice: BoundedLattice): TypedAst.Property = {

      val ops = latticeOps(lattice)

      import ops._

      val exp = {
        val (x, y, z) = (mkVar("x"), mkVar("y"), mkVar("z"))

        ∀(x, y, z)(→(∧(⊑(z, x), ⊑(z, y)), ⊑(z, ⊓(x, y))))
      }

      TypedAst.Property(Law.GreatestLowerBound, exp, lattice.loc)
    }

  }

  /**
    * The function `f` must be strict in all its arguments.
    */
  def mkStrict1(f: TypedAst.Definition.Constant, root: TypedAst.Root): TypedAst.Property = {
    val exp = {
      val lambda = f.tpe.asInstanceOf[Lambda]
      val (tpe :: Nil) = lambda.args
      val retTpe = lambda.retTpe
      val argLat = root.lattices(tpe)
      val retLat = root.lattices(retTpe)
      ∀()(≡(f(argLat.bot), retLat.bot))
    }

    TypedAst.Property(Law.Strict, exp, f.loc)
  }

  /**
    * The function `f` must be strict in all its arguments.
    */
  def mkStrict2(f: TypedAst.Definition.Constant, root: TypedAst.Root): TypedAst.Property = {
    val exp = {
      val (tpe1 :: tpe2 :: Nil) = f.tpe.asInstanceOf[Type.Lambda].args
      val retTpe = f.tpe.asInstanceOf[Type.Lambda].retTpe
      val (arg1Lat, arg2Lat) = (root.lattices(tpe1), root.lattices(tpe2))
      val retLat = root.lattices(retTpe)

      ∀()(≡(f(arg1Lat.bot, arg2Lat.bot), retLat.bot))
    }

    TypedAst.Property(Law.Strict, exp, f.loc)
  }

  /**
    * The function `f` must be monotone in all its arguments.
    */
  def mkMonotone1(f: TypedAst.Definition.Constant, root: TypedAst.Root): TypedAst.Property = {
    val exp = {
      val lat1 = latticeOps(root.lattices(f.formals.head.tpe))
      val lat2 = latticeOps(root.lattices(f.tpe.asInstanceOf[Type.Lambda].retTpe))

      val (x, y) = (lat1.mkVar("x"), lat1.mkVar("y"))

      ∀(x, y)(→(lat1.⊑(x, y), lat2.⊑(f(x), f(y))))
    }

    TypedAst.Property(Law.Monotone, exp, f.loc)
  }

  /**
    * The function `f` must be monotone in all its arguments.
    */
  def mkMonotone2(f: TypedAst.Definition.Constant, root: TypedAst.Root): TypedAst.Property = {
    val exp = {
      val (tpe1 :: tpe2 :: Nil) = f.formals.map(_.tpe)
      val (lat1, lat2) = (latticeOps(root.lattices(tpe1)), latticeOps(root.lattices(tpe2)))
      val lat3 = latticeOps(root.lattices(f.tpe.asInstanceOf[Type.Lambda].retTpe))

      val (x1, y1, x2, y2) = (lat1.mkVar("x1"), lat1.mkVar("y1"), lat2.mkVar("x2"), lat2.mkVar("y2"))

      ∀(x1, y1, x2, y2)(→(∧(lat1.⊑(x1, x2), lat2.⊑(y1, y2)), lat3.⊑(f(x1, y1), f(x2, y2))))
    }

    TypedAst.Property(Law.Monotone, exp, f.loc)
  }

  object AscendingChainCondition {

    /**
      * Height function must be non-negative.
      */
    def mkHeightNonNegative(lattice: BoundedLattice): TypedAst.Property = {
      val ops = latticeOps(lattice)

      import ops._

      val exp = {
        val x = mkVar("x")

        ∀(x)(Expression.Binary(BinaryOperator.GreaterEqual, ???, ???, Type.Bool, SourceLocation.Unknown))
      }

      TypedAst.Property(Law.HeightNonNegative, exp, lattice.loc)
    }

    /**
      * Height function must be decreasing.
      */
    def mkHeightStrictlyDecreasing(lattice: BoundedLattice): TypedAst.Property = {
      val ops = latticeOps(lattice)

      import ops._

      val exp = {
        val (x, y) = (mkVar("x"), mkVar("y"))

        ∀(x, y)(
          →(
            ∧(⊑(x, y), ¬(≡(x, y))),
            Expression.Binary(BinaryOperator.Greater, ???, ???, Type.Bool, SourceLocation.Unknown)
          ))
      }

      TypedAst.Property(Law.HeightStrictlyDecreasing, exp, lattice.loc)
    }

  }

  /**
    * Returns all the verification conditions required to ensure the safety of the given AST `root`.
    */
  def collectProperties(root: TypedAst.Root): TypedAst.Root = {
    val t = System.nanoTime()

    val lattices = root.lattices.values.toList

    // Collect partial order properties.
    val partialOrderProperties = lattices flatMap {
      case l => List(
        PartialOrder.mkReflexivity(l),
        PartialOrder.mkAntiSymmetry(l),
        PartialOrder.mkTransitivity(l)
      )
    }

    // Collect lattice properties.
    val latticeProperties = lattices flatMap {
      case l => List(
        JoinSemiLattice.mkLeastElement(l),
        JoinSemiLattice.mkUpperBound(l),
        JoinSemiLattice.mkLeastUpperBound(l),
        MeetSemiLattice.mkGreatestElement(l),
        MeetSemiLattice.mkLowerBound(l),
        MeetSemiLattice.mkGreatestLowerBound(l)
        //        TypedAst.Property.AscendingChainCondition.HeightNonNegative(l), // TODO
        //        TypedAst.Property.AscendingChainCondition.HeightStrictlyDecreasing(l) // TODO
      )
    }

    // Collect function properties.
    val functionProperties = root.constants.values flatMap {
      case f if f.ann.isUnchecked => Nil
      case f => f.ann.annotations.flatMap {
        case Annotation.Associative(loc) => Some(mkAssociativity(f))
        case Annotation.Commutative(loc) => Some(mkCommutativity(f))
        case Annotation.Strict(loc) => f.formals match {
          case Nil => None // A constant function is always strict.
          case a :: Nil => Some(mkStrict1(f, root))
          case a1 :: a2 :: Nil => Some(mkStrict2(f, root))
          case _ => throw new InternalCompilerException("Strictness currently not supported for functions of arity 3+.")
        }
        case Annotation.Monotone(loc) => f.formals match {
          case Nil => None // A constant function is always monotone.
          case a :: Nil => Some(mkMonotone1(f, root))
          case a1 :: a2 :: Nil => Some(mkMonotone2(f, root))
          case _ => throw new InternalCompilerException("Monotonicity currently not supported for functions of arity 3+.")
        }
        case _ => Nil
      }
    }

    // return all the collected properties.
   // TODO: Sort
    val properties = partialOrderProperties ++ latticeProperties ++ functionProperties

    val e = System.nanoTime() - t
    root.copy(properties = properties, time = root.time.copy(propertyGen = e))
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
  def ∀(q: Expression.Var*)(e: Expression): TypedAst.Expression = {
    val params = q.toList.map(e => Ast.FormalParam(e.ident, e.tpe))
    TypedAst.Expression.Universal(params, e, SourceLocation.Unknown)
  }

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

  implicit class RichLambda(val defn: TypedAst.Definition.Constant) {
    def apply(e1: Expression): Expression = {
      val t = defn.tpe.asInstanceOf[Type.Lambda]
      val r = Expression.Ref(defn.name, t, SourceLocation.Unknown)
      Expression.Apply(r, List(e1), t.retTpe, SourceLocation.Unknown)
    }

    def apply(e1: Expression, e2: Expression): Expression = {
      val t = defn.tpe.asInstanceOf[Type.Lambda]
      val r = Expression.Ref(defn.name, t, SourceLocation.Unknown)
      Expression.Apply(r, List(e1, e2), t.retTpe, SourceLocation.Unknown)
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
      Apply(lattice.leq, List(e1, e2), Type.Bool, SourceLocation.Unknown)

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
