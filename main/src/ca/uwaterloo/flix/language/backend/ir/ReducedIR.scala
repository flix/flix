package ca.uwaterloo.flix.language.backend.ir

import ca.uwaterloo.flix.language.ast.{Name, BinaryOperator, SourceLocation, UnaryOperator}
import ca.uwaterloo.flix.runtime.Value

sealed trait ReducedIR

object ReducedIR {

  case class ValuePool(strings: StringPool) {
    def valueOf(int: Int, tpe: ReducedIR.Type): Value = ???
  }

  case class StringPool(xs: Array[String])

  sealed trait Definition

  object Definition {

    /**
     * An AST node that represents the definition of a function.
     *
     * @param name the resolved name of the function.
     * @param args the arguments of the function, for debugging purposes.
     * @param body the expression body of the function.
     * @param tpe the (lambda) type of the function.
     * @param loc the source location of the function definition.
     */
    case class Function(name: Name.Resolved, args: List[String], body: ReducedIR.Expression, tpe: ReducedIR.Type.Lambda, loc: SourceLocation) extends ReducedIR.Definition {
      val descriptor = tpe.descriptor
    }

  }

  sealed trait Expression

  // TODO: Consider whether we want Exp8, Exp16, Exp32, Exp64 or if that information is associated with the type?

  object Expression {

    // NB: Offset is *always* in bits.

    case class LoadBool(e: ReducedIR.Expression, offset: Int) extends ReducedIR.Expression

    case class LoadInt8(e: ReducedIR.Expression, offset: Int) extends ReducedIR.Expression

    case class LoadInt16(e: ReducedIR.Expression, offset: Int) extends ReducedIR.Expression

    case class LoadInt32(e: ReducedIR.Expression, offset: Int) extends ReducedIR.Expression

    case class StoreBool(e: ReducedIR.Expression, offset: Int, v: ReducedIR.Expression) extends ReducedIR.Expression

    case class StoreInt8(e: ReducedIR.Expression, offset: Int, v: ReducedIR.Expression) extends ReducedIR.Expression

    case class StoreInt16(e: ReducedIR.Expression, offset: Int, v: ReducedIR.Expression) extends ReducedIR.Expression

    case class StoreInt32(e: ReducedIR.Expression, offset: Int, v: ReducedIR.Expression) extends ReducedIR.Expression


    /**
      * An AST node that represents a constant integer literal.
      * Note that calling this case class "Int" conflicts with scala.Int.
      *
      * @param value the integer value.
      * @param tpe the type of the integer.
      * @param loc the source location of the integer.
      */
    case class Const(value: Long, tpe: ReducedIR.Type, loc: SourceLocation) extends ReducedIR.Expression

    /**
     * A typed AST node representing a local variable expression (i.e. a parameter or let-bound variable).
     *
     * @param localVar the local variable being referenced.
     * @param tpe the type of the variable.
     * @param loc the source location of the variable.
     */
    case class Var(localVar: ReducedIR.LocalVar, tpe: ReducedIR.Type, loc: SourceLocation) extends ReducedIR.Expression

    /**
     * A typed AST node representing a function call.
     *
     * @param name the name of the function being called.
     * @param args the function arguments.
     * @param tpe the return type of the function.
     * @param loc the source location.
     */
    case class Apply(name: Name.Resolved, args: List[ReducedIR.Expression], tpe: ReducedIR.Type, loc: SourceLocation) extends ReducedIR.Expression

    /**
     * A typed AST node representing a let expression.
     *
     * @param localVar the bound variable.
     * @param exp1 the value of the bound variable.
     * @param exp2 the body expression in which the bound variable is visible.
     * @param tpe the type of the expression (which is equivalent to the type of the body expression).
     * @param loc the source location.
     */
    case class Let(localVar: ReducedIR.LocalVar, exp1: ReducedIR.Expression, exp2: ReducedIR.Expression, tpe: ReducedIR.Type, loc: SourceLocation) extends ReducedIR.Expression

    /**
      * An AST node that represents a unary expression.
      *
      * @param op the unary operator.
      * @param exp the expression.
      * @param tpe the type of the expression.
      * @param loc the source location of the expression.
      */
    case class Unary(op: UnaryOperator, exp: ReducedIR.Expression, tpe: ReducedIR.Type, loc: SourceLocation) extends ReducedIR.Expression

    /**
      * An AST node that represents a binary expression.
      *
      * @param op the binary operator.
      * @param exp1 the left expression.
      * @param exp2 the right expression.
      * @param tpe the type of the expression.
      * @param loc the source location of the expression.
      */
    case class Binary(op: BinaryOperator, exp1: ReducedIR.Expression, exp2: ReducedIR.Expression, tpe: ReducedIR.Type, loc: SourceLocation) extends ReducedIR.Expression

    /**
      * An AST node that represents an if-then-else expression.
      *
      * @param exp1 the conditional expression.
      * @param exp2 the consequent expression.
      * @param exp3 the alternative expression.
      * @param tpe the type of the consequent and alternative expression.
      * @param loc the source location of the expression.
      */
    case class IfThenElse(exp1: ReducedIR.Expression, exp2: ReducedIR.Expression, exp3: ReducedIR.Expression, tpe: ReducedIR.Type, loc: SourceLocation) extends ReducedIR.Expression

    case class Tag(name: Name.Resolved, tag: String, exp: ReducedIR.Expression, tpe: ReducedIR.Type, loc: SourceLocation) extends ReducedIR.Expression

    // THIS RETURNS A BOOLEAN
    case class TagOf(exp: ReducedIR.Expression, name: Name.Resolved, tag: String, tpe: ReducedIR.Type, loc: SourceLocation) extends ReducedIR.Expression

    case class Tuple(elms: List[ReducedIR.Expression], tpe: ReducedIR.Type, loc: SourceLocation) extends ReducedIR.Expression

    // TODO: ElementAt( (1,2), 0) = 1
    case class TupleAt(base: ReducedIR.Expression, offset: Int, tpe: ReducedIR.Type, loc: SourceLocation) extends ReducedIR.Expression

    case class Set(elms: List[ReducedIR.Expression], tpe: ReducedIR.Type, loc: SourceLocation) extends ReducedIR.Expression

    case class Error(loc: SourceLocation) extends ReducedIR.Expression

  }

  /**
    * A common super-type for types.
    */
  sealed trait Type {
    val descriptor: String
  }

  object Type {

    /**
      * The type of booleans.
      * Maps to a JVM boolean, even though all boolean operations are done as int operations.
      */
    case object Bool extends ReducedIR.Type {
      override val descriptor = "Z"
    }

    /**
      * The type of 8-bit signed integers.
      * Maps to a JVM int because Flix doesn't have bytes.
      */
    case object Int8 extends ReducedIR.Type {
      override val descriptor = "I"
    }

    /**
      * The type of 16-bit signed integers.
      * Maps to a JVM int because Flix doesn't have shorts.
      */
    case object Int16 extends ReducedIR.Type {
      override val descriptor = "I"
    }

    /**
      * The type of 32-bit signed integers.
      * Maps to a JVM int.
      */
    case object Int32 extends ReducedIR.Type {
      // Maps to a JVM int
      override val descriptor = "I"
    }

    /**
      * The type of 64-bit signed integers.
      * Maps to a JVM long.
      */
    case object Int64 extends ReducedIR.Type {
      override val descriptor = "J"
    }

    case class Tag(name: Name.Resolved, ident: Name.Ident, tpe: ReducedIR.Type) extends ReducedIR.Type {
      // Maps to a Flix tag
      override val descriptor = ???
    }

    case class Enum(cases: Map[String, ReducedIR.Type.Tag]) extends ReducedIR.Type {
      // Maps to a Flix enum
      override val descriptor = ???
    }

    case class Tuple(elms: List[ReducedIR.Type]) extends ReducedIR.Type {
      // Maps to a Scala tuple
      override val descriptor = ???
    }

    case class Set(elmTyp: ReducedIR.Type) extends ReducedIR.Type {
      // Maps to a Scala set
      override val descriptor = ???
    }

    case class Lambda(args: List[ReducedIR.Type], retTpe: ReducedIR.Type) extends ReducedIR.Type {
      override val descriptor = s"""(${ args.map(_.descriptor).mkString })${retTpe.descriptor}"""
    }

  }

  /**
   * A local variable that is being referenced.
   *
   * @param offset the (0-based) local variable slot in the JVM method.
   * @param name the name of the variable, for debugging purposes
   */
  case class LocalVar(offset: Int, name: String) extends ReducedIR


}
