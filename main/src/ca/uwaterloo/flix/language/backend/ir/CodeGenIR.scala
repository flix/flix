package ca.uwaterloo.flix.language.backend.ir

import ca.uwaterloo.flix.language.ast.{Name, BinaryOperator, SourceLocation, UnaryOperator}
import ca.uwaterloo.flix.runtime.Value

// TODO: Rename?
sealed trait CodeGenIR

object CodeGenIR {

  case class ValuePool(strings: StringPool) {
    def valueOf(int: Int, tpe: CodeGenIR.Type): Value = ???
  }

  case class StringPool(xs: Array[String])

  sealed trait Definition

  object Definition {

    case class Function(name: Name.Resolved, args: List[ArgVar], body: CodeGenIR.Expression, tpe: CodeGenIR.Type, loc: SourceLocation) extends CodeGenIR.Definition

  }

  sealed trait Expression

  object Expression {

    /**
      * An AST node that represents a literal integer expression.
      *
      * @param value the integer value.
      * @param tpe the type of the integer.
      * @param loc the source location of the integer.
      */
    case class Int(value: Int, tpe: CodeGenIR.Type, loc: SourceLocation) extends CodeGenIR.Expression


    case class Var(localVar: CodeGenIR.LocalVar, tpe: CodeGenIR.Type, loc: SourceLocation) extends CodeGenIR.Expression


    case class Apply(name: Name.Resolved, args: List[CodeGenIR.Expression], tpe: CodeGenIR.Type, loc: SourceLocation) extends CodeGenIR.Expression


    case class Let(localVar: CodeGenIR.LocalVar, exp1: CodeGenIR.Expression, exp2: CodeGenIR.Expression, tpe: CodeGenIR.Type, loc: SourceLocation) extends CodeGenIR.Expression

    /**
      * An AST node that represents a unary expression.
      *
      * @param op the unary operator.
      * @param exp the expression.
      * @param tpe the type of the expression.
      * @param loc the source location of the expression.
      */
    case class Unary(op: UnaryOperator, exp: CodeGenIR.Expression, tpe: CodeGenIR.Type, loc: SourceLocation) extends CodeGenIR.Expression

    /**
      * An AST node that represents a binary expression.
      *
      * @param op the binary operator.
      * @param exp1 the left expression.
      * @param exp2 the right expression.
      * @param tpe the type of the expression.
      * @param loc the source location of the expression.
      */
    case class Binary(op: BinaryOperator, exp1: CodeGenIR.Expression, exp2: CodeGenIR.Expression, tpe: CodeGenIR.Type, loc: SourceLocation) extends CodeGenIR.Expression

    /**
      * An AST node that represents an if-then-else expression.
      *
      * @param exp1 the conditional expression.
      * @param exp2 the consequent expression.
      * @param exp3 the alternative expression.
      * @param tpe the type of the consequent and alternative expression.
      * @param loc the source location of the expression.
      */
    case class IfThenElse(exp1: CodeGenIR.Expression, exp2: CodeGenIR.Expression, exp3: CodeGenIR.Expression, tpe: CodeGenIR.Type, loc: SourceLocation) extends CodeGenIR.Expression

    case class Tag(tag: TagName, exp: CodeGenIR.Expression, tpe: CodeGenIR.Type, loc: SourceLocation) extends CodeGenIR.Expression

    case class Tuple(elms: List[CodeGenIR.Expression], tpe: CodeGenIR.Type, loc: SourceLocation) extends CodeGenIR.Expression

    case class Set(elms: List[CodeGenIR.Expression], tpe: CodeGenIR.Type, loc: SourceLocation) extends CodeGenIR.Expression

    // TODO: ElementAt( (1,2), 0) = 1
    case class ElementAt(exp1: CodeGenIR.Expression, exp2: CodeGenIR.Expression, tpe: CodeGenIR.Type, loc: SourceLocation) extends CodeGenIR.Expression

    case class DerefTag() extends CodeGenIR.Expression

    case class Error(loc: SourceLocation) extends CodeGenIR.Expression

  }

  /**
    * A common super-type for types.
    */
  sealed trait Type

  object Type {

    /**
      * The type of booleans.
      */
    case object Bool extends CodeGenIR.Type

    /**
      * The type of 8-bit signed integers.
      */
    case object Int8 extends CodeGenIR.Type

    /**
      * The type of 16-bit signed integers.
      */
    case object Int16 extends CodeGenIR.Type

    /**
      * The type of 32-bit signed integers.
      */
    case object Int32 extends CodeGenIR.Type

    /**
      * The type of 64-bit signed integers.
      */
    case object Int64 extends CodeGenIR.Type

    /**
      * The type of 8-bit unsigned integers.
      */
    case object UInt8 extends CodeGenIR.Type

    /**
      * The type of 16-bit unsigned integers.
      */
    case object UInt16 extends CodeGenIR.Type

    /**
      * The type of 32-bit unsigned integers.
      */
    case object UInt32 extends CodeGenIR.Type

    /**
      * The type of 64-bit unsigned integers.
      */
    case object UInt64 extends CodeGenIR.Type

    /**
      * The type of a binary encoded tagged value.
      *
      * @param tag the tag name as a byte.
      * @param tpe the tagged type.
      */
    case class Tag(tag: Byte, tpe: CodeGenIR.Type) extends CodeGenIR.Type

    /**
      * The type of a binary encoded tuple.
      */
    case class Tuple(elms: List[CodeGenIR.Type]) extends CodeGenIR.Type


    /**
      *
      */
    case object StrRef extends CodeGenIR.Type

    /**
      *
      */
    case object TagRef extends CodeGenIR.Type

    /**
      *
      */
    case object TupleRef extends CodeGenIR.Type

    /**
      *
      */
    case object SetRef extends CodeGenIR.Type

    /**
      *
      */
    case object MapRef extends CodeGenIR.Type

    /**
      *
      */
    case class Lambda(args: List[CodeGenIR.Type], retTpe: CodeGenIR.Type) extends CodeGenIR.Type

  }

  case class ArgVar(offset: Int) extends CodeGenIR

  case class LocalVar(offset: Int) extends CodeGenIR

  case class TagName(x: Int) extends CodeGenIR

}
