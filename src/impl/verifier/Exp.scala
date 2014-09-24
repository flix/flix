package impl.verifier

import impl.logic._
import syntax.Terms.RichTerm

/**
 * A common type for boolean and integer expressions.
 */
trait Exp

/**
 * A common type for boolean expressions.
 */
sealed trait BoolExp extends Exp {
  def fmt: String = this.toString
}

object BoolExp {

  /**
   * Returns the given term `t` as a boolean expression.
   */
  def compile(t: Term): BoolExp = t match {
    case Term.Bool(true) => True
    case Term.Bool(false) => False
    case Term.UnaryOp(op, t1) => op match {
      case UnaryOperator.Not => Not(compile(t1))
      case _ => throw new RuntimeException(s"Unsupported unary operator $op.")
    }
    case Term.BinaryOp(op, t1, t2) =>
      op match {
        case BinaryOperator.Or => Or(compile(t1), compile(t2))
        case BinaryOperator.And => And(compile(t1), compile(t2))

        case BinaryOperator.Equal => Eq(IntExp.compile(t1), IntExp.compile(t2))
        case BinaryOperator.NotEqual => Not(Eq(IntExp.compile(t1), IntExp.compile(t2)))

        case BinaryOperator.GreaterEqual => NonNeg(IntExp.Minus(IntExp.compile(t1), IntExp.compile(t1)))
        case BinaryOperator.Greater => NonNeg(IntExp.Minus(IntExp.Minus(IntExp.compile(t1), IntExp.compile(t2)), IntExp.Int(1)))
        case BinaryOperator.LessEqual => NonNeg(IntExp.Minus(IntExp.compile(t2), IntExp.compile(t1)))
        case BinaryOperator.Less => NonNeg(IntExp.Minus(IntExp.Minus(IntExp.compile(t2), IntExp.compile(t1)), IntExp.Int(1)))

        case BinaryOperator.Minimum =>
          compile(Term.IfThenElse(Term.BinaryOp(BinaryOperator.LessEqual, t1, t2), t1, t2))
        case BinaryOperator.Maximum =>
          compile(Term.IfThenElse(Term.BinaryOp(BinaryOperator.GreaterEqual, t1, t2), t1, t2))

        case _ => throw new RuntimeException(s"Unsupported binary operator $op.")

      }
    case Term.IfThenElse(t1, t2, t3) => ???
    case _ => throw new RuntimeException(s"Unable to compile ${t.fmt} to an boolean expression.")
  }

  /**
   * A true boolean.
   */
  case object True extends BoolExp

  /**
   * A false boolean.
   */
  case object False extends BoolExp

  /**
   * A negation of an expression.
   */
  case class Not(e1: BoolExp) extends BoolExp

  /**
   * A disjunction of two expressions.
   */
  case class Or(e1: BoolExp, e2: BoolExp) extends BoolExp

  /**
   * A conjunction of two expressions.
   */
  case class And(e1: BoolExp, e2: BoolExp) extends BoolExp

  /**
   * A equality of two integer expressions.
   */
  case class Eq(e1: IntExp, e2: IntExp) extends BoolExp

  /**
   * A non-negative integer expression.
   */
  case class NonNeg(e1: IntExp) extends BoolExp

}

/**
 * A common type for integer expressions.
 */
sealed trait IntExp extends Exp {
  /**
   * Returns a string representation of the expression in the SMT-LIB format.
   */
  def fmt: String = {
    def visit(c: IntExp, indent: Int): String = this match {
      case IntExp.Var(x) => x.s
      case IntExp.Int(i) => i.toString
      case IntExp.Plus(e1, e2) => "(+" + visit(e1, indent) + " " + visit(e2, indent) + ")"
      case IntExp.Minus(e1, e2) => "(-" + visit(e1, indent) + " " + visit(e2, indent) + ")"
      case IntExp.Times(e1, e2) => "(*" + visit(e1, indent) + " " + visit(e2, indent) + ")"
      case IntExp.Divide(e1, e2) => "(/" + visit(e1, indent) + " " + visit(e2, indent) + ")"
    }

    visit(this, 0)
  }
}

object IntExp {

  /**
   * Returns the given term `t` as an integer expression.
   */
  def compile(t: Term): IntExp = t match {
    case Term.Var(x) => Var(x)
    case Term.Int(i) => Int(i)
    case Term.UnaryOp(op, t1) => op match {
      case UnaryOperator.UnaryPlus => compile(t1)
      case UnaryOperator.UnaryMinus => Minus(Int(0), compile(t1))
      case _ => throw new RuntimeException(s"Unsupported unary operator $op.")
    }
    case Term.BinaryOp(op, t1, t2) =>
      val e1 = compile(t1)
      val e2 = compile(t2)
      op match {
        case BinaryOperator.Plus => Plus(e1, e2)
        case BinaryOperator.Minimum => Minus(e1, e2)
        case BinaryOperator.Times => Times(e1, e2)
        case BinaryOperator.Divide => Divide(e1, e2)
        case _ => throw new RuntimeException(s"Unsupported binary operator $op.")
      }

    case _ => throw new RuntimeException(s"Unable to compile ${t.fmt} to an integer expression.")
  }

  /**
   * An integer variable.
   */
  case class Var(x: Symbol.VariableSymbol) extends IntExp

  /**
   * A integer literal.
   */
  case class Int(i: scala.Int) extends IntExp

  /**
   * A sum of two expressions.
   */
  case class Plus(e1: IntExp, e2: IntExp) extends IntExp

  /**
   * The difference of two expressions.
   */
  case class Minus(e1: IntExp, e2: IntExp) extends IntExp

  /**
   * The product of two expressions.
   */
  case class Times(e1: IntExp, e2: IntExp) extends IntExp

  /**
   * The division of two expressions.
   */
  case class Divide(e1: IntExp, e2: IntExp) extends IntExp

}

