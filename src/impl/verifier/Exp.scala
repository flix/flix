package impl.verifier

import impl.logic.Term.BinaryOp
import impl.logic.{BinaryOperator, UnaryOperator, Symbol, Term}
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


    case Term.BinaryOp(op, t1, t2) =>
      import impl.logic.{BinaryOperator => Op}
      op match {
        case Op.Or => BoolExp.Or(compile(t1), compile(t2))
        case Op.And => BoolExp.And(compile(t1), compile(t2))


      }
  }


  case object True extends BoolExp

  case object False extends BoolExp

  case class Not(e1: BoolExp) extends BoolExp

  case class Or(e1: BoolExp, e2: BoolExp) extends BoolExp

  case class And(e1: BoolExp, e2: BoolExp) extends BoolExp

  case class Ite(e1: BoolExp, e2: BoolExp, e3: BoolExp) extends BoolExp

  case class Eq(e1: IntExp, e2: IntExp) extends BoolExp

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

