package ca.uwaterloo.flix.runtime.verifier

import ca.uwaterloo.flix.language.ast.{Name, Type}
import ca.uwaterloo.flix.util.InternalCompilerException

sealed trait SmtExpr {
  def tpe: Type
}

object SmtExpr {

  /**
    * Int8 expression.
    */
  case class Int8(lit: Byte) extends SmtExpr {
    def tpe: Type = Type.Int8
  }

  /**
    * Int16 expression.
    */
  case class Int16(lit: Short) extends SmtExpr {
    def tpe: Type = Type.Int16
  }

  /**
    * Int32 expression.
    */
  case class Int32(lit: Int) extends SmtExpr {
    def tpe: Type = Type.Int32
  }

  /**
    * Int64 expression.
    */
  case class Int64(lit: Long) extends SmtExpr {
    def tpe: Type = Type.Int64
  }

  /**
    * Variables expression.
    */
  case class Var(ident: Name.Ident, tpe: Type) extends SmtExpr


  case class Plus(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assert(numeric(common(e1.tpe, e2.tpe)))

    def tpe: Type = common(e1.tpe, e2.tpe)
  }

  case class Minus(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assert(numeric(common(e1.tpe, e2.tpe)))

    def tpe: Type = common(e1.tpe, e2.tpe)
  }

  case class Times(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assert(numeric(common(e1.tpe, e2.tpe)))

    def tpe: Type = common(e1.tpe, e2.tpe)
  }

  case class Divide(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assert(numeric(common(e1.tpe, e2.tpe)))

    def tpe: Type = common(e1.tpe, e2.tpe)
  }

  case class Modulo(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assert(numeric(common(e1.tpe, e2.tpe)))

    def tpe: Type = common(e1.tpe, e2.tpe)
  }

  case class Exponentiate(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assert(numeric(common(e1.tpe, e2.tpe)))

    def tpe: Type = common(e1.tpe, e2.tpe)
  }

  case class Less(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assert(numeric(common(e1.tpe, e2.tpe)))

    def tpe: Type = Type.Bool
  }

  case class LessEqual(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assert(numeric(common(e1.tpe, e2.tpe)))

    def tpe: Type = Type.Bool
  }

  case class Greater(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assert(numeric(common(e1.tpe, e2.tpe)))

    def tpe: Type = Type.Bool
  }

  case class GreaterEqual(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assert(numeric(common(e1.tpe, e2.tpe)))

    def tpe: Type = Type.Bool
  }

  case class Equal(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assert(e1.tpe == e2.tpe)

    def tpe: Type = Type.Bool
  }

  case class NotEqual(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assert(e1.tpe == e2.tpe)

    def tpe: Type = Type.Bool
  }

  case class Not(e: SmtExpr) extends SmtExpr {
    assert(e.tpe == Type.Bool)

    def tpe: Type = Type.Bool
  }

  case class LogicalAnd(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assert(e1.tpe == Type.Bool && e2.tpe == Type.Bool)

    def tpe: Type = Type.Bool
  }

  case class LogicalOr(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assert(e1.tpe == Type.Bool && e2.tpe == Type.Bool)

    def tpe: Type = Type.Bool
  }

  case class Implication(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assert(e1.tpe == Type.Bool && e2.tpe == Type.Bool)

    def tpe: Type = Type.Bool
  }

  case class Bicondition(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assert(e1.tpe == Type.Bool && e2.tpe == Type.Bool)

    def tpe: Type = Type.Bool
  }

  case class BitwiseNegate(e: SmtExpr) extends SmtExpr {
    assert(numeric(e.tpe))

    def tpe: Type = e.tpe
  }

  case class BitwiseAnd(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assert(numeric(common(e1.tpe, e2.tpe)))

    def tpe: Type = common(e1.tpe, e2.tpe)
  }

  case class BitwiseOr(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assert(numeric(common(e1.tpe, e2.tpe)))

    def tpe: Type = common(e1.tpe, e2.tpe)
  }

  case class BitwiseXor(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assert(numeric(common(e1.tpe, e2.tpe)))

    def tpe: Type = common(e1.tpe, e2.tpe)
  }

  case class BitwiseLeftShift(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assert(numeric(common(e1.tpe, e2.tpe)))

    def tpe: Type = common(e1.tpe, e2.tpe)
  }

  case class BitwiseRightShift(e1: SmtExpr, e2: SmtExpr) extends SmtExpr {
    assert(numeric(common(e1.tpe, e2.tpe)))

    def tpe: Type = common(e1.tpe, e2.tpe)
  }

  // TODO: DOC
  private def common(tpe1: Type, tpe2: Type): Type =
    if (tpe1 == tpe2)
      tpe1
    else
      throw InternalCompilerException(s"Unexpected non-equal types: '$tpe1' and '$tpe2'.")

  // TODO: DOC
  private def numeric(tpe: Type): Boolean = ???

}