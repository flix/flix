package syntax

import impl.logic.Term
import syntax.Operators._
import syntax.Symbols._

/**
 * Embedded DSL syntax for terms.
 */
object Terms {

  /**
   * Rich Terms
   */
  implicit class RichTerm(t: Term) {
    def fmt: String = t match {
      case Term.Unit => "unit"
      case Term.Bool(b) => b.toString
      case Term.Int(i) => i.toString
      case Term.Str(s) => s
      case Term.Set(xs) => "{" + xs.map(x => x.fmt).mkString(", ") + "}"

      case Term.Var(s) => s.fmt
      case Term.Abs(x, typ, t1) => "λ" + x.fmt + " " + t1.fmt
      case Term.App(t1, t2) => t1.fmt + " " + t2.fmt
      case Term.Let(x, t1, t2) => "let " + x.fmt + " = " + t1.fmt + " in " + t2.fmt

      case Term.UnaryOp(op, t1) => op.fmt + " " + t1.fmt
      case Term.BinaryOp(op, t1, t2) => t1.fmt + " " + op.fmt + " " + t2.fmt
      case Term.IfThenElse(t1, t2, t3) => "if " + t1.fmt + " then " + t2.fmt + " else " + t3.fmt

      case Term.Case(t, cases) => ???

      case Term.Tagged(s, t1, typ) => s.fmt + " " + t1.fmt
      case Term.Tuple2(t1, t2) =>              "(" + t1.fmt + "," + t2.fmt + ")"
      case Term.Tuple3(t1, t2, t3) =>          "(" + t1.fmt + "," + t2.fmt + "," + t3.fmt + ")"
      case Term.Tuple4(t1, t2, t3, t4) =>      "(" + t1.fmt + "," + t2.fmt + "," + t3.fmt + "," + t4.fmt + ")"
      case Term.Tuple5(t1, t2, t3, t4, t5) =>  "(" + t1.fmt + "," + t2.fmt + "," + t3.fmt + "," + t4.fmt + "," + t5.fmt + ")"
    }
  }

}
